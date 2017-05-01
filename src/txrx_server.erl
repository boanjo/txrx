-module(txrx_server).
-behaviour(gen_server).

-include("../include/txrx.hrl").

-export([start_link/0, send_to_serial/1]).
-export([log_terminal/1, log_file/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-export([get_all_devices/0,get_all_sensors/0]).
-export([set_device_info/3, device/2]).
-export([get_device/1, get_sensor/1, get_sensor_value/1]).
-export([reset_serial/0]).
-export([tab2file/0]).
-record(state, {serial, acc_str, sensor_cnt}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->

    process_flag(trap_exit, true),

    gen_event:start({local, txrx_monitor}),

    log_terminal(on),
    %%log_file(on, "log/txrx.txt"),

    DetsSensorFile = "etc/sensor.txt",
    case filelib:is_regular(DetsSensorFile)  of
	false ->
	    ets:new(sensor_table, [named_table, set, {keypos, #sensor.id}, public]);
	true ->
	    file2tab(DetsSensorFile)
    end,
    
    DetsDeviceFile = "etc/device.txt",
    case filelib:is_regular(DetsDeviceFile) of
	false ->
	    ets:new(device_table, [named_table, set, {keypos, #device.id}, public]);
	true ->
	    file2tab(DetsDeviceFile)
    end,

    Self = self(),

    error_logger:info_msg("Startig ~p pid ~p~n ", [?MODULE, Self]),
    Port = get_first_available_port(get_serial_port_prio_list()),
    error_logger:info_msg("Selected Port ~p~n ", [Port]),
    Pid = serial:start([{speed,115200},{open, Port}]),

    gen_server:cast(?MODULE, {start_watchdog, 300}),
    {ok, #state{serial = Pid, acc_str = [], sensor_cnt=0}}.


file2tab(File) ->
    case ets:file2tab(File) of
	{ok, Tab} ->
	    error_logger:info_msg("Table read ~p~n ", [Tab]);
	{error, Reason} ->
	    error_logger:error_msg("Table read ~p~n ", [Reason])
    end.


get_serial_port_prio_list() ->
    ["/dev/ttyACM0","/dev/ttyUSB0",
     "/dev/ttyACM1","/dev/ttyUSB1"].

get_first_available_port([]) ->
    error_logger:error_msg("No Serial Port found~n ", []),
    "/dev/ttyACM0";
get_first_available_port([Port|List]) ->
    case file:read_file_info(Port) of
	{ok, _}-> Port;
	_ -> get_first_available_port(List)
    end.

tab2file() ->
    ets:tab2file(sensor_table, "etc/sensor.txt"),
    ets:tab2file(device_table, "etc/device.txt").


log_terminal(on) ->
    gen_event:add_handler(txrx_monitor, txrx_terminal_logger, []);
log_terminal(off) ->
    gen_event:delete_handler(txrx_monitor, txrx_terminal_logger, []).

log_file(on, File) ->
    gen_event:add_handler(txrx_monitor, txrx_file_logger, [File]);
log_file(off, _File) ->
    gen_event:delete_handler(txrx_monitor, txrx_file_logger, []).

get_next(_Table, '$end_of_table', Acc) ->
    Acc;
get_next(Table, Prev, Acc) ->
    [Val] = ets:lookup(Table, Prev),
    get_next(Table, ets:next(Table, Prev), [Val|Acc]).

get_all_devices() ->
    Table = device_table,
    First = ets:first(Table),
    get_next(Table, First, []).

get_all_sensors() ->
    Table = sensor_table,
    First = ets:first(Table),
    get_next(Table, First, []).

lookup(Table, Id) ->
    Val = ets:lookup(Table, Id),
    case Val of
	[] -> not_found;
	List -> [Ret] = List, 
		Ret
    end.
    
%% We want to set/update the ID
set_device_info(Address, Unit, Id) ->
    case ets:match(device_table, {device, '_', Address, Unit, '$1', '$2'}) of
	[] ->
	    ets:insert(device_table, 
		       #device{id=Id,
			       address=Address,
			       unit=Unit, 
			       state=off, 
			       last_state_change_time=erlang:now()});
	[[State, LastUpdated]] ->
	    ets:insert(device_table, 
		       #device{id=Id,
			       address=Address,
			       unit=Unit,
			       state=State, 
			       last_state_change_time=LastUpdated})
    end,
    ok.

device(Id, Action) ->
    [[Address, Unit]] = ets:match(device_table, {device, Id, '$1', '$2', '_', '_'}),
    send_to_serial("{device," 
		   ++ atom_to_list(Action) ++ "," 
		   ++ integer_to_list(Address) ++ ","
		   ++ integer_to_list(Unit) ++ "}").

get_sensor(Id) ->
    lookup(sensor_table, Id).

get_sensor_value(Id) ->
    Rec = lookup(sensor_table, Id),
    case Rec of
	{sensor, Id, Val, _Min, _Max, _Today, _Battery, _Upd} ->
	    Val;
	_ -> not_found
    end.

get_device(Id) ->
    lookup(device_table, Id).

send_to_serial(Msg) ->
    gen_server:call(?MODULE, {send, list_to_binary(Msg)}).

update_device_state([], _Device) ->
    ok;
update_device_state([Head|Tail], Device) ->
    [Id,Unit] = Head,
    ets:insert(device_table, Device#device{id=Id, unit=Unit}),
    update_device_state(Tail, Device).

get_id([]) ->
    undefined;
get_id([[Id]]) ->
    Id.

reset_serial() ->
    gen_server:call(?MODULE, {reset_serial}).

	

check_serial(0, Pid) ->
    exit(Pid, kill),

    Port = get_first_available_port(get_serial_port_prio_list()),
    error_logger:info_msg("Watchdog elapsed and no sensors heard... Reset! Selected Port ~p~n ", [Port]),

    NewPid = serial:start([{speed,115200},{open, Port}]),
    NewPid;

check_serial(_, Pid) ->
    Pid.
		      
%% callbacks

handle_call({send, Binary}, _From, #state{serial = Pid} = State) ->
    %% error_logger:info_msg("Send ~p! ~n", [binary_to_list(Binary)]),
    Pid ! {send, Binary},
    {reply, ok, State};

handle_call({reset_serial}, _From, #state{serial = Pid, acc_str = Str, sensor_cnt = _Cnt}) ->

    NewPid = check_serial(0, Pid),
    NewState = #state{serial = NewPid, acc_str = Str, sensor_cnt = 0},
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    error_logger:info_msg("handle_call~n", []),
    {reply, Reply, State}.

handle_cast({start_watchdog, Secs}, State) ->
    timer:send_after(Secs*1000, self(), {watchdog_timeout, Secs}),
    error_logger:info_msg("Starting wd ~p~n", [Secs]),
    {noreply, State};

handle_cast(_Msg, State) ->
    error_logger:info_msg("handle_cast~n", []),
    {noreply, State}.

handle_info({data, Data}, State) ->
    %% error_logger:info_msg("Data=~p~n", [Data]),
    {NewAcc, NewCnt} = 
	handle_acc(State#state.acc_str ++ binary_to_list(Data), State#state.sensor_cnt),	    
    {noreply, State#state{acc_str=NewAcc, sensor_cnt=NewCnt}};    

handle_info({watchdog_timeout, Secs}, #state{serial = Pid, acc_str = Str, sensor_cnt = Cnt}) ->
    NewPid = check_serial(Cnt, Pid),
    timer:send_after(Secs*1000, self(), {watchdog_timeout, Secs}),
    
    NewState = #state{serial = NewPid, acc_str = Str, sensor_cnt = 0},
    {noreply, NewState};

handle_info(Info, State) ->
    error_logger:info_msg("handle_info ~p~n", [Info]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



handle_acc(Acc, Cnt) ->
    case string:str(Acc, "\r\n") of
	0 -> 
	    {Acc, Cnt};
	Index ->
	    Line = string:substr(Acc, 1, Index-1),
	    NewAcc = string:substr(Acc, Index+2),
	    %%error_logger:info_msg("~p ~p: ~p~n ", [erlang:localtime(), ?MODULE, Line]),
	    gen_event:notify(txrx_monitor, io_lib:format("~p ~p: ~p~n ", 
							 [erlang:localtime(), 
							  ?MODULE, Line])),
	    try 
		{ok, Tokens, _} = erl_scan:string(Line),
		{ok, B} = erl_parse:parse_term(Tokens), 
		NewCnt = details(B, Cnt),
		handle_acc(NewAcc, NewCnt)
	    catch
		_:_ ->
		    error_logger:error_msg("Incorrect erlang term recd, skipping! ~n", []),
		    {[], Cnt}
	    end
    end.


get_max(not_found, Value, _Date) ->
    Value;
get_max({sensor, _Id, _Val, _Prev, _Min, Max, Today, _Battery,_Upd}, Value, Date) 
  when Max > Value,  Date == Today ->
    Max;
get_max(_, Value, _Date) ->
    Value.

get_min(not_found, Value, _Date) ->
    Value;
get_min({sensor, _Id, _Val, _Prev, Min, _Max, Today, _Battery, _Upd}, Value, Date) 
  when Min < Value,  Date == Today ->
    Min;
get_min(_, Value, _Date) ->
    Value.


validate_sensor_value(not_found, Value) ->
    Value;
validate_sensor_value({sensor, _Id, _Val, Prev, _Min, _Max, _Today, _Battery, _Upd}, Value) 
  when (Prev + 1.0) > Value, (Prev - 1.0) < Value ->
    Value;
validate_sensor_value({sensor, _Id, Val, _Prev, _Min, _Max, _Today, _Battery, _Upd}, _Value) ->
    Val.


update_sensor(Id, Value, Battery, Validate)->
    Sensor = lookup(sensor_table, Id),

    Date = erlang:date(), 

    ValidatedValue = 
	case Validate of
	    true ->
		validate_sensor_value(Sensor, Value);
	    _ ->
		Value
	end,

    Max = get_max(Sensor, ValidatedValue, Date),
    Min = get_min(Sensor, ValidatedValue, Date),
    
    ets:insert(sensor_table, 
	       #sensor{id=Id, 
		       value=ValidatedValue,
		       prev_value=Value,
		       min=Min,
		       max=Max,
		       today=Date,
		       battery=Battery,
		       last_update_time=erlang:now()}),
    
    ok.

    

details({temperature, {ch, Channel}, {value, Value}, {battery, Battery}}, Cnt) ->
    update_sensor(130+Channel, Value, Battery, true),
    Cnt + 1;

details({humidity, {ch, Channel}, {value, Value}, {battery, Battery}}, Cnt) ->
    update_sensor(140+Channel, Value, Battery, true),
    Cnt + 1;

details({rain, {ch, Channel}, {total, Value}, {rate, _Rate}, {battery, Battery}}, Cnt) ->
    update_sensor(150+Channel, Value, Battery, true),
    Cnt + 1;

details({wind, {ch, Channel}, {gust, Gust}, {avg, Average}, {dir, Dir}, {battery, Battery}}, Cnt) ->
    update_sensor(160+Channel, Gust, Battery, false),
    update_sensor(170+Channel, Average, Battery, false),
    update_sensor(180+Channel, Dir, Battery, false),
    Cnt + 1;


details({device, {action, Action}, {address, Address}, {unit, _Unit}, {group_bit, 1}}, Cnt) ->
    %% Get all registered IDs and Units with the same Address
    MatchList = ets:match(device_table, {device, '$1', Address, '$2', '_', '_'}),
    
    NewState = #device{address=Address, 
		       state=Action,
		       last_state_change_time=erlang:now()},
    update_device_state(MatchList, NewState),
    Cnt + 1;

details({device, {action, Action}, {address, Address}, {unit, Unit}, {group_bit, 0}}, Cnt) ->
    %% Should result in a 1 sized list (or 0 if not updated with id)
    List = ets:match(device_table, {device, '$1', Address, Unit, '_', '_'}),
    ets:insert(device_table, #device{id=get_id(List),
				     address=Address, 
				     unit=Unit,
				     state=Action,
				     last_state_change_time=erlang:now()}),
    Cnt + 1;

details(_, Cnt) ->
    Cnt.

terminate(Reason, _State) ->
    error_logger:error_msg("~p ~p: ~p~n ", [erlang:localtime(), ?MODULE, Reason]),    
    ok.
    
