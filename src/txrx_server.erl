-module(txrx_server).
-behaviour(gen_server).

-include("../include/txrx.hrl").

-export([start_link/0, send_to_serial/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-export([get_all_devices/0,get_all_temperatures/0,get_all_humidities/0, get_all_raintotals/0]).
-export([set_device_info/2, device/2]).
-export([get_device/1, get_humidity/1,get_temperature/1, get_raintotal/1]).
-export([handle_acc/1]).
-record(state, {serial, acc_str}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->

    process_flag(trap_exit, true),

    ets:new(humidity_table, [named_table, set, {keypos, #humidity.id}, public]),
    ets:new(raintotal_table, [named_table, set, {keypos, #raintotal.id}, public]),
    ets:new(temperature_table, [named_table, set, {keypos, #temperature.id}, public]),
    ets:new(device_table, [named_table, set, {keypos, #device.dev_addr}, public]),

    Self = self(),

    io:format("Startig ~p pid ~p~n ", [?MODULE, Self]),

    Pid = serial:start([{speed,115200},{open,"/dev/ttyACM0"}]),

    {ok, #state{serial = Pid, acc_str = []}}.

get_next(_Table, '$end_of_table', Acc) ->
    Acc;
get_next(Table, Prev, Acc) ->
    [Val] = ets:lookup(Table, Prev),
    get_next(Table, ets:next(Table, Prev), [Val|Acc]).

get_all_devices() ->
    Table = device_table,
    First = ets:first(Table),
    get_next(Table, First, []).

get_all_temperatures() ->
    Table = temperature_table,
    First = ets:first(Table),
    get_next(Table, First, []).

get_all_humidities() ->
    Table = humidity_table,
    First = ets:first(Table),
    get_next(Table, First, []).

get_all_raintotals() ->
    Table = raintotal_table,
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
set_device_info(DevAddr, Id) ->
    case lookup(device_table, DevAddr) of
	not_found ->
	    ets:insert(device_table, 
		       #device{dev_addr=DevAddr,
			       id=Id, 
			       state=off, 
			       last_state_change_time=erlang:now()});
	{device, A, _, State, Time} ->
	    ets:insert(device_table, 
		       #device{dev_addr=A,
			       id=Id, 
			       state=State, 
			       last_state_change_time=Time})
    end,
    ok.

device(Id, Action) ->
    [[{Address, Unit}]] = ets:match(device_table, {device, '$1', Id, '_', '_'}),
    send_to_serial("{device," 
		   ++ atom_to_list(Action) ++ "," 
		   ++ Address ++ ","
		   ++ Unit ++ "}").

get_temperature(Id) ->
    lookup(temperature_table, Id).

get_humidity(Id) ->
    lookup(humidity_table, Id).

get_raintotal(Id) ->
    lookup(raintotal_table, Id).

get_device(Id) ->
    lookup(device_table, Id).

send_to_serial(Msg) ->
    gen_server:call(?MODULE, {send, list_to_binary(Msg)}).
			      
%% callbacks

handle_call({send, Binary}, _From, #state{serial = Pid} = State) ->
    %%    io:format("Send ~p! ~n", [binary_to_list(Binary)]),
    Pid ! {send, Binary},
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    io:format("handle_call~n", []),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    io:format("handle_cast~n", []),
    {noreply, State}.

handle_info({data, Data}, State) ->
    %% io:format("Data=~p~n", [Data]),
    NewAcc = 
	handle_acc(State#state.acc_str ++ binary_to_list(Data)),	    
    {noreply, State#state{acc_str=NewAcc}};    

handle_info(Info, State) ->
    io:format("handle_info ~p~n", [Info]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



handle_acc(Acc) ->
    case string:str(Acc, "\r\n") of
	0 -> 
	    Acc;
	Index ->
	    Line = string:substr(Acc, 1, Index-1),
	    NewAcc = string:substr(Acc, Index+2),
	    io:format("~p ~p: ~p~n ", [erlang:localtime(), ?MODULE, Line]),

	    try 
		{ok, Tokens, _} = erl_scan:string(Line),
		{ok, B} = erl_parse:parse_term(Tokens), 
		details(B),
		handle_acc(NewAcc)
	    catch
		_:_ ->
		    ok
	    end
    end.


details({temperature, {ch, Channel}, {value, Value}}) ->
    ets:insert(temperature_table, 
	       #temperature{id=130 + Channel, 
			    value=Value, 
			    last_update_time=erlang:now()}),
    ok;

details({humidity, {ch, Channel}, {value, Value}}) ->
    ets:insert(humidity_table, 
	       #humidity{id=130 + Channel, 
			 value=Value, 
			 last_update_time=erlang:now()}),
    
    ok;

details({rain, {ch, Channel}, {total, Total}, {tips, _Tips}}) ->
    ets:insert(raintotal_table, 
	       #raintotal{id=130 + Channel, 
			  value=Total, 
			  last_update_time=erlang:now()}),    
    ok;

details({device, {action, Action}, {address, Address}, {unit, Unit}, {group_bit, _GroupBit}}) ->
    ets:insert(device_table, 
	       #device{dev_addr={Address,Unit}, 
		       state=Action, 
		       last_state_change_time=erlang:now()}),    
    ok;

details(_) ->
    ok.

terminate(Reason, _State) ->
    io:format("~p ~p: ~p~n ", [erlang:localtime(), ?MODULE, Reason]),    
    ok.
    
