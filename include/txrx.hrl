-ifndef(txrx).
-define(txrx, ok).

-record(device, {dev_addr, id, state, last_state_change_time}).
-record(temperature, {id, value, last_update_time}).
-record(humidity, {id, value, last_update_time}).
-record(rainrate, {id, value, last_update_time}).
-record(raintotal, {id, value, last_update_time}).


-endif.
