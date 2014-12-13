-ifndef(txrx).
-define(txrx, ok).

-record(device, {id, address, unit, state, last_state_change_time}).
-record(sensor, {id, value, prev_value, min, max, today, last_update_time}).


-endif.
