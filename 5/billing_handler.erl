-module(billing_handler).
-export([init/1, terminate/1, handle_event/2]).

init(CallLog) -> CallLog.
terminate(CallLog) -> CallLog.
handle_event({Action, Number}, CallLog) when Action == connect; Action == disconnect ->
    {MegaSec, Sec, MicroSec} = now(),
    [{MegaSec, Sec, MicroSec, Action, Number}|CallLog];
handle_event(_, CallLog) -> CallLog.
