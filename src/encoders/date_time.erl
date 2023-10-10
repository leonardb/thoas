-module(date_time).

%% API
-export([date/1,
         datetime/1]).

date({Y, M, D})
    when is_integer(Y) andalso Y >= 0 andalso Y =< 9999 andalso
    is_integer(M) andalso M >= 1 andalso M =< 12 andalso
    is_integer(D) andalso D >= 1 andalso D =< 31 ->
    Dt = io_lib:format("~4..0b-~2..0b-~2..0b", [Y, M, D]),
    iolist_to_binary(Dt);
date(InVal) ->
    InVal.

datetime({{Y, M, D}, {H, I, S}})
    when is_integer(Y) andalso Y >= 0 andalso Y =< 9999 andalso
    is_integer(M) andalso M >= 1 andalso M =< 12 andalso
    is_integer(D) andalso D >= 1 andalso D =< 31 andalso
    is_integer(H) andalso H >= 0 andalso H =< 23 andalso
    is_integer(I) andalso I >= 0 andalso I =< 59 andalso
    is_integer(S) andalso S >= 0 andalso S =< 59 ->
    Dt = io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~sZ",
                       [Y, M, D, H, I, integer_to_list(S)]),
    iolist_to_binary(Dt);
datetime({{Y, M, D}, {H, I, S}})
    when is_integer(Y) andalso Y >= 0 andalso Y =< 9999 andalso
    is_integer(M) andalso M >= 1 andalso M =< 12 andalso
    is_integer(D) andalso D >= 1 andalso D =< 31 andalso
    is_integer(H) andalso H >= 0 andalso H =< 23 andalso
    is_integer(I) andalso I >= 0 andalso I =< 59 andalso
    is_float(S) andalso S >= 0 andalso S < 60 ->
    Dt = io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~sZ",
                       [Y, M, D, H, I, float_to_binary(S, [short])]),
    iolist_to_binary(Dt);
datetime(InVal) ->
    InVal.
