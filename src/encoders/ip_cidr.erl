%% Encoders for ip addresses and cidrs
-module(ip_cidr).

%% API
-export([encode/1]).

encode({O1, O2, O3, O4} = InVal)
    when is_integer(O1) andalso O1 >= 0 andalso O1 =< 255 andalso
    is_integer(O2) andalso O2 >= 0 andalso O2 =< 255 andalso
    is_integer(O3) andalso O3 >= 0 andalso O3 =< 255 andalso
    is_integer(O4) andalso O4 >= 0 andalso O4 =< 255 ->
    case ip_to_binary_(InVal) of
        {error, einval} ->
            InVal;
        IpStr ->
            IpStr
    end;
encode({{O1, O2, O3, O4} = Ip, Mask} = InVal)
    when is_integer(O1) andalso O1 >= 0 andalso O1 =< 255 andalso
    is_integer(O2) andalso O2 >= 0 andalso O2 =< 255 andalso
    is_integer(O3) andalso O3 >= 0 andalso O3 =< 255 andalso
    is_integer(O4) andalso O4 >= 0 andalso O4 =< 255 andalso
    is_integer(Mask) andalso Mask >= 0 andalso Mask =< 32 ->
    case ip_to_binary_(Ip) of
        {error, einval} ->
            InVal;
        IpStr ->
            <<IpStr/binary, "/", (integer_to_binary(Mask))/binary>>
    end;
encode({O1, O2, O3, O4, O5, O6, O7, O8} = InVal)
    when is_integer(O1) andalso O1 >= 0 andalso O1 =< 65535 andalso
    is_integer(O2) andalso O2 >= 0 andalso O2 =< 65535 andalso
    is_integer(O3) andalso O3 >= 0 andalso O3 =< 65535 andalso
    is_integer(O4) andalso O4 >= 0 andalso O4 =< 65535 andalso
    is_integer(O5) andalso O5 >= 0 andalso O5 =< 65535 andalso
    is_integer(O6) andalso O6 >= 0 andalso O6 =< 65535 andalso
    is_integer(O7) andalso O7 >= 0 andalso O7 =< 65535 andalso
    is_integer(O8) andalso O8 >= 0 andalso O8 =< 65535 ->
    case ip_to_binary_(InVal) of
        {error, einval} ->
            InVal;
        IpStr ->
            IpStr
    end;
encode({{O1, O2, O3, O4, O5, O6, O7, O8} = Ip, Mask} = InVal)
    when is_integer(O1) andalso O1 >= 0 andalso O1 =< 65535 andalso
    is_integer(O2) andalso O2 >= 0 andalso O2 =< 65535 andalso
    is_integer(O3) andalso O3 >= 0 andalso O3 =< 65535 andalso
    is_integer(O4) andalso O4 >= 0 andalso O4 =< 65535 andalso
    is_integer(O5) andalso O5 >= 0 andalso O5 =< 65535 andalso
    is_integer(O6) andalso O6 >= 0 andalso O6 =< 65535 andalso
    is_integer(O7) andalso O7 >= 0 andalso O7 =< 65535 andalso
    is_integer(O8) andalso O8 >= 0 andalso O8 =< 65535 andalso
    is_integer(Mask) andalso Mask >= 0 andalso Mask =< 64 ->
    case ip_to_binary_(Ip) of
        {error, einval} ->
            InVal;
        IpStr ->
            <<IpStr/binary, "/", (integer_to_binary(Mask))/binary>>
    end;
encode(InVal) ->
    InVal.

ip_to_binary_(Ip) ->
    case inet:ntoa(Ip) of
        {error, einval} = Err ->
            Err;
        IpStr ->
            list_to_binary(IpStr)
    end.
