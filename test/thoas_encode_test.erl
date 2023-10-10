-module(thoas_encode_test).
-include_lib("eunit/include/eunit.hrl").
-compile([{no_auto_import, [float/1]}]).

-import(thoas_encode, [
    true/0, false/0, null/0, boolean/1, integer/1, float/1, string/1,
    non_recursive_array/1, non_recursive_object/1, encode/2
]).

true_test() ->
    ?assertEqual(true(), <<"true">>).

false_test() ->
    ?assertEqual(false(), <<"false">>).

boolean_true_test() ->
    ?assertEqual(boolean('false'), <<"false">>).

boolean_false_test() ->
    ?assertEqual(boolean('true'), <<"true">>).

null_test() ->
    ?assertEqual(null(), <<"null">>).

integer_test_() ->
    Cases = [
        {0, "0"},
        {1, "1"},
        {-0, "0"},
        {-1, "-1"},
        {-100000, "-100000"},
        {100000, "100000"}
    ],
    [
        ?_assertEqual(Expected, integer(Input))
        || {Input, Expected} <- Cases
    ].

float_test_() ->
    Cases = [
        {0.0, <<"0.0">>},
        {1.1, <<"1.1">>},
        {-0.0, <<"-0.0">>},
        {-1.3, <<"-1.3">>},
        {-100000.4234, <<"-100000.4234">>},
        {100000.214, <<"100000.214">>}
    ],
    [
        ?_assertEqual(Expected, float(Input))
        || {Input, Expected} <- Cases
    ].

string_test_() ->
    Cases = [
        {<<"Hello, Joe">>, <<"\"Hello, Joe\"">>},
        {<<>>, <<"\"\"">>},
        {<<"\n">>, <<"\"\\n\"">>},
        {<<"🤔">>, <<"\"\\u0014\"">>},
        {<<"Goodbye, Joe">>, <<"\"Goodbye, Joe\"">>}
    ],
    [
        ?_assertEqual(Expected, iolist_to_binary(string(Input)))
        || {Input, Expected} <- Cases
    ].


non_recursive_array_test_() ->
    Cases = [
        {[true()], <<"[true]">>},
        {[true(), false(), null()], <<"[true,false,null]">>},
        {[true(), string(<<"\n">>)], <<"[true,\"\\n\"]">>},
        {[], <<"[]">>}
    ],
    [
        ?_assertEqual(Expected, iolist_to_binary(non_recursive_array(Input)))
        || {Input, Expected} <- Cases
    ].

non_recursive_object_test_() ->
    Cases = [
        {
            [
                {<<"name">>, string(<<"Gleam">>)},
                {<<"isCool">>, true()}
            ],
            <<"{\"name\":\"Gleam\",\"isCool\":true}">>
        },
        {
            [
                {<<"\n">>, string(<<"That needed to be escaped">>)}
            ],
            <<"{\"\\n\":\"That needed to be escaped\"}">>
        },
        {
            [
                {<<"isCool">>, true()}
            ],
            <<"{\"isCool\":true}">>
        },
        {
            [
            ],
            <<"{}">>
        }
    ],
    [
        ?_assertEqual(Expected, iolist_to_binary(non_recursive_object(Input)))
        || {Input, Expected} <- Cases
    ].

encode_test_() ->
    Cases = [
        {#{0 => 0}, <<"{\"0\":0}">>},
        {[{<<"foo">>, 1}], <<"{\"foo\":1}">>},
        {#{<<"bar">> => 2}, <<"{\"bar\":2}">>}
    ],
    [
        ?_assertEqual(Expected, iolist_to_binary(encode(Input, #{})))
        || {Input, Expected} <- Cases
    ].

types_test_() ->
    Cases =[
        {#{<<"date">> => {2023,8,7}}, <<"{\"date\":\"2023-08-07\"}">>},
        {#{<<"datetime">> => {{2023,8,7},{19,2,24}}}, <<"{\"datetime\":\"2023-08-07T19:02:24Z\"}">>},
        {#{<<"datetime">> => {{2023,8,7},{19,2,24.65432}}}, <<"{\"datetime\":\"2023-08-07T19:02:24.65432Z\"}">>},
        {#{<<"ipv4">> => {192,168,1,100}}, <<"{\"ipv4\":\"192.168.1.100\"}">>},
        {#{<<"ipv4cidr">> => {{192,168,1,100}, 24}}, <<"{\"ipv4cidr\":\"192.168.1.100/24\"}">>},
        {#{<<"ipv6">> => {9735,64401,34956,5623,30910,49280,12263,4004}}, <<"{\"ipv6\":\"2607:fb91:888c:15f7:78be:c080:2fe7:fa4\"}">>},
        {#{<<"ipv6cidr">> => {{9735,64401,34956,5623,30910,49280,12263,4004}, 64}}, <<"{\"ipv6cidr\":\"2607:fb91:888c:15f7:78be:c080:2fe7:fa4/64\"}">>}
    ],
    Encoders = #{date =>        {date_time, date},
                 datetime =>    {date_time, datetime},
                 ip_cidr =>     {ip_cidr, encode}},
    [
        ?_assertEqual(Expected, iolist_to_binary(encode(Input, #{encoders => Encoders})))
        || {Input, Expected} <- Cases
    ].
