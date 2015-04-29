%% rfc2822_tests.erl
%% Unit tests for the RFC2822 parser provided by rfc2822.erl
%%
%% author: mikeyhc <mikeyhc@atmosia.net>

-module(rfc2822_tests).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%
%%% Test Helpers %%%
%%%%%%%%%%%%%%%%%%%%

-define(random_byte_tests(Fun, List),
        [ ?random_byte_pass_tests(10, Fun, List),
          ?random_byte_fail_tests(10, Fun, List),
          ?_assertError({badarg, a}, Fun(a))
        ]).

-define(random_byte_pass_tests(Number, Fun, List),
        lists:map(fun(X) -> ?_assertEqual({X, <<>>}, Fun(<<X>>)) end,
                  select_n_random(Number, List))).

-define(random_byte_fail_tests(Number, Fun, List),
    lists:map(fun(X) ->
                      ?_assertThrow({parse_error, expected, _}, Fun(<<X>>))
              end,
              select_n_random(Number,
                              lists:subtract(lists:seq(0,255), List)))).

select_n_random(N, L) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A, B, C),
    select_n_random_(N, L).

select_n_random_(0, _) -> [];
select_n_random_(_, []) -> [];
select_n_random_(N, L) ->
    R = lists:nth(random:uniform(length(L)), L),
    [R|select_n_random(N - 1, lists:delete(R, L))].

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Useful Combinators %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

maybe_option_test_() ->
    [ ?_assertEqual({undefined, <<"0">>},
                    rfc2822:maybe_option(fun rfc2234:alpha/1, <<"0">>)),
      ?_assertEqual({undefined, <<>>},
                    rfc2822:maybe_option(fun rfc2234:alpha/1, <<>>)),
      ?_assertEqual({$a, <<>>},
                    rfc2822:maybe_option(fun rfc2234:alpha/1, <<"a">>)),
      ?_assertError({badfun, a}, rfc2822:maybe_option(a, <<"a">>)),
      ?_assertError({badarg, a},
                    rfc2822:maybe_option(fun rfc2234:alpha/1, a))
    ].

% TODO: unfold/2 tests
% TODO: header/3 tests
% TODO: obs_header/3 tests

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitive Tokens (section 3.2.1) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

no_ws_ctl_test_() ->
    ?random_byte_tests(fun rfc2822:no_ws_ctl/1,
                       lists:seq(1, 8) ++ [11, 12] ++
                       lists:seq(14, 31) ++  [127]).

text_test_() ->
    ?random_byte_tests(fun rfc2822:text/1,
                       lists:subtract(lists:seq(1, 127), [$\n, $\r])).

specials_test_() ->
    ?random_byte_tests(fun rfc2822:specials/1, "()<>[]:;@,.\\\"").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Quoted characters (section 3.2.2) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

quoted_pair_test_() ->
    PassList = lists:map(fun(X) ->
                                 I = <<$\\, X>>,
                                 ?_assertEqual({I, <<>>},
                                               rfc2822:quoted_pair(I))
                         end,
                         select_n_random(10, lists:seq(0, 255))),
    FailList = lists:map(fun(X) ->
                                 ?_assertThrow({parse_error, expected, _},
                                               rfc2822:quoted_pair(<<X>>))
                         end,
                         select_n_random(10,
                                         lists:subtract(lists:seq(0, 255),
                                                        [$\\]))),
    PassList ++ FailList ++
        [ ?_assertError({badarg, a}, rfc2822:quoted_pair(a)) ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Folding white space and comments (section 3.2.3) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: fws/1 tests

ctext_test_() ->
    ?random_byte_tests(fun rfc2822:ctext/1,
                       lists:seq(33, 39) ++ lists:seq(42, 91) ++
                       lists:seq(93, 126) ++ lists:seq(128, 255)).

% TODO: comment/1 tests
% TODO: cfws/1 tests

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Atom (section 3.2.4) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

atext_test_() ->
    ?random_byte_tests(fun rfc2822:atext/1,
                       lists:seq($A, $Z) ++ lists:seq($a, $z) ++
                       lists:seq($0, $9) ++ "!#$%&'*+-/=?^_`{|}~").

% TODO: atom/1 tests
% TODO: dot_atom/1 tests
% TODO: dot_atom_text/1 tests

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Quoted Strings (section 3.2.5) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qtext_() ->
    ?random_byte_tests(fun rfc2822:qtext/1,
                       [33|lists:seq(35, 91) ++ lists:seq(93, 126)]).

% TODO: qcontent/1 tests
% TODO: quoted_string/1 tests

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Miscellaneous tokens (section 3.2.6) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: word/1 tests
% TODO: phrase/1 tests

utext_test_() ->
    ?random_byte_tests(fun rfc2822:utext/1,
                       [9,10,13,32,33|lists:seq(35, 91) ++
                        lists:seq(93, 126)]).

% TODO: unstructured/1 tests

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Date and Time Specification (section 3.3) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: date_time/1 tests
% TODO: day_of_week/1 tests
% TODO: binary-split_at/2 tests

day_name_test_() ->
    [ lists:map(fun({X, Y}) ->
                        T = binary:list_to_bin(Y),
                        ?_assertEqual({X, <<>>}, rfc2822:day_name(T))
                end,
                [ {monday, "Mon"},
                  {tuesday, "Tue"},
                  {wednesday, "Wed"},
                  {thursday, "Thu"},
                  {friday, "Fri"},
                  {saturday, "Sat"},
                  {sunday, "Sun"} ]),
      ?_assertThrow({parse_error, expected, "name of a day-of-the-week"},
                    rfc2822:day_name(<<"not-a-day">>)),
      ?_assertError({badarg, a}, rfc2822:day_name(a))
    ].

% TODO: date/1 tests

year_test_() ->
    [ ?_assertEqual({1920, <<>>}, rfc2822:year(<<"1920">>)),
      ?_assertEqual({19201, <<>>}, rfc2822:year(<<"19201">>)),
      ?_assertThrow({parse_error, expected, "year"},
                    rfc2822:year(<<"192">>)),
      ?_assertError({badarg, a}, rfc2822:year(a))
    ].
