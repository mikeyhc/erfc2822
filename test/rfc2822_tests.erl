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

% TODO: month/1 tests

month_name_test_() ->
    [ lists:map(fun({X, Y}) ->
                        T = binary:list_to_bin(Y),
                        ?_assertEqual({X, <<>>}, rfc2822:month_name(T))
                end,
                [ {january, "Jan"},
                  {feburary, "Feb"},
                  {march, "Mar"},
                  {april, "Apr"},
                  {may, "May"},
                  {june, "Jun"},
                  {july, "Jul"},
                  {august, "Aug"},
                  {september, "Sep"},
                  {october, "Oct"},
                  {november, "Nov"},
                  {december, "Dec"} ]),
      ?_assertThrow({parse_error, expected, "month name"},
                    rfc2822:month_name(<<"not-a-month">>)),
      ?_assertError({badarg, a}, rfc2822:month_name(a))
    ].

day_of_month_test_() ->
    [ ?_assertEqual({1, <<>>}, rfc2822:day_of_month(<<"1">>)),
      ?_assertEqual({12, <<>>}, rfc2822:day_of_month(<<"12">>)),
      ?_assertEqual({12, <<"3">>}, rfc2822:day_of_month(<<"123">>))
    ].

% TODO: day/1
% TODO: time/1
% TODO: time_of_day/1

hour_test_() ->
    [ ?_assertEqual({1, <<>>}, rfc2822:hour(<<"01">>)),
      ?_assertThrow({parse_error, expected, "hour"},
                    rfc2822:hour(<<"1">>)),
      ?_assertThrow({parse_error, expected, "hour"},
                    rfc2822:hour(<<"a">>)),
      ?_assertError({badarg, a}, rfc2822:hour(a))
    ].

minute_test_() ->
    [ ?_assertEqual({1, <<>>}, rfc2822:minute(<<"01">>)),
      ?_assertThrow({parse_error, expected, "minute"},
                    rfc2822:minute(<<"1">>)),
      ?_assertThrow({parse_error, expected, "minute"},
                    rfc2822:minute(<<"a">>)),
      ?_assertError({badarg, a}, rfc2822:minute(a))
    ].

second_test_() ->
    [ ?_assertEqual({1, <<>>}, rfc2822:second(<<"01">>)),
      ?_assertThrow({parse_error, expected, "second"},
                    rfc2822:second(<<"1">>)),
      ?_assertThrow({parse_error, expected, "second"},
                    rfc2822:second(<<"a">>)),
      ?_assertError({badarg, a}, rfc2822:second(a))
    ].

%TODO: zone/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Address Specification (section 3.4) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: address/1
% TODO: mailbox/1
% TODO: name_addr/1
% TODO: angle_addr/1
% TODO: group/1
% TODO: display_name/1
% TODO: mailbox_list/1
% TODO: address_list/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Addr-spec specification (section 3.4.1) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: addr_spec/1
% TODO: local_part/1
% TODO: domain/1
% TODO: domain_literal/1
% TODO: dcontent/1

dtext_test_() ->
    ?random_byte_tests(fun rfc2822:dtext/1,
                       lists:seq(0, 90) ++ lists:seq(94, 126)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Overall Message Syntax (section 3.5) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: message/1

body_test_() -> [ ?_assertEqual({a,<<>>}, rfc2822:body(a)) ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Field Definitions (section 3.6) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: fields/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The origination date field (section 3.6.1) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: orig_date/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Originator fields (section 3.6.2) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: from/1
% TODO: sender/1
% TODO: reply_to/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Destination address fields (section 3.6.3) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: to/1
% TODO: cc/1
% TODO: bcc/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Identification fields (section 3.6.4) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: message_id/1
% TODO: in_reply_to/1
% TODO: references/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Informational fields (section 3.6.5) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: subject/1
% TODO: comments/1
% TODO: keywords/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Resent fields (section 3.6.6) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: resent_date/1
% TODO: resent_from/1
% TODO: resent_sender/1
% TODO: resent_to/1
% TODO: resent_cc/1
% TODO: resent_bcc/1
% TODO: resent_msg_id/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trace Fields (section 3.6.7) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: return_path/1
% TODO: received/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Optional Fields (section 3.6.8) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: optional_field/1
% TODO: field_name/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Miscellaneous obsolete tokens (section 4.1) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: obs_qp/1
% TODO: obs_text/1
% TODO: obs_char/1
% TODO: obs_utext/1
% TODO: obs_phrase/1
% TODO: obs_phrase_list/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete folding white space (section 4.2) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: obs_fws/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete Date and Time (section 4.3) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: obs_day_of_week/1

obs_year_() ->
    [ ?_assertEqual({2001, <<>>}, rfc2822:obs_year(<<"01">>)),
      ?_assertEqual({1999, <<>>}, rfc2822:obs_year(<<"99">>)),
      ?_assertEqual({2001, <<>>}, rfc2822:obs_year(<<"2001">>)),
      ?_assertEqual({2001, <<>>}, rfc2822:obs_year(<<"  2001\t">>)),
      ?_assertThrow({parse_error, expected, "year"},
                    rfc2822:obs_year(<<"a">>)),
      ?_assertError({badarg, a}, rfc2822:obs_year(a))
    ].

% TODO: obs_month/1
% TODO: obs_day/1

obs_hour() ->
    [ ?_assertEqual({20, <<>>}, rfc2822:obs_hour(<<"  20  ">>))
    ].
