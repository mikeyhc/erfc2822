%% rfc2822_tests.erl
%% Unit tests for the RFC2822 parser provided by rfc2822.erl
%%
%% author: mikeyhc <mikeyhc@atmosia.net>

-module(rfc2822_tests).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("calender_time.hrl").
-include("name_addr.hrl").
-include("timediff.hrl").

%%%%%%%%%%%%%%%%%%%%
%%% Test Helpers %%%
%%%%%%%%%%%%%%%%%%%%

-define(random_byte_tests(Fun, List),
        [ ?random_byte_pass_tests(10, Fun, List),
          ?random_byte_fail_tests(10, Fun, List),
          ?_assertError({badarg, a}, Fun(a))
        ]).

-define(random_byte_pass_tests(Number, Fun, List),
        [ ?_assertEqual({X, <<>>}, Fun(<<X>>))
          || X <- select_n_random(Number, List) ]).

-define(random_byte_fail_tests(Number, Fun, List),
    lists:map(fun(X) ->
                      ?_assertThrow({parse_error, expected, _}, Fun(<<X>>))
              end,
              select_n_random(Number,
                              lists:subtract(lists:seq(0,255), List)))).

-define(string_list_test(Func, List),
        ?string_list_pair_test(Func, [ {X, X} || X <- List ])).

-define(string_list_pair_test(Func, List),
        ?list_pair_test(Func,
                        [ {binary:list_to_bin(A),
                           binary:list_to_bin(B)} || {A, B} <- List ])).

-define(list_pair_test(Func, List),
        [ [ ?_assertEqual({X, <<>>}, Func(Y)) || {X, Y} <- List ],
          ?_assertError({badarg, a}, Func(a))
        ]).

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

unfold_test_() ->
    [lists:map(fun(X) ->
                       B = binary:list_to_bin(X),
                       ?_assertEqual({$a, <<>>},
                                     rfc2822:unfold(fun rfc2234:alpha/1, B))
               end,
               [ "\ta\t", " \ta ", "a\t", " a ", "\t\ta" ]),
     ?_assertEqual({$a, <<"b">>}, rfc2822:unfold(fun rfc2234:alpha/1,
                                                 <<"\t a\t\tb">>))
    ].

header_test_() ->
    [ ?_assertEqual({$a,<<>>}, rfc2822:header("test", fun rfc2234:alpha/1,
                                              <<"test:a\r\n">>)),
      ?_assertThrow({parse_error, expected, "test header line"},
                    rfc2822:header("test", fun rfc2234:alpha/1,
                                   <<"test:0\r\n">>)),
      ?_assertThrow({parse_error, expected, "test header line"},
                    rfc2822:header("test", fun rfc2234:alpha/1,
                                   <<"test:a">>)),
      ?_assertError({badarg, a}, rfc2822:header(a, fun rfc2234:alpha/1, <<>>))
    ].

obs_header_test_() ->
    [ ?_assertEqual({$a, <<>>}, rfc2822:obs_header("test", fun rfc2234:alpha/1,
                                                   <<"test \t:a\r\n">>)),
      ?_assertThrow({parse_error, expected, "test obsolete header line"},
                    rfc2822:obs_header("test", fun rfc2234:alpha/1,
                                       <<"test:0\r\n">>))
    ].

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

fws_test_() -> ?string_list_test(fun rfc2822:fws/1,
                                 [ " ", "\t", " \r\n ", "\r\n \r\n " ]).

ctext_test_() ->
    ?random_byte_tests(fun rfc2822:ctext/1,
                       lists:seq(33, 39) ++ lists:seq(42, 91) ++
                       lists:seq(93, 126) ++ lists:seq(128, 255)).

comment_test_() ->
    ?string_list_test(fun rfc2822:comment/1,
                      [ "(this is a comment)",
                        "(comment \a quoted)",
                        "(this is more folding \t)"]).
cfws_test_() ->
    ?string_list_test(fun rfc2822:cfws/1,
                      [ " \t(leading fws)",
                        "(trailing fws) \t",
                        "\t (both)\t\r\n " ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Atom (section 3.2.4) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

atext_test_() ->
    ?random_byte_tests(fun rfc2822:atext/1,
                       lists:seq($A, $Z) ++ lists:seq($a, $z) ++
                       lists:seq($0, $9) ++ "!#$%&'*+-/=?^_`{|}~").

atom_test_() ->
    [ ?string_list_test(fun rfc2822:atom/1, [ "abc", "20a0", "&/=?abc" ]),
      ?_assertThrow({parse_error, expected, _}, rfc2822:atom(<<"\r">>))
    ].

dot_atom_test_() ->
    [ ?string_list_test(fun rfc2822:dot_atom/1, [ "abc.xyz", "2.3.4" ]),
      ?_assertThrow({parse_error, expected, _}, rfc2822:dot_atom(<<"\n">>))
    ].

intersperse_test_() -> [ ?_assertEqual([], rfc2822:intersperse([], $c)) ].

dot_atom_text_test_() ->
    [ ?string_list_test(fun rfc2822:dot_atom_text/1, [ "abc.xyz", "2.3.4" ]),
      ?_assertThrow({parse_error, expected, _}, rfc2822:dot_atom_text(<<"a">>))
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Quoted Strings (section 3.2.5) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qtext_test_() ->
    ?random_byte_tests(fun rfc2822:qtext/1,
                       [33|lists:seq(35, 91) ++ lists:seq(93, 126)]).

qcontent_test_() ->
    ?string_list_test(fun rfc2822:qcontent/1, ["abcd", "\\\t" ]).

quoted_string_test_() ->
    [ ?string_list_test(fun rfc2822:quoted_string/1,
                        ["\"abcd\"", "\"\\\t\"", "\"\\\tabcxyz\"",
                         "\"\\\t\\\r\"", "\"  \tabc \""]),
      ?_assertThrow({parse_error, expected, "quoted string"},
                    rfc2822:quoted_string(<<"x">>)),
      ?_assertThrow({parse_error, expected, "quoted string"},
                    rfc2822:quoted_string(<<>>))
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Miscellaneous tokens (section 3.2.6) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

word_test_() ->
    ?string_list_test(fun rfc2822:word/1,
                      [ "atom_test", "\" quoted \\\t test \"" ]).

phrase_test_() -> obs_phrase_test_().

utext_test_() ->
    ?random_byte_tests(fun rfc2822:utext/1,
                       [9,32,33|lists:seq(35, 91) ++
                        lists:seq(93, 126)]).

unstructured_test_() ->
    ?string_list_test(fun rfc2822:unstructured/1,
                      [ "unstructured",
                        "un\r\n struct\r\n ured",
                        "\t\r\n un\r\n\tSt810"
                      ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Date and Time Specification (section 3.3) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

date_time_test_() ->
    Time = #calender_time{year=1900, month=january, day=1, hour=0,
                          min=0, sec=0, week_day=monday, tz_diff=0},
    Time2 = #calender_time{year=1900, month=january, day=1, hour=0,
                           min=0, sec=0, week_day=undefined, tz_diff=0},
    [ ?_assertEqual({Time, <<>>},
                    rfc2822:date_time(<<"Mon, 01 Jan 1900 00:00:00 +0000">>)),
      ?_assertEqual({Time2, <<>>},
                    rfc2822:date_time(<<"01 Jan 1900 00:00:00 +0000">>))
    ].

day_of_week_test_() ->
    [ lists:map(fun({X, Y}) ->
                        B = binary:list_to_bin(Y),
                        ?_assertEqual({X, <<>>}, rfc2822:day_name(B))
                end,
                [ {monday, "Mon"},
                  {tuesday, "Tue"},
                  {wednesday, "Wed"},
                  {thursday, "Thu"},
                  {friday, "Fri"},
                  {saturday, "Sat"},
                  {sunday, "Sun"}
                ])
    ].

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

date_test_() ->
    [ ?_assertEqual({1900, january, 1, <<>>}, rfc2822:date(<<"01 Jan 1900">>)),
      ?_assertThrow({parse_error, expected, "date specification"},
                    rfc2822:date(<<"a">>)),
      ?_assertError({badarg, a}, rfc2822:date(a))
    ].

year_test_() ->
    [ ?_assertEqual({1920, <<>>}, rfc2822:year(<<"1920">>)),
      ?_assertEqual({19201, <<>>}, rfc2822:year(<<"19201">>)),
      ?_assertThrow({parse_error, expected, "year"},
                    rfc2822:year(<<"192">>)),
      ?_assertError({badarg, a}, rfc2822:year(a))
    ].

month_test_() ->
    [ ?_assertEqual({january, <<>>}, rfc2822:month(<<" Jan\t">>))
    ].

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

day_test_() ->
    [ ?_assertEqual({1, <<>>}, rfc2822:day(<<"01">>)),
      ?_assertEqual({1, <<>>}, rfc2822:day(<<"1">>)),
      ?_assertThrow({parse_error, expected, "day of month"},
                    rfc2822:day(<<"a">>)),
      ?_assertError({badarg, a}, rfc2822:day(a))
    ].

time_test_() ->
    [ ?_assertEqual({#timediff{hour=0, min=0, sec=0}, 60, <<>>},
                    rfc2822:time(<<"00:00:00 +0100">>))
    ].

time_of_day_test_() ->
    [ ?_assertEqual({#timediff{hour=0, min=0, sec=0}, <<>>},
                    rfc2822:time_of_day(<<"00:00:00">>)),
      ?_assertEqual({#timediff{hour=0, min=0, sec=0}, <<>>},
                    rfc2822:time_of_day(<<"00:00">>))
    ].

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

zone_test_() ->
    [ ?_assertEqual({0, <<>>}, rfc2822:zone(<<"+0000">>)),
      ?_assertEqual({60, <<>>}, rfc2822:zone(<<"+0100">>)),
      ?_assertEqual({1, <<>>}, rfc2822:zone(<<"+0001">>)),
      ?_assertEqual({-60, <<>>}, rfc2822:zone(<<"-0100">>)),
      ?_assertThrow({parse_error, expected, "timezone"},
                    rfc2822:zone(<<"a">>)),
      ?_assertError({badarg, a}, rfc2822:zone(a))
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Address Specification (section 3.4) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

address_test_() ->
    [ ?list_pair_test(fun rfc2822:mailbox/1,
                      [ {#name_addr{addr= <<"mike@atmosia.net">>},
                         <<"mike@atmosia.net">>},
                        {#name_addr{name= <<"\"Michael Blockley\"">>,
                                    addr= <<"mike@atmosia.net">>},
                         <<"\"Michael Blockley\" <mike@atmosia.net>">>}
                      ]),
      ?list_pair_test(fun rfc2822:group/1,
                      [ {[], <<"group: ;">>},
                        {[#name_addr{addr= <<"mike@atmosia.net">>},
                          <<"group: mike@atmosia.net; ">>]},
                        {[#name_addr{addr= <<"mike@atmosia.net">>},
                          #name_addr{addr= <<"mike@atmosia.net">>,
                                     name= <<"\"Michael Blockley\"">>}],
                         <<"group: <mike@atmosia.net>, \"Michael Blockley\" "
                         "<mike@atmosia.net>;">>}
                      ])
    ].

mailbox_test_() ->
    [ ?list_pair_test(fun rfc2822:mailbox/1,
                      [ {#name_addr{addr= <<"mike@atmosia.net">>},
                         <<"mike@atmosia.net">>},
                        {#name_addr{name= <<"\"Michael Blockley\"">>,
                                    addr= <<"mike@atmosia.net">>},
                         <<"\"Michael Blockley\" <mike@atmosia.net>">>}
                      ]),
      ?_assertThrow({parse_error, expected, "mailbox"},
                    rfc2822:mailbox(<<"\0">>))
    ].

name_addr_test_() ->
    [ ?list_pair_test(fun rfc2822:name_addr/1,
                      [ {#name_addr{addr= <<"mike@atmosia.net">>},
                         <<"<mike@atmosia.net>">>},
                        {#name_addr{name= <<"\"Michael Blockley\"">>,
                                    addr= <<"mike@atmosia.net">>},
                         <<"\"Michael Blockley\" <mike@atmosia.net>">>}
                      ]),
      ?_assertThrow({parse_error, expected, "name addr"},
                    rfc2822:name_addr(<<"\0">>))
    ].

angle_addr_test_() ->
    [ ?string_list_pair_test(fun rfc2822:angle_addr/1,
                             lists:map(fun(X) ->
                                               {X, "<" ++ X ++ ">"}
                                       end,
                                       [ "mike@atmosia.net",
                                         "mike.blockley@atmosia.net",
                                         "\"mike\"@[192.168.1.1]" ])),
      ?_assertThrow({parse_error, expected, "angle address"},
                    rfc2822:angle_addr(<<"\0">>))
    ].

group_test_() ->
    [ ?list_pair_test(fun rfc2822:group/1,
                      [ {[], <<"group: ;">>},
                        {[#name_addr{addr= <<"mike@atmosia.net">>},
                          <<"group: mike@atmosia.net; ">>]},
                        {[#name_addr{addr= <<"mike@atmosia.net">>},
                          #name_addr{addr= <<"mike@atmosia.net">>,
                                     name= <<"\"Michael Blockley\"">>}],
                         <<"group: <mike@atmosia.net>, \"Michael Blockley\" "
                         "<mike@atmosia.net>;">>}
                      ]),
      ?_assertThrow({parse_error, expected, "address list"},
                    rfc2822:group(<<"\0">>))
    ].

display_name_test_() ->
    [ ?string_list_test(fun rfc2822:display_name/1,
                        [ "mike", "\"mike blockley\""]),
      ?_assertThrow({parse_error, expected, "display name"},
                    rfc2822:display_name(<<"\0">>))
    ].

mailbox_list_test_() ->
    [ ?list_pair_test(fun rfc2822:mailbox_list/1,
                      [ {[], <<"">>},
                        {[#name_addr{addr= <<"mike@atmosia.net">>},
                          <<"mike@atmosia.net">>]},
                        {[#name_addr{addr= <<"mike@atmosia.net">>},
                          #name_addr{addr= <<"mike@atmosia.net">>,
                                     name= <<"\"Michael Blockley\"">>}],
                         <<"<mike@atmosia.net>, \"Michael Blockley\" "
                         "<mike@atmosia.net>">>}
                      ])
    ].

address_list_test_() ->
    [ ?list_pair_test(fun rfc2822:address_list/1,
                      [ {[], <<"">>},
                        {[#name_addr{addr= <<"mike@atmosia.net">>},
                          <<"mike@atmosia.net">>]},
                        {[[#name_addr{addr= <<"mike@atmosia.net">>}],
                          [#name_addr{addr= <<"mike@atmosia.net">>,
                                     name= <<"\"Michael Blockley\"">>}]],
                         <<"<mike@atmosia.net>, \"Michael Blockley\" "
                         "<mike@atmosia.net>">>},
                        {[[#name_addr{addr= <<"mike@atmosia.net">>},
                           #name_addr{addr= <<"mike@atmosia.net">>,
                                      name= <<"\"Michael Blockley\"">>}],
                          [#name_addr{addr= <<"mike@atmosia.net">>}]],
                         <<"group: <mike@atmosia.net>, \"Michael Blockley\" "
                           "<mike@atmosia.net>;, <mike@atmosia.net>">>}
                      ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Addr-spec specification (section 3.4.1) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addr_spec_test_() ->
    [ ?string_list_test(fun rfc2822:addr_spec/1,
                        [ "mike@atmosia.net", "mike.blockley@atmosia.net",
                          "\"mike\"@[192.168.1.1]" ]),
      ?_assertThrow({parse_error, expected, "address specification"},
                    rfc2822:addr_spec(<<"\0">>))
    ].

local_part_test_() ->
    [ ?string_list_test(fun rfc2822:local_part/1,
                        [ "mike", "mike.blockley", "\"mike\""]),
      ?_assertThrow({parse_error, expected, "'address' local part"},
                    rfc2822:local_part(<<"\0">>))
    ].

domain_literal_test_() ->
    [ ?string_list_test(fun rfc2822:domain_literal/1,
                        [ "[atmosia.net]", "[google.com]", "[192.168.1.1]" ]),
      ?_assertThrow({parse_error, expected, "domain literal"},
                    rfc2822:domain_literal(<<"\0">>))
    ].

dcontent_test_() ->
    [ ?string_list_test(fun rfc2822:dcontent/1,
                        [ "atmosia.net", "google.com", "192.168.1.1" ]),
      ?_assertThrow({parse_error, expected, "domain literal content"},
                    rfc2822:dcontent(<<"\0">>))
    ].

dtext_test_() ->
    ?random_byte_tests(fun rfc2822:dtext/1,
                       lists:seq(33, 90) ++ lists:seq(94, 126)).

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

orig_date_test_() ->
    [ ?list_pair_test(fun rfc2822:orig_date/1,
                      [ {#calender_time{year=1900, month=january, day=1,
                                        hour=0, min=0, sec=0,
                                        tz_diff=0, week_day=undefined},
                         <<"Date: 1 Jan 1900 00:00:00 +0000\r\n">>}
                      ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Originator fields (section 3.6.2) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

from_test_() ->
    [ ?list_pair_test(fun rfc2822:from/1,
                      [ {[#name_addr{addr= <<"mike@atmosia.net">>}],
                         <<"From: mike@atmosia.net\r\n">>} ])
    ].

sender_test_() ->
    [ ?list_pair_test(fun rfc2822:sender/1,
                      [ {#name_addr{addr= <<"mike@atmosia.net">>},
                         <<"Sender: mike@atmosia.net\r\n">>} ])
    ].

reply_to_test_() ->
    [ ?list_pair_test(fun rfc2822:reply_to/1,
                      [ {[[ #name_addr{addr= <<"mike@atmosia.net">>} ]],
                         <<"Reply-to: mike@atmosia.net\r\n">>} ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Destination address fields (section 3.6.3) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_test_() ->
    [ ?list_pair_test(fun rfc2822:to/1,
                      [ {[[ #name_addr{addr= <<"mike@atmosia.net">>} ]],
                         <<"To: mike@atmosia.net\r\n">>} ])
    ].

cc_test_() ->
    [ ?list_pair_test(fun rfc2822:cc/1,
                      [ {[[ #name_addr{addr= <<"mike@atmosia.net">>} ]],
                         <<"Cc: mike@atmosia.net\r\n">>} ])
    ].

bcc_test_() ->
    [ ?list_pair_test(fun rfc2822:bcc/1,
                      [ {[[ #name_addr{addr= <<"mike@atmosia.net">>} ]],
                         <<"Bcc: mike@atmosia.net\r\n">>}
                      ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Identification fields (section 3.6.4) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message_id_test_() ->
    [ ?list_pair_test(fun rfc2822:message_id/1,
                      [ {<<"<abc.1234@atmosia.net>">>,
                         <<"Message-ID: <abc.1234@atmosia.net>\r\n">>},
                        {<<"<\"xyzabc\"@[192.168.1.1]>">>,
                         <<"Message-ID: <\"xyzabc\"@[192.168.1.1]>\r\n">>}
                      ])
    ].

in_reply_to_test_() ->
    [ ?list_pair_test(fun rfc2822:in_reply_to/1,
                      [ {[<<"<abc.1234@atmosia.net>">>],
                         <<"In-Reply-To: <abc.1234@atmosia.net>\r\n">>},
                        {[<<"<abc.1234@atmosia.net>">>,
                          <<"<\"xyzabc\"@[192.168.1.1]>">>],
                         <<"In-Reply-To: <abc.1234@atmosia.net>\r\n"
                           "             <\"xyzabc\"@[192.168.1.1]>\r\n">>}
                      ])
    ].



references_test_() ->
    [ ?list_pair_test(fun rfc2822:references/1,
                      [ {[<<"<abc.1234@atmosia.net>">>],
                         <<"References: <abc.1234@atmosia.net>\r\n">>},
                        {[<<"<abc.1234@atmosia.net>">>,
                          <<"<\"xyzabc\"@[192.168.1.1]>">>],
                         <<"References: <abc.1234@atmosia.net>\r\n"
                           "            <\"xyzabc\"@[192.168.1.1]>\r\n">>}
                      ])
    ].


msg_id_test_() ->
    [ ?string_list_test(fun rfc2822:msg_id/1,
                        [ "<abc.1234@abc.1234>",
                          "<\"xyzabc\"@[192.168.1.1]>"
                        ]),
      ?_assertThrow({parse_error, expected, "message id"},
                    rfc2822:msg_id(<<"\0">>))
    ].

id_left_test_() ->
    [ ?string_list_test(fun rfc2822:id_left/1,
                        [ "abc.1234", "\"xyzabc\"" ]),
      ?_assertThrow({parse_error, expected, "left part of message ID"},
                    rfc2822:id_left(<<"\0">>))
    ].

id_right_test_() ->
    [ ?string_list_test(fun rfc2822:id_right/1,
                        [ "abc.1234", "[192.168.1.1]"]),
      ?_assertThrow({parse_error, expected, "right part of message ID"},
                    rfc2822:id_right(<<"\0">>))
    ].

no_fold_quote_test_() ->
    [ ?string_list_test(fun rfc2822:no_fold_quote/1,
                        [ "\"\\quoted\"",
                          "\"non.quoted\"" ]),
      ?_assertThrow({parse_error, expected, "non-folding quoted string"},
                    rfc2822:no_fold_quote(<<"\0">>))
    ].

no_fold_literal_test_() ->
    [ ?string_list_test(fun rfc2822:no_fold_literal/1,
                        [ "[\\qouted]",
                          "[non.quoted]" ]),
      ?_assertThrow({parse_error, expected, "non-folding domain literal"},
                    rfc2822:no_fold_literal(<<"\0">>))
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Informational fields (section 3.6.5) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subject_test_() ->
    [ ?string_list_pair_test(fun rfc2822:subject/1,
                             [ {" this is a subject",
                                "Subject: this is a subject\r\n"},
                               {" also a\r\n subject",
                                "Subject: also a\r\n subject\r\n"}
                             ])
    ].

comments_test_() ->
    [ ?string_list_pair_test(fun rfc2822:comments/1,
                             [ {" this is a comment",
                                "Comments: this is a comment\r\n"},
                               {" also a\r\n comment",
                                "Comments: also a\r\n comment\r\n"}
                             ])
    ].

keywords_test_() ->
    [ ?list_pair_test(fun rfc2822:keywords/1,
                      [ {[[<<"abc">>]], <<"Keywords: abc\r\n">>},
                        {[[<<"abc">>, <<"xyz">>]],
                         <<"Keywords: abc xyz\r\n">>},
                        {[[<<"abc">>, <<"xyz">>, <<"123">>]],
                         <<"Keywords: abc xyz\r\n 123\r\n">>},
                        {[[<<"abc">>, <<"xyz">>], [<<"123">>]],
                         <<"Keywords: abc xyz, 123\r\n">>}
                      ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Resent fields (section 3.6.6) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resent_date_test_() ->
    [ ?list_pair_test(fun rfc2822:resent_date/1,
                      [ {#calender_time{year=1900, month=january, day=1,
                                        hour=0, min=0, sec=0,
                                        week_day=undefined, tz_diff=0},
                         <<"Resent-Date: 1 Jan 1900 00:00:00 +0000\r\n">>}
                      ])
    ].

resent_from_test_() ->
    [ ?list_pair_test(fun rfc2822:resent_from/1,
                      [ {[#name_addr{addr= <<"mike@atmosia.net">>}],
                         <<"Resent-From: <mike@atmosia.net>\r\n">>},
                        {[#name_addr{addr= <<"mike@atmosia.net">>},
                          #name_addr{addr= <<"mike@atmosia.net">>,
                                     name= <<"\"Michael Blockley\"">>}],
                         <<"Resent-From: <mike@atmosia.net>, "
                           "\"Michael Blockley\" <mike@atmosia.net>\r\n">>}
                      ])
    ].

resent_sender_test_() ->
    [ ?list_pair_test(fun rfc2822:resent_sender/1,
                      [ {#name_addr{addr= <<"mike@atmosia.net">>},
                         <<"Resent-Sender: <mike@atmosia.net>\r\n">>},
                        {#name_addr{addr= <<"mike@atmosia.net">>,
                                    name= <<"\"Michael Blockley\"">>},
                         <<"Resent-Sender: \"Michael Blockley\" "
                           "<mike@atmosia.net>\r\n">>}])
    ].

resent_to_test_() ->
    [ ?list_pair_test(fun rfc2822:resent_to/1,
                      [ {[[#name_addr{addr= <<"mike@atmosia.net">>}]],
                         <<"Resent-To: <mike@atmosia.net>\r\n">>},
                        {[[#name_addr{addr= <<"mike@atmosia.net">>}],
                          [#name_addr{addr= <<"mike@atmosia.net">>,
                                      name= <<"\"Michael Blockley\"">>}]],
                         <<"Resent-To: <mike@atmosia.net>, "
                           "\"Michael Blockley\" <mike@atmosia.net>\r\n">>}
                      ])
    ].

resent_cc_test_() ->
    [ ?list_pair_test(fun rfc2822:resent_cc/1,
                      [ {[[#name_addr{addr= <<"mike@atmosia.net">>}]],
                         <<"Resent-Cc: <mike@atmosia.net>\r\n">>},
                        {[[#name_addr{addr= <<"mike@atmosia.net">>}],
                          [#name_addr{addr= <<"mike@atmosia.net">>,
                                      name= <<"\"Michael Blockley\"">>}]],
                         <<"Resent-Cc: <mike@atmosia.net>, "
                           "\"Michael Blockley\" <mike@atmosia.net>\r\n">>}
                      ])
    ].

resent_bcc_test_() ->
    [ ?list_pair_test(fun rfc2822:resent_bcc/1,
                      [ {[], <<"Resent-Bcc:\r\n">>},
                        {[[#name_addr{addr= <<"mike@atmosia.net">>}]],
                         <<"Resent-Bcc: <mike@atmosia.net>\r\n">>},
                        {[[#name_addr{addr= <<"mike@atmosia.net">>}],
                          [#name_addr{addr= <<"mike@atmosia.net">>,
                                      name= <<"\"Michael Blockley\"">>}]],
                         <<"Resent-Bcc: <mike@atmosia.net>, "
                           "\"Michael Blockley\" <mike@atmosia.net>\r\n">>},
                        {[], <<"Resent-Bcc: \r\n">>}
                      ])
    ].

resent_msg_id_test_() ->
    [ ?list_pair_test(fun rfc2822:resent_msg_id/1,
                      [ {<<"<abc.1234@atmosia.net>">>,
                         <<"Resent-Message-ID: <abc.1234@atmosia.net>\r\n">>},
                        {<<"<\"xyzabc\"@[192.168.1.1]>">>,
                         <<"Resent-Message-ID: "
                           "<\"xyzabc\"@[192.168.1.1]>\r\n">>}
                      ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trace Fields (section 3.6.7) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

return_path_test_() ->
    [ ?string_list_pair_test(fun rfc2822:return_path/1,
                             [ {"<mike@atmosia.net>",
                                "Return-Path:<mike@atmosia.net>\r\n"},
                               {"<>",
                                "Return-Path:<>\r\n"}
                             ])
    ].

path_test_() ->
    [ ?string_list_test(fun rfc2822:path/1, ["<mike@atmosia.net>", "<>"]) ].

received_test_() ->
    [ ?list_pair_test(fun rfc2822:received/1,
                      [ {{[{<<"this">>, <<"mike@atmosia.net">>}],
                          #calender_time{year=1900, month=january, day=1,
                                         hour=0, min=0, sec=0,
                                         week_day=undefined, tz_diff=0}},
                        <<"Received: this mike@atmosia.net; "
                          "1 Jan 1900 00:00:00 +0000\r\n">>}
                      ])
    ].

name_val_list_test_() ->
    [ ?list_pair_test(fun rfc2822:name_val_list/1,
                      [ {[{<<"this">>, <<"mike@atmosia.net">>},
                          {<<"a123">>, <<"atmosia.net">>},
                          {<<"a-xy-01">>, <<"atom">>},
                          {<<"a-xy-02">>, [<<"mike@atmosia.net">>]},
                          {<<"a-xy-03">>, [<<"mike@atmosia.net">>,
                                           <<"bob@atmosia.net">>]}
                         ], <<"this mike@atmosia.net "
                              "a123 atmosia.net\r\n"
                              " a-xy-01 atom "
                              " a-xy-02 <mike@atmosia.net>\r\n"
                              " a-xy-03 <mike@atmosia.net>\r\n"
                              "         <bob@atmosia.net>">>
                        }]),
      ?_assertThrow({parse_error, expected, "list of name/value pairs"},
                    rfc2822:name_val_list(<<"\0">>))
    ].

name_val_pair_test_() ->
    [ ?list_pair_test(fun rfc2822:name_val_pair/1,
                      [ {{<<"this">>, <<"mike@atmosia.net">>},
                         <<"this mike@atmosia.net">>},
                        {{<<"a123">>, <<"atmosia.net">>},
                         <<"a123 atmosia.net">>},
                        {{<<"a-xy-01">>, <<"atom">>},
                         <<"a-xy-01 atom">>},
                        {{<<"a-xy-01">>, [<<"mike@atmosia.net">>]},
                         <<"a-xy-01 <mike@atmosia.net>">>},
                        {{<<"a-xy-01">>, [<<"mike@atmosia.net">>,
                                          <<"bob@atmosia.net">>]},
                         <<"a-xy-01 <mike@atmosia.net> <bob@atmosia.net>">>}
                      ])
    ].

item_name_test_() ->
    [ ?string_list_test(fun rfc2822:item_name/1,
                        [ "this", "a123", "a-xy-01" ])
    ].

item_value_test_() ->
    [ ?string_list_test(fun rfc2822:item_value/1,
                        [ "mike@atmosia.net",
                          "atmosia.net",
                          "atom" ]),
      ?list_pair_test(fun rfc2822:item_value/1,
                             [ {[<<"mike@atmosia.net">>],
                                 <<"<mike@atmosia.net>">>},
                               {[<<"mike@atmosia.net">>,
                                 <<"bob@atmosia.net">>],
                                <<"<mike@atmosia.net> <bob@atmosia.net>">>}
                             ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Optional Fields (section 3.6.8) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

optional_field_test_() ->
    [ ?list_pair_test(fun rfc2822:optional_field/1,
                      [{{<<"field-a">>, <<" a">>},
                        <<"field-a: a\r\n">>},
                       {{<<"field-b">>, <<" a\r\n b">>},
                        <<"field-b: a\r\n b\r\n">>}
                      ]),
      ?_assertThrow({parse_error, expected,
                     "optional (unspecified) header line"},
                    rfc2822:optional_field(<<"\0">>)),
      ?_assertThrow({parse_error, expected,
                     "optional (unspecified) header line"},
                    rfc2822:optional_field(<<"field-a: a">>))
    ].

field_name_test_() ->
    [ ?string_list_test(fun rfc2822:field_name/1,
                        [ "additional_field",
                          "another-field",
                          "field-007" ])
    ].

ftext_test_() ->
    ?random_byte_tests(fun rfc2822:ftext/1,
                       lists:seq(33, 57) ++ lists:seq(59, 126)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Miscellaneous obsolete tokens (section 4.1) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obs_text_test_set() -> [ "text", "\n\n\rtext" ].

obs_qp_test_() ->
    [ ?string_list_test(fun rfc2822:obs_qp/1,
                        [ "\\a", "\\b", "\\\n", "\\\r" ]),
      ?_assertThrow({parse_error, expected, "any quoted US-ASCII character"},
                    rfc2822:obs_qp(<<"xy">>)),
      ?_assertThrow({parse_error, expected, "any quoted US-ASCII character"},
                    rfc2822:obs_qp(<<"\\">>))
    ].

obs_text_test_() ->
    [ ?string_list_test(fun rfc2822:obs_text/1, obs_text_test_set())
    ].

obs_char_test_() ->
    [ ?random_byte_tests(fun rfc2822:obs_char/1,
                         lists:seq(0, 9) ++ [11, 12] ++
                         lists:seq(14, 127)),
      ?_assertThrow({parse_error, expected,
                     "any ASCII character except CR and LF"},
                    rfc2822:obs_char(<<>>))
    ].

obs_utext_test_() ->
    [ ?string_list_test(fun rfc2822:obs_utext/1, obs_text_test_set())
    ].

obs_phrase_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_phrase/1,
                      [ {[<<"abc">>], <<"abc">>},
                        {[<<"abc">>, <<"xyz">>], <<"abc xyz">>},
                        {[<<"abc">>, <<".">>, <<"xyz">>], <<"abc.xyz">>}
                      ])
    ].

obs_phrase_list_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_phrase_list/1,
                      [ {[[<<"abc">>]], <<"abc,">>},
                        {[[<<"abc">>,<<".">>,<<"xyz">>]],
                         <<"abc.xyz,">>},
                        {[[<<"abc">>,<<".">>,<<"xyz">>],
                          [<<"123">>,<<".">>,<<"456">>]],
                         <<"abc.xyz, 123.456">>}
                      ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete folding white space (section 4.2) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obs_fws_test_() ->
    [ ?string_list_test(fun rfc2822:obs_fws/1,
                        [ "\t", " ", " \r\n " ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete Date and Time (section 4.3) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obs_day_of_week_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_day_of_week/1,
                      [ {monday, <<"\tMon  ">>},
                        {tuesday, <<"\r\n Tue">>},
                        {wednesday, <<" Wed\r\n ">>},
                        {thursday, <<"Thu">>},
                        {friday, <<"Fri">>},
                        {saturday, <<"Sat">>},
                        {sunday, <<"Sun">>}
                      ]),
      ?_assertThrow({parse_error, expected, "day of the week name"},
                    rfc2822:obs_day_of_week(<<"\0">>))
    ].

obs_year_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_year/1,
                      [ {2001, <<"01">>},
                        {1999, <<"99">>},
                        {2001, <<"2001">>},
                        {2001, <<"    2001\t">>}
                      ]),
      ?_assertThrow({parse_error, expected, "year"},
                    rfc2822:obs_year(<<"a">>))
    ].

obs_month_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_month/1,
                      [ {january, <<"\tJan ">>},
                        {feburary, <<"\r\n Feb">>},
                        {march, <<" Mar\r\n ">>},
                        {april, <<"Apr">>},
                        {may, <<"May">>},
                        {june, <<"Jun">>},
                        {july, <<"Jul">>},
                        {august, <<"Aug">>},
                        {september, <<"Sep">>},
                        {october, <<"Oct">>},
                        {november, <<"Nov">>},
                        {december, <<"Dec">>} ])
    ].

obs_day_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_day/1,
                      [ {1, <<"  01">>},
                        {2, <<"\t02\r\n ">>},
                        {3, <<"\r\n 03">>}
                      ])
    ].

obs_hour_test_() ->
    [ ?_assertEqual({20, <<>>}, rfc2822:obs_hour(<<"  20  ">>)) ].

obs_minute_test_() ->
    [ ?_assertEqual({20, <<>>}, rfc2822:obs_minute(<<"  20\t">>)) ].

obs_second_test_() ->
    [ ?_assertEqual({20, <<>>}, rfc2822:obs_second(<<"\t20">>)) ].

obs_zone_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_zone/1,
                      [ {0, <<"UT">>},
                        {0, <<"GMT">>},
                        {-5 * 3600, <<"EST">>},
                        {-4 * 3600,  <<"EDT">>},
                        {-6 * 3600, <<"CST">>},
                        {-5 * 3600, <<"CDT">>},
                        {-7 * 3600, <<"MST">>},
                        {-6 * 3600, <<"MDT">>},
                        {-8 * 3600, <<"PST">>},
                        {-7 * 3600, <<"PDT">>},
                        {0, <<"A">>},
                        {3600, <<"B">>},
                        {($K - $B) * 3600, <<"K">>},
                        {-3600, <<"N">>},
                        {-($O - $M) * 3600, <<"O">>},
                        {0, <<"Z">>}
                      ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete Addressing (section 4.4) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obs_angle_addr_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_angle_addr/1,
                      [ {<<"<mike@atmosia.net>">>,
                         <<"<@example1.org,@example2.org:mike@atmosia.net>">>}
                      ])
    ].

obs_route_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_route/1,
                      [ {[<<"example1.org">>],
                         <<"@example1.org:">>},
                        {[<<"example1.org">>, <<"example2.org">>],
                         <<"@example1.org, @example2.org:">>}
                      ]),
      ?_assertThrow({parse_error, expected,
                     "route of an obsolete angle address"},
                    rfc2822:obs_route(<<"\0">>))
    ].

obs_domain_list_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_domain_list/1,
                      [ {[<<"example1.org">>],
                         <<"@example1.org">>},
                        {[<<"example1.org">>, <<"example2.org">>],
                         <<"@example1.org, @example2.org">>}
                      ])
    ].

obs_local_part_test_() ->
    [ ?string_list_test(fun rfc2822:obs_local_part/1,
                        [ "this", "this.that" ])
    ].

obs_domain_test_() ->
    [ ?string_list_test(fun rfc2822:obs_domain/1,
                        [ "this", "this.that" ])
    ].

obs_mbox_list_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_mbox_list/1,
                      [ {[], <<",">>},
                        {[#name_addr{addr= <<"mike@atmosia.net">>}],
                         <<"mike@atmosia.net,">>},
                        {[#name_addr{addr= <<"mike@atmosia.net">>},
                          #name_addr{addr= <<"joe@example.com">>}],
                         <<"mike@atmosia.net, joe@example.com">>},
                        {[#name_addr{addr= <<"mike@atmosia.net">>,
                                     name= <<"\"Michael Blockley\"">>}],
                         <<"\"Michael Blockley\" <mike@atmosia.net>,">>}
                      ])
    ].

obs_addr_list_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_addr_list/1,
                      [ {[], <<",">>},
                        {[[#name_addr{addr= <<"mike@atmosia.net">>}]],
                         <<"mike@atmosia.net,">>},
                        {[[#name_addr{addr= <<"mike@atmosia.net">>}],
                          [#name_addr{addr= <<"joe@example.com">>}]],
                         <<"mike@atmosia.net, joe@example.com">>},
                        {[[#name_addr{addr= <<"mike@atmosia.net">>,
                                      name= <<"\"Michael Blockley\"">>}]],
                         <<"\"Michael Blockley\" <mike@atmosia.net>,">>},
                        {[[#name_addr{addr= <<"mike@atmosia.net">>},
                           #name_addr{addr= <<"joe@example.com">>}]],
                         <<"group: <mike@atmosia.net>, <joe@example.com>;,">>}
                      ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete header fields (section 4.5) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: obs_fields/1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete origination date field (section 4.5.1) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obs_orig_date_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_orig_date/1,
                      [ {#calender_time{year=1900, month=january, day=1,
                                        hour=0, min=0, sec=0,
                                        tz_diff=0, week_day=undefined},
                         <<"Date: 1 Jan 1900 00:00:00 +0000\r\n">>}
                      ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete Originator fields (section 4.5.2) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obs_from_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_from/1,
                      [ {[#name_addr{addr= <<"mike@atmosia.net">>}],
                         <<"From: mike@atmosia.net\r\n">>} ])
    ].

obs_sender_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_sender/1,
                      [ {#name_addr{addr= <<"mike@atmosia.net">>},
                         <<"Sender: mike@atmosia.net\r\n">>} ])
    ].

obs_reply_to_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_reply_to/1,
                      [ {[ #name_addr{addr= <<"mike@atmosia.net">>} ],
                         <<"Reply-to: mike@atmosia.net\r\n">>} ])
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete destination address fields (section 4.5.3) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obs_to_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_to/1,
                      [ {[[ #name_addr{addr= <<"mike@atmosia.net">>} ]],
                         <<"To: mike@atmosia.net\r\n">>} ])
    ].

obs_cc_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_cc/1,
                      [ {[[ #name_addr{addr= <<"mike@atmosia.net">>} ]],
                         <<"Cc: mike@atmosia.net\r\n">>} ])
    ].

obs_bcc_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_bcc/1,
                      [ {[[ #name_addr{addr= <<"mike@atmosia.net">>} ]],
                         <<"Bcc: mike@atmosia.net\r\n">>}
                      ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete identification fields (section 4.5.4) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: obs_message_id/1
% TODO: obs_in_reply_to/1
% TODO: obs_references/1
obs_message_id_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_message_id/1,
                      [ {<<"<abc.1234@atmosia.net>">>,
                         <<"Message-ID: <abc.1234@atmosia.net>\r\n">>},
                        {<<"<\"xyzabc\"@[192.168.1.1]>">>,
                         <<"Message-ID: <\"xyzabc\"@[192.168.1.1]>\r\n">>}
                      ])
    ].

obs_in_reply_to_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_in_reply_to/1,
                      [ {[<<"<abc.1234@atmosia.net>">>],
                         <<"In-Reply-To: <abc.1234@atmosia.net>\r\n">>},
                        {[<<"<abc.1234@atmosia.net>">>,
                          <<"<\"xyzabc\"@[192.168.1.1]>">>],
                         <<"In-Reply-To: <abc.1234@atmosia.net>\r\n"
                           "             <\"xyzabc\"@[192.168.1.1]>\r\n">>}
                      ])
    ].



obs_references_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_references/1,
                      [ {[<<"<abc.1234@atmosia.net>">>],
                         <<"References: <abc.1234@atmosia.net>\r\n">>},
                        {[<<"<abc.1234@atmosia.net>">>,
                          <<"<\"xyzabc\"@[192.168.1.1]>">>],
                         <<"References: <abc.1234@atmosia.net>\r\n"
                           "            <\"xyzabc\"@[192.168.1.1]>\r\n">>}
                      ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete informational fields (section 4.5.5) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obs_subject_test_() ->
    [ ?string_list_pair_test(fun rfc2822:obs_subject/1,
                             [ {" this is a subject",
                                "Subject: this is a subject\r\n"},
                               {" also a\r\n subject",
                                "Subject: also a\r\n subject\r\n"}
                             ])
    ].

obs_comments_test_() ->
    [ ?string_list_pair_test(fun rfc2822:obs_comments/1,
                             [ {" this is a comment",
                                "Comments: this is a comment\r\n"},
                               {" also a\r\n comment",
                                "Comments: also a\r\n comment\r\n"}
                             ])
    ].

obs_keywords_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_keywords/1,
                      [ {[[<<"abc">>]], <<"Keywords: abc,\r\n">>},
                        {[[<<"abc">>], [<<"xyz">>]],
                         <<"Keywords: abc, xyz\r\n">>},
                        {[[<<"abc">>], [<<"xyz">>], [<<"123">>]],
                         <<"Keywords: abc, xyz,\r\n 123\r\n">>},
                        {[[<<"abc">>], [<<"xyz">>], [<<"123">>]],
                         <<"Keywords: abc, xyz, 123\r\n">>}
                      ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete resent fields (section 4.5.6) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obs_resent_date_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_resent_date/1,
                      [ {#calender_time{year=1900, month=january, day=1,
                                        hour=0, min=0, sec=0,
                                        week_day=undefined, tz_diff=0},
                         <<"Resent-Date: 1 Jan 1900 00:00:00 +0000\r\n">>}
                      ])
    ].

obs_resent_from_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_resent_from/1,
                      [ {[#name_addr{addr= <<"mike@atmosia.net">>}],
                         <<"Resent-From: <mike@atmosia.net>\r\n">>},
                        {[#name_addr{addr= <<"mike@atmosia.net">>},
                          #name_addr{addr= <<"mike@atmosia.net">>,
                                     name= <<"\"Michael Blockley\"">>}],
                         <<"Resent-From: <mike@atmosia.net>, "
                           "\"Michael Blockley\" <mike@atmosia.net>\r\n">>}
                      ])
    ].

obs_resent_sender_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_resent_send/1,
                      [ {#name_addr{addr= <<"mike@atmosia.net">>},
                         <<"Resent-Sender: <mike@atmosia.net>\r\n">>},
                        {#name_addr{addr= <<"mike@atmosia.net">>,
                                    name= <<"\"Michael Blockley\"">>},
                         <<"Resent-Sender: \"Michael Blockley\" "
                           "<mike@atmosia.net>\r\n">>}])
    ].

obs_resent_to_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_resent_to/1,
                      [ {[#name_addr{addr= <<"mike@atmosia.net">>}],
                         <<"Resent-To: <mike@atmosia.net>\r\n">>},
                        {[#name_addr{addr= <<"mike@atmosia.net">>},
                          #name_addr{addr= <<"mike@atmosia.net">>,
                                      name= <<"\"Michael Blockley\"">>}],
                         <<"Resent-To: <mike@atmosia.net>, "
                           "\"Michael Blockley\" <mike@atmosia.net>\r\n">>}
                      ])
    ].

obs_resent_cc_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_resent_cc/1,
                      [ {[#name_addr{addr= <<"mike@atmosia.net">>}],
                         <<"Resent-Cc: <mike@atmosia.net>\r\n">>},
                        {[#name_addr{addr= <<"mike@atmosia.net">>},
                          #name_addr{addr= <<"mike@atmosia.net">>,
                                      name= <<"\"Michael Blockley\"">>}],
                         <<"Resent-Cc: <mike@atmosia.net>, "
                           "\"Michael Blockley\" <mike@atmosia.net>\r\n">>}
                      ])
    ].

obs_resent_bcc_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_resent_bcc/1,
                      [ {[], <<"Resent-Bcc:\r\n">>},
                        {[#name_addr{addr= <<"mike@atmosia.net">>}],
                         <<"Resent-Bcc: <mike@atmosia.net>\r\n">>},
                        {[#name_addr{addr= <<"mike@atmosia.net">>},
                          #name_addr{addr= <<"mike@atmosia.net">>,
                                      name= <<"\"Michael Blockley\"">>}],
                         <<"Resent-Bcc: <mike@atmosia.net>, "
                           "\"Michael Blockley\" <mike@atmosia.net>\r\n">>}
                      ])
    ].

obs_resent_mid_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_resent_mid/1,
                      [ {<<"<abc.1234@atmosia.net>">>,
                         <<"Resent-Message-ID: <abc.1234@atmosia.net>\r\n">>},
                        {<<"<\"xyzabc\"@[192.168.1.1]>">>,
                         <<"Resent-Message-ID: "
                           "<\"xyzabc\"@[192.168.1.1]>\r\n">>}
                      ])
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete trace fields (section 4.5.7) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obs_return_test_() ->
    [ ?string_list_pair_test(fun rfc2822:obs_return/1,
                             [ {"<mike@atmosia.net>",
                                "Return-Path: <mike@atmosia.net>\r\n"},
                               {"<>", "Return-Path:<>\r\n"}
                             ])
    ].

obs_path_test_() ->
    [ ?string_list_test(fun rfc2822:obs_path/1,
                        ["<mike@atmosia.net>"]) ].

obs_received_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_received/1,
                      [ {[{<<"this">>, <<"mike@atmosia.net">>}],
                         <<"Received: this mike@atmosia.net\r\n">>}
                      ])
    ].

obs_optional_test_() ->
    [ ?list_pair_test(fun rfc2822:obs_optional/1,
                      [{{<<"field-a">>, <<" a">>},
                        <<"field-a: a\r\n">>},
                       {{<<"field-b">>, <<" a\r\n b">>},
                        <<"field-b: a\r\n b\r\n">>}
                      ]),
      ?_assertThrow({parse_error, expected,
                     "optional (unspecified) header line"},
                    rfc2822:optional_field(<<"\0">>)),
      ?_assertThrow({parse_error, expected,
                     "optional (unspecified) header line"},
                    rfc2822:optional_field(<<"field-a: a">>))
    ].
