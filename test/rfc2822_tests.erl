%% rfc2822_tests.erl
%% Unit tests for the RFC2822 parser provided by rfc2822.erl
%%
%% author: mikeyhc <mikeyhc@atmosia.net>

-module(rfc2822_tests).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Useful Combinators %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

maybe_option_test_() ->
    [ ?_assertEqual({$a, <<"bc">>},
                    rfc2822:maybe_option(parserlang, char, [$a, <<"abc">>])),
      ?_assertEqual({undefined, <<"abc">>},
                    rfc2822:maybe_option(parserlang, char, [$x, <<"abc">>]))
    ].

%% TODO: unfold tests (cfws not defined yet)

header_test_() ->
    [ ?_assertEqual({<<" a">>, <<"b">>},
                    rfc2822:header(<<"header">>, binary, copy,
                                   <<"header: a\r\nb">>)),
      ?_assertEqual({<<" a">>, <<"b">>},
                    rfc2822:header(<<"header">>, binary, copy,
                                   <<"Header: a\r\nb">>)),
      ?_assertThrow({parse_error, expected, "header header line"},
                    rfc2822:header(<<"header">>, binary, copy,
                                   <<"Subject: a\r\nb">>)),
      ?_assertThrow({parse_error, expected, "header header line"},
                    rfc2822:header(<<"header">>, binary, copy,
                                   <<"header: a">>)),
      ?_assertError({badarg, a}, rfc2822:header(a, b, c, d))
    ].

obs_header_test_() ->
    [ ?_assertEqual({<<" a">>, <<"b">>},
                    rfc2822:obs_header(<<"header">>, binary, copy,
                                       <<"header : a\r\nb">>)),
      ?_assertThrow({parse_error, expected, "xyz obsolete header line"},
                    rfc2822:obs_header(<<"xyz">>, binary, copy,
                                       <<"Subject: a\r\nb">>))
    ].

no_ws_ctl_test_() ->
    [ ?_assertEqual({7, <<>>}, rfc2822:no_ws_ctl(<<7>>)),
      ?_assertThrow({parse_error, expected,
                     "US-ASCII non-whitespace control character"},
                    rfc2822:no_ws_ctl(<<"a">>)),
      ?_assertError({badarg, a}, rfc2822:no_ws_ctl(a))
    ].

text_test_() ->
    [ ?_assertEqual({$a, <<>>}, rfc2822:text(<<"a">>)),
      ?_assertThrow({parse_error, expected,
                     "US-ASCII character (excluding CR and LF)"},
                    rfc2822:text(<<"\r">>)),
      ?_assertError({badarg, a}, rfc2822:text(a))
    ].

specials_test_() ->
    [ ?_assertEqual({$@, <<>>}, rfc2822:specials(<<"@">>)),
      ?_assertThrow({parse_error, expected, _}, rfc2822:specials(<<"a">>)),
      ?_assertError({badarg, a}, rfc2822:specials(a))
    ].


