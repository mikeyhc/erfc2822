%% rfc2822.erl
%% This module provides parsers for the grammer defined in RFC2822,
%% "Internet Message Format", <http://www.faqs.org/rfcs/rfc2822.html>.
%% This code is largely a port from the haskell library hsemail
%% <https://hackage.haskell.org/package/hsemail-1.7.7/docs/src/
%% Text-ParserCombinators-Parsec-Rfc2822.html>
%%
%% author: mikeyhc <mikeyhc@atmosia.net>

-module(rfc2822).
-include("calender_time.hrl").
-include("message.hrl").
-include("name_addr.hrl").
-include("timediff.hrl").
%-compile(export_all).
-export([% userful parser combinators
         maybe_option/2, unfold/2, obs_header/3, header/3,

         % primitive tokens (section 3.2.1)
         no_ws_ctl/1, text/1, specials/1,

         % quoted char (section 3.2.2)
         quoted_pair/1,

         % folding whitespace and comments (section 3.2.3)
         fws/1, ctext/1, comment/1, cfws/1,

         % atom (section 3.2.4)
         atext/1, atom/1, dot_atom/1, dot_atom_text/1,

         % quoted strings (section 3.2.5)
         qtext/1, qcontent/1, quoted_string/1,

         % miscellaneous tokens (section 3.2.6)
         word/1, phrase/1, utext/1, unstructured/1,

         % date and time specification (section 3.3)
         date_time/1, day_of_week/1, day_name/1, date/1, year/1, month/1,
         month_name/1, day_of_month/1, day/1, time/1, time_of_day/1,
         hour/1, minute/1, second/1, zone/1,

         % address specification (section 3.4)
         address/1, mailbox/1, name_addr/1, angle_addr/1, group/1,
         display_name/1, mailbox_list/1, address_list/1,

         % addr-spec specification (section 3.4.1)
         addr_spec/1, local_part/1, domain/1, domain_literal/1,
         dcontent/1, dtext/1,

         % overall message syntax (section 3.5)
         message/1, body/1,

         % field definitions (section 3.6)
         fields/1,

         % the origination date field (section 3.6.1)
         orig_date/1,

         % originator fields (section 3.6.2)
         from/1, sender/1, reply_to/1,

         % destination address fields (section 3.6.3)
         to/1, cc/1, bcc/1,

         % identification fields (section 3.6.4)
         message_id/1, in_reply_to/1, references/1, msg_id/1, id_left/1,
         id_right/1, no_fold_quote/1, no_fold_literal/1,

         % informational fields (section 3.6.5)
         subject/1, comments/1, keywords/1,

         % resent fields (section 3.6.6)
         resent_date/1, resent_from/1, resent_sender/1, resent_to/1,
         resent_cc/1, resent_bcc/1, resent_msg_id/1,

         % trace fields (section 3.6.7)
         return_path/1, path/1, received/1, name_val_list/1, name_val_pair/1,
         item_name/1, item_value/1,

         % optional fields (section 3.6.8)
         optional_field/1, field_name/1, ftext/1,

         % miscellaneous obsolete tokens (section 4.1)
         obs_qp/1, obs_text/1, obs_char/1, obs_utext/1, obs_phrase/1,
         obs_phrase_list/1,

         % obsolete folding whitespace (section 4.2)
         obs_fws/1,

         % obsolete date and time (sectio 4.3)
         obs_day_of_week/1, obs_year/1, obs_month/1, obs_day/1, obs_hour/1,
         obs_minute/1, obs_second/1, obs_zone/1,

         % obsolete addressing (section 4.4)
         obs_angle_addr/1, obs_route/1, obs_domain_list/1, obs_local_part/1,
         obs_domain/1, obs_mbox_list/1, obs_addr_list/1,

         % obsolete header fields (section 4.5)
         obs_fields/1,

         % obsolete origination date field (section 4.5.1)
         obs_orig_date/1,

         % obsolete originator fields (section 4.5.2)
         obs_from/1, obs_sender/1, obs_reply_to/1,

         % obsolete destination address fields (section 4.5.3)
         obs_to/1, obs_cc/1, obs_bcc/1,

         % obsolete identification fields (section 4.5.4)
         obs_message_id/1, obs_in_reply_to/1, obs_references/1,
         obs_id_left/1, obs_id_right/1,

         % obsolete informational fields (section 4.5.5)
         obs_subject/1, obs_comments/1, obs_keywords/1,

         % obsolete resent fields (section 4.5.6)
         obs_resent_from/1, obs_resent_send/1, obs_resent_date/1,
         obs_resent_to/1, obs_resent_cc/1, obs_resent_bcc/1,
         obs_resent_mid/1, obs_resent_reply/1,

         % obsolete trace fields (section 4.5.7)
         obs_return/1, obs_received/1, obs_path/1, obs_optional/1,

         % TESTING ONLY FUNCTIONS
         % TODO: remove these
         bin_to_int/1, intersperse/2]).

-type dow() :: monday | tuesday | wednesday | thursday | friday | saturday |
               sunday.
-type month() :: january | feburary | march | april | may | june | july |
                 august | september | october | november |december.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Useful Combinators %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% return 'undefined' if the given parser doesn't match
-spec maybe_option(fun((binary()) -> {any(), binary}), binary())
      -> {any(), binary()}.
maybe_option(F, A) -> parserlang:option(undefined,F,A).

%% wraps the parser in optional cfws
-spec unfold(fun((binary()) -> {binary(), binary()}), binary())
      -> {binary(), binary()}.
unfold(F, A) ->
    {_, T1} = parserlang:optional(fun cfws/1, A),
    {Body, T2} = F(T1),
    {_, T3} = parserlang:optional(fun cfws/1, T2),
    {Body, T3}.

%% construct a parser for a message header line from the header
%% name and a parser for the body
-spec header(<<_:8,_:_*8>>, fun((binary()) -> {any(), binary()}),
             <<_:8,_:_*8>>) -> {any(), binary()}.
header(N, F, A) when is_binary(N) ->
    HF = fun(X) -> parserlang:case_string(<<N/binary, $:>>, X) end,
    try
        parserlang:between(HF, crlf_tail_fun(), F, A)
    catch
        _ -> throw({parse_error, expected,
                    binary:bin_to_list(N) ++ " header line"})
    end;
header(N, F, A) when is_list(N) -> header(binary:list_to_bin(N), F, A);
header(N, _, _) -> error({badarg, N}).

%% returns a function which will match an crlf
-spec crlf_tail_fun() -> fun((binary()) -> {match, binary()} | no_match).
crlf_tail_fun() ->
    fun(X) ->
            try
                {_, T} = rfc2234:crlf(X),
                {match, T}
            catch
                error:{badmatch, _} -> throw({parse_error, expected, "\r\n"})
            end
    end.

%% like 'header' but allows the obsolete white-space rules.
-spec obs_header(<<_:8,_:_*8>>, fun((binary()) -> {any(), binary()}),
                 <<_:8,_:_*8>>) -> {any(), binary()}.
obs_header(N, F, A) when is_list(N) -> obs_header(binary:list_to_bin(N), F, A);
obs_header(N, F, A) ->
    H = fun(X) ->
                {H1, T1} = parserlang:case_string(N, X),
                {H2, T2} = parserlang:many(fun rfc2234:wsp/1, T1),
                {_, T3} = parserlang:char($:, T2),
                BC= parserlang:bin_concat(H2),
                {<<H1/binary, BC/binary, $:>>, T3}
        end,
    try
        parserlang:between(H, crlf_tail_fun(), F, A)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             binary:bin_to_list(N) ++
                                             " obsolete header line"})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitive Tokens (section 3.2.1) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% match any US-ASCII non-whitespace control character.
-spec no_ws_ctl(<<_:8,_:_*8>>) -> {1..8 | 11..12 | 14..31 | 127, binary()}.
no_ws_ctl(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= 1 andalso H =< 8 orelse
       H == 11 orelse H == 12 orelse
       H >= 14 andalso H =< 31 orelse
       H == 127 -> {H, T};
       true     -> throw({parse_error, expected,
                          "US-ASCII non-whitespace control character"})
    end;
no_ws_ctl(X) -> error({badarg, X}).

%% match any US-ASCII character except for \r and \n
-spec text(<<_:8,_:_*8>>) -> {1..9 | 11..12 | 14..127, binary()}.
text(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= 1 andalso H =< 9 orelse
       H == 11 orelse H == 12 orelse
       H >= 14 andalso H =< 127 -> {H, T};
       true -> throw({parse_error, expected,
                      "US-ASCII character (excluding CR and LF)"})
    end;
text(X) -> error({badarg, X}).

%% match any of the RFC's "special" characters: ()<>[]:;@,.\"
-spec specials(<<_:8,_:_*8>>) -> {byte(), binary()}.
specials(X) when is_binary(X) -> parserlang:oneof(<<"()<>[]:;@,.\\\"">>, X);
specials(X) -> error({badarg, X}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Quoted characters (section 3.2.2) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% match a "quoted pair". All characters matched by 'text' may be quoted.
%% Note that the parser return 'both' characters, the backslash and the
%% actual content.
-spec quoted_pair(<<_:8,_:_*8>>) -> {<<_:16>>, binary()}.
quoted_pair(Text) when is_binary(Text) ->
    F = fun(X) -> <<$\\, C, T/binary>> = X, {<<$\\, C>>, T} end,
    parserlang:orparse([fun obs_qp/1, F], Text, "quoted pair");
quoted_pair(Text) -> error({badarg, Text}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Folding white space and comments (section 3.2.3) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% match "folding whitespace". That is any combination of 'wsp' and 'crlf'
%% followed by 'wsp'.
-spec fws(binary()) -> {[byte() | binary()], binary()}.
fws(X) ->
    Blanks = fun(Y) -> parserlang:many1(fun rfc2234:wsp/1, Y) end,
    LineBreak = fun(Y) ->
                        {H1, T1} = rfc2234:crlf(Y),
                        {H2, T2} = Blanks(T1),
                        {[H1, H2], T2}
                end,
    Choice = parserlang:choice([Blanks, LineBreak], "folding whitespace"),
    {R, T} = parserlang:many1(Choice, X),
    {binary:list_to_bin(lists:concat(R)), T}.

%% match any non-whitespace, non-control character except for "(", ")" and
%% "\". this is used to describe the legal content of 'comment's.
%%
%% Note: This parser accepts 8-bit characters, even though this is not
%% legal according to the RFC. Unfortunately, 8-bit content in comments
%% has become fairly common in the real world, so we'll just accept the
%% fact.
-spec ctext(<<_:8,_:_*8>>) -> {33..39 | 42..91 | 93..126 | 128..255, binary()}.
ctext(X) when is_binary(X) ->
    Err = {parse_error, expected, "any regular character (excluding '(', ')'"
           ", '\\')"},
    ExtraChars = fun(Y) ->
                         <<H, T/binary>> = Y,
                         if H >= 33 andalso H =< 39 orelse
                            H >= 42 andalso H =< 91 orelse
                            H >= 93 andalso H =< 126 orelse
                            H >= 128 andalso H =< 255 -> {H, T};
                            true -> throw(Err)
                         end
                 end,
    % parserlang:orparse([{rfc2822, no_ws_ctl}, ExtraChars], X, Err).
    parserlang:orparse([ExtraChars], X, Err);
ctext(X) -> error({badarg, X}).

%% match a "comment". that is any 'ctext', 'quoted_pair's, and 'fws' between
%% brackets. comments may nest.
-spec comment(<<_:16,_:_*8>>) -> {<<_:16,_:_*16>>, binary()}.
comment(X) ->
    ManyCtext = fun(Y) ->
                        {R, T} = parserlang:many1(fun ctext/1, Y),
                        {parserlang:bin_concat(R), T}
                end,
    CContent = fun(Y) ->
                        Choice = parserlang:choice([ManyCtext,
                                                    fun quoted_pair/1,
                                                    fun comment/1],
                                                   "comment text"),
                        {H1, T1} = parserlang:option(<<>>, fun fws/1, Y),
                        {H2, T2} = Choice(T1),
                        {parserlang:bin_join(H1, H2), T2}
               end,
    {_, T1} = parserlang:char($(, X),
    {H2, T2} = parserlang:many(CContent, T1),
    {H3, T3} = parserlang:option(<<>>, fun fws/1, T2),
    {_, T4} = parserlang:char($), T3),
    BC = parserlang:bin_concat(H2),
    {<<$(, BC/binary, H3/binary, $)>>, T4}.

%% match any combination of 'fws' and 'comments'.
-spec cfws(binary()) -> {binary(), binary()}.
cfws(X) ->
    Choice = parserlang:choice([fun(Y) -> fws(Y) end,
                                fun(Y) -> comment(Y) end],
                               "comment or folding white space"),
    {R, T} = parserlang:many1(Choice, X),
    {parserlang:bin_concat(R), T}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Atom (section 3.2.4) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% match any US-ASCII character except for control characters, 'specials',
%% or space. 'aom' and 'dot_atom' are made up of this.
-spec atext(<<_:8,_:_*8>>) -> {byte(), binary()}.
atext(X) ->
    parserlang:orparse([{rfc2234, alpha},
                        {rfc2234, digit},
                        fun(Y) ->
                                parserlang:oneof(<<"!#$%&'*+-/=?^_`{|}~">>, Y)
                        end], X,
                       "US-ASCII character (excluding controls, space and "
                       "specials)").

%% match one or more 'atext' characters and skip and preceeding or trailing
%% 'cfws'.
-spec atom(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
atom(X) ->
    try
        {R, T} = unfold(fun(Y) -> parserlang:many1(fun atext/1, Y) end, X),
        {parserlang:bin_concat(R), T}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected, "atom"})
    end.

%% match 'dot_atom_text' and skip any preceeding or trailing 'cfws'
-spec dot_atom(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
dot_atom(X) ->
    try
        unfold(fun(Y) -> dot_atom_text(Y) end, X)
    catch
        {parse_error, expected, _} ->
            throw({parse_error, expected, "dot atom"})
    end.

%% add the given element between each element
intersperse([], _) -> [];
intersperse([X], _) -> [X];
intersperse([H|T], C) -> [H,C|intersperse(T, C)].

%% match two or mote 'atext's interspersed with dots
-spec dot_atom_text(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
dot_atom_text(X) ->
    Content = fun(Y) -> parserlang:many1(fun atext/1, Y) end,
    Sep = fun(Y) -> parserlang:char($., Y) end,
    {R, T} = parserlang:sepby1(Content, Sep, X),
    {binary:list_to_bin(lists:concat(intersperse(R, "."))), T}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Quoted Strings (section 3.2.5) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% match any non-whitespace, non-control US-ASCII character except for
%% "\" and """
-spec qtext(<<_:8,_:_*8>>) -> {33 | 35..91 | 93..126, binary()}.
qtext(X) ->
    QChars = fun(Y) when is_binary(Y) ->
                     <<H, T/binary>> = Y,
                     if H == 33 orelse H >= 35 andalso H =< 91 orelse
                        H >= 93 andalso H =< 126 -> {H, T};
                        true -> throw({parse_error, expected, "nomsg"})
                     end;
                (Y) -> error({badarg, Y})
             end,
    %parserlang:orparse([{rfc2822, no_ws_ctl}, QChars], X,
    %                   "US-ASCII character (excluding '\\' and '\"')").
    parserlang:orparse([QChars], X,
                       "US-ASCII character (excluding '\\' and '\"')").

%% match either 'qtext' or 'quoted_pair'
-spec qcontent(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
qcontent(X) ->
    ManyQText = fun(Y) ->
                        {R, T} = parserlang:many1(fun qtext/1, Y),
                        {binary:list_to_bin(R), T}
                end,
    parserlang:orparse([ManyQText, fun quoted_pair/1], X,
                       "quoted string content").

%% match any number of 'qcontent' between double quotes. Any 'cfws'
%% preceeding or following the "atom" is skip automatically.
-spec quoted_string(<<_:16,_:_*8>>) -> {<<_:16,_:_*8>>, binary()}.
quoted_string(X) ->
    Body = fun(Y) ->
                   {H1, T1} = parserlang:option(<<>>, fun fws/1, Y),
                   {H2, T2} = qcontent(T1),
                   {parserlang:bin_join(H1, H2), T2}
           end,
    Quoted = fun(Y) ->
                     {H1, T1} = rfc2234:dquote(Y),
                     {H2, T2} = parserlang:many(Body, T1),
                     {H3, T3} = parserlang:option(<<>>, fun fws/1, T2),
                     {H4, T4} = rfc2234:dquote(T3),
                     BC = parserlang:bin_concat(H2),
                     {parserlang:bin_concat([H1, BC, H3, H4]), T4}
             end,
    try
        unfold(Quoted, X)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "quoted string"});
        error:{badmatch, _} -> throw({parse_error, expected, "quoted string"})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Miscellaneous tokens (section 3.2.6) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% match either 'atom' or 'quoted_string'
-spec word(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
word(X) ->
    Word = fun(Y) ->
                   parserlang:orparse([fun atom/1, fun quoted_string/1],
                                      Y, "word")
           end,
    unfold(Word, X).

%% match either one or more 'word's or an 'obs_phrase'
-spec phrase(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
phrase(X) -> obs_phrase(X).

%% match any non-whitespace, non-control US-ASCII character except for
%% "\" and """
-spec utext(<<_:8,_:_*8>>) -> {byte(), binary()}.
utext(X) ->
    UChar = fun(Y) when is_binary(Y) ->
                     <<H, T/binary>> = Y,
                     if H == 9 orelse H == 32 orelse H == 33 orelse
                        H >= 35 andalso H =< 91 orelse
                        H >= 93 andalso H =< 126 -> {H, T};
                        true -> throw({parse_error, expected,
                                       "regular US-ASCII character "
                                       "(excluding '\\' and '\"')"})
                     end;
                (Y) -> error({badarg, Y})
            end,
    %parserlang:orparse([{rfc2822, no_ws_ctl}, UChar], X,
    %                   "regular US-ASCII character (excluding "
    %                   "'\\' and '\"'").
    parserlang:orparse([UChar], X, "regular US-ASCII character "
                       "(excluding '\\' and '\"'").

%% match any number of 'utext' tokens.
%%
%% "Unstructured text" is used in free text fields such as 'subject'.
%% Please not that any comment or whitespace that prefaces or follows
%% the actual 'utext' is *included* in the returned string.
-spec unstructured(<<_:8,_:_*8>>) -> {<<_:_*8>>, binary()}.
unstructured(X) ->
    Func = fun(Y) ->
                   {H1, T1} = utext(Y),
                   {H2, T2} = parserlang:option(<<>>, fun fws/1, T1),
                   {<<H1, H2/binary>>, T2}
           end,
    {H1, T1} = parserlang:option(<<>>, fun fws/1, X),
    {H2, T2} = parserlang:many(Func, T1),
    {parserlang:bin_concat([H1|H2]), T2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Date and Time Specification (section 3.3) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Parse a date and time specification of the form
%%
%% > Thu, 19 Dec 2002 20:35:46 +0200
%%
%% where the weekday specification "Thu," is optional. The parser returns
%% a calender_time record, which is set to the appropriate values. Note,
%% though, that not all fields of 'calender_time' will be set correctly.
%% The day_of_year field will always be returned as 0. The timezone name
%% will always be empty: "".
%%
%% Nor with the 'date_time' parser perform *any* consistency checking.
%% It will accept
%%
%% >  40 Apr 2002 13:12 +0100
%%
%% as a perfectly valid date.

% TODO: accurate min-size for type
-spec date_time(<<_:8,_:_*8>>) -> {#calender_time{}, binary()}.
date_time(X) ->
    F = fun(Y) ->
                {H1, T1} = day_of_week(Y),
                {_, T2} = parserlang:char($,, T1),
                {H1, T2}
        end,
    {WD, T1} = parserlang:option(undefined, F, X),
    {Y, M, D, T2} = date(T1),
    {_, T3} = fws(T2),
    {TD, Z, T4} = time(T3),
    {_, T5} = parserlang:optional(fun cfws/1, T4),
    {#calender_time{year=Y, month=M, day=D, hour=TD#timediff.hour,
                    min=TD#timediff.min, sec=TD#timediff.sec, week_day=WD,
                    tz_diff=Z}, T5}.

%% this parser matchs a 'day_name' or an 'obs_day_of_week' (optionally
%% wrapped in folding whitespace) and returns its 'Day' value.
-spec day_of_week(<<_:24,_:_*8>>) -> {dow(), binary()}.
day_of_week(X) ->
    F = fun(Y) -> parserlang:optional(fun fws/1, Y) end,
    DayName = fun(Y) -> parserlang:between(F, F, fun day_name/1, Y) end,
    parserlang:orparse([DayName, {rfc2822, obs_day_of_week}], X,
                       "name of a day-of-the-week").

%% this parser will take the abbreviated weekday name ("Mon", "Tue", ...)
%% and return the appropriate atom.
-spec day_name(<<_:16,_:_*8>>) -> {dow(), binary()}.
day_name(X) ->
    parserlang:orparse([name_atom_helper("Mon", monday),
                        name_atom_helper("Tue", tuesday),
                        name_atom_helper("Wed", wednesday),
                        name_atom_helper("Thu", thursday),
                        name_atom_helper("Fri", friday),
                        name_atom_helper("Sat", saturday),
                        name_atom_helper("Sun", sunday)
                       ], X, "name of a day-of-the-week").

%% helper method for generating day abbrev -> day atom functions
-spec name_atom_helper(string(), X) -> fun((binary()) -> {X, binary()}).
name_atom_helper(String, Ret) ->
    Bin = binary:list_to_bin(String),
    fun(X) ->
            try
                {_, T} = parserlang:case_string(Bin, X),
                {Ret, T}
            catch
                {parse_error, expected, _} ->
                    throw({parse_error, expected, String})
            end
    end.

%% this parser will match a date of the form "dd Month yyyy" and return a
%% triple of of the form (Int, Month, Int) - corresponding to
%% (year, month, day)
-spec date(<<_:48,_:_*8>>) -> {integer(), integer(), integer(), binary()}.
date(X) ->
    try
        {D, T1} = day(X),
        {M, T2} = month(T1),
        {Y, T3} = year(T2),
        {Y, M, D, T3}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "date specification"})
    end.

%% this parser will match a four digit number and return is integer value.
%% No range checking is performed.
-spec year(<<_:32,_:_*8>>) -> {integer(), binary()}.
year(X) ->
    try
        {H, T} = parserlang:manyN(4, fun(Y) -> rfc2234:digit(Y) end, X),
        {bin_to_int(parserlang:bin_concat(H)), T}
    catch
        % {parse_error, expected, _} -> throw({parse_error, expected, "year"});
        % error:{badarg, _} -> throw({parse_error, expected, "year"});
        error:{badmatch, _} -> throw({parse_error, expected, "year"})
    end.

%% converts a number in binary to its integer representation, no checking
%% is performed
-spec bin_to_int(binary()) -> integer().
bin_to_int(L) -> lists:foldl(fun(X, Acc) -> Acc * 10 + X - $0 end, 0,
                             binary:bin_to_list(L)).

%% this parser will match a 'month_name', optionally wrapped in a folding
%% whitespace, or an 'obs_month' and return a month atom.
-spec month(<<_:24,_:_*8>>) -> {month(), binary()}.
month(X) ->
    H = fun(Y) -> parserlang:optional(fun fws/1, Y) end,
    Month = fun(Y) -> parserlang:between(H, H, fun month_name/1, Y) end,
    parserlang:orparse([Month, {rfc2822, obs_month}], X, "month name").

%% this parser will match the abbreviated month names ("Jan", "Feb", ...) and
%% return the approriate month atom.
-spec month_name(<<_:16,_:_*8>>) -> month().
month_name(X) ->
    parserlang:orparse([name_atom_helper("Jan", january),
                        name_atom_helper("Feb", feburary),
                        name_atom_helper("Mar", march),
                        name_atom_helper("Apr", april),
                        name_atom_helper("May", may),
                        name_atom_helper("Jun", june),
                        name_atom_helper("Jul", july),
                        name_atom_helper("Aug", august),
                        name_atom_helper("Sep", september),
                        name_atom_helper("Oct", october),
                        name_atom_helper("Nov", november),
                        name_atom_helper("Dec", december)],
                       X, "month name").

%% internal helper function: match a 1 or 2-digit number (day of month)
-spec day_of_month(<<_:8,_:_*8>>) -> {integer(), binary()}.
day_of_month(X) ->
    {H, T} = parserlang:manyNtoM(1, 2, fun rfc2234:digit/1, X),
    {bin_to_int(parserlang:bin_concat(H)), T}.

%% match a 1 or 2-digit number (day of month), recognizing both standard
%% and obsolete folding syntax.
-spec day(<<_:8,_:_*8>>) -> {integer(), binary()}.
day(X) -> parserlang:orparse([{rfc2822, obs_day},
                              {rfc2822, day_of_month}],
                             X, "day of month").

%% this parser will match a 'time_of_day' specification followed by a
%% 'zone'. It returns the tuple (timediff, int) corresponding to the return
%% values or either parser.
% TODO: accurate min-val on type
-spec time(<<_:8,_:_*8>>) -> {timediff(), integer(), binary()}.
time(X) ->
    {Time, T1} = time_of_day(X),
    {_, T2} = fws(T1),
    {Zone, T3} = zone(T2),
    {Time, Zone, T3}.

%% this parser will match a time-of-day specification of "hh:mm" or
%% "hh:mm:ss" and return the corresponding time ad a 'timediff'.
-spec time_of_day(<<_:40,_:_*8>>) -> {timediff(), binary()}.
time_of_day(X) ->
    F = fun(Y) ->
                {_, T1} = parserlang:char($:, Y),
                second(T1)
        end,
    {H, T1} = hour(X),
    {_, T2} = parserlang:char($:, T1),
    {M, T3} = minute(T2),
    {S, T4} = parserlang:option(0, F, T3),
    {#timediff{hour=H, min=M, sec=S}, T4}.

%% this parser will amtch a two-digit number and return its integer value.
%% no range checking is performed
-spec hour(<<_:16,_:_*8>>) -> {integer(), binary()}.
hour(X) ->
    F = fun(Y) -> rfc2234:digit(Y) end,
    try
        {R, T} = parserlang:count(2, F, X),
        {bin_to_int(parserlang:bin_concat(R)), T}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected, "hour"});
        error:{badmatch, _} -> throw({parse_error, expected, "hour"})
    end.

%% this parser will match a 2 digit number and returns its integer value.
%% no range checking is performed.
-spec minute(<<_:16,_:_*8>>) -> {integer(), binary()}.
minute(X) ->
    try
        hour(X)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected, "minute"})
    end.

%% this parser will match a  2 digit number and return its integer value.
%% no range checking is performed.
-spec second(<<_:16,_:_*8>>) -> {integer(), binary()}.
second(X) ->
    try
        hour(X)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected, "second"})
    end.

%% this parser will match a timezone specification of the form "+hhmm" or
%% "-hhmm" and return the zone's offset to UTC in seconds as an integer.
%% 'obs_zone' is matched as well.
-spec zone(<<_:16,_:_*8>>) -> {integer(), binary()}.
zone(X) ->
    PosZone = fun(Y) ->
                      {_, T1} = parserlang:char($+, Y),
                      {H, T2} = hour(T1),
                      {M, T3} = minute(T2),
                      {H*60+M, T3}
              end,
    NegZone = fun(Y) ->
                      {_, T1} = parserlang:char($-, Y),
                      {H, T2} = hour(T1),
                      {M, T3} = minute(T2),
                      {-(H*60+M), T3}
              end,
    F = fun(Y) -> parserlang:orparse([PosZone, NegZone], Y, "timezone") end,
    parserlang:orparse([F, {rfc2822, obs_zone}], X, "timezone").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Address Specification (section 3.4) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a single 'mailbox' or an address 'group' and return the
%% address(es).
-spec address(<<_:24,_:_*8>>) -> {<<_:16,_:_*8>>, binary()}.
address(X) ->
    F = fun(Y) ->
                {H, T} = mailbox(Y),
                {[H], T}
        end,
    parserlang:orparse([F, fun group/1], X, "address").

%% parse a 'name_addr' or and 'addr_spec'  and return the address.
-spec mailbox(<<_:24,_:_*8>>) -> {name_addr(), binary()}.
mailbox(X) ->
    F = fun(Y) ->
                {H, T} = addr_spec(Y),
                {#name_addr{addr=H}, T}
        end,
    parserlang:orparse([F, fun name_addr/1], X, "mailbox").

%% parse an 'angle_addr', optionally prefaced with a 'display_name',
%% and return the address
-spec name_addr(<<_:48,_:_*8>>) -> {name_addr(), binary()}.
name_addr(X) ->
    try
        {Name, T1} = maybe_option(fun display_name/1, X),
        {Addr, T2} = angle_addr(T1),
        {#name_addr{name=Name, addr=Addr}, T2}
    catch
        {parse_error, expected, _} ->
            throw({parse_error, expected, "name addr"})
    end.

%% parse and 'angle_addr' or and 'obs_angle_addr' and return the address.
-spec angle_addr(<<_:48,_:_*8>>) -> {<<_:32,_:_*8>>, binary()}.
angle_addr(X) ->
    AngleAddr = fun(Y) ->
                        {_, T1} = parserlang:char($<, Y),
                        {R, T2} = addr_spec(T1),
                        {_, T3} = parserlang:char($>, T2),
                        {R, T3}
                end,
    F = fun(Y) -> unfold(AngleAddr, Y) end,
    parserlang:orparse([F, fun obs_angle_addr/1], X, "angle address").

%% parse a "group" of addresses. That is a 'display_name', followed by a colon,
%% optionally followed by a 'mailbox_list', followed by a semicolon. The found
%% address(es) are returned - which may be none.
%% Here is an example
%%
%% 1> group(<<"my group: user1@example.org, user2@example.org;">>).
%% [#name_addr{name=undefined, addr=<<"user1@example.org">>},
%%  #name_addr{name=undefined, addr=<<"user2@example.org">>}]
%% TODO: store the group name
-spec group(<<_:16,_:_*8>>) -> {[name_addr()], binary()}.
group(X) ->
    try
        {_, T1} = display_name(X),
        {_, T2} = parserlang:char($:, T1),
        {R, T3} = parserlang:option([], fun mailbox_list/1, T2),
        {_, T4} = unfold(fun(Y) -> parserlang:char($;, Y) end, T3),
        {R, T4}
    catch
        {parse_error, expected, _} ->
            throw({parse_error, expected, "address list"})
    end.

%% parse and return a 'phrase'.
-spec display_name(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
display_name(X) ->
    try
        {H, T} = unfold(fun(Y) -> phrase(Y) end, X),
        {lists:foldl(fun(Y, Acc) -> <<Y/binary, Acc/binary>> end, <<>>, H), T}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "display name"})
    end.

%% parse a list of 'mailbox' addresses, every two addresses being seperated
%% by a comma, and return the list of found addresses.
-spec mailbox_list(binary()) -> {[binary()], binary()}.
mailbox_list(X) ->
    Comma = fun(Y) -> parserlang:char($,, Y) end,
    parserlang:sepby(fun mailbox/1, Comma, X).

%% parse a list of 'address' addresses, every two addresses being seperated
%% by a comma, and return the list of found addresses.
-spec address_list(binary()) -> {[binary()], binary()}.
address_list(X) ->
    Comma = fun(Y) -> parserlang:char($,, Y) end,
    parserlang:sepby(fun address/1, Comma, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Addr-spec specification (section 3.4.1) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse an "address specification". that is a 'local_part', followed by
%% an "@" character, followed by a 'domain'. Return the complete address
%% as 'String', ignoring any whitespace or commments.
-spec addr_spec(<<_:24,_:_*8>>) -> {<<_:24,_:_*8>>, binary()}.
addr_spec(X) ->
    try
        {R1, T1} = local_part(X),
        {_, T2} = parserlang:char($@, T1),
        {R2, T3} = domain(T2),
        {<<R1/binary, $@, R2/binary>>, T3}
    catch
        {parse_error, expected, _} ->
            throw({parse_error, expected, "address specification"})
    end.

%% parse and return a "local part" of an 'addr_spec'. That is either a
%% 'dot_atom' or a 'quoted_string'.
-spec local_part(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
local_part(X) ->
    parserlang:orparse([fun obs_local_part/1,
                        fun dot_atom/1,
                        fun quoted_string/1], X, "'address' local part").

%% parse and return a "domain part" or an 'addr_spec'. That is either a
%% 'dot_atom' or a 'domain_literal'.
-spec domain(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
domain(X) ->
    parserlang:orparse([fun obs_domain/1,
                        fun dot_atom/1,
                        fun domain_literal/1],
                       X, "'address' domain part").

%% parse a "domain literal". That is a "[" character, followed by any
%% amount of 'dcontent', followed by a terminating "]" character.
%% The complete string is returned verbatim.
-spec domain_literal(<<_:16,_:_*8>>) -> {<<_:16,_:_*8>>, binary()}.
domain_literal(X) ->
    DContent = fun(Y) ->
                       {_, T1} = parserlang:optional(fun fws/1, Y),
                       dcontent(T1)
               end,
    DLiteral = fun(Y) ->
                       {_, T1} = parserlang:char($[, Y),
                       {R, T2} = parserlang:many(DContent, T1),
                       {_, T3} = parserlang:char($], T2),
                       {parserlang:bin_concat([$[|R] ++ [$]]), T3}
               end,
    try
        unfold(DLiteral, X)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "domain literal"})
    end.

%% parse and return any characters that are legal in a 'domain_literal'.
%% That is 'dtext' or a 'quoted_pair'.
-spec dcontent(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
dcontent(X) ->
    F = fun(Y) ->
                {H, T} = parserlang:many1(fun dtext/1, Y),
                {parserlang:bin_concat(H), T}
        end,
    parserlang:orparse([F, fun quoted_pair/1], X, "domain literal content").

%% parse and return any ASCII characters except "[", "]" and "\".
-spec dtext(<<_:8,_:_*8>>) -> {byte(), binary()}.
dtext(X) when is_binary(X) ->
    ErrStr = "any ASCII character (excluding '[', ']' and '\\')",
    Err= {parse_error, expected, ErrStr},
    F = fun(Y) when is_binary(Y) ->
                <<H, T/binary>> = Y,
                if H >= 33 andalso H =< 90 orelse
                   H >= 94 andalso H =< 126 -> {H, T};
                   true -> throw(Err)
                end
        end,
    % parserlang:orparse([F, fun no_ws_ctl/1], X, ErrStr).
    parserlang:orparse([F], X, ErrStr);
dtext(X) -> error({badarg, X}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Overall Message Syntax (section 3.5) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Parse a complete message as defined by this RFC and it is broken down
%% into the seperate header fields and the mesassge body. Header lines,
%% which contain syntax errors, will not cause the parser to abort.
%% Rather, these headers will appear as 'optional_field's (which are
%% unparsed) in the resulting 'message'. A message must be really,
%% really badly broken for this parser to fail.
%%
%% This behaviour was chosen because it is impossible to predict what the
%% user of this module considers to be a fatal error;
%% traditionally, parsers are very forgiving when it comes to Internet
%% mesages.
%%
%% If you want to implement a really strict parser, you'll have to put
%% the appropriate parser together yourself. You'll find this is rather
%% easy to do. Refer to the 'fields' parser for further details.
-spec message(binary()) -> message().
message(X) ->
    Body = fun(Y) ->
                   {_, T1} = rfc2234:crlf(Y),
                   body(T1)
           end,
    {F, T1} = fields(X),
    {B, _} = parserlang:option(<<>>, Body, T1),
    #message{fields=F, body=B}.

%% a message body is just an unstructured sequence of characters.
-spec body(binary()) -> {binary(), binary()}.
body(X) -> {X, <<>>}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Field Definitions (section 3.6) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This parser will parse an arbitrary number of header fields as defined
%% in this RFC. For each field, an appropriate 'field' value is created,
%% all of them making up the 'field' list that this parser returns.
%%
%% If you look at this parser you will find that it will attempt all
%% other fields before falling back to a 'optional_field'.
-spec fields(binary()) -> {{atom(), any()}, binary()}.
fields(X) ->
    Options = fun(Y) ->
                      parserlang:orparse(
                        [field_helper(V) ||
                         V <- [ from, sender, return_path, reply_to, to, cc,
                                bcc, message_id, in_reply_to, references,
                                subject, comments, keywords, orig_date,
                                resent_date, resent_from, resent_sender,
                                resent_to, resent_cc, resent_bcc,
                                resent_msg_id, resent_reply_to, received,
                                obs_received, optional_field ]],
                        Y, "field")
              end,
    parserlang:many(Options, X).

%% helper fuction to construct lambdas
-spec field_helper(atom()) -> {{atom(), any()}, binary()}.
field_helper(X) -> field_helper(X, X).

-spec field_helper(atom(), atom()) -> {{atom(), any()}, binary()}.
field_helper(Type, F) ->
    fun(X) ->
            {H, T} = rfc2822:F(X),
            {{Type, H}, T}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The origination date field (section 3.6.1) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a "date" header line and return the date it contains as a
%% 'calender_time'.
-spec orig_date(<<_:64,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
orig_date(X) -> header("Date", fun date_time/1, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Originator fields (section 3.6.2) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a "from" header line and return the 'mailbox_list'
%% address(es) contained in it.
-spec from(<<_:64,_:_*8>>) -> {[binary()], binary()}.
from(X) -> header("From", fun mailbox_list/1, X).

%% parse a "sender" header line and return the 'mailbox' address
%% contained in it.
-spec sender(<<_:80,_:_*8>>) -> {binary(), binary()}.
sender(X) -> header("Sender", fun mailbox/1, X).

%% parse a "reply-to" header line and return the 'address_list'
%% addres(es) contained in it.
-spec reply_to(<<_:88,_:_*8>>) -> {[binary()], binary()}.
reply_to(X) -> header("Reply-to", fun address_list/1, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Destination address fields (section 3.6.3) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a "to" header line and return the 'address_list'
%% address(es) contained in it.
-spec to(<<_:48,_:_*8>>) -> {binary(), binary()}.
to(X) -> header("To", fun address_list/1, X).

%% parse a "cc" header line and return the 'address_list'
%% address(es) contained in it.
-spec cc(<<_:48,_:_*8>>) -> {binary(), binary()}.
cc(X) -> header("Cc", fun address_list/1, X).

%% parse a "bcc" header line and return the 'address_list'
%% address(es) contained in it.
-spec bcc(<<_:56,_:_*8>>) -> {binary(), binary()}.
bcc(X) ->
    %F = fun(Y) ->
    %            {_, T} = parserlang:optional(fun cfws/1, Y),
    %            {<<>>, T}
    %    end,
    %Or = fun(Y) -> parserlang:orparse([fun address_list/1, F], Y, "Bcc") end,
    %header("Bcc", Or, X).
    header("Bcc", fun address_list/1, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Identification fields (section 3.6.4) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a "message-id" header line and return the 'msg_id' contained
%% in it.
-spec message_id(<<_:104,_:_*8>>) -> {binary(), binary()}.
message_id(X) -> header("Message-ID", fun msg_id/1, X).

%% parse a "in-reply-to" header line and return the list of 'msg_id's
%% contained in it.
-spec in_reply_to(<<_:112,_:_*8>>) -> {binary(), binary()}.
in_reply_to(X) ->
    F = fun(Y) -> parserlang:many1(fun msg_id/1, Y) end,
    header("In-Reply-To", F, X).

%% parse a "references" header line and return the lists of 'msg_id's
%% contained in it.
-spec references(<<_:104,_:_*8>>) -> {binary(), binary()}.
references(X) ->
    F = fun(Y) -> parserlang:many1(fun msg_id/1, Y) end,
    header("References", F, X).

%% parse a "message id" and return it. A message is almost identical
%% to an 'angle_addr' but with stricter rules about folding and
%% whitespace.
-spec msg_id(<<_:24,_:_*8>>) -> {binary(), binary()}.
msg_id(X) ->
    F = fun(Y) ->
                {_, T1} = parserlang:char($<, Y),
                {IDL, T2} = id_left(T1),
                {_, T3} = parserlang:char($@, T2),
                {IDR, T4} = id_right(T3),
                {_, T5} = parserlang:char($>, T4),
                {<<$<, IDL/binary, $@, IDR/binary, $>>>, T5}
        end,
    try
        unfold(F, X)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "message id"})
    end.

%% parse a "left ID" part of a 'msg_id'. This is almost identical to the
%% 'local_part' or an email address, but with stricter rules about folding
%% and whitespace.
-spec id_left(binary()) -> {binary(), binary()}.
id_left(X) -> parserlang:orparse([fun dot_atom_text/1,
                                  fun no_fold_quote/1],
                                 X, "left part of message ID").

%% parse a "right ID" part of a 'msg_id'. This is almost identical to the
%% 'domain' of an email address, but with stricter rules about folding and
%% whitespace.
-spec id_right(binary()) -> {binary(), binary()}.
id_right(X) -> parserlang:orparse([fun dot_atom_text/1,
                                   fun no_fold_literal/1],
                                   X, "right part of message ID").

%% parse one or more occurances of 'qtext' or 'quoted_pair' and return the
%% concatenated string. this makes up the 'id_left' of a 'msg_id'.
-spec no_fold_quote(<<_:24,_:_*8>>) -> {<<_:24,_:_*8>>, binary()}.
no_fold_quote(X) ->
    ManyQText = fun(Y) ->
                        {R, T} = parserlang:many1(fun qtext/1, Y),
                        {parserlang:bin_concat(R), T}
                end,
    F = fun(Y) ->
                parserlang:orparse([ManyQText, fun quoted_pair/1], Y,
                                   "no fold quote")
        end,
    try
        {_, T1} = rfc2234:dquote(X),
        {R, T2} = parserlang:many(F, T1),
        {_, T3} = rfc2234:dquote(T2),
        BC = parserlang:bin_concat(R),
        {<<$", BC/binary, $">>, T3}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "non-folding quoted string"})
    end.

%% parse one or more occurances of 'dtext' or 'quoted_pair' and return
%% the concatenated string. this makes up the 'id_right' of a 'msg_id'.
-spec no_fold_literal(<<_:24,_:_*8>>) -> {<<_:24,_:_*8>>, binary()}.
no_fold_literal(X) ->
    ManyDText = fun(Y) ->
                        {R, T} = parserlang:many1(fun dtext/1, Y),
                        {parserlang:bin_concat(R), T}
                end,
    F = fun(Y) -> parserlang:orparse([ManyDText, fun quoted_pair/1],
                                     Y, "non-folding domain literal")
        end,
    try
        {_, T1} = parserlang:char($[, X),
        {R, T2} = parserlang:many(F, T1),
        {_, T3} = parserlang:char($], T2),
        BC = parserlang:bin_concat(R),
        {<<$[, BC/binary, $]>>, T3}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "non-folding domain literal"})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Informational fields (section 3.6.5) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a "subject" header line and return its contents verbatim. Please note
%% that all whitespace and/or comments are preserved, i.e the result of
%% parsing "subject: foo" is " foo", not "foo".
-spec subject(<<_:80,_:_*8>>) -> {binary(), binary()}.
subject(X) -> header("subject", fun unstructured/1, X).

%% parse a "comments" header line and return its contents verbatim. Please
%% note that all whitespace and/or comments are preserved, i.e the result of
%% parsing "comments: foo" is " foo", not "foo".
-spec comments(<<_:88,_:_*8>>) -> {binary(), binary()}.
comments(X) -> header("comments", fun unstructured/1, X).

%% parse a "keywords" header line and return the list of 'phrase's found.
%% Please note that each phrase is again a list of 'atom's, as returned by
%% the 'phrase' parser.
-spec keywords(<<_:80,_:_*8>>) -> {[binary()], binary()}.
keywords(X) ->
    Phrases = fun(Y) ->
                      {_, T1} = parserlang:char($,, Y),
                      phrase(T1)
              end,
    F = fun(Y) ->
                {R1, T1} = phrase(Y),
                {R2, T2} = parserlang:many(Phrases, T1),
                {[R1|R2], T2}
        end,
    header("Keywords", F, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Resent fields (section 3.6.6) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a "resent-date" header line and return the date it contains as a
%% 'calender_time'.
-spec resent_date(<<_:112,_:_*8>>) -> {#calender_time{}, binary()}.
resent_date(X) -> header("Resent-Date", fun date_time/1, X).

%% parse a "resent-from" header line and return the 'mailbox_list'
%% address(es) contained in it.
-spec resent_from(<<_:112,_:_*8>>) -> {[#name_addr{}], binary()}.
resent_from(X) -> header("Resent-From", fun mailbox_list/1, X).

%% parse a "resent-sender" header line and return the 'mailbox' address
%% contained in it.
-spec resent_sender(<<_:128,_:_*8>>) -> {#name_addr{}, binary()}.
resent_sender(X) -> header("Resent-Sender", fun mailbox/1, X).

%% parse a "resent-to" header line and return the 'address_list'
%% address(es) contained in it.
-spec resent_to(<<_:96,_:_*8>>) -> {[#name_addr{}], binary()}.
resent_to(X) -> header("Resent-To", fun address_list/1, X).

%% parse a "resent-cc" header line and return the 'address_list'
%% address(es) contained in it.
-spec resent_cc(<<_:96,_:_*8>>) -> {[#name_addr{}], binary()}.
resent_cc(X) -> header("Resent-Cc", fun address_list/1, X).

%% parse a "resent-bcc" header line and return the 'address_list'
%% address(es) contained in it. (This list may be empty.)
-spec resent_bcc(<<_:104,_:_*8>>) -> {[#name_addr{}], binary()}.
resent_bcc(X) ->
    Options = fun(Y) ->
                      {R, T} = rfc2822:address_list(Y),
                      % this is a hack to deal with the differences in the
                      % parserlang sepby and the haskell one
                      case R of
                          [] ->
                              {_, NT} = parserlang:many(fun rfc2234:wsp/1, T),
                              {R, NT};
                          _  -> {R, T}
                      end
              end,
    header("Resent-Bcc", Options, X).

%% parse a "resent-message-id" header line and return the 'msg_id'
%% contained in it.
-spec resent_msg_id(<<_:160,_:_*8>>) -> {binary(), binary()}.
resent_msg_id(X) -> header("resent-message-id", fun msg_id/1, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Trace Fields (section 3.6.7) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a "return-path" header line and return the 'path' contained
%% in it.
-spec return_path(<<_:112,_:_*8>>) -> {binary(), binary()}.
return_path(X) -> header("Return-Path", fun path/1, X).

%% parse a "path spec". A path is an optional addr_spec between "<"
%% and ">"
-spec path(binary()) -> {binary(), binary()}.
path(X) ->
    PathText = fun(Y) ->
                       {_, T1} = parserlang:char($<, Y),
                       {R, T2} = parserlang:option(<<>>, fun addr_spec/1, T1),
                       {_, T3} = parserlang:char($>, T2),
                       {<<$<, R/binary, $>>>, T3}
               end,
    {_, T} = parserlang:many(fun rfc2234:wsp/1, X),
    parserlang:orparse([PathText, fun obs_path/1], T, "return path spec").

%% parse a "received" header line and returns the 'name_val_list'
%% followed by 'date_time' contained in it.
-spec received(<<_:88,_:_*8>>)
      -> {{[{binary(), binary()}], #calender_time{}}, binary()}.
received(X) ->
    F = fun(Y) ->
                {R1, T1} = name_val_list(Y),
                {_, T2} = parserlang:char($;, T1),
                {R2, T3} = date_time(T2),
                {{R1, R2}, T3}
        end,
    header("Received", F, X).

%% parse a "name_val_list", this is just a collection of 'name_val_pair's
%% optionally prefaced by 'cfws'.
-spec name_val_list(<<_:24,_:_*8>>) -> {[{binary(), binary()}], binary()}.
name_val_list(X) ->
    try
        {_, T1} = parserlang:optional(fun cfws/1, X),
        parserlang:many1(fun name_val_pair/1, T1)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "list of name/value pairs"})
    end.

%% parse a "name_val_pair", thjis is an 'item_name', followed by 'cwfs'
%% and then an 'item_value'.
-spec name_val_pair(<<_:24,_:_*8>>)
      -> {{<<_:8,_:_*8>>, <<_:8,_:_*8>>}, binary()}.
name_val_pair(X) ->
    try
        {R1, T1} = item_name(X),
        {_, T2} = cfws(T1),
        {R2, T3} = item_value(T2),
        {{R1, R2}, T3}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "a name/value pair"})
    end.

%% parse a "item_name", this starts with an 'alpha' followed by any
%% number of 'alpha', digit or "-".
-spec item_name(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
item_name(X) ->
    Choice = parserlang:choice([fun(Y) -> parserlang:char($-, Y) end,
                                fun rfc2234:alpha/1,
                                fun rfc2234:digit/1 ],
                               "name of a name/value pair"),
    try
        {R1, T1} = rfc2234:alpha(X),
        {R2, T2} = parserlang:many(Choice, T1),
        {parserlang:bin_concat([R1|R2]), T2}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "name of a name/value pair"})
    end.

%% parse a "item_value", this can be either a a collection of 'angle_addr',
%% a addr_spec, a domain, a message id or an atom.
-spec item_value(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
item_value(X) ->
    ManyAngle = fun(Y) -> parserlang:many1(fun angle_addr/1, Y) end,
    Choice = parserlang:choice([ManyAngle,
                                fun rfc2822:addr_spec/1,
                                fun rfc2822:domain/1,
                                fun rfc2822:msg_id/1,
                                fun rfc2822:atom/1 ],
                               "value of a name/value pair"),
    Choice(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Optional Fields (section 3.6.8) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse an arbitrary header field and return a tuple containing the
%% 'field_name' and 'unstructured' text of the header. The name will
%% *not* contain the terminating colon.
-spec optional_field(<<_:32,_:_*8>>) -> {{<<_:8,_:_*8>>, binary()}, binary()}.
optional_field(X) ->
    Err = {parse_error, expected, "optional (unspecified) header line"},
    try
        {N, T1} = field_name(X),
        {_, T2} = parserlang:char($:, T1),
        {B, T3} = unstructured(T2),
        {_, T4} = rfc2234:crlf(T3),
        {{N, B}, T4}
    catch
        {parse_error, expected, _} -> throw(Err);
        error:{badmatch, _} -> throw(Err)
    end.

%% parse and return an arbitrary header field name. that is one or more
%% 'ftext' characters.
-spec field_name(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
field_name(X) ->
    try
        {R, T} = parserlang:many1(fun ftext/1, X),
        {parserlang:bin_concat(R), T}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "header line name"})
    end.

%% match and return any ASCII character except for control characters,
%% whitespace and ":"
-spec ftext(<<_:8,_:_*8>>) -> {33..57 | 59..126, binary()}.
ftext(X) when is_binary(X) ->
    Err = {parse_error, expected,
           "character (excluding controls, space and ':'"},
    try
        <<H, T/binary>> = X,
        if H >= 33 andalso H =< 57 orelse
           H >= 59 andalso H =< 126 -> {H, T};
           true -> throw(Err)
        end
    catch
        error:{badmatch, _} -> throw(Err)
    end;
ftext(X) -> error({badarg, X}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Miscellaneous obsolete tokens (section 4.1) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Match the obsolete "quoted pair" syntax, which - unlike 'quoted_pair'
%% - allowed *any* ASCII character to be specified when quoted. This
%% parser will return both the backslash and the actual character.
-spec obs_qp(<<_:16,_:_*8>>) -> {<<_:16>>, binary()}.
obs_qp(X) when is_binary(X) ->
    Err = {parse_error, expected, "any quoted US-ASCII character"},
    try
        {_, T1} = parserlang:char($\\, X),
        <<C, T2/binary>> = T1,
        if C >= 0 andalso C =< 127 -> {<<$\\, C>>, T2};
           true -> throw(Err)
        end
    catch
        {parse_error, expected, _} -> throw(Err);
        error:{badmatch, _} -> throw(Err)
    end;
obs_qp(X) -> error({badarg, X}).

%% match the obsolete "text" syntax, which - unlike 'text'- allowed
%% "carriage returns" and "line feeds". This is really weird; you better
%% consult the RFC for details. The parser will return the complete string,
%% including those special characters.
-spec obs_text(binary()) -> {binary(), binary()}.
obs_text(X) ->
    F = fun(Y) ->
                {R1, T1} = obs_char(Y),
                {R2, T2} = parserlang:many(fun rfc2234:lf/1, T1),
                {R3, T3} = parserlang:many(fun rfc2234:cr/1, T2),
                {parserlang:bin_concat([R1|R2 ++ R3]), T3}
        end,
    {R1, T1} = parserlang:many(fun rfc2234:lf/1, X),
    {R2, T2} = parserlang:many(fun rfc2234:cr/1, T1),
    {R3, T3} = parserlang:many(F, T2),
    {parserlang:bin_concat(R1 ++ R2 ++ R3), T3}.

%% match and return the obsolete "char" syntax, whicch - unlike 'character'
%% - did not allow "carriage return" and "line feed".
-spec obs_char(<<_:8,_:_*8>>) -> {0..9 | 11..12 | 14..127, binary()}.
obs_char(X) when is_binary(X) ->
    Err = {parse_error, expected, "any ASCII character except CR and LF"},
    try
        <<H, T/binary>> = X,
        if H >= 0 andalso H =< 9 orelse
           H == 11 orelse H == 12 orelse
           H >= 14 andalso H =< 127 -> {H, T};
           true -> throw(Err)
        end
    catch
        {parse_error, expected, _} -> throw(Err);
        error:{badmatch, _} -> throw(Err)
    end;
obs_char(X) -> error({badarg, X}).

%% match and return the obsolete "utext" syntax, which is identical to
%% 'obs_text'.
-spec obs_utext(binary()) -> {binary(), binary()}.
obs_utext(X) -> obs_text(X).

%% match the obsolete "phrase" syntax, which - unlike 'phrase' - allows
%% dots between tokens.
-spec obs_phrase(binary()) -> {binary(), binary()}.
obs_phrase(X) ->
    Choice = parserlang:choice([fun word/1,
                                fun(Y) ->
                                        {R, T} = parserlang:char($., Y),
                                        {<<R>>, T}
                                end,
                                fun(Y) -> {_, T1} = cfws(Y), {<<>>, T1} end],
                               "obsolete phrase text"),
    {R1, T1} = word(X),
    {R2, T2} = parserlang:many(Choice, T1),
    {[R1|lists:filter(fun(Y) -> Y =/= <<>> end, R2)], T2}.

%% match a "phrase list" syntax and return the list of 'string's that make
%% up the phrase. In contrast to a 'phrase', the 'obs_phrase_list'
%% seperates individual words by commas.
-spec obs_phrase_list(binary()) -> {binary(), binary()}.
obs_phrase_list(X) ->
    ManyPhrase = fun(Y) ->
                         {R, T1} = parserlang:option(<<>>,
                                                     fun rfc2822:phrase/1, Y),
                         {_, T2} = unfold(fun(Z) -> parserlang:char($,, Z) end,
                                          T1),
                         {R, T2}
                 end,
    {R1, T1} = parserlang:many1(ManyPhrase, X),
    {R2, T2} = parserlang:option(<<>>, fun phrase/1, T1),
    if R2 =/= <<>> -> {lists:filter(fun(Y) -> Y =/= <<>> end, R1) ++ [R2], T2};
       true        -> {lists:filter(fun(Y) -> Y =/= <<>> end, R1), T2}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete folding white space (section 4.2) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse and return an "obsolete fws" token. That is at least one 'wsp'
%% character, followed by and arbitrary number (including zero) of 'crlf'
%% followed by at least one more 'wsp' character.
-spec obs_fws(<<_:8,_:_*8>>) -> {<<_:8,_:_*8>>, binary()}.
obs_fws(X) ->
    F = fun(Y) ->
                {R1, T1} = rfc2234:crlf(Y),
                {R2, T2} = parserlang:many1(fun rfc2234:wsp/1, T1),
                {parserlang:bin_concat([R1|R2]), T2}
        end,
    {R1, T1} = parserlang:many1(fun rfc2234:wsp/1, X),
    {R2, T2} = parserlang:many(F, T1),
    {parserlang:bin_concat(R1 ++ R2), T2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete Date and Time (section 4.3) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a 'day_name' but allow for the obsolete folding syntax
-spec obs_day_of_week(<<_:24,_:_*8>>) -> {dow(), binary()}.
obs_day_of_week(X) ->
    try
        unfold(fun day_name/1, X)
    catch
        {parse_error, expected,  _} -> throw({parse_error, expected,
                                              "day of the week name"})
    end.

%% parse a 'year' but allow for a two-digit number (obsolete) and the
%% obsolete folding syntax.
-spec obs_year(<<_:16,_:_*8>>) -> {integer(), binary()}.
obs_year(X) ->
    F = fun(Y) ->
                {R1, T1} = parserlang:manyN(2, fun rfc2234:digit/1, Y),
                Z = bin_to_int(parserlang:bin_concat(R1)),
                if Z =< 49  -> {2000 + Z, T1};
                   Z =< 999 -> {1900 + Z, T1};
                   true     -> {Z, T1}
                end
        end,
    try
        unfold(F, X)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected, "year"})
    end.

%% parse a 'month_name' but allow for the obsolete folding syntax.
-spec obs_month(<<_:24,_:_*8>>) -> {month(), binary()}.
obs_month(X) -> unfold(fun month_name/1, X).

%% parse a 'day' but allow for the obsolete folding syntax
-spec obs_day(<<_:8,_:_*8>>) -> {integer(), binary()}.
obs_day(X) -> unfold(fun day_of_month/1, X).

%% parse a 'hour' but allow for obsolete folding syntax.
-spec obs_hour(<<_:8,_:_*8>>) -> {integer(), binary()}.
obs_hour(X) -> unfold(fun hour/1, X).

%% parse a 'minute' but allow for obsolete folding syntax
-spec obs_minute(<<_:8,_:_*8>>) -> {integer(), binary()}.
obs_minute(X) -> unfold(fun minute/1, X).

%% parse a 'second' but allow for the obsolete folding syntax
-spec obs_second(<<_:8,_:_*8>>) -> {integer(), binary()}.
obs_second(X) -> unfold(fun second/1, X).

%% match the obsolete zone names and return the appropriate offset
-spec obs_zone(<<_:16,_:_*8>>) -> {integer(), binary()}.
obs_zone(X) ->
    MkZone = fun(N, O) ->
                     NB = binary:list_to_bin(N),
                     fun(Y) ->
                             {_, T1} = parserlang:string(NB, Y),
                             {O * 60 * 60, T1}
                     end
             end,
    MilZone = fun(Set, Offset) ->
                      BSet = binary:list_to_bin(Set),
                      fun(Y) ->
                              {R, T} = parserlang:oneof(BSet, Y),
                              {(R - Offset) * 60 * 60, T}
                      end
              end,
    NegMil = fun(Set, Offset) ->
                     BSet = binary:list_to_bin(Set),
                     fun(Y) ->
                             {R, T} = parserlang:oneof(BSet, Y),
                             {-(R - Offset) * 60 * 60, T}
                     end
             end,
    Choice = parserlang:choice([MkZone("UT", 0),
                                MkZone("GMT", 0),
                                MkZone("EST", -5),
                                MkZone("EDT", -4),
                                MkZone("CST", -6),
                                MkZone("CDT", -5),
                                MkZone("MST", -7),
                                MkZone("MDT", -6),
                                MkZone("PST", -8),
                                MkZone("PDT", -7),
                                MilZone(lists:seq($A, $I), $A),
                                MilZone(lists:seq($K, $M), $B),
                                NegMil(lists:seq($N, $Y), $M),
                                fun(Y) ->
                                        {_, T} = parserlang:char($Z, Y),
                                        {0, T}
                                end],
                               "obsolete timezone"),
    Choice(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete Addressing (section 4.4) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This parser matches the "obsolete angle address" syntax, a construct that
%% used to be called "route address" in earlier RFCs. It differs from a
%% standard 'angle_addr' in two ways: (1) it allows far more liberal
%% insertion of folding whitespace and comments and (2) the address may
%% contain a "route" (which this parser ignores):
%%
%% 1> obs_angle_addr(<<"<@example1.org,@example2.org:joe@example.org>">>).
%% {<<"<joe@example.org>">>, <<>>}
-spec obs_angle_addr(<<_:40,_:_*8>>) -> {<<_:40,_:_*8>>, binary()}.
obs_angle_addr(X) ->
    F = fun(Y) ->
                {_, T1} = parserlang:char($<, Y),
                {_, T2} = parserlang:option(<<>>, fun obs_route/1, T1),
                {Addr, T3} = addr_spec(T2),
                {_, T4} = parserlang:char($>, T3),
                % TODO: route is lost here
                {parserlang:bin_concat([$<, Addr, $>]), T4}
        end,
    try
        unfold(F, X)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "obsolete angle address"})
    end.

%% this parser parses the "route" part of 'obs_angle_addr' and returns
%% the list of 'string's that make up this route. Relies one
%% 'obs_domain_list' for the actual parsing.
-spec obs_route(<<_:8,_:_*8>>) -> {[<<_:24,_:_*8>>], binary()}.
obs_route(X) ->
    F = fun(Y) ->
                {R, T1} = obs_domain_list(Y),
                {_, T2} = parserlang:char($:, T1),
                {R, T2}
        end,
    try
        unfold(F, X)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "route of an obsolete angle "
                                             "address"})
    end.

%% This parser parses a list of domain names, each of them prefaced with an
%% "@". Multiple names are seperated by a comma. The list of 'domain's is
%% returned - and may be empty.
-spec obs_domain_list(<<_:8,_:_*8>>) -> {[binary(), ...], binary()}.
obs_domain_list(X) ->
    Comma = fun(Y) -> parserlang:string(<<",">>, Y) end,
    F = fun(Y) ->
                {_, T1} = parserlang:orparse([fun cfws/1, Comma],
                                             Y, "cfws or comma"),
                {_, T2} = parserlang:optional(fun cfws/1, T1),
                {_, T3} = parserlang:char($@, T2),
                domain(T3)
        end,
    {_, T1} = parserlang:char($@, X),
    {R1, T2} = domain(T1),
    {R2, T3} = parserlang:many(F,T2),
    {[R1|R2], T3}.

%% parse the obsolete syntax of a 'local_part', which allowed for more
%% liberal insertion of folding whitespace and comments. The actual
%% string is returned.
-spec obs_local_part(binary()) -> {binary(), binary()}.
obs_local_part(X) ->
    F = fun(Y) ->
                {_, T1} = parserlang:char($., Y),
                {R, T2} = word(T1),
                {parserlang:bin_join($., R), T2}
        end,
    {R1, T1} = word(X),
    {R2, T2} = parserlang:many(F, T1),
    {parserlang:bin_concat([R1|R2]), T2}.

%% parse the obsolete syntax of a 'domain', which allowed for more
%% liberal insertion of folding whitespace and comments. The actual string
%% is returned.
-spec obs_domain(binary()) -> {binary(), binary()}.
obs_domain(X) ->
    F = fun(Y) ->
                {_, T1} = parserlang:char($., Y),
                {R, T2} = atom(T1),
                {parserlang:bin_join($., R), T2}
        end,
    {R1, T1} = atom(X),
    {R2, T2} = parserlang:many(F, T1),
    {parserlang:bin_concat([R1|R2]), T2}.

%% this parser will match the obsolete syntax for a 'mailbox_list'.
%% This one is quite weird: An 'obs_mbox_list' contains an arbitrary
%% number of mailbox'es - including none -, which are seperated by
%% commas. But you may have multiple consecutive commas without giving
%% a 'mailbox'. You may also have a valid 'obs_mbox_list' that contains
%% *no* 'mailbox' at all. On the otherhand, you *must* have at least one
%% comma. The following example is valid:
%%
%% 1> obs_mbox_list(<<",">>).
%% {[], <<>>}
%%
%% But this one is not:
%%
%% 2> obs_mbox_list(<<"joe@example.org">>).
%% ** exception throw: {parse_error,expected,
%%                                  "obsolete syntax for a list of mailboxes"}
-spec obs_mbox_list(<<_:8,_:_*8>>) -> {[#name_addr{}], binary()}.
obs_mbox_list(X) ->
    F = fun(Y) ->
                {R, T1} = maybe_option(fun mailbox/1, Y),
                {_, T2} = unfold(fun(Z) -> parserlang:char($,, Z) end, T1),
                {R, T2}
        end,
    {R1, T1} = parserlang:many1(F, X),
    {R2, T2} = maybe_option(fun mailbox/1, T1),
    {lists:filter(fun(Y) -> Y =/= undefined end, R1 ++ [R2]), T2}.

%% This parser is identical to 'obs_mbox_list' but parses a list of 'address'es
%% rather than 'maiolbox'es. The main difference is that an 'address' may
%% contain 'group's. Please note that as of now, the parser will return a
%% simple list of addresses; the grouping information is lost.
-spec obs_addr_list(<<_:8,_:_*8>>) -> {[binary()], binary()}.
obs_addr_list(X) ->
    F = fun(Y) ->
                {R, T1} = maybe_option(fun address/1, Y),
                {_, T2} = parserlang:optional(fun rfc2822:cfws/1, T1),
                {_, T3} = parserlang:char($,, T2),
                {_, T4} = parserlang:optional(fun rfc2822:cfws/1, T3),
                {R, T4}
        end,
    {R1, T1} = parserlang:many(F, X),
    {R2, T2} = maybe_option(fun address/1, T1),
    {lists:filter(fun(Y) -> Y =/= undefined end, R1 ++ [R2]), T2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete header fields (section 4.5) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% a list of all of the obsolete fields
-spec obs_fields(binary()) -> {{atom(), any()}, binary()}.
obs_fields(X) ->
    MkField = fun(F, Y) ->
                      fun(A) ->
                              {R, T} = F(A),
                              {{Y, R}, T}
                      end
              end,
    ObsOpt = fun(Y) ->
                     {{Name, Cont}, T} = obs_optional(Y),
                     {{optional_field, Name, Cont}, T}
             end,
    FieldList = parserlang:orparse([MkField(fun obs_from/1, from),
                                    MkField(fun obs_sender/1, sender),
                                    MkField(fun obs_return/1, return),
                                    MkField(fun obs_reply_to/1, reply_to),
                                    MkField(fun obs_to/1, to),
                                    MkField(fun obs_cc/1, cc),
                                    MkField(fun obs_bcc/1, bcc),
                                    MkField(fun obs_message_id/1, message_id),
                                    MkField(fun obs_in_reply_to/1,
                                            in_reply_to),
                                    MkField(fun obs_references/1, references),
                                    MkField(fun obs_subject/1, subject),
                                    MkField(fun obs_comments/1, comments),
                                    MkField(fun obs_keywords/1, keywords),
                                    MkField(fun obs_orig_date/1, date),
                                    MkField(fun obs_resent_date/1,
                                            resent_date),
                                    MkField(fun obs_resent_from/1,
                                            resent_from),
                                    MkField(fun obs_resent_send/1,
                                            resent_sender),
                                    MkField(fun obs_resent_to/1, resent_to),
                                    MkField(fun obs_resent_cc/1, resent_cc),
                                    MkField(fun obs_resent_bcc/1, resent_bcc),
                                    MkField(fun obs_resent_mid/1,
                                            resent_message_id),
                                    MkField(fun obs_resent_reply/1,
                                            resent_reply_to),
                                    MkField(fun obs_received/1, received),
                                    ObsOpt],
                                   "obsolete field", X),
    parserlang:many(FieldList, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete origination date field (section 4.5.1) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a 'date' header line but allow for the obsolete folding syntax.
-spec obs_orig_date(<<_:56,_:_*8>>) -> {binary(), binary()}.
obs_orig_date(X) -> obs_header("Date", fun date_time/1, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete Originator fields (section 4.5.2) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a 'from' header line but allow for the obsolete folding syntax.
-spec obs_from(<<_:56,_:_*8>>) -> {[binary()], binary()}.
obs_from(X) -> obs_header("From", fun mailbox_list/1, X).

%% parse a 'sender' header line but allow for the obsolete folding syntax.
-spec obs_sender(<<_:72,_:_*8>>) -> {binary(), binary()}.
obs_sender(X) -> obs_header("Sender", fun mailbox/1, X).

%% parse a 'reply_to' header line but allow for the obsolete folding syntax.
-spec obs_reply_to(<<_:88,_:_*8>>) -> {[binary()], binary()}.
obs_reply_to(X) -> obs_header("Reply-To", fun mailbox_list/1, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete destination address fields (section 4.5.3) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a 'to' header line but allow for the obsolete folding syntax.
-spec obs_to(<<_:40,_:_*8>>) -> {[binary()], binary()}.
obs_to(X) -> obs_header("To", fun address_list/1, X).

%% parse a 'cc' header line but allow for the obsolete folding syntax.
-spec obs_cc(<<_:40,_:_*8>>) -> {[binary()], binary()}.
obs_cc(X) -> obs_header("Cc", fun address_list/1, X).

%% parse a 'bcc' header line but allow for the obsolete folding syntax.
-spec obs_bcc(<<_:48,_:_*8>>) -> {[binary()], binary()}.
obs_bcc(X) -> obs_header("Bcc", fun address_list/1, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete identification fields (section 4.5.4) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a 'message_id' header line but allow for the obsolete folding syntax.
-spec obs_message_id(<<_:104,_:_*8>>) -> {<<_:24,_:_*8>>, binary()}.
obs_message_id(X) -> obs_header("Message-ID", fun msg_id/1, X).

%% parse and 'in_reply_to' header line but allow for the obsolete folding and
%% obsolete phrase syntax.
-spec obs_in_reply_to(<<_:112,_:_*8>>) -> {binary(), binary()}.
obs_in_reply_to(X) ->
    A = fun(Y) ->
                {_, T1} = phrase(Y),
                {<<>>, T1}
        end,
    B = fun(Y) ->
                parserlang:orparse([A, fun msg_id/1], Y,
                                   "message id or phrase")
        end,
    C = fun(Y) ->
                {R, T} = parserlang:many(B, Y),
                {lists:filter(fun(Z) -> Z =/= <<>> end, R), T}
        end,
    obs_header("In-Reply-To", C, X).

%% parse a 'references' header line but allow for the obsolete folding and
%% obsolete phrase syntax.
-spec obs_references(<<_:104,_:_*8>>) -> {[binary()], binary()}.
obs_references(X) ->
    A = fun(Y) ->
                {_, T1} = phrase(Y),
                {<<>>, T1}
        end,
    B = fun(Y) ->
                parserlang:orparse([A, fun msg_id/1], Y,
                                   "message id or phrase")
        end,
    C = fun(Y) ->
                {R, T} = parserlang:many(B, Y),
                {lists:filter(fun(Z) -> Z =/= <<>> end, R), T}
        end,
    obs_header("References", C, X).

%% parse the "left part" of a message ID, but allows the obsolete syntax,
%% which is identical to a 'local_part'.
-spec obs_id_left(binary()) -> {binary(), binary()}.
obs_id_left(X) ->
    try
        local_part(X)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "left part of a message ID"})
    end.

%% parses the "right part" of a message ID, but allows the obsolete syntax,
%% which is identical to a 'domain'.
-spec obs_id_right(binary()) -> {binary(), binary()}.
obs_id_right(X) ->
    try
        domain(X)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "right part of a message ID"})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete informational fields (section 4.5.5) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a 'subject' header line but allow for the obsolete folding syntax.
-spec obs_subject(<<_:80,_:_*8>>) -> {binary(), binary()}.
obs_subject(X) -> obs_header("Subject", fun unstructured/1, X).

%% parse a 'comments' header line but allow for the obsolete folding syntax.
-spec obs_comments(<<_:88,_:_*8>>) -> {binary(), binary()}.
obs_comments(X) -> obs_header("Comments", fun unstructured/1, X).

%% parse a 'keywords' header line but allow for the obsolete folding syntax.
%% Also, this parser accepts 'obs_phrase_list'.
-spec obs_keywords(<<_:88,_:_*8>>) -> {[binary()], binary()}.
obs_keywords(X) -> obs_header("Keywords", fun obs_phrase_list/1, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete resent fields (section 4.5.6) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a 'resent_from' header line but allow for the obsolete folding syntax.
-spec obs_resent_from(<<_:112,_:_*8>>) -> {[binary()], binary()}.
obs_resent_from(X) -> obs_header("Resent-From", fun mailbox_list/1, X).

%% parse a 'resent_sender' header line but allow for the obsolete folding
%% syntax.
-spec obs_resent_send(<<_:128,_:_*8>>) -> {binary(), binary()}.
obs_resent_send(X) -> obs_header("Resent-Sender", fun mailbox/1, X).

%% parse a 'resent_date' header line but allow for the obsolete folding syntax.
-spec obs_resent_date(<<_:112,_:_*8>>) -> {binary(), binary()}.
obs_resent_date(X) -> obs_header("Resent-Date", fun date_time/1, X).

%% parse a 'resent_to' header line but allow for the obsolete folding syntax.
-spec obs_resent_to(<<_:96,_:_*8>>) -> {binary(), binary()}.
obs_resent_to(X) -> obs_header("Resent-To", fun mailbox_list/1, X).

%% parse a 'resent_cc' header line but allow for the obsolete folding syntax.
-spec obs_resent_cc(<<_:80,_:_*8>>) -> {[binary()], binary()}.
obs_resent_cc(X) -> obs_header("Resent-Cc", fun mailbox_list/1, X).

%% parse a 'resent_bcc' header line but allow for the obsolete folding syntax.
-spec obs_resent_bcc(<<_:88,_:_*8>>) -> {[binary()], binary()}.
obs_resent_bcc(X) -> obs_header("Resent-Bcc", fun mailbox_list/1, X).

%% parse a 'resent_msg_id' header line but allow for the obsolete folding
%% syntax.
-spec obs_resent_mid(<<_:160,_:_*8>>) -> {binary(), binary()}.
obs_resent_mid(X) -> obs_header("Resent-Message-ID", fun msg_id/1, X).

%% parse a 'resent_reply_to' header line but allow for the obsolete folding
%% syntax.
-spec obs_resent_reply(<<_:144,_:_*8>>) -> {[binary()], binary()}.
obs_resent_reply(X) -> obs_header("Resent-Reply-To", fun address_list/1, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Obsolete trace fields (section 4.5.7) %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% parse a 'return' header line but allow for the obsolete folding syntax.
-spec obs_return(<<_:112,_:_*8>>) -> {binary(), binary()}.
obs_return(X) -> obs_header("Return-Path", fun path/1, X).

%% match a 'received' header line but allow for the obsolete folding syntax.
-spec obs_received(<<_:88,_:_*8>>) -> {[{binary(), binary()}], binary()}.
obs_received(X) -> obs_header("Received", fun name_val_list/1, X).

%% match 'obs_angle_addr'
-spec obs_path(<<_:24,_:_*8>>) -> {<<_:24,_:_*8>>, binary()}.
obs_path(X) -> obs_angle_addr(X).

%% this parser is identical to 'optional_field' but allows the more liberal
%% line-folding suntax between the 'field_name" nad the "field_text".
-spec obs_optional(<<_:40,_:_*8>>)
      -> {{<<_:8,_:_*8>>, <<_:8,_:_*8>>}, binary()}.
obs_optional(X) ->
    try
        {N, T1} = field_name(X),
        {_, T2} = parserlang:many(fun rfc2234:wsp/1, T1),
        {_, T3} = parserlang:char($:, T2),
        {B, T4} = unstructured(T3),
        {_, T5} = rfc2234:crlf(T4),
        {{N, B}, T5}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "obsolete (unspecified) header"
                                             " line"})
    end.
