-module(prop_emails).
-include_lib("proper/include/proper.hrl").

-export([prop_test/0]).

%TODO: Magic number
-define(INFINITY, 10).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Mailbox, 'mailbox'(), begin
        _ = io:format("~p~n", [Mailbox]),
        validate_mailbox(Mailbox)
    end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
validate_mailbox(Mailbox) ->
    RealRes = email_validator:validate(Mailbox),
    case RealRes of
        ok         -> true;
        {error, _} -> false
    end.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

mailbox() ->
    ?LET({LocalPart, Domain}, {local_part(), domain()}, <<LocalPart/binary, $@, Domain/binary>>).

% domain          =   domain-name / address-literal
domain() ->
    ?SUCHTHAT(
        Domain,
        '__alt'([
            'domain-name'(),
            'address-literal'()
        ]),
        byte_size(Domain) =< 255
    ).

% domain-name     =   sub-domain *("." sub-domain)
'domain-name'() ->
    ?LET(
        {Subdomain, DotSubdomainList},
        {'sub-domain'(), '__repeat'('dot-sub-domain'(), 0, infinity)},
        <<Subdomain/binary, DotSubdomainList/binary>>
    ).

% sub-domain      =   let-dig [ldh-str]
'sub-domain'() ->
    ?LET({LetDig, LdhStr}, {'let-dig'(), '__opt'('ldh-str'())}, <<LetDig, LdhStr/binary>>).

'dot-sub-domain'() ->
    ?LET(Subdomain, 'sub-domain'(), <<$., Subdomain/binary>>).

% let-dig         =   ALPHA / DIGIT
'let-dig'() ->
    '__alt'(['ALPHA'(), 'DIGIT'()]).

% hyphen-let-dig  =   "-" let-dig
'hyphen-let-dig'() ->
    ?LET(LetDig, 'let-dig'(), <<$-, LetDig>>).

% ldh-str         =   1*(hyphen-let-dig / let-dig)
'ldh-str'() ->
    '__repeat'('__alt'(['hyphen-let-dig'(), 'let-dig'()]), 1, infinity).

%% @TODO

% address-literal =   "[" ( IPv4-address-literal / IPv6-address-literal / General-address-literal ) "]"
'address-literal'() ->
    ?LET(Alt, '__alt'([
        'IPv4-address-literal'(),
        'IPv6-address-literal'(),
        'General-address-literal'()
    ]), <<$[, Alt/binary, $]>>).

% IPv4-address-literal     =   Snum 3("."  Snum)
'IPv4-address-literal'() ->
    ?LET({Snum, DotSnum}, {'Snum'(), '__repeat'('dot-Snum'(), 3)}, <<Snum/binary, DotSnum/binary>>).

'dot-Snum'() ->
    ?LET(Snum, 'Snum'(), <<$., Snum/binary>>).

% IPv6-address-literal     =   "IPv6:" IPv6-addr
'IPv6-address-literal'() ->
    ?LET(IPv6, 'IPv6-addr'(), <<"IPv6:", IPv6/binary>>).

% General-address-literal  =   Standardized-tag ":" 1*dcontent
'General-address-literal'() ->
    ?LET(
        {STag, DContent},
        {'Standardized-tag'(), '__repeat'('dcontent'(), 1, infinity)},
        <<STag/binary, $:, DContent/binary>>
    ).

% Standardized-tag         =   ldh-str
'Standardized-tag'() ->
    'ldh-str'().

% dcontent        =   %d33-90 / ; Printable US-ASCII
%                     %d94-126  ; excl. "[", "\", "]"
'dcontent'() ->
    '__alt'([
        choose(33, 90),
        choose(94, 126)
    ]).

% IPv6-addr       =   IPv6-full / IPv6-comp / IPv6v4-full / IPv6v4-comp
'IPv6-addr'() ->
    '__alt'([
        'IPv6-full'(),
        'IPv6-comp'(),
        'IPv6v4-full'(),
        'IPv6v4-comp'()
    ]).

% IPv6-hex        =   1*4HEXDIG
'IPv6-hex'() ->
    '__repeat'('HEXDIG'(), 1, 4).

'colon-IPv6-hex'() ->
    ?LET(IPv6, 'IPv6-hex'(), <<":", IPv6/binary>>).

% IPv6-full       =   IPv6-hex 7(":" IPv6-hex)
'IPv6-full'() ->
    ?LET(
        {IPv6Start, IPv6End},
        {'IPv6-hex'(), '__repeat'('colon-IPv6-hex'(), 7)},
        <<IPv6Start/binary, IPv6End/binary>>
    ).

% IPv6-comp       =   [IPv6-hex *5(":" IPv6-hex)] "::"
%                     [IPv6-hex *5(":" IPv6-hex)]
'IPv6-comp'() ->
    ?LET(
        {CompTokenStart, CompTokenEnd},
        {'__opt'('IPv6-comp-token'()), '__opt'('IPv6-comp-token'())},
        <<CompTokenStart/binary, "::", CompTokenEnd/binary>>
    ).

'IPv6-comp-token'() ->
    ?LET(
        {IPv6Start, IPv6End},
        {'IPv6-hex'(), '__repeat'('colon-IPv6-hex'(), 0, 5)},
        <<IPv6Start/binary, IPv6End/binary>>
    ).

% IPv6v4-full     =   IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal
'IPv6v4-full'() ->
    ?LET(
        {IPv6Start, IPv6Mid, IPv4End},
        {'IPv6-hex'(), '__repeat'('colon-IPv6-hex'(), 5), 'IPv4-address-literal'()},
        <<IPv6Start/binary, IPv6Mid/binary, $:, IPv4End/binary>>
    ).

% IPv6v4-comp     =   [IPv6v4-comp-token] "::" [IPv6v4-comp-token-colon] IPv4-address-literal
'IPv6v4-comp'() ->
    ?LET(
        {IPv6Start, IPv6Mid, IPv4End},
        {'__opt'('IPv6v4-comp-token'()), '__opt'('IPv6v4-comp-token-colon'()), 'IPv4-address-literal'()},
        <<IPv6Start/binary, "::", IPv6Mid/binary, IPv4End/binary>>
    ).

% IPv6v4-comp-token = IPv6-hex *3(":" IPv6-hex)
'IPv6v4-comp-token'() ->
    ?LET(
        {IPv6Start, IPv6End},
        {'IPv6-hex'(), '__repeat'('colon-IPv6-hex'(), 0, 3)},
        <<IPv6Start/binary, IPv6End/binary>>
    ).
% IPv6v4-comp-token-colon = IPv6v4-comp-token ":"
'IPv6v4-comp-token-colon'() ->
    ?LET(IPv6CompToken, 'IPv6v4-comp-token'(), <<IPv6CompToken/binary, $:>>).

% Snum            =   1*3DIGIT
'Snum'() ->
    '__repeat'('DIGIT'(), 1, 3).

% local-part      =   dot-string / quoted-string
local_part() ->
    ?SUCHTHAT(
        LocalPart,
        '__alt'([
            'dot-string'(),
            'quoted-string'()
        ]),
        byte_size(LocalPart) =< 64
    ).

% quoted-string   =   DQUOTE *qcontentSMTP DQUOTE
'quoted-string'() ->
    ?LET(QContentSMTP, '__repeat'('qcontentSMTP'(), 0, infinity), <<$", QContentSMTP/binary, $">>).

% qcontentSMTP    =   qtextSMTP / quoted-pairSMTP
'qcontentSMTP'() ->
    '__alt'(['qtextSMTP'(), 'quoted-pairSMTP'()]).

% qtextSMTP       =   %d32-33 /       ; i.e., within a quoted string, any
%                     %d35-91 /       ; ASCII graphic or space is permitted
%                     %d93-126 /      ; without blackslash-quoting except
%                     UTF8-non-ascii  ; double-quote and the backslash itself.
'qtextSMTP'() ->
    '__alt'([choose(32, 33), choose(35, 91), choose(93, 126), 'UTF8-non-ascii'()]).

% quoted-pairSMTP =   %d92 %d32-126   ; i.e., backslash followed by any ASCII
%                                     ; graphic (including itself) or SPace
'quoted-pairSMTP'() ->
    ?LET(QP, choose(32, 126), <<$\\, QP>>).

% dot-string      =   atom *("."  atom)
'dot-string'() ->
    ?LET(
        {Atom, DotAtomsList},
        {'Atom'(), '__repeat'('dot-atom'(), 0, infinity)},
        <<Atom/binary, DotAtomsList/binary>>
    ).

'dot-atom'() ->
    ?LET(Atom, 'Atom'(), <<$., Atom/binary>>).

% atom            =   1*atext
'Atom'() ->
    '__repeat'('atext'(), 1, infinity).

% atext           =   ALPHA / DIGIT / ; Printable US-ASCII
%     "!" / "#" /     ; characters not including
%     "$" / "%" /     ; specials. Used for atoms.
%     "&" / "'" /
%     "*" / "+" /
%     "-" / "/" /
%     "=" / "?" /
%     "^" / "_" /
%     "`" / "{" /
%     "|" / "}" /
%     "~" / UTF8-non-ascii
'atext'() ->
    '__alt'([
        'ALPHA'(), 'DIGIT'(), 'UTF8-non-ascii'(),
        oneof([$!, $#, $$, $%, $&, $', $*, $+, $-, $/, $=, $?, $^, $_, $`, ${, $|, $}, $~])
    ]).

% ALPHA           =   %x41-5A / %x61-7A   ; A-Z / a-z
'ALPHA'() ->
    '__alt'([choose($a, $z), choose($A, $Z)]).

% DIGIT           =   %x30-39             ; 0-9
'DIGIT'() ->
    choose($0, $9).

% HEXDIG          =   DIGIT / "A" / "B" / "C" / "D" / "E" / "F" ; 0-9 A-F
'HEXDIG'() ->
    '__alt'([
        'DIGIT'(),
        choose($A, $F)
    ]).

% UTF8-non-ascii  =   UTF8-2 / UTF8-3 / UTF8-4
'UTF8-non-ascii'() ->
    '__alt'(['UTF8-2'(), 'UTF8-3'(), 'UTF8-4'()]).

% UTF8-2          =   %xC2-DF UTF8-tail
'UTF8-2'() ->
    ?LET({HEX, TL}, {choose(16#C2, 16#DF), 'UTF8-tail'()}, <<HEX, TL/binary>>).

% UTF8-3          =   %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) /
%                     %xED %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )
'UTF8-3'() ->
    '__alt'(['UTF8-3-1'(), 'UTF8-3-2'(), 'UTF8-3-3'(), 'UTF8-3-4'()]).

'UTF8-3-1'() ->
    ?LET({HEX, TL}, {choose(16#A0, 16#BF), 'UTF8-tail'()}, <<16#E0, HEX, TL/binary>>).

'UTF8-3-2'() ->
    ?LET({HEX, TL}, {choose(16#E1, 16#EC), '__repeat'('UTF8-tail'(), 2)}, <<HEX, TL/binary>>).

'UTF8-3-3'() ->
    ?LET({HEX, TL}, {choose(16#80, 16#9F), 'UTF8-tail'()}, <<16#ED, HEX, TL/binary>>).

'UTF8-3-4'() ->
    ?LET({HEX, TL}, {choose(16#EE, 16#EF), '__repeat'('UTF8-tail'(), 2)}, <<HEX, TL/binary>>).

% UTF8-4          =   %xF0 %x90-BF 2( UTF8-tail ) / %xF1-F3 3( UTF8-tail ) /
%                     %xF4 %x80-8F 2( UTF8-tail )
'UTF8-4'() ->
    '__alt'(['UTF8-4-1'(), 'UTF8-4-2'(), 'UTF8-4-3'()]).

'UTF8-4-1'() ->
    ?LET({HEX, TL}, {choose(16#90, 16#BF), '__repeat'('UTF8-tail'(), 2)}, <<16#F0, HEX, TL/binary>>).

'UTF8-4-2'() ->
    ?LET({HEX, TL}, {choose(16#F1, 16#F3), '__repeat'('UTF8-tail'(), 3)}, <<HEX, TL/binary>>).

'UTF8-4-3'() ->
    ?LET({HEX, TL}, {choose(16#80, 16#8F), '__repeat'('UTF8-tail'(), 2)}, <<16#F4, HEX, TL/binary>>).

% UTF8-tail       =   %x80-BF
'UTF8-tail'() ->
    ?LET(TL, choose(16#80, 16#BF), <<TL>>).

%%%%%%%%%%%%%
%%% UTILS %%%
%%%%%%%%%%%%%

'__repeat'(Generator, Times) ->
    '__repeat'(Generator, Times, Times).

'__repeat'(Generator, Min, infinity) ->
    '__repeat'(Generator, Min, ?INFINITY);
'__repeat'(Generator, Size, Size) ->
    ?LET(Vector, vector(Size, Generator), list_to_binary(Vector));
'__repeat'(Generator, Min, Max) ->
    ?LET(Size, choose(Min, Max),
        ?LET(Vector, vector(Size, Generator), list_to_binary(Vector))
    ).

'__opt'(Generator) ->
    '__repeat'(Generator, 0, 1).

'__alt'(Alternatives) ->
    Frequencies = [{1, Alt} || Alt <- Alternatives],
    frequency(Frequencies).
