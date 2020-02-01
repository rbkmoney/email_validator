-module(prop_emails).
-include_lib("proper/include/proper.hrl").

-export([prop_test/0]).

%TODO: Magic numbers. Since reliably limiting sizes of mailbox parts doesn't seem to be an option
-define(INFINITY, 10).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Mailbox, 'mailbox'(), begin
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
            'domain-name'()
            %address_literal()
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
    ?LET({LetDig, LdhStr}, {'let-dig'(), '__repeat'('ldh-str'(), 0, 1)}, <<LetDig, LdhStr/binary>>).

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

% IPv4-address-literal     =   Snum 3("."  Snum)

% IPv6-address-literal     =   "IPv6:" IPv6-addr

% General-address-literal  =   Standardized-tag ":" 1*dcontent

% Standardized-tag         =   ldh-str

% dcontent        =   %d33-90 / ; Printable US-ASCII
%                     %d94-126  ; excl. "[", "\", "]"

% IPv6-addr       =   IPv6-full / IPv6-comp / IPv6v4-full / IPv6v4-comp

% IPv6-hex        =   1*4HEXDIG

% IPv6-full       =   IPv6-hex 7(":" IPv6-hex)

% IPv6-comp       =   [IPv6-hex *5(":" IPv6-hex)] "::"
%                     [IPv6-hex *5(":" IPv6-hex)]

% IPv6v4-full     =   IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal

% IPv6v4-comp     =   [IPv6-hex *3(":" IPv6-hex)] "::"
%                     [IPv6-hex *3(":" IPv6-hex) ":"]
%                     IPv4-address-literal

% Snum            =   1*3DIGIT

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
    ?LET({HEX, TL}, {choose(16#E1, 16#EC), '__repeat'('UTF8-tail'(), 2, 2)}, <<HEX, TL/binary>>).

'UTF8-3-3'() ->
    ?LET({HEX, TL}, {choose(16#80, 16#9F), 'UTF8-tail'()}, <<16#ED, HEX, TL/binary>>).

'UTF8-3-4'() ->
    ?LET({HEX, TL}, {choose(16#EE, 16#EF), '__repeat'('UTF8-tail'(), 2, 2)}, <<HEX, TL/binary>>).

% UTF8-4          =   %xF0 %x90-BF 2( UTF8-tail ) / %xF1-F3 3( UTF8-tail ) /
%                     %xF4 %x80-8F 2( UTF8-tail )
'UTF8-4'() ->
    '__alt'(['UTF8-4-1'(), 'UTF8-4-2'(), 'UTF8-4-3'()]).

'UTF8-4-1'() ->
    ?LET({HEX, TL}, {choose(16#90, 16#BF), '__repeat'('UTF8-tail'(), 2, 2)}, <<16#F0, HEX, TL/binary>>).

'UTF8-4-2'() ->
    ?LET({HEX, TL}, {choose(16#F1, 16#F3), '__repeat'('UTF8-tail'(), 3, 3)}, <<HEX, TL/binary>>).

'UTF8-4-3'() ->
    ?LET({HEX, TL}, {choose(16#80, 16#8F), '__repeat'('UTF8-tail'(), 2, 2)}, <<16#F4, HEX, TL/binary>>).

% UTF8-tail       =   %x80-BF
'UTF8-tail'() ->
    ?LET(TL, choose(16#80, 16#BF), <<TL>>).

%%%%%%%%%%%%%
%%% UTILS %%%
%%%%%%%%%%%%%

'__repeat'(Generator, Min, infinity) ->
    '__repeat'(Generator, Min, ?INFINITY);
'__repeat'(Generator, Min, Max) ->
    ?LET(Size, choose(Min, Max),
        ?LET(Vector, vector(Size, Generator), list_to_binary(Vector))
    ).

'__alt'(Alternatives) ->
    Frequencies = [{1, Alt} || Alt <- Alternatives],
    frequency(Frequencies).
