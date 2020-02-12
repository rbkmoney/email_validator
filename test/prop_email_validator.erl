-module(prop_email_validator).
-include_lib("proper/include/proper.hrl").

-export([prop_test/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Mailbox, 'mailbox'(), validate_mailbox(Mailbox)).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
validate_mailbox(Mailbox) ->
    case email_validator:validate(Mailbox) of
        ok         -> true;
        {error, _} -> false
    end.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

mailbox() ->
    ?LET({LocalPart, Domain}, {local_part(), domain()}, <<LocalPart/binary, $@, Domain/binary>>).

%%% DOMAIN %%%

% domain          =   domain-name / address-literal
domain() ->
    ?SUCHTHAT(
        Domain,
        oneof([
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

'dot-sub-domain'() ->
    ?LET(Subdomain, 'sub-domain'(), <<$., Subdomain/binary>>).

% sub-domain      =   let-dig [ldh-str] / U-Label
'sub-domain'() ->
    oneof(['ascii-subdomain'(), 'U-Label'()]).

'ascii-subdomain'() ->
    ?LET({LetDig, LdhStr}, {'let-dig'(), '__opt'('ldh-str'())}, <<LetDig, LdhStr/binary>>).

'U-Label'() ->
    '__repeat'('UTF8-non-ascii'(), 1, infinity).

%%% NOTE: let-dig and ldh-str using original RFC5321 specification

% let-dig         =   ALPHA / DIGIT
'let-dig'() ->
    oneof(['ALPHA'(), 'DIGIT'()]).

% ldh-str         =   *( ALPHA / DIGIT / "-" ) Let-dig
'ldh-str'() ->
    ?LET({LDH, LetDig}, {'__repeat'(ldh(), 0, infinity), 'let-dig'()}, <<LDH/binary, LetDig>>).

'ldh'() ->
    oneof(['ALPHA'(), 'DIGIT'(), $-]).

% address-literal =   "[" ( IPv4-address-literal / IPv6-address-literal ) "]"
'address-literal'() ->
    ?LET(Alt, oneof([
        'IPv4-address-literal'(),
        'IPv6-address-literal'()
    ]), <<$[, Alt/binary, $]>>).

%%% LOCAL PART %%%

% local-part      =   dot-string / quoted-string
local_part() ->
    ?SUCHTHAT(
        LocalPart,
        oneof([
            'dot-string'(),
            'quoted-string'()
        ]),
        byte_size(LocalPart) =< 64
    ).

% dot-string      =   atom *("."  atom)
'dot-string'() ->
    ?LET(
        {Atom, DotAtomsList},
        {'Atom'(), '__repeat'('dot-atom'(), 0, infinity)},
        <<Atom/binary, DotAtomsList/binary>>
    ).

'dot-atom'() ->
    ?LET(Atom, 'Atom'(), <<$., Atom/binary>>).

% quoted-string   =   DQUOTE *qcontentSMTP DQUOTE
'quoted-string'() ->
    ?LET(
        {DQuote, QContentSMTP},
        {'DQUOTE'(), '__repeat'('qcontentSMTP'(), 0, infinity)},
        <<DQuote, QContentSMTP/binary, DQuote>>
    ).

% qcontentSMTP    =   qtextSMTP / quoted-pairSMTP
'qcontentSMTP'() ->
    oneof(['qtextSMTP'(), 'quoted-pairSMTP'()]).

% quoted-pairSMTP =   %d92 %d32-126
'quoted-pairSMTP'() ->
    ?LET(QP, choose(32, 126), <<$\\, QP>>).

% qtextSMTP       =   %d32-33 /       ; i.e., within a quoted string, any
%                     %d35-91 /       ; ASCII graphic or space is permitted
%                     %d93-126 /      ; without blackslash-quoting except
%                     UTF8-non-ascii  ; double-quote and the backslash itself.
'qtextSMTP'() ->
    oneof([choose(32, 33), choose(35, 91), choose(93, 126), 'UTF8-non-ascii'()]).

%%% ATOM %%%

% atext           =   ALPHA / DIGIT / ; Printable US-ASCII
%                     "!" / "#" /     ; characters not including
%                     "$" / "%" /     ; specials. Used for atoms.
%                     "&" / "'" /
%                     "*" / "+" /
%                     "-" / "/" /
%                     "=" / "?" /
%                     "^" / "_" /
%                     "`" / "{" /
%                     "|" / "}" /
%                     "~" / UTF8-non-ascii
'atext'() ->
    oneof([
        'ALPHA'(), 'DIGIT'(), 'UTF8-non-ascii'(),
        $!, $#, $$, $%, $&, $', $*, $+, $-, $/, $=, $?, $^, $_, $`, ${, $|, $}, $~
    ]).

% atom            =   1*atext
'Atom'() ->
    '__repeat'('atext'(), 1, infinity).

%%% UTF-8 %%%

% UTF8-non-ascii  =   UTF8-2 / UTF8-3 / UTF8-4
'UTF8-non-ascii'() ->
    oneof(['UTF8-2'(), 'UTF8-3'(), 'UTF8-4'()]).

% UTF8-2          =   %xC2-DF UTF8-tail
'UTF8-2'() ->
    ?LET({HEX, TL}, {choose(16#C2, 16#DF), 'UTF8-tail'()}, <<HEX, TL/binary>>).

% UTF8-3          =   %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) /
%                     %xED %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )
'UTF8-3'() ->
    oneof(['UTF8-3-1'(), 'UTF8-3-2'(), 'UTF8-3-3'(), 'UTF8-3-4'()]).

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
    oneof(['UTF8-4-1'(), 'UTF8-4-2'(), 'UTF8-4-3'()]).

'UTF8-4-1'() ->
    ?LET({HEX, TL}, {choose(16#90, 16#BF), '__repeat'('UTF8-tail'(), 2)}, <<16#F0, HEX, TL/binary>>).

'UTF8-4-2'() ->
    ?LET({HEX, TL}, {choose(16#F1, 16#F3), '__repeat'('UTF8-tail'(), 3)}, <<HEX, TL/binary>>).

'UTF8-4-3'() ->
    ?LET({HEX, TL}, {choose(16#80, 16#8F), '__repeat'('UTF8-tail'(), 2)}, <<16#F4, HEX, TL/binary>>).

% UTF8-tail       =   %x80-BF
'UTF8-tail'() ->
    ?LET(TL, choose(16#80, 16#BF), <<TL>>).

%%% IP Address Literals %%%

% IPv4-address-literal     =   dec-octet 3("." dec-octet)
'IPv4-address-literal'() ->
    ?LET(
        {DecOctet, DotDecOctet},
        {'dec-octet'(), '__repeat'('dot-dec-octet'(), 3)},
        <<DecOctet/binary, DotDecOctet/binary>>
    ).

'dot-dec-octet'() ->
    ?LET(DecOctet, 'dec-octet'(), <<$., DecOctet/binary>>).

% dec-octet                =   "25" %x30-35          ; 250-255
%                            / "2" %x30-34 DIGIT     ; 200-249
%                            / "1" 2DIGIT            ; 100-199
%                            / %x31-39 DIGIT         ; 10-99
%                            / DIGIT                 ; 0-9
'dec-octet'() ->
    ?LET(Int, choose(0, 255), integer_to_binary(Int)). % Lazy but straightforward

% IPv6-address-literal     =   "IPv6:" IPv6-addr
'IPv6-address-literal'() ->
    ?LET(IPv6, 'IPv6-addr'(), <<"IPv6:", IPv6/binary>>).

% IPv6-addr                =                               6( H16 ":" ) LS32
%                            /                        "::" 5( H16 ":" ) LS32
%                            / [ H16                ] "::" 4( H16 ":" ) LS32
%                            / [ H16 0*1( ":" H16 ) ] "::" 3( H16 ":" ) LS32
%                            / [ H16 0*2( ":" H16 ) ] "::" 2( H16 ":" ) LS32
%                            / [ H16 0*3( ":" H16 ) ] "::"    H16 ":"   LS32
%                            / [ H16 0*4( ":" H16 ) ] "::"              LS32
%                            / [ H16 0*5( ":" H16 ) ] "::"              H16
%                            / [ H16 0*6( ":" H16 ) ] "::"
'IPv6-addr'() ->
    ?LET(Case, choose(0, 8), 'IPv6-addr'(Case)).

'IPv6-addr'(Case) ->
    ?LET(
        Frags,
        ['IPv6-addr-beginning'(Case), 'IPv6-addr-separator'(Case), 'IPv6-addr-middle'(Case), 'IPv6-addr-end'(Case)],
        iolist_to_binary(Frags)
    ).

'IPv6-addr-beginning'(Case) when Case >= 2, Case =< 8 ->
    '__opt'('H16-repeat-colon-H16'(0, Case - 2));
'IPv6-addr-beginning'(_Case) ->
    <<>>.

'IPv6-addr-separator'(Case) when Case >= 1, Case =< 8->
    <<"::">>;
'IPv6-addr-separator'(_Case) ->
    <<>>.

'IPv6-addr-middle'(Case) when Case >= 0, Case =< 6 ->
    '__repeat'('H16-colon'(), 6 - Case);
'IPv6-addr-middle'(_Case) ->
    <<>>.

'IPv6-addr-end'(Case) when Case >= 0, Case =< 6 ->
    'LS32'();
'IPv6-addr-end'(Case) when Case =:= 7 ->
    'H16'();
'IPv6-addr-end'(_Case) ->
    <<>>.

'H16-repeat-colon-H16'(Min, Max) ->
    ?LET({H16, ColonH16}, {'H16'(), '__repeat'('colon-H16'(), Min, Max)}, <<H16/binary, ColonH16/binary>>).

'H16-colon'() ->
    ?LET(H16, 'H16'(), <<H16/binary, $:>>).

'colon-H16'() ->
    ?LET(H16, 'H16'(), <<$:, H16/binary>>).

% H16                      =   1*4HEXDIG
'H16'() ->
    '__repeat'('HEXDIG'(), 1, 4).

% LS32                     =   ( H16 ":" H16 ) / IPv4-address-literal
'LS32'() ->
    oneof([
        ?LET({First, Second}, {'H16'(), 'H16'()}, <<First/binary, $:, Second/binary>>),
        'IPv4-address-literal'()
    ]).

%%% RFC4234 CORE %%%

% ALPHA           =   %x41-5A / %x61-7A   ; A-Z / a-z
'ALPHA'() ->
    oneof([choose(16#41, 16#5A), choose(16#61, 16#7A)]).

% DIGIT           =   %x30-39             ; 0-9
'DIGIT'() ->
    choose(16#30, 16#39).

% DQUOTE          =   %x22                ; " (Double Quote)
'DQUOTE'() ->
    16#22.

% HEXDIG          =   DIGIT / "A" / "B" / "C" / "D" / "E" / "F" ; 0-9 A-F
'HEXDIG'() ->
    oneof([
        'DIGIT'(),
        choose($A, $F)
    ]).

%%%%%%%%%%%%%
%%% UTILS %%%
%%%%%%%%%%%%%
'__repeat'(Generator, Times) ->
    '__repeat'(Generator, Times, Times).

'__repeat'(Generator, Min, infinity) ->
    ?SIZED(S,
        '__repeat'(Generator, Min, max(Min, S))
    );
'__repeat'(Generator, Size, Size) ->
    ?LET(Vector, vector(Size, Generator), list_to_binary(Vector));
'__repeat'(Generator, Min, Max) ->
    ?LET(Size, choose(Min, Max),
        ?LET(Vector, vector(Size, Generator), list_to_binary(Vector))
    ).

'__opt'(Generator) ->
    '__repeat'(Generator, 0, 1).
