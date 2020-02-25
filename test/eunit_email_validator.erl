-module(eunit_email_validator).
-include_lib("eunit/include/eunit.hrl").

manual_cases() -> [
    {"simple@example.com", ok},
    {"very.common@example.com", ok},
    {"disposable.style.email.with+symbol@example.com", ok},
    {"other.email-with-hyphen@example.com", ok},
    {"fully-qualified-domain@example.com", ok},
    {"user.name+tag+sorting@example.com", ok},
    {"x@example.com", ok},
    {"example-indeed@strange-example.com", ok},
    {"admin@mailserver1", ok},
    {"example@s.example", ok},
    {"\" \"@example.org", ok},
    {"\"john..doe\"@example.org", ok},
    {"mailhost!username@example.org", ok},
    {"user%example.com@example.org", ok},
    % (backslashes, spaces and @ allowed in quotes)
    {"\"ab\\c\"@example.org", ok},
    {"\"Abc@def\"@example.com", ok},
    {"\"Fred Bloggs\"@example.com", ok},
    {"customer/department=shipping@example.com", ok},
    {"$A12345@example.com", ok},
    {"!def!xyz%abc@example.com", ok},
    {"_somename@example.com", ok},
    % ip addresses
    {"ip@[127.0.0.1]", ok},
    {"ip@[IPv6:::1]", ok},
    {"ip@[IPv6:::127.0.0.127]", ok},
    {"ip@[IPv6:dead::beef]", ok},
    {"ip@[IPv6:dead::]", ok},
    {"ip@[IPv6:dead:beef::7.0.0.1]", ok},
    {"ip@[IPv6:d:e:a:d:be:ef:7.0.0.3]", ok},
    {"ip@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]", ok},
    % UTF-8
    {<<"öö@example.com"/utf8>>, ok},
    {<<"испытание@пример.рф"/utf8>>, ok},
    {<<"我買@屋企.香港"/utf8>>, ok},
    {<<"संपर्क@डाटामेल.भारत"/utf8>>, ok},
    {<<"संपर्क@डाटामेल.भारत"/utf8>>, ok},
    %% Failures
    {"not even close", fail},
    {"@closerbutnotquite", fail},
    {"youtried@", fail},
    % smtp doesn't support comments
    {"simple(comment in local)@example.com", fail},
    {"(comment in local)simple@example.com", fail},
    {"simple@(comment in domain)example.com", fail},
    {"simple@example.com(comment in domain)", fail},
    % invalid domain
    {"simple@-example.com", fail},
    {"simple@example-.com", fail},
    {"simple@exam-$%ple.com", fail},
    {"Abc\\@def@example.com", fail},
    {"Fred\\ Bloggs@example.com", fail},
    {"Joe.\\\\Blow@example.com", fail},
    % (no @ character)
    {"Abc.example.com", fail},
    % (only one @ is allowed outside quotation marks)
    {"A@b@c@example.com", fail},
    % (none of the special characters in this local-part are allowed outside quotation marks)
    {"a\"b(c)d,e:f;g<h>i[j\\k]l@example.com", fail},
    % (quoted strings must be dot separated or the only element making up the local-part)
    {"just\"not\"right@example.com", fail},
    % (spaces, quotes, and backslashes may only exist when within quoted strings and preceded by a backslash)
    {"this is\"not\\allowed@example.com", fail},
    % (even if escaped (preceded by a backslash), spaces, quotes, and backslashes must still be contained by quotes)
    {"this\\ still\\\"not\\allowed@example.com", fail},
    % (local part is longer than 64 characters)
    {"1234567890123456789012345678901234567890123456789012345678901234+x@example.com", fail},
    % ip addresses
    {"ip@[999.0.0.0]", fail},
    {"ip@[127.1.1.1.1]", fail},
    {"ip@[IPv6:dead:beef:7.0.0.2]", fail},
    {"ip@[IPv6:d:e:a:d:be:e:f:7.0.0.4]", fail},
    {"ip@[IPv6:d:e:a:d:be:ef::7.0.0.5]", fail},
    {"ip@[IPv6:d:e:a:d:be:ef::7.0.0.5]", fail},
    {"ip@[GENERAL:address-literal", fail},
    {"ip@[GENERAL:", fail},
    {"ip@[GENERAL:[test]]", fail},
    % UTF-8
    {<<"те\\ ст@example.com"/utf8>>, fail},
    {<<"т ес\"т@example.com"/utf8>>, fail},
    {<<"испытание@пример.рфascii"/utf8>>, fail}
].

email_validator_test_() ->
    [{Addr, ?_assertEqual(ExpectedResult, validate(Addr))} || {Addr, ExpectedResult} <- manual_cases()].

validate(Addr) ->
    case email_validator:validate(Addr) of
        ok         -> ok;
        {error, _} -> fail
    end.
