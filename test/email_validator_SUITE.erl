-module(email_validator_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([
    default_alphanumeric_test/1,
    manual_cases_test/1
]).

-type test_case_name()  :: atom().
-type config()          :: [{atom(), any()}].

-spec all() ->
    [test_case_name()].
all() ->
    [
        default_alphanumeric_test,
        manual_cases_test
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Config.

-spec end_per_suite(config()) -> _.
end_per_suite(Config) ->
    Config.

-spec default_alphanumeric_test(config()) -> _.
default_alphanumeric_test(_Config) ->
    Emails = get_random_valid_emails(10000),
    true = validate_and_check(Emails),
    ok.

-spec manual_cases_test(config()) -> _.
manual_cases_test(_Config) ->
    ManualCases = [
        {"simple@example.com", ok},
        {"simple(comment in local)@example.com", ok},
        {"(comment in local)simple@example.com", ok},
        {"simple@(comment in domain)example.com", ok},
        {"simple@example.com(comment in domain)", ok},
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
        %% Failures
        {"not even close", fail},
        {"@closerbutnotquite", fail},
        {"youtried@", fail},
        % absolutely definitely against rfc5322 and the earlier one (backslashes and escaping not allowed)
        {"Abc\\@def@example.com", fail},
        {"Fred\\ Bloggs@example.com", fail},
        {"Joe.\\\\Blow@example.com", fail},
        {"phil.h\\@\\@ck@haacked.com", fail},
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
        {"1234567890123456789012345678901234567890123456789012345678901234+x@example.com", fail}
    ],
    true = validate_and_check(ManualCases),
    ok.

%% TEST COMMON

validate_and_check(Data) ->
    ValidationResults = validate(Data),
    check(ValidationResults).

validate(Data) ->
    lists:map(fun({Addr, Expected}) ->
        Res = email_validator:validate(Addr),
        {Addr, Expected, Res}
    end, Data).

check(ValidationResults) ->
    check(ValidationResults, true).

check([], true) ->
    true;
check([{_Addr, Expected, Res} | T], _Acc) when Expected =:= Res ->
    check(T, true);
check([H | _T], _Acc) ->
    throw({fail, H}).


%% UTILS

get_random_valid_emails(Num) ->
    get_random_valid_emails(Num, []).

get_random_valid_emails(0, Acc) ->
    Acc;
get_random_valid_emails(Num, Acc) ->
    get_random_valid_emails(Num - 1, [make_email() | Acc]).

make_email() ->
    {lists:flatten([make_local(), $@, make_domain()]), ok}.

make_local() ->
    random_local_part(16).

make_domain() ->
    lists:flatten([random_alpha_lower(8), $., random_alpha_lower(2)]).

random_alpha_lower(Length) ->
    random_string(Length, lists:seq($a, $z)).

random_local_part(Length) ->
    random_alpha_lower(1) ++
    random_string(
        Length - 2,
        lists:seq($a, $z) ++ lists:seq($A, $Z) ++ lists:seq($0, $9) ++ [$., $+, $-, $_]
    ) ++
    random_alpha_lower(1).

random_string(Length, AllowedChars) ->
    lists:foldl(
        fun
            (_, []) ->
                [random_char(AllowedChars)];
            (_, [Prev | _] = Acc) ->
                [random_char_non_repeating(Prev, AllowedChars) | Acc]
        end,
        [],
        lists:seq(1, Length)
    ).

random_char_non_repeating(Prev, AllowedChars) ->
    random_char(AllowedChars -- [Prev]).

random_char(AllowedChars) ->
    lists:nth(rand:uniform(length(AllowedChars)), AllowedChars).