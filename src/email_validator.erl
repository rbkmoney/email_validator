-module(email_validator).

-export([validate/1]).
-export([parse/1]).

-define(DOMAIN_MAX_SIZE, 255).
-define(LOCAL_MAX_SIZE,  64).
-define(ADDR_MAX_SIZE, ?LOCAL_MAX_SIZE + ?DOMAIN_MAX_SIZE + 1).

-type parse_result() :: #{
    'local-part' := binary(),
    'domain'     := binary()
}.

-spec validate(binary() | string()) ->
    ok | {error, term()}.

validate(Address) ->
    case parse(Address) of
        {ok, _Result} ->
            ok;
        {incomplete, Result} ->
            {error, {parse_incomplete, Result}};
        Error ->
            Error
    end.

-spec parse(binary() | string()) ->
    {ok, parse_result()} | {incomplete, term()} | {error, term()}.

parse(Address) when is_list(Address) ->
    case unicode:characters_to_binary(Address) of
        Result when is_binary(Result) ->
            parse(Result);
        {error, Encoded, Rest} ->
            {error, {unicode, {invalid, Encoded, Rest}}};
        {incomplete, Encoded, Rest} ->
            {error, {unicode, {incomplete, Encoded, Rest}}}
    end;
parse(Address) when is_binary(Address) ->
    parse_addr(Address);
parse(Address) ->
    erlang:error(badarg, [Address]).

%%

% As per https://tools.ietf.org/html/rfc5321#section-4.5.3.1
% we need to validate maximum sizes
parse_addr(Address) when byte_size(Address) =< ?ADDR_MAX_SIZE ->
    case parse_rule('mailbox', Address) of
        {ok, ParsedAddr} ->
            validate_sizes(ParsedAddr);
        Error ->
            Error
    end;
parse_addr(Address) ->
    {error, {'mailbox', {too_big, Address}}}.

validate_sizes(#{'local-part' := Local, 'domain' := Domain} = ParsedAddr) ->
    case validate_part_sizes(Local, Domain) of
        ok -> {ok, ParsedAddr};
        Error -> Error
    end.

validate_part_sizes(Local, Domain) ->
    case validate_local_size(Local) of
        ok -> validate_domain_size(Domain);
        Error -> Error
    end.

validate_local_size(Local) when byte_size(Local) =< ?LOCAL_MAX_SIZE ->
    ok;
validate_local_size(Local) ->
    {error, {'local-part', {too_big, Local}}}.

validate_domain_size(Domain) when byte_size(Domain) =< ?DOMAIN_MAX_SIZE ->
    ok;
validate_domain_size(Domain) ->
    {error, {'domain', {too_big, Domain}}}.

parse_rule(Rule, Data) ->
    case email_validator_abnf:decode(Rule, Data) of
        {ok, ParseResult, <<>>} ->
            {ok, decode_parse_result(ParseResult)};
        {ok, Incomplete, Remainder} ->
            {incomplete, {Rule, decode_parse_result(Incomplete, Remainder)}};
        fail ->
            {error, {Rule, {parse_failed, Data}}}
    end.

decode_parse_result([Local, $@, Domain]) ->
    #{
        'local-part' => decode_local_part(Local),
        'domain'     => decode_domain(Domain)
    }.

decode_parse_result(Incomplete, Remainder) ->
    {list_to_binary(lists:flatten(Incomplete)), Remainder}.

%TODO: More result processing, normalize IP addresses, etc
decode_local_part(Local) ->
    list_to_binary(lists:flatten(Local)).

decode_domain(Domain) ->
    list_to_binary(lists:flatten(Domain)).
