-module(email_validator).

-export([validate/1]).

-define(DOMAIN_MAX_SIZE, 255).
-define(LOCAL_MAX_SIZE,  64).
-define(ADDR_MAX_SIZE, ?LOCAL_MAX_SIZE + ?DOMAIN_MAX_SIZE + 1).

-spec validate(binary() | string()) ->
    ok | {error, term()}.

validate(Address) when is_list(Address) ->
    case unicode:characters_to_binary(Address) of
        Result when is_binary(Result) ->
            validate(Result);
        {error, Encoded, Rest} ->
            {error, {unicode, {invalid, Encoded, Rest}}};
        {incomplete, Encoded, Rest} ->
            {error, {unicode, {incomplete, Encoded, Rest}}}
    end;
validate(Address) when is_binary(Address) ->
    validate_addr(Address);
validate(Address) ->
    {error, {not_an_address, Address}}.

%%

% As per https://tools.ietf.org/html/rfc5321#section-4.5.3.1
% we need to validate maximum sizes
validate_addr(Address) when byte_size(Address) =< ?ADDR_MAX_SIZE ->
    case split_local_domain(Address) of
        [Local, Domain] ->
            validate_local_domain(Local, Domain);
        _ ->
            {error, {not_an_address, Address}}
    end;
validate_addr(Address) ->
    {error, {'mailbox', {too_big, Address}}}.

split_local_domain(Address) ->
    string:split(Address, "@", trailing).

validate_local_domain(Local, Domain) ->
    case validate_local(Local) of
        ok ->
            validate_domain(Domain);
        {error, _} = Error ->
            Error
    end.

validate_local(Local) when byte_size(Local) =< ?LOCAL_MAX_SIZE ->
    validate_rule('local-part', Local);
validate_local(Local) ->
    {error, {'local-part', {too_big, Local}}}.

validate_domain(Domain) when byte_size(Domain) =< ?DOMAIN_MAX_SIZE ->
    validate_rule('domain', Domain);
validate_domain(Domain) ->
    {error, {'domain', {too_big, Domain}}}.

validate_rule(Rule, Data) ->
    case email_validator_abnf:decode(Rule, Data) of
        {ok, _, <<>>} -> ok;
        _ -> {error, {Rule, {parse_failed, Data}}}
    end.
