-module(email_validator).

-export([validate/1]).

-spec validate(string()) ->
    ok | fail.
validate(Address) ->
    % As per https://tools.ietf.org/html/rfc5321#section-4.5.3.1
    % we need to validate maximum sizes
    case split_local_domain(Address) of
        [Local, Domain] ->
            validate_local_domain(Local, Domain);
        _ ->
            fail
    end.

%%

split_local_domain(Address) ->
    string:split(Address, "@", trailing).

validate_local_domain(Local, Domain) ->
    case {validate_local(Local), validate_domain(Domain)} of
        {ok, ok} -> ok;
        _ -> fail
    end.

validate_local(Local) when length(Local) =< 64 ->
    validate_rule('local-part', Local);
validate_local(_Local) ->
    fail.

validate_domain(Domain) when length(Domain) =< 255 ->
    validate_rule('domain', Domain);
validate_domain(_Domain) ->
    fail.

validate_rule(Rule, Data) ->
    case email_validator_abnf:decode(Rule, Data) of
        {ok, _, []} -> ok;
        _ -> fail
    end.
