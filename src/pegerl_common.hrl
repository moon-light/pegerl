-type more_input() :: fun((term()) -> {ok, term(), binary()} | fail).

-type rule_define() ::
    [ any
    | {choise, rule_define()}
    | {'+' | '*' | '?', rule_define()}
    | {string, boolean(), binary(), pos_integer(),
        non_neg_integer(), non_neg_integer()}
    | fun(() -> rule_define())
    | fun((binary(), binary(), [term()], rule_define(), boolean(), more_input())
        -> {term(), binary()} | fail)
    ].

more_fail(_) ->
    fail.

p(<<Input/binary>>, {Line, Byte} = LB,
    [{string, false, String, Size, L, B} = Rule | RuleTail],
    Stack, MoreInput)
->
    case Input
    of <<String:Size/binary, Rest/binary>> ->
        NextLB = {Line + L, Byte + B},
        p(Rest, NextLB, RuleTail, [String | Stack], MoreInput)
    ; _ ->
        {error, Rule, Input, LB}
    end;
p(<<Input/binary>>, {Line, Byte} = LB,
    [{string, true, String, Size, L, B} = Rule | RuleTail],
    Stack, MoreInput)
->
    case Input
    of <<String:Size/binary, Rest/binary>> ->
        NextLB = {Line + L, Byte + B},
        p(Rest, NextLB, RuleTail, [String | Stack], MoreInput)
    ; <<Begin:Size/binary, Rest/binary>> ->
        case string:casefold(Begin)
        of String ->
            NextLB = {Line + L, Byte + B},
            p(Rest, NextLB, RuleTail, [String | Stack], MoreInput)
        ; _ ->
            {error, Rule, Input, LB}
        end
    ; _ ->
        {error, Rule, Input, LB}
    end;
p(<<$\n, Input/binary>>, {Line, _}, [any | RuleTail], Stack, MoreInput) ->
    p(Input, {Line + 1, 0}, RuleTail, [<<$\n>> | Stack], MoreInput);
p(<<C/utf8, Input/binary>>, {Line, Byte}, [any | RuleTail], Stack, MoreInput)
->
    Char = <<C/utf8>>,
    p(Input, {Line, Byte + size(Char)}, RuleTail, [Char | Stack], MoreInput);
p(Input, LB, [Fun | RuleTail], Stack, MoreInput) when is_function(Fun, 0) ->
    p(Input, LB, Fun() ++ RuleTail, Stack, MoreInput);
p(Input, LB, [Fun | RuleTail], Stack, MoreInput) when is_function(Fun, 5) ->
    Fun(Input, LB, RuleTail, Stack, MoreInput);
p(Input, LB, [Fun | RuleTail], Stack, MoreInput) when is_function(Fun, 6) ->
    Fun(Input, <<>>, Stack, LB, RuleTail, MoreInput);
p(Input, LB, [{choise, ChoiseList} | RuleTail], Stack, MoreInput) ->
    p_choise(Input, LB, ChoiseList, RuleTail, Stack, MoreInput, none);
p(Input, LB, [{Type, Rule} | RuleTail], Stack, MoreInput) ->
    p_repeat(Input, LB, Type, Rule, [], RuleTail, Stack, MoreInput);
p(Rest, LB, [], [Value], _) ->
    {Value, Rest, LB};
p(Rest, LB, [], Value, _) ->
    {lists:reverse(Value), Rest, LB};
p(Input, LB, RuleList, Stack, MoreInput) ->
    error(eparse, [Input, LB, RuleList, Stack, MoreInput]).

p_choise(Input, LB, [Rule | ChoiseTail], RuleList, Stack, MoreInput, _) ->
    case p(Input, LB, Rule, [], MoreInput)
    of {error, _, _, _} = Error ->
        p_choise(Input, LB, ChoiseTail, RuleList, Stack, MoreInput, Error)
    ; {Value, Rest, NextLB} ->
        p(Rest, NextLB, RuleList, [Value | Stack], MoreInput)
    end;
p_choise(_, _, [], _, _, _, Error) ->
    Error.

p_repeat(Input, LB, Type, Rule, Acc, RuleTail, Stack, MoreInput) ->
    case p(Input, LB, Rule, [], MoreInput)
    of {error, _, _, _} = Error when Type == '+', Acc == [] ->
        Error
    ; {error, _, _, _} ->
        p(Input, LB, RuleTail, [lists:reverse(Acc) | Stack], MoreInput)
    ; {Value, Rest, NextLB} when Type == '?' ->
        p(Rest, NextLB, RuleTail, [Value | Stack], MoreInput)
    ; {Value, Rest, NextLB} ->
        NextAcc = [Value | Acc],
        p_repeat(Rest, NextLB, Type, Rule, NextAcc, RuleTail, Stack, MoreInput)
    end.
