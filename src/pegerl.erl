-module(pegerl).

-include_lib("syntax_tools/include/merl.hrl").

-export([generate/1]).

-spec generate(file:filename()) -> ok.
generate(File) ->
    {ok, Binary} = file:read_file(File),
    {{RuleList, UtilCode}, <<>>, _} = peg_grammar:rules(Binary),
    ModuleName = list_to_atom(filename:basename(File, ".peg")),
    {RuleExported, RuleFunction, RuleCode} = generate_parser(File, RuleList),
    %TODO: optimization
    % a <- ATOM+;       => a <- [a-z]+;
    % ATOM <- [a-z];
    % 
    % a <- b? c?        => a <- "b"* "c"*
    % b <- "b"+
    % c <- "c"*
    FormList = lists:flatten(
        [ ?Q("-module('@ModuleName@').")
        , ?Q("-export(['@_RuleExported'/1]).")
        , RuleFunction
        , ?Q("%% Code from peg file")
        ]),
    %% Write source to file
    Text = erl_prettypr:format(
            erl_syntax:form_list(FormList), [{paper, 80}, {ribbon, 80}]),
    OutFile = [filename:rootname(File, ".peg"), ".erl"],
    {ok, OutFD} = file:open(OutFile, [write, binary]),
    ok = file:write(OutFD, unicode:characters_to_binary(Text)),
    ok = file:write(OutFD, unicode:characters_to_binary(RuleCode)),
    CommonFileName = code:lib_dir(pegerl, src) ++ "/pegerl_common.hrl",
    {ok, CommonPart} = file:read_file(CommonFileName),
    ok = file:write(OutFD, <<"\n-file(\"pegerl_common.hrl\", 0).\n">>),
    ok = file:write(OutFD, [CommonPart, $\n]),
    ok = file:write(OutFD, util_code(File, UtilCode)).

generate_parser(FileName, RuleList) ->
    lists:unzip3(
        [ {rule_export(Rule), rule_define(Rule), rule_code(FileName, Rule)}
        || Rule <- RuleList]).

rule_export({{_, Name}, {false, IsEnter}, _, _}) ->
    enter_rule(Name, IsEnter);
rule_export({{Type, Name}, {true, IsEnter}, _, _}) ->
    [export_rule(Name, Type) | enter_rule(Name, IsEnter)];
rule_export({{_, Name}, {false, _, IsEnter}, _, _}) ->
    enter_rule(Name, IsEnter);
rule_export({{Type, Name}, {true, _, IsEnter}, _, _}) ->
    [export_rule(Name, Type) | enter_rule(Name, IsEnter)].

export_rule(Name, grammar) ->
    ?Q("'@Name@'/0");
export_rule(Name, binary) ->
    ?Q("'@Name@'/6").

enter_rule(Name, true) ->
    [?Q("'@Name@'/1"), ?Q("'@Name@'/2")];
enter_rule(_, false) ->
    [].

rule_define({{grammar, Name}, {_, true}, Body, Code}) ->
    [ ?Q("-spec '@Name@'(binary()) -> {term(), binary()} | fail.")
    , ?Q("'@Name@'(Input) -> '@Name@'(Input, fun more_fail/1).")
    , ?Q("-spec '@Name@'(binary(), more_input()) -> {term(), binary()} | fail.")
    , ?Q(
        [ "'@Name@'(Input, MoreInput) ->"
        , "p(Input, {0, 0}, '@Name@'(), [], MoreInput)."
        ])
    | rule_define({{grammar, Name}, {false, false}, Body, Code})];
rule_define({{grammar, Name}, {_, false}, Body, Code}) ->
    [ ?Q("-spec '@Name@'() -> rule_define().")
    , ?Q("'@Name@'() -> [_@body, _@code].",
        [ {body, grammar_body(Body)}
        , {code, grammar_body_code(Name, Code)}
        ])
    , grammar_code(Name, Body, Code)
    ];
rule_define({{binary, Name}, {_, HasContinue, true}, Body, Code}) ->
    [ ?Q("-spec '@Name@'(binary()) -> {term(), binary()} | fail.")
    , ?Q("'@Name@'(Input) -> '@Name@'(Input, fun more_fail/1).")
    , ?Q("-spec '@Name@'(binary(), more_input()) -> {term(), binary()} | fail.")
    , ?Q(
        [ "'@Name@'(Input, MoreInput) ->"
        , "p(Input, {0, 0}, [fun '@Name@'/6], [], MoreInput)."
        ])
    | rule_define({{binary, Name}, {false, HasContinue, false}, Body, Code})];
% TODO: what if HasContinue?
rule_define({{binary, Name}, {_, _, false}, Body, _}) ->
    [ ?Q(
        [ "-spec '@Name@'("
        , "binary(), binary(), list(), list(rule_define()), boolean(), "
        , "more_input()) -> rule_define()."
        ])
    , binary_body(Name, Body)
    ].

grammar_body({[], Body, []}) ->
    grammar_body(Body);
grammar_body({[], Body, Type}) ->
    ?Q("{'@type', [_@body]}",
        [ {type, erl_syntax:abstract(Type)}, {body, grammar_body(Body)}]);
grammar_body(any) ->
    merl:term(any);
grammar_body({local, {grammar, Name}}) ->
    ?Q("fun '@Name@'/0");
grammar_body({local, {binary, Name}}) ->
    ?Q("fun '@Name@'/6");
grammar_body({remote, Remote, {grammar, Name}}) ->
    ?Q("fun '@Remote@':'@Name@'/0");
grammar_body({remote, Remote, {binary, Name}}) ->
    ?Q("fun '@Remote@':'@Name@'/6");
grammar_body({string, IsCaseSensitive, String}) ->
    Binary = casefold(list_to_binary(String), IsCaseSensitive),
    Size = size(Binary),
    LinePos = binary:matches(Binary, <<$\n>>),
    Line = length(LinePos),
    Byte = last_part_length(LinePos, Size),
    ?Q("{string, _@IsCaseSensitive@, _@Binary@, _@Size@, _@Line@, _@Byte@}");
grammar_body({choise, ChoiseList}) ->
    BodyElementList = [grammar_body(Choise) || Choise <- ChoiseList],
    BodyList = [?Q("[_@Element]") || Element <- BodyElementList],
    ?Q("{choise, [_@BodyList]}");
grammar_body({seq, SeqList}) ->
    BodyList = [grammar_body(Seq) || Seq <- SeqList],
    ?Q("[_@_BodyList]").

casefold(String, false) ->
    String;
casefold(String, true) ->
    string:casefold(String).

last_part_length([], Size) ->
    Size;
last_part_length([{Start, _}], Size) ->
    Size - Start - 1;
last_part_length([_ | LinePosTail], Size) ->
    last_part_length(LinePosTail, Size).

grammar_body_code(_, []) ->
    ?Q("");
grammar_body_code(Name, _) ->
    CodeFunName = code_fun_name(Name),
    ?Q("fun '@CodeFunName@'/5").

code_fun_name(Name) ->
    list_to_atom("code_" ++ atom_to_list(Name)).

grammar_code(_, _, []) ->
    [];
grammar_code(Name, Body, _) ->
    CodeFunName = code_fun_name(Name),
    Count = value_stack_count(Body),
    ?Q( [ "'@CodeFunName@'(Input, LB, RuleList, Stack, MoreInput) ->"
        , "    {ValueStack, StackTail} = lists:split(_@Count@, Stack),"
        , "    Value = '@CodeFunName@'(ValueStack),"
        , "    p(Input, LB, RuleList, [Value | StackTail], MoreInput)."
        ]).

value_stack_count({seq, SeqList}) ->
    length(SeqList);
value_stack_count(_) ->
    1.

binary_body(Name, {repeat, Type, {chars, CharList}}) ->
    Clauses = lists:flatten(
        [ binary_match_clause(Name, Type, CharList)
        % TODO: HasContinue clause
        , binary_final_clause(Name, Type)
        ]),
    erl_syntax:function(merl:term(Name), Clauses).

binary_match_clause(Name, Type, CharList)
when Type == []; Type == '?' ->
    CodeFunName = code_fun_name(Name),
    [
        ?Q( [ "(<<_@Char@, Input/binary>>, <<>>, Stack, {L, __B}, RuleList, "
            , "MoreInput) ->"
            , "    Value = '@CodeFunName@'(<<_@Char@>>, L),"
            , "    p(Input, _@lb, RuleList, [Value | Stack], MoreInput)"
            ]
            , [{lb, lb_move(Char)}])
    || Char <- CharList];
binary_match_clause(Name, Type, CharList)
when Type == '*'; Type == '+' ->
    [
        ?Q( [ "(<<_@Char@, Input/binary>>, Acc, Stack, {L, __B}, RuleList, "
            , "MoreInput) ->"
            , "    '@Name@'(Input, <<Acc/binary, _@Char@>>, Stack, _@lb, "
            , "        RuleList, MoreInput)"
            ]
            , [{lb, lb_move(Char)}])
    || Char <- CharList].

lb_move($\n) ->
    ?Q("{L + 1, 0}");
lb_move(_) ->
    ?Q("{L, __B + 1}").

binary_final_clause(Name, '?') ->
    CodeFunName = code_fun_name(Name),
    ?Q( [ "(Input, Acc, Stack, LB, RuleList, MoreInput) ->"
        , "    Value = '@CodeFunName@'(Acc, -1),"
        , "    p(Input, LB, RuleList, [Value | Stack], MoreInput)"
        ]);
binary_final_clause(Name, '*') ->
    CodeFunName = code_fun_name(Name),
    ?Q( [ "(Input, Acc, Stack, LB, RuleList, MoreInput) ->"
        , "    Value = '@CodeFunName@'(Acc, -1),"
        , "    p(Input, LB, RuleList, [Value | Stack], MoreInput)"
        ]);
binary_final_clause(Name, '+') ->
    CodeFunName = code_fun_name(Name),
    [ ?Q("(Input, <<>>, _, LB, _, _) -> {error, '@Name@', Input, LB}")
    , ?Q(
        [ "(Input, Acc, Stack, LB, RuleList, MoreInput) ->"
        , "    Value = '@CodeFunName@'(Acc, -1),"
        , "    p(Input, LB, RuleList, [Value | Stack], MoreInput)"
        ])
    ];
binary_final_clause(Name, []) ->
    ?Q("(Input, <<>>, _, LB, _, _) -> {error, '@Name@', Input, LB}").

rule_code(_, {{grammar, _Name}, {_, _}, _, []}) ->
    [];
rule_code(FileName, {{grammar, Name}, {_, _}, Body, {Line, Code}}) ->
    Count = value_stack_count(Body),
    CodeFunName = atom_to_list(code_fun_name(Name)),
    GetStack =
        [ "__", integer_to_list(Count)
        , [ [", __", integer_to_list(C)] || C <- lists:seq(Count - 1, 1, -1) ]
        ],
    [ "-compile({inline, ", CodeFunName, "/1}).\n"
    , file_line(FileName, Line)
    , CodeFunName, "([", GetStack, "]) ->", Code, ".\n"
    ];
rule_code(_, {{binary, Name}, _, _, []}) ->
    CodeFunName = atom_to_list(code_fun_name(Name)),
    [ "-compile({inline, ", CodeFunName, "/2}).\n"
    , CodeFunName, "(__1, __L) -> __1.\n"
    ];
rule_code(FileName, {{binary, Name}, _, _, {Line, Code}}) ->
    CodeFunName = atom_to_list(code_fun_name(Name)),
    [ "-compile({inline, ", CodeFunName, "/2}).\n"
    , file_line(FileName, Line)
    , CodeFunName, "(__1, __L) ->", Code, ".\n"
    ].

file_line(FileName, Line) ->
    ["-file(\"", FileName, "\", ", integer_to_list(Line), ").\n"].

util_code(_, []) ->
    [];
util_code(FileName, {Line, Code}) ->
    [file_line(FileName, Line), Code].
