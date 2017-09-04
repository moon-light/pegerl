-module(pegerl_peg_grammar).

-export([rules / 1, rules / 2]).

-spec rules(binary()) -> {term(), binary()} | fail.

rules(Input) -> rules(Input, fun more_fail/1).

-spec rules(binary(), more_input()) -> {term(), binary()} | fail.

rules(Input, MoreInput) -> p(Input, {0, 0}, rules(), [], MoreInput).

-spec rules() -> rule_define().

rules() ->
    [fun skip/0, {'+', [{choise, [[fun grammar_rule/0], [fun binary_rule/0]]}]},
     {'?', [fun code/0]}, fun skip/0, fun code_rules/5].

code_rules(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(4, Stack),
    Value = code_rules(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec skip() -> rule_define().

skip() -> [{'*', [{choise, [[fun 'SPACE'/6], [fun comment/0]]}]}].

-spec comment() -> rule_define().

comment() ->
    [{string, false, <<37>>, 1, 0, 1}, fun 'COMMENT_CHAR'/6,
     {string, false, <<10>>, 1, 1, 0}].

-spec grammar_rule() -> rule_define().

grammar_rule() ->
    [{'?', [{string, false, <<58>>, 1, 0, 1}]}, fun lower_rule_name/0,
     fun skip/0, {string, false, <<60, 45>>, 2, 0, 2}, fun 'IS_ENTER_RULE'/6,
     fun skip/0, fun grammar_rule_define/0, fun skip/0, {'?', [fun code/0]},
     fun skip/0, {string, false, <<59>>, 1, 0, 1}, fun skip/0,
     fun code_grammar_rule/5].

code_grammar_rule(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(12, Stack),
    Value = code_grammar_rule(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec lower_rule_name() -> rule_define().

lower_rule_name() ->
    [fun 'LOWER_CHAR'/6, {'?', [fun 'ATOM'/6]}, fun code_lower_rule_name/5].

code_lower_rule_name(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(2, Stack),
    Value = code_lower_rule_name(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec grammar_rule_define() -> rule_define().

grammar_rule_define() -> [{choise, [[fun choise/0], [fun seq/0]]}].

-spec binary_rule() -> rule_define().

binary_rule() ->
    [{'?', [{string, false, <<58>>, 1, 0, 1}]}, fun upper_rule_name/0,
     fun skip/0, {string, false, <<60>>, 1, 0, 1}, fun 'HAS_CONTINUE'/6,
     fun 'IS_ENTER_RULE'/6, fun skip/0, fun binary_rule_define/0, fun skip/0,
     {'?', [fun code/0]}, fun skip/0, {string, false, <<59>>, 1, 0, 1},
     fun skip/0, fun code_binary_rule/5].

code_binary_rule(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(13, Stack),
    Value = code_binary_rule(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec upper_rule_name() -> rule_define().

upper_rule_name() ->
    [fun 'UPPER_CHAR'/6, {'?', [fun 'ATOM'/6]}, fun code_upper_rule_name/5].

code_upper_rule_name(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(2, Stack),
    Value = code_upper_rule_name(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec binary_rule_define() -> rule_define().

binary_rule_define() ->
    [fun chars/0, {'?', [fun 'SUFFIX'/6]}, fun code_binary_rule_define/5].

code_binary_rule_define(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(2, Stack),
    Value = code_binary_rule_define(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec choise() -> rule_define().

choise() ->
    [fun seq/0,
     {'+',
      [fun skip/0, {string, false, <<47>>, 1, 0, 1}, fun skip/0, fun seq/0]},
     fun code_choise/5].

code_choise(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(2, Stack),
    Value = code_choise(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec seq() -> rule_define().

seq() -> [fun atomic/0, {'*', [fun skip/0, fun atomic/0]}, fun code_seq/5].

code_seq(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(2, Stack),
    Value = code_seq(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec atomic() -> rule_define().

atomic() ->
    [{'?', [fun 'PREFIX'/6]}, fun value/0, {'?', [fun 'SUFFIX'/6]},
     fun code_atomic/5].

code_atomic(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(3, Stack),
    Value = code_atomic(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec value() -> rule_define().

value() ->
    [{choise,
      [[fun rule_name/0], [fun string/0], [fun chars/0], [fun any/0],
       [fun parentheses/0]]}].

-spec rule_name() -> rule_define().

rule_name() ->
    [{'?', [fun remote/0]},
     {choise, [[fun upper_rule_name/0], [fun lower_rule_name/0]]},
     fun code_rule_name/5].

code_rule_name(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(2, Stack),
    Value = code_rule_name(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec remote() -> rule_define().

remote() -> [fun 'ATOM'/6, {string, false, <<58>>, 1, 0, 1}, fun code_remote/5].

code_remote(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(2, Stack),
    Value = code_remote(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec string() -> rule_define().

string() ->
    [{string, false, <<34>>, 1, 0, 1}, fun 'STRING_CHAR'/6,
     {'*', [{string, false, <<92>>, 1, 0, 1}, any, fun 'STRING_CHAR'/6]},
     {string, false, <<34>>, 1, 0, 1},
     {'?', [{string, false, <<105>>, 1, 0, 1}]}, fun code_string/5].

code_string(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(5, Stack),
    Value = code_string(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec chars() -> rule_define().

chars() ->
    [{string, false, <<91>>, 1, 0, 1}, fun 'CHARS_CHAR'/6,
     {'*', [{string, false, <<92>>, 1, 0, 1}, any, fun 'CHARS_CHAR'/6]},
     {string, false, <<93>>, 1, 0, 1}, fun code_chars/5].

code_chars(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(4, Stack),
    Value = code_chars(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec any() -> rule_define().

any() -> [{string, false, <<46>>, 1, 0, 1}, fun code_any/5].

code_any(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(1, Stack),
    Value = code_any(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec parentheses() -> rule_define().

parentheses() ->
    [{string, false, <<40>>, 1, 0, 1}, {'?', [fun skip/0]},
     fun grammar_rule_define/0, {'?', [fun skip/0]},
     {string, false, <<41>>, 1, 0, 1}, fun code_parentheses/5].

code_parentheses(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(5, Stack),
    Value = code_parentheses(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec code() -> rule_define().

code() ->
    [fun 'CODE_BEGIN_CHAR'/6, fun 'CODE_CHAR'/6,
     {'*', [{string, false, <<92>>, 1, 0, 1}, any, fun 'CODE_CHAR'/6]},
     fun 'CODE_BEGIN_CHAR'/6, fun code_code/5].

code_code(Input, LB, RuleList, Stack, MoreInput) ->
    {ValueStack, StackTail} = lists:split(4, Stack),
    Value = code_code(ValueStack),
    p(Input, LB, RuleList, [Value | StackTail], MoreInput).

-spec 'SPACE'(binary(), binary(), list(), [rule_define()], boolean(),
	      more_input()) -> rule_define().

'SPACE'(<<9, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'SPACE'(Input, <<Acc/binary, 9>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'SPACE'(<<10, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'SPACE'(Input, <<Acc/binary, 10>>, Stack, {L + 1, 0}, RuleList, MoreInput);
'SPACE'(<<13, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'SPACE'(Input, <<Acc/binary, 13>>, Stack, {L, __B + 1}, RuleList,
	    MoreInput);
'SPACE'(<<32, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'SPACE'(Input, <<Acc/binary, 32>>, Stack, {L, __B + 1}, RuleList,
	    MoreInput);
'SPACE'(Input, <<>>, _, LB, _, _) -> {error, 'SPACE', Input, LB};
'SPACE'(Input, Acc, Stack, LB, RuleList, MoreInput) ->
    Value = code_SPACE(Acc, -1),
    p(Input, LB, RuleList, [Value | Stack], MoreInput).

-spec 'COMMENT_CHAR'(binary(), binary(), list(), [rule_define()], boolean(),
		     more_input()) -> rule_define().

'COMMENT_CHAR'(<<0, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 0>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<1, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 1>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<2, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 2>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<3, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 3>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<4, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 4>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<5, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 5>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<6, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 6>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<7, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 7>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<8, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 8>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<9, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 9>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<11, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 11>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<12, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 12>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<13, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 13>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<14, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 14>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<15, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 15>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<16, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 16>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<17, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 17>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<18, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 18>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<19, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 19>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<20, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 20>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<21, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 21>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<22, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 22>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<23, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 23>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<24, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 24>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<25, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 25>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<26, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 26>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<27, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 27>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<28, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 28>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<29, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 29>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<30, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 30>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<31, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 31>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<32, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 32>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<33, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 33>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<34, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 34>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<35, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 35>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<36, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 36>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<37, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 37>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<38, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 38>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<39, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 39>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<40, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 40>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<41, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 41>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<42, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 42>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<43, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 43>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<44, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 44>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<45, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 45>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<46, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 46>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<47, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 47>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<48, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 48>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<49, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 49>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<50, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 50>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<51, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 51>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<52, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 52>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<53, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 53>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<54, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 54>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<55, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 55>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<56, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 56>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<57, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 57>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<58, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 58>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<59, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 59>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<60, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 60>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<61, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 61>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<62, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 62>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<63, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 63>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<64, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 64>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<65, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 65>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<66, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 66>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<67, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 67>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<68, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 68>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<69, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 69>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<70, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 70>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<71, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 71>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<72, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 72>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<73, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 73>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<74, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 74>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<75, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 75>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<76, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 76>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<77, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 77>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<78, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 78>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<79, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 79>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<80, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 80>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<81, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 81>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<82, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 82>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<83, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 83>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<84, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 84>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<85, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 85>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<86, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 86>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<87, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 87>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<88, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 88>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<89, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 89>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<90, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 90>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<91, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 91>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<92, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 92>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<93, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 93>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<94, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 94>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<95, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 95>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<96, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 96>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<97, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 97>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<98, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 98>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<99, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 99>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<100, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 100>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<101, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 101>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<102, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 102>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<103, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 103>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<104, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 104>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<105, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 105>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<106, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 106>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<107, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 107>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<108, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 108>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<109, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 109>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<110, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 110>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<111, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 111>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<112, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 112>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<113, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 113>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<114, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 114>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<115, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 115>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<116, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 116>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<117, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 117>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<118, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 118>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<119, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 119>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<120, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 120>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<121, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 121>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<122, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 122>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<123, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 123>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<124, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 124>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<125, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 125>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<126, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 126>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<127, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 127>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<128, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 128>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<129, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 129>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<130, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 130>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<131, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 131>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<132, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 132>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<133, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 133>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<134, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 134>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<135, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 135>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<136, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 136>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<137, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 137>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<138, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 138>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<139, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 139>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<140, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 140>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<141, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 141>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<142, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 142>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<143, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 143>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<144, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 144>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<145, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 145>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<146, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 146>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<147, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 147>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<148, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 148>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<149, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 149>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<150, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 150>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<151, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 151>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<152, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 152>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<153, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 153>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<154, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 154>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<155, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 155>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<156, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 156>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<157, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 157>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<158, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 158>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<159, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 159>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<160, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 160>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<161, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 161>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<162, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 162>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<163, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 163>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<164, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 164>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<165, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 165>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<166, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 166>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<167, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 167>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<168, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 168>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<169, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 169>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<170, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 170>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<171, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 171>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<172, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 172>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<173, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 173>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<174, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 174>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<175, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 175>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<176, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 176>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<177, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 177>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<178, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 178>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<179, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 179>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<180, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 180>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<181, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 181>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<182, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 182>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<183, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 183>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<184, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 184>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<185, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 185>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<186, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 186>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<187, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 187>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<188, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 188>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<189, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 189>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<190, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 190>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<191, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 191>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<192, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 192>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<193, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 193>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<194, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 194>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<195, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 195>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<196, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 196>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<197, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 197>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<198, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 198>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<199, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 199>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<200, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 200>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<201, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 201>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<202, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 202>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<203, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 203>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<204, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 204>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<205, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 205>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<206, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 206>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<207, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 207>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<208, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 208>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<209, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 209>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<210, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 210>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<211, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 211>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<212, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 212>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<213, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 213>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<214, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 214>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<215, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 215>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<216, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 216>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<217, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 217>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<218, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 218>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<219, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 219>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<220, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 220>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<221, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 221>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<222, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 222>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<223, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 223>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<224, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 224>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<225, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 225>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<226, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 226>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<227, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 227>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<228, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 228>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<229, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 229>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<230, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 230>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<231, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 231>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<232, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 232>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<233, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 233>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<234, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 234>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<235, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 235>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<236, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 236>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<237, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 237>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<238, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 238>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<239, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 239>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<240, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 240>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<241, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 241>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<242, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 242>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<243, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 243>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<244, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 244>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<245, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 245>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<246, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 246>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<247, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 247>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<248, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 248>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<249, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 249>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<250, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 250>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<251, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 251>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<252, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 252>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<253, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 253>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<254, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 254>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(<<255, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    'COMMENT_CHAR'(Input, <<Acc/binary, 255>>, Stack, {L, __B + 1}, RuleList,
		   MoreInput);
'COMMENT_CHAR'(Input, Acc, Stack, LB, RuleList, MoreInput) ->
    Value = code_COMMENT_CHAR(Acc, -1),
    p(Input, LB, RuleList, [Value | Stack], MoreInput).

-spec 'LOWER_CHAR'(binary(), binary(), list(), [rule_define()], boolean(),
		   more_input()) -> rule_define().

'LOWER_CHAR'(<<97, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<97>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<98, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<98>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<99, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<99>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<100, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<100>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<101, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<101>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<102, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<102>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<103, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<103>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<104, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<104>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<105, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<105>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<106, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<106>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<107, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<107>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<108, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<108>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<109, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<109>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<110, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<110>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<111, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<111>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<112, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<112>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<113, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<113>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<114, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<114>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<115, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<115>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<116, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<116>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<117, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<117>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<118, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<118>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<119, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<119>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<120, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<120>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<121, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<121>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(<<122, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_LOWER_CHAR(<<122>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'LOWER_CHAR'(Input, <<>>, _, LB, _, _) -> {error, 'LOWER_CHAR', Input, LB}.

-spec 'UPPER_CHAR'(binary(), binary(), list(), [rule_define()], boolean(),
		   more_input()) -> rule_define().

'UPPER_CHAR'(<<65, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<65>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<66, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<66>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<67, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<67>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<68, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<68>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<69, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<69>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<70, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<70>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<71, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<71>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<72, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<72>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<73, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<73>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<74, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<74>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<75, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<75>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<76, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<76>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<77, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<77>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<78, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<78>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<79, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<79>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<80, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<80>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<81, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<81>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<82, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<82>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<83, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<83>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<84, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<84>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<85, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<85>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<86, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<86>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<87, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<87>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<88, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<88>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<89, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<89>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(<<90, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    Value = code_UPPER_CHAR(<<90>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'UPPER_CHAR'(Input, <<>>, _, LB, _, _) -> {error, 'UPPER_CHAR', Input, LB}.

-spec 'ATOM'(binary(), binary(), list(), [rule_define()], boolean(),
	     more_input()) -> rule_define().

'ATOM'(<<48, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 48>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<49, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 49>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<50, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 50>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<51, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 51>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<52, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 52>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<53, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 53>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<54, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 54>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<55, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 55>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<56, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 56>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<57, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 57>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<64, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 64>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<65, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 65>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<66, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 66>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<67, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 67>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<68, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 68>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<69, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 69>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<70, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 70>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<71, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 71>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<72, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 72>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<73, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 73>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<74, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 74>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<75, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 75>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<76, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 76>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<77, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 77>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<78, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 78>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<79, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 79>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<80, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 80>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<81, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 81>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<82, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 82>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<83, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 83>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<84, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 84>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<85, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 85>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<86, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 86>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<87, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 87>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<88, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 88>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<89, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 89>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<90, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 90>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<95, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 95>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<97, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 97>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<98, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 98>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<99, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 99>>, Stack, {L, __B + 1}, RuleList, MoreInput);
'ATOM'(<<100, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 100>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<101, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 101>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<102, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 102>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<103, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 103>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<104, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 104>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<105, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 105>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<106, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 106>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<107, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 107>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<108, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 108>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<109, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 109>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<110, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 110>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<111, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 111>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<112, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 112>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<113, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 113>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<114, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 114>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<115, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 115>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<116, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 116>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<117, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 117>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<118, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 118>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<119, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 119>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<120, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 120>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<121, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 121>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(<<122, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'ATOM'(Input, <<Acc/binary, 122>>, Stack, {L, __B + 1}, RuleList,
	   MoreInput);
'ATOM'(Input, <<>>, _, LB, _, _) -> {error, 'ATOM', Input, LB};
'ATOM'(Input, Acc, Stack, LB, RuleList, MoreInput) ->
    Value = code_ATOM(Acc, -1),
    p(Input, LB, RuleList, [Value | Stack], MoreInput).

-spec 'HAS_CONTINUE'(binary(), binary(), list(), [rule_define()], boolean(),
		     more_input()) -> rule_define().

'HAS_CONTINUE'(<<45, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    Value = code_HAS_CONTINUE(<<45>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'HAS_CONTINUE'(<<126, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
	       MoreInput) ->
    Value = code_HAS_CONTINUE(<<126>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'HAS_CONTINUE'(Input, <<>>, _, LB, _, _) -> {error, 'HAS_CONTINUE', Input, LB}.

-spec 'IS_ENTER_RULE'(binary(), binary(), list(), [rule_define()], boolean(),
		      more_input()) -> rule_define().

'IS_ENTER_RULE'(<<62, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
		MoreInput) ->
    Value = code_IS_ENTER_RULE(<<62>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'IS_ENTER_RULE'(Input, Acc, Stack, LB, RuleList, MoreInput) ->
    Value = code_IS_ENTER_RULE(Acc, -1),
    p(Input, LB, RuleList, [Value | Stack], MoreInput).

-spec 'PREFIX'(binary(), binary(), list(), [rule_define()], boolean(),
	       more_input()) -> rule_define().

'PREFIX'(<<33, Input/binary>>, <<>>, Stack, {L, __B}, RuleList, MoreInput) ->
    Value = code_PREFIX(<<33>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'PREFIX'(<<38, Input/binary>>, <<>>, Stack, {L, __B}, RuleList, MoreInput) ->
    Value = code_PREFIX(<<38>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'PREFIX'(Input, <<>>, _, LB, _, _) -> {error, 'PREFIX', Input, LB}.

-spec 'SUFFIX'(binary(), binary(), list(), [rule_define()], boolean(),
	       more_input()) -> rule_define().

'SUFFIX'(<<42, Input/binary>>, <<>>, Stack, {L, __B}, RuleList, MoreInput) ->
    Value = code_SUFFIX(<<42>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'SUFFIX'(<<43, Input/binary>>, <<>>, Stack, {L, __B}, RuleList, MoreInput) ->
    Value = code_SUFFIX(<<43>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'SUFFIX'(<<63, Input/binary>>, <<>>, Stack, {L, __B}, RuleList, MoreInput) ->
    Value = code_SUFFIX(<<63>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'SUFFIX'(Input, <<>>, _, LB, _, _) -> {error, 'SUFFIX', Input, LB}.

-spec 'STRING_CHAR'(binary(), binary(), list(), [rule_define()], boolean(),
		    more_input()) -> rule_define().

'STRING_CHAR'(<<32, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 32>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<33, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 33>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<35, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 35>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<36, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 36>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<37, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 37>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<38, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 38>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<39, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 39>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<40, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 40>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<41, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 41>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<42, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 42>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<43, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 43>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<44, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 44>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<45, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 45>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<46, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 46>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<47, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 47>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<48, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 48>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<49, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 49>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<50, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 50>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<51, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 51>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<52, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 52>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<53, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 53>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<54, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 54>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<55, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 55>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<56, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 56>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<57, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 57>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<58, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 58>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<59, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 59>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<60, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 60>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<61, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 61>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<62, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 62>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<63, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 63>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<64, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 64>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<65, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 65>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<66, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 66>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<67, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 67>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<68, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 68>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<69, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 69>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<70, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 70>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<71, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 71>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<72, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 72>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<73, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 73>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<74, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 74>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<75, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 75>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<76, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 76>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<77, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 77>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<78, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 78>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<79, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 79>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<80, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 80>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<81, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 81>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<82, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 82>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<83, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 83>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<84, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 84>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<85, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 85>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<86, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 86>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<87, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 87>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<88, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 88>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<89, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 89>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<90, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 90>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<91, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 91>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<93, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 93>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<94, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 94>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<95, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 95>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<96, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 96>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<97, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 97>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<98, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 98>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<99, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 99>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<100, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 100>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<101, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 101>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<102, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 102>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<103, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 103>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<104, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 104>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<105, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 105>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<106, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 106>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<107, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 107>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<108, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 108>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<109, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 109>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<110, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 110>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<111, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 111>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<112, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 112>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<113, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 113>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<114, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 114>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<115, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 115>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<116, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 116>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<117, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 117>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<118, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 118>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<119, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 119>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<120, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 120>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<121, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 121>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<122, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 122>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<123, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 123>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<124, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 124>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<125, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 125>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<126, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 126>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<127, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 127>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<128, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 128>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<129, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 129>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<130, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 130>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<131, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 131>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<132, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 132>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<133, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 133>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<134, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 134>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<135, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 135>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<136, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 136>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<137, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 137>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<138, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 138>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<139, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 139>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<140, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 140>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<141, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 141>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<142, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 142>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<143, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 143>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<144, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 144>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<145, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 145>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<146, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 146>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<147, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 147>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<148, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 148>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<149, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 149>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<150, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 150>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<151, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 151>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<152, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 152>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<153, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 153>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<154, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 154>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<155, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 155>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<156, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 156>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<157, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 157>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<158, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 158>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<159, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 159>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<160, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 160>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<161, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 161>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<162, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 162>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<163, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 163>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<164, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 164>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<165, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 165>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<166, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 166>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<167, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 167>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<168, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 168>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<169, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 169>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<170, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 170>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<171, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 171>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<172, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 172>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<173, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 173>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<174, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 174>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<175, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 175>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<176, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 176>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<177, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 177>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<178, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 178>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<179, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 179>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<180, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 180>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<181, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 181>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<182, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 182>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<183, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 183>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<184, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 184>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<185, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 185>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<186, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 186>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<187, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 187>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<188, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 188>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<189, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 189>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<190, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 190>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<191, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 191>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<192, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 192>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<193, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 193>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<194, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 194>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<195, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 195>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<196, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 196>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<197, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 197>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<198, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 198>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<199, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 199>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<200, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 200>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<201, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 201>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<202, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 202>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<203, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 203>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<204, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 204>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<205, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 205>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<206, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 206>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<207, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 207>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<208, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 208>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<209, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 209>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<210, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 210>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<211, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 211>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<212, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 212>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<213, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 213>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<214, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 214>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<215, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 215>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<216, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 216>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<217, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 217>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<218, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 218>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<219, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 219>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<220, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 220>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<221, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 221>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<222, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 222>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<223, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 223>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<224, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 224>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<225, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 225>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<226, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 226>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<227, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 227>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<228, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 228>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<229, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 229>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<230, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 230>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<231, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 231>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<232, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 232>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<233, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 233>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<234, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 234>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<235, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 235>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<236, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 236>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<237, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 237>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<238, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 238>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<239, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 239>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<240, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 240>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<241, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 241>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<242, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 242>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<243, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 243>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<244, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 244>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<245, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 245>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<246, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 246>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<247, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 247>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<248, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 248>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<249, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 249>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<250, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 250>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<251, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 251>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<252, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 252>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<253, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 253>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<254, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 254>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(<<255, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	      MoreInput) ->
    'STRING_CHAR'(Input, <<Acc/binary, 255>>, Stack, {L, __B + 1}, RuleList,
		  MoreInput);
'STRING_CHAR'(Input, Acc, Stack, LB, RuleList, MoreInput) ->
    Value = code_STRING_CHAR(Acc, -1),
    p(Input, LB, RuleList, [Value | Stack], MoreInput).

-spec 'CHARS_CHAR'(binary(), binary(), list(), [rule_define()], boolean(),
		   more_input()) -> rule_define().

'CHARS_CHAR'(<<32, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 32>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<33, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 33>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<34, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 34>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<35, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 35>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<36, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 36>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<37, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 37>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<38, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 38>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<39, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 39>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<40, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 40>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<41, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 41>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<42, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 42>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<43, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 43>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<44, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 44>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<45, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 45>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<46, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 46>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<47, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 47>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<48, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 48>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<49, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 49>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<50, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 50>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<51, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 51>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<52, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 52>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<53, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 53>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<54, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 54>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<55, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 55>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<56, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 56>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<57, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 57>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<58, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 58>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<59, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 59>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<60, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 60>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<61, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 61>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<62, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 62>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<63, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 63>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<64, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 64>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<65, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 65>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<66, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 66>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<67, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 67>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<68, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 68>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<69, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 69>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<70, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 70>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<71, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 71>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<72, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 72>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<73, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 73>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<74, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 74>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<75, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 75>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<76, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 76>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<77, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 77>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<78, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 78>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<79, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 79>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<80, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 80>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<81, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 81>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<82, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 82>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<83, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 83>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<84, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 84>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<85, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 85>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<86, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 86>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<87, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 87>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<88, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 88>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<89, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 89>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<90, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 90>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<91, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 91>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<94, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 94>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<95, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 95>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<96, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 96>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<97, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 97>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<98, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 98>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<99, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 99>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<100, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 100>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<101, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 101>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<102, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 102>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<103, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 103>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<104, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 104>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<105, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 105>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<106, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 106>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<107, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 107>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<108, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 108>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<109, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 109>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<110, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 110>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<111, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 111>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<112, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 112>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<113, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 113>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<114, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 114>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<115, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 115>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<116, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 116>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<117, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 117>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<118, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 118>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<119, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 119>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<120, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 120>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<121, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 121>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<122, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 122>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<123, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 123>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<124, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 124>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<125, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 125>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(<<126, Input/binary>>, Acc, Stack, {L, __B}, RuleList,
	     MoreInput) ->
    'CHARS_CHAR'(Input, <<Acc/binary, 126>>, Stack, {L, __B + 1}, RuleList,
		 MoreInput);
'CHARS_CHAR'(Input, Acc, Stack, LB, RuleList, MoreInput) ->
    Value = code_CHARS_CHAR(Acc, -1),
    p(Input, LB, RuleList, [Value | Stack], MoreInput).

-spec 'CODE_CHAR'(binary(), binary(), list(), [rule_define()], boolean(),
		  more_input()) -> rule_define().

'CODE_CHAR'(<<0, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 0>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<1, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 1>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<2, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 2>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<3, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 3>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<4, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 4>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<5, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 5>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<6, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 6>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<7, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 7>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<8, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 8>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<9, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 9>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<10, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 10>>, Stack, {L + 1, 0}, RuleList,
		MoreInput);
'CODE_CHAR'(<<11, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 11>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<12, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 12>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<13, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 13>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<14, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 14>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<15, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 15>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<16, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 16>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<17, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 17>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<18, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 18>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<19, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 19>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<20, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 20>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<21, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 21>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<22, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 22>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<23, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 23>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<24, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 24>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<25, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 25>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<26, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 26>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<27, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 27>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<28, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 28>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<29, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 29>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<30, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 30>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<31, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 31>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<32, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 32>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<33, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 33>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<34, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 34>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<35, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 35>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<36, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 36>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<37, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 37>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<38, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 38>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<39, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 39>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<40, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 40>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<41, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 41>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<42, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 42>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<43, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 43>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<44, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 44>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<45, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 45>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<46, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 46>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<47, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 47>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<48, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 48>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<49, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 49>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<50, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 50>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<51, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 51>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<52, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 52>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<53, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 53>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<54, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 54>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<55, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 55>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<56, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 56>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<57, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 57>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<58, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 58>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<59, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 59>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<60, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 60>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<61, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 61>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<62, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 62>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<63, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 63>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<64, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 64>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<65, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 65>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<66, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 66>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<67, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 67>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<68, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 68>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<69, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 69>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<70, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 70>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<71, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 71>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<72, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 72>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<73, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 73>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<74, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 74>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<75, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 75>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<76, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 76>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<77, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 77>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<78, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 78>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<79, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 79>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<80, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 80>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<81, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 81>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<82, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 82>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<83, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 83>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<84, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 84>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<85, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 85>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<86, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 86>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<87, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 87>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<88, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 88>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<89, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 89>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<90, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 90>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<91, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 91>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<93, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 93>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<94, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 94>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<95, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 95>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<97, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 97>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<98, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 98>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<99, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 99>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<100, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 100>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<101, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 101>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<102, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 102>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<103, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 103>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<104, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 104>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<105, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 105>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<106, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 106>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<107, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 107>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<108, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 108>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<109, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 109>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<110, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 110>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<111, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 111>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<112, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 112>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<113, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 113>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<114, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 114>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<115, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 115>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<116, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 116>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<117, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 117>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<118, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 118>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<119, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 119>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<120, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 120>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<121, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 121>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<122, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 122>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<123, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 123>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<124, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 124>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<125, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 125>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<126, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 126>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<127, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 127>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<128, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 128>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<129, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 129>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<130, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 130>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<131, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 131>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<132, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 132>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<133, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 133>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<134, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 134>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<135, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 135>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<136, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 136>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<137, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 137>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<138, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 138>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<139, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 139>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<140, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 140>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<141, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 141>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<142, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 142>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<143, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 143>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<144, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 144>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<145, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 145>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<146, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 146>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<147, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 147>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<148, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 148>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<149, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 149>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<150, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 150>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<151, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 151>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<152, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 152>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<153, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 153>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<154, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 154>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<155, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 155>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<156, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 156>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<157, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 157>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<158, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 158>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<159, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 159>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<160, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 160>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<161, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 161>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<162, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 162>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<163, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 163>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<164, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 164>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<165, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 165>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<166, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 166>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<167, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 167>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<168, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 168>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<169, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 169>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<170, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 170>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<171, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 171>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<172, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 172>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<173, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 173>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<174, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 174>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<175, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 175>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<176, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 176>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<177, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 177>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<178, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 178>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<179, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 179>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<180, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 180>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<181, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 181>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<182, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 182>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<183, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 183>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<184, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 184>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<185, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 185>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<186, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 186>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<187, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 187>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<188, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 188>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<189, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 189>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<190, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 190>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<191, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 191>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<192, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 192>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<193, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 193>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<194, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 194>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<195, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 195>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<196, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 196>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<197, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 197>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<198, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 198>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<199, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 199>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<200, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 200>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<201, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 201>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<202, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 202>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<203, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 203>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<204, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 204>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<205, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 205>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<206, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 206>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<207, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 207>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<208, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 208>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<209, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 209>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<210, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 210>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<211, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 211>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<212, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 212>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<213, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 213>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<214, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 214>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<215, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 215>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<216, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 216>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<217, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 217>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<218, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 218>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<219, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 219>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<220, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 220>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<221, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 221>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<222, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 222>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<223, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 223>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<224, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 224>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<225, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 225>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<226, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 226>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<227, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 227>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<228, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 228>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<229, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 229>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<230, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 230>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<231, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 231>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<232, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 232>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<233, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 233>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<234, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 234>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<235, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 235>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<236, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 236>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<237, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 237>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<238, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 238>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<239, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 239>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<240, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 240>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<241, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 241>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<242, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 242>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<243, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 243>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<244, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 244>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<245, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 245>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<246, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 246>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<247, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 247>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<248, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 248>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<249, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 249>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<250, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 250>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<251, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 251>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<252, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 252>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<253, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 253>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<254, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 254>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(<<255, Input/binary>>, Acc, Stack, {L, __B}, RuleList, MoreInput) ->
    'CODE_CHAR'(Input, <<Acc/binary, 255>>, Stack, {L, __B + 1}, RuleList,
		MoreInput);
'CODE_CHAR'(Input, Acc, Stack, LB, RuleList, MoreInput) ->
    Value = code_CODE_CHAR(Acc, -1),
    p(Input, LB, RuleList, [Value | Stack], MoreInput).

-spec 'CODE_BEGIN_CHAR'(binary(), binary(), list(), [rule_define()], boolean(),
			more_input()) -> rule_define().

'CODE_BEGIN_CHAR'(<<96, Input/binary>>, <<>>, Stack, {L, __B}, RuleList,
		  MoreInput) ->
    Value = code_CODE_BEGIN_CHAR(<<96>>, L),
    p(Input, {L, __B + 1}, RuleList, [Value | Stack], MoreInput);
'CODE_BEGIN_CHAR'(Input, <<>>, _, LB, _, _) ->
    {error, 'CODE_BEGIN_CHAR', Input, LB}.

%% Code from peg file
-compile({inline, code_rules/1}).
-file("priv/pegerl_peg_grammar.peg", 0).
code_rules([__4, __3, __2, __1]) ->{__2, __3}.
-compile({inline, code_grammar_rule/1}).
-file("priv/pegerl_peg_grammar.peg", 9).
code_grammar_rule([__12, __11, __10, __9, __8, __7, __6, __5, __4, __3, __2, __1]) ->{__2, {__1 =/= [], __5}, __7, __9}.
-compile({inline, code_lower_rule_name/1}).
-file("priv/pegerl_peg_grammar.peg", 12).
code_lower_rule_name([__2, __1]) ->{grammar, binary_to_atom(iolist_to_binary([__1, __2]), utf8)}.
-compile({inline, code_binary_rule/1}).
-file("priv/pegerl_peg_grammar.peg", 19).
code_binary_rule([__13, __12, __11, __10, __9, __8, __7, __6, __5, __4, __3, __2, __1]) ->{__2, {__1 =/= [], __5, __6}, __8, __10}.
-compile({inline, code_upper_rule_name/1}).
-file("priv/pegerl_peg_grammar.peg", 22).
code_upper_rule_name([__2, __1]) ->{binary, binary_to_atom(iolist_to_binary([__1, __2]), utf8)}.
-compile({inline, code_binary_rule_define/1}).
-file("priv/pegerl_peg_grammar.peg", 24).
code_binary_rule_define([__2, __1]) ->{repeat, __2, __1}.
-compile({inline, code_choise/1}).
-file("priv/pegerl_peg_grammar.peg", 27).
code_choise([__2, __1]) ->{choise, [__1 | [ E || [_, _, _, E] <- __2 ]]}.
-compile({inline, code_seq/1}).
-file("priv/pegerl_peg_grammar.peg", 29).
code_seq([__2, __1]) ->{seq, [__1 | [E || [_, E] <- __2]]}.
-compile({inline, code_atomic/1}).
-file("priv/pegerl_peg_grammar.peg", 31).
code_atomic([__3, __2, __1]) ->{__1, __2, __3}.
-compile({inline, code_rule_name/1}).
-file("priv/pegerl_peg_grammar.peg", 36).
code_rule_name([__2, __1]) ->case __1 of [] -> {local, __2}; Remote -> {remote, Remote, __2} end.
-compile({inline, code_remote/1}).
-file("priv/pegerl_peg_grammar.peg", 38).
code_remote([__2, __1]) ->binary_to_atom(__1, utf8).
-compile({inline, code_string/1}).
-file("priv/pegerl_peg_grammar.peg", 40).
code_string([__5, __4, __3, __2, __1]) ->
    String = binary_to_list(iolist_to_binary([$", __2, __3, $"])),
    {ok, [{string, _, RealString}], _} = erl_scan:string(String),
    {string, __5 =/= [], RealString}
.
-compile({inline, code_chars/1}).
-file("priv/pegerl_peg_grammar.peg", 46).
code_chars([__4, __3, __2, __1]) ->
    case __2
    of <<$^, Rest/binary>> ->
        NotChars = chars(iolist_to_binary([Rest, __3]), []),
        {chars, not_chars(NotChars)}
    ; _ ->
        {chars, chars(iolist_to_binary([__2, __3]), [])}
    end
.
-compile({inline, code_any/1}).
-file("priv/pegerl_peg_grammar.peg", 56).
code_any([__1]) ->any.
-compile({inline, code_parentheses/1}).
-file("priv/pegerl_peg_grammar.peg", 58).
code_parentheses([__5, __4, __3, __2, __1]) ->__3.
-compile({inline, code_code/1}).
-file("priv/pegerl_peg_grammar.peg", 61).
code_code([__4, __3, __2, __1]) ->{__1, iolist_to_binary([__2, __3])}.
-compile({inline, code_SPACE/2}).
code_SPACE(__1, __L) -> __1.
-compile({inline, code_COMMENT_CHAR/2}).
code_COMMENT_CHAR(__1, __L) -> __1.
-compile({inline, code_LOWER_CHAR/2}).
code_LOWER_CHAR(__1, __L) -> __1.
-compile({inline, code_UPPER_CHAR/2}).
code_UPPER_CHAR(__1, __L) -> __1.
-compile({inline, code_ATOM/2}).
code_ATOM(__1, __L) -> __1.
-compile({inline, code_HAS_CONTINUE/2}).
-file("priv/pegerl_peg_grammar.peg", 73).
code_HAS_CONTINUE(__1, __L) ->__1 == <<"~">>.
-compile({inline, code_IS_ENTER_RULE/2}).
-file("priv/pegerl_peg_grammar.peg", 75).
code_IS_ENTER_RULE(__1, __L) ->__1 =/= <<>>.
-compile({inline, code_PREFIX/2}).
code_PREFIX(__1, __L) -> __1.
-compile({inline, code_SUFFIX/2}).
-file("priv/pegerl_peg_grammar.peg", 79).
code_SUFFIX(__1, __L) ->binary_to_atom(__1, utf8).
-compile({inline, code_STRING_CHAR/2}).
code_STRING_CHAR(__1, __L) -> __1.
-compile({inline, code_CHARS_CHAR/2}).
code_CHARS_CHAR(__1, __L) -> __1.
-compile({inline, code_CODE_CHAR/2}).
code_CODE_CHAR(__1, __L) -> __1.
-compile({inline, code_CODE_BEGIN_CHAR/2}).
-file("priv/pegerl_peg_grammar.peg", 88).
code_CODE_BEGIN_CHAR(__1, __L) ->__L.

-file("pegerl_common.hrl", 0).
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

-file("priv/pegerl_peg_grammar.peg", 90).

% Encoded char
chars(<<$\\, $x, Hi, Lo, Tail/binary>>, Chars) ->
    Char = list_to_integer([Hi, Lo], 16),
    range(Tail, Char, Chars);
chars(<<$\\, $t, Tail/binary>>, Chars) ->
    range(Tail, $\t, Chars);
chars(<<$\\, $s, Tail/binary>>, Chars) ->
    range(Tail, $\s, Chars);
chars(<<$\\, $r, Tail/binary>>, Chars) ->
    range(Tail, $\r, Chars);
chars(<<$\\, $n, Tail/binary>>, Chars) ->
    range(Tail, $\n, Chars);
chars(<<$\\, $\\, Tail/binary>>, Chars) ->
    range(Tail, $\\, Chars);
chars(<<$\\, $], Tail/binary>>, Chars) ->
    range(Tail, $], Chars);
% Simple char
chars(<<A, Tail/binary>>, Chars) ->
    range(Tail, A, Chars);
chars(<<>>, Chars) ->
    lists:sort(Chars).

% Encoded char
range(<<"-", $\\, $x, Hi, Lo, Tail/binary>>, BeginChar, Chars) ->
    EndChar = list_to_integer([Hi, Lo], 16),
    chars(Tail, lists:seq(BeginChar, EndChar) ++ Chars);
range(<<"-", $\\, $t, Tail/binary>>, BeginChar, Chars) ->
    chars(Tail, lists:seq(BeginChar, $\t) ++ Chars);
range(<<"-", $\\, $s, Tail/binary>>, BeginChar, Chars) ->
    chars(Tail, lists:seq(BeginChar, $\s) ++ Chars);
range(<<"-", $\\, $r, Tail/binary>>, BeginChar, Chars) ->
    chars(Tail, lists:seq(BeginChar, $\r) ++ Chars);
range(<<"-", $\\, $n, Tail/binary>>, BeginChar, Chars) ->
    chars(Tail, lists:seq(BeginChar, $\n) ++ Chars);
range(<<"-", $\\, $\\, Tail/binary>>, BeginChar, Chars) ->
    chars(Tail, lists:seq(BeginChar, $\\) ++ Chars);
range(<<"-", $\\, $], Tail/binary>>, BeginChar, Chars) ->
    chars(Tail, lists:seq(BeginChar, $]) ++ Chars);
range(<<"-", $\\, End, Tail/binary>>, BeginChar, Chars) ->
    chars(Tail, lists:seq(BeginChar, End) ++ Chars);
% Simple char
range(<<"-", End, Tail/binary>>, BeginChar, Chars) ->
    chars(Tail, lists:seq(BeginChar, End) ++ Chars);
% Not range
range(Tail, Char, Chars) ->
    chars(Tail, [Char | Chars]).

not_chars(NotChars) ->
    not_chars(lists:seq(0, 255), NotChars).

not_chars([X | Tail], [X | NotCharsTail]) ->
    not_chars(Tail, NotCharsTail);
not_chars([X | Tail], NotChars) ->
    [X | not_chars(Tail, NotChars)];
not_chars([], []) ->
    [].
