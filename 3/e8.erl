-module(e8).
-export([parser/1, evaluator/1, compiler/1, simulator/1, simplifier/1]).

% missing conditional and local definition extensions

extract_entities([$~ | Operand]) -> { negate, Operand };
extract_entities([$( | T]) -> extract_entities([$( | T], 0, {}, []);
extract_entities(N) -> { num, list_to_integer(N) }.

extract_entities(Expressions, Parens, Entities, EntityAccu) ->
    case Expressions of
        [] -> Entities;
        [$( | T] when Parens == 0 -> 
            extract_entities(T, 1, Entities, EntityAccu);
        [$( | T] -> 
            extract_entities(T, Parens+1, Entities, EntityAccu++[$(]);
        [$) | _T] when Parens == 1 -> 
            erlang:append_element(Entities, EntityAccu);
        [$) | T] -> 
            extract_entities(T, Parens-1, Entities, EntityAccu++[$)]);
        [$+ | T] when Parens == 1 -> 
            extract_entities(T, Parens, erlang:append_element(erlang:append_element(Entities, EntityAccu), plus), []);
        [$- | T] when Parens == 1 -> 
            extract_entities(T, Parens, erlang:append_element(erlang:append_element(Entities, EntityAccu), minus), []);
        [$* | T] when Parens == 1 -> 
            extract_entities(T, Parens, erlang:append_element(erlang:append_element(Entities, EntityAccu), multiply), []);
        [$/ | T] when Parens == 1 -> 
            extract_entities(T, Parens, erlang:append_element(erlang:append_element(Entities, EntityAccu), divide), []);
        [H | T] -> extract_entities(T, Parens, Entities, EntityAccu++[H])
    end.


parser(Expressions) ->
    case extract_entities(Expressions) of
        { num, N } -> { num, N };
        { Operator, Operand } -> { Operator, parser(Operand) };
        { Operand1, Operator, Operand2 } -> 
            { Operator, parser(Operand1), parser(Operand2) }
    end.

evaluator(Operations) ->
    case Operations of
        { num, N } -> N;
        { negate, A } -> -evaluator(A);
        { plus, A, B } -> evaluator(A) + evaluator(B);
        { minus, A, B } -> evaluator(A) - evaluator(B);
        { multiply, A, B } -> evaluator(A) * evaluator(B);
        { divide, A, B } -> evaluator(A) / evaluator(B)
    end.

compiler(Operations) ->
    case Operations of
        { num, N } -> [N];
        { negate, A } -> compiler(A) ++ [ negate ];
        { Operator, A, B } -> compiler(A) ++ compiler(B) ++ [ Operator ]
    end.

simulator(Instructions) -> simulator(Instructions, []).
simulator(Instructions, Stack) ->
    case Instructions of
        [] -> hd(Stack);
        [N | T] when is_integer(N) -> simulator(T, [N | Stack]);
        [negate | T] ->
            [N1 | NT] = Stack,
            simulator(T, [-N1 | NT]);
        [Operator | T] ->
            [N1, N2 | NT] = Stack,
            N3 = case Operator of
                plus -> N1 + N2;
                minus -> N1 - N2;
                multiply -> N1 * N2;
                divide -> N1 / N2
            end,
            simulator(T, [N3 | NT])
    end.

simplifier(Operations) ->
    case Operations of
        { multiply, { num, 0 }, _ } -> { num, 0 };
        { multiply, _, { num, 0 } } -> { num, 0 };
        { multiply, { num, 1 }, B } -> simplifier(B);
        { multiply, A, { num, 1 } } -> simplifier(A);
        { plus, { num, 0 }, B } -> simplifier(B);
        { plus, A, { num, 0 } } -> simplifier(A);
        { minus, A, { num, 0 } } -> simplifier(A);
        { negate, A } -> { negate, simplifier(A) };
        { num, A } -> { num, A };
        { Operator, A, B } -> { Operator, simplifier(A), simplifier(B) }
    end.

