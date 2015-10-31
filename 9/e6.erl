-module(e6).
-export([treeToBitString/1, bitStringToTree/1, tree/0, test/0]).

treeToBitString({leaf,N}) ->
    Bin = term_to_binary(N),
    Size = byte_size(Bin) + 2,
    IsLeaf = 1,
    << Size, IsLeaf, Bin/binary >>;
treeToBitString({node,T1,T2}) ->
    TTB1 = treeToBitString(T1),
    << Size1, _/binary >> = TTB1,
    TTB2 = treeToBitString(T2),
    << Size2, _/binary >> = TTB2,
    Size = Size1 + Size2 + 2,
    << Size, 0, TTB1/binary, TTB2/binary >>.

bitStringToTree(<< _Size/integer, IsLeaf/integer, Bin/binary >>) ->
    case IsLeaf of
        1 -> 
            {leaf,binary_to_term(Bin)};
        0 ->
            << SizeL/integer, _/binary >> = Bin,
            << Bin1:SizeL/binary, Bin2/binary >> = Bin,
            {node,bitStringToTree(Bin1),bitStringToTree(Bin2)}
    end.

tree() ->
    {node,
     {node,
      {leaf,cat},
      {node,
       {leaf,dog},
       {leaf,emu}
      }
     },
     {leaf,fish}
    }.

test() ->
    B = treeToBitString(tree()),
    T = bitStringToTree(B),
    T == tree().

