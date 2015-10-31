-module(e6).
-export([treeToBitString/1, bitStringToTree/1, tree/0]).

treeToBitString({leaf,N}) ->
    Bin = term_to_binary(N),
    Size = byte_size(Bin) + 1,
    << Size, Bin/binary >>;
treeToBitString({node,T1,T2}) ->
    TTB1 = treeToBitString(T1),
    << Size1, _/binary >> = TTB1,
    TTB2 = treeToBitString(T2),
    << Size2, _/binary >> = TTB2,
    << (Size1+Size2+1), TTB1/binary, TTB2/binary >>.

bitStringToTree(<< Size/integer, Bin/binary >>) ->
    case byte_size(Bin)+1 of
        Size -> 
            {leaf,binary_to_term(Bin)};
        _ ->
            << SizeL/integer, _/binary >> = Bin,
            BitSize = SizeL*8,
            << Bin1:BitSize/binary, Bin2/binary >> = Bin,
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
