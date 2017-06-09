-module(binary_test).

binary() ->
  Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>,
  <<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = Pixels,
  <<R:8, G:8, B:8>> = <<Pix1:24>>,
  <<R:8, Rest/binary>> = Pixels,
  RGB = [ {R,G,B} || <<R:8,G:8,B:8>> <= Pixels ],
  A = 2#00010.

bc() ->
  Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>,
  RGB = [ {R,G,B} || <<R:8,G:8,B:8>> <= Pixels ],
  << <<R:8, G:8, B:8>> ||  {R,G,B} <- RGB >>.

triples_to_bin(T) ->
    triples_to_bin(T, <<>>).

triples_to_bin :: ([{Integer, Integer, Integer}], Binary) -> Binary;
                  ([], Binary) -> Binary.
triples_to_bin([{X,Y,Z} | T], Acc) ->
    triples_to_bin(T, <<Acc/binary,X:32,Y:32,Z:32>>);
triples_to_bin([], Acc) ->
    Acc.
