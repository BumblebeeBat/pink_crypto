-module(hash).
-export([doit/1, doit/2, bin_to_hex/1]).

doit(S) ->
    doit(S, 32).
doit(S, Size) when not(is_binary(S)) -> 
    doit(sign:serialize(S), Size);
doit(S, Size) -> 
    HD = Size * 8,
    <<X:HD, _/bitstring>> = crypto:hash(sha256, S),%crypto:hmac(sha256, S, ""),
    <<X:HD>>.
bin_to_hex(<<>>) -> "";
bin_to_hex(<<A, B/binary>>) ->
    byte_to_hex(<<A>>) ++ bin_to_hex(B).
byte_to_hex(<< N1:4, N2:4 >>) ->
    [erlang:integer_to_list(N1, 16), erlang:integer_to_list(N2, 16)].
    
