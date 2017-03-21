-module(hash).
-export([doit/2]).

doit(S, Size) when not(is_binary(S)) -> 
    doit(term_to_binary(S), Size);
doit(S, Size) -> 
    HD = Size * 8,
    <<X:HD, _/bitstring>> = crypto:hmac(sha256, S, ""),
    <<X:HD>>.
    
