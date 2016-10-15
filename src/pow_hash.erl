-module(pow_hash).
-export([doit/1,test/0,hash/1]).

hash(S) -> 
    crypto:hmac(sha256, S, "").
doit(S) when is_binary(S) -> hash(S);
doit(S) -> hash(term_to_binary(S)).
-record(p, {p = ""}).
test() -> 
    doit(123),
    doit(abc),
    doit([123]),
    doit([[[]]]),
    doit(#p{}),
    success.
