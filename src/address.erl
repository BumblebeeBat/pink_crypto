-module(address).
-export([pub2addr/2, test/0]).

-define(weight_bits, 64).

pub2addr(L, M) ->
    pub2addr2(order(L), <<M:?weight_bits>>).
order(L) -> %this is merge sort
    M = column(L),%put everything into 1-lists.
    order2(M).
column([]) -> [];
column([H|T]) -> [[H]|column(T)].
order2([X]) -> X; %keep doing it until there is only 1 left
order2([H|T]) -> 
    order2(order3([H|T])).
order3([A|[B|T]]) -> %merge pairs
    [merge(A, B)|order3(T)];
order3([]) -> [];
order3([X]) -> [X].
merge([], X) -> X; %merge pair
merge(X, []) -> X;
merge([F1|B], [F2|D]) -> 
    {<<A1:96, _/binary>>, _} = F1,
    {<<A2:96, _/binary>>, _} = F2,
    if
	A1 > A2 -> [F1|merge(B, [F2|D])];
	A2 > A1 -> [F2|merge([F1|B], D)]
    end.
    
pub2addr2([], B) -> hash:doit(B);
pub2addr2([{Pub, Weight}|T], B) ->%This is slow for big addresses. 
    pub2addr2(T, <<Pub/binary, Weight:?weight_bits, B/binary>>).
verify_signature(Addr, PW, Min, Tx, L) ->
    PW2 = order(PW),
    Addr = pub2addr2(PW2, <<Min:?weight_bits>>),
    verify2(PW2, order(L), Tx, Min, 0).
verify2([], [], _, A, B) when B =< A ->
    false;
verify2([], [], _, A, B) when B > A ->
    true;
verify2([{Pub, Weight}|T], [{Pub, Signature}|S], Tx, Min, Sum) ->
    B = sign:verify_sig(Tx, Signature, Pub),
    W = if
	    B -> Weight;
	    true -> 0
	end,
    verify2(T, S, Tx, Min, Sum+W);
verify2(_,_,_,_,_) ->
    {error, "address verify2"}.

    
test() ->
    {Pub1, Priv1} = sign:new_key(),
    {Pub2, _Priv2} = sign:new_key(),
    {Pub3, Priv3} = sign:new_key(),
    {Pub4, _Priv4} = sign:new_key(),
    A = {Pub1, 10},
    B = {Pub2, 5},
    C = {Pub3, 15},
    D = {Pub4, 3},
    PW = [A, B, C, D],
    PW2 = [D, C, A, B],
    Min = 20,
    Addr = pub2addr(PW, Min),
    Addr = pub2addr(PW2, Min),
    Tx = <<1,5,3,4>>,
    L = [{Pub1, sign:sign(Tx, Priv1)}, {Pub3, sign:sign(Tx, Priv3)}, {Pub4, <<>>}, {Pub2, <<>>}],
    true = verify_signature(Addr, PW, Min, Tx, L),
    true = verify_signature(Addr, PW2, Min, Tx, L),
    true = verify_signature(Addr, PW2, Min, Tx, lists:reverse(L)),
    success.
    
    
