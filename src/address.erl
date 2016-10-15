-module(address).
-export([pub2addr/2, test/0]).

-define(weight_bits, 64).

pub2addr(L, M) ->
    pub2addr2(order(L), <<M:?weight_bits>>).
column([]) -> [];
column([H|T]) -> [[H]|column(T)].
order(L) ->
    M = column(L),
    order2(M).
order2([X]) -> X;
order2([H|T]) -> 
    order2(order3([H|T])).
order3([A|[B|T]]) ->
    [merge(A, B)|order3(T)];
order3([]) -> [];
order3([X]) -> [X].
merge([], X) -> X;
merge(X, []) -> X;
merge([F1|B], [F2|D]) -> 
    {P1, _} = F1,
    {P2, _} = F2,
    <<A1:96, _/binary>> = P1,
    <<A2:96, _/binary>> = P2,
    if
	A1 > A2 -> [F1|merge(B, [F2|D])];
	A2 > A1 -> [F2|merge([F1|B], D)]
    end.
    
pub2addr2([], B) -> hash:doit(B);
pub2addr2([{Pub, Weight}|T], B) ->%This is slow for big addresses. 
    pub2addr2(T, <<Pub/binary, Weight:?weight_bits, B/binary>>).
verify_signatures(Addr, PW, Min, Tx, L) ->
    Addr = pub2addr(PW, Min),
    verify2(PW, L, Tx, Min, 0).
verify2([], [], _, A, B) when B =< A ->
    false;
verify2([], [], _, A, B) when B > A ->
    true;
verify2([{Pub, Weight}|T], [Signature|S], Tx, Min, Sum) ->
    if
	Signature == 0 -> io:fwrite("zero\n");
	true -> ok
    end,
    B = sign:verify_sig(Tx, Signature, Pub),
    W = if
	    B -> Weight;
	    true -> 0
	end,
    verify2(T, S, Tx, Min, Sum+W);
verify2(_,_,_,_,_) ->
    {error, "signatures element incorrect length"}.

    
test() ->
    {Pub1, Priv1} = sign:new_key(),
    {Pub2, Priv2} = sign:new_key(),
    {Pub3, Priv3} = sign:new_key(),
    {Pub4, Priv4} = sign:new_key(),
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
    L = [sign:sign(Tx, Priv1), <<>>, sign:sign(Tx, Priv3), <<>>],
    true = verify_signatures(Addr, PW, Min, Tx, L),
    success.
    
    
