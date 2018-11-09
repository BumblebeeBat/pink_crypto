-module(elliptic).
-export([test/0, test2/0,test4/0,
pedersen/2,
add/2, make_point/2, multiply/2, hex2int/1, base/0, random_int/0]).

%Y^2 = X^3 + 7.

% the prime p that defines the group is "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"

%The base point G in compressed form is:

%    G = 02 79BE667E F9DCBBAC 55A06295 CE870B07 029BFCDB 2DCE28D9 59F2815B 16F81798

%and in uncompressed form is:

%    G = 04 79BE667E F9DCBBAC 55A06295 CE870B07 029BFCDB 2DCE28D9 59F2815B 16F81798 483ADA77 26A3C465 5DA4FBFC 0E1108A8 FD17B448 A6855419 9C47D08F FB10D4B8

% A = "79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798" 
% = 55066263022277343669578718895168534326250603453777594175500187360389116729240
% B = "483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"
% = 32670510020758816978083085130507043184471273380659243275938904335757337482424
% N (order) = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"
% = 115792089237316195423570985008687907852837564279074904382605163141518161494337

%(Xp, Yp) + (Xq, Yq) = (Xr, Yr)
%L = (Yq - Yp) / (Xq - Xp) %L is the slope of the line
%Xr = L^2 - Xp - Xq
%Yr = L(Xp - Xr) - Yp

%If Xp == Xq and Yp == Yq
%L = (3*(Xp)^2 + a) / (2*Yp) % a is zero for bitcoin curve.

-record(point, {x, y}).
-define(Base, #point{x = 55066263022277343669578718895168534326250603453777594175500187360389116729240, 
		     y = 32670510020758816978083085130507043184471273380659243275938904335757337482424}).
-define(p, 115792089237316195423570985008687907853269984665640564039457584007908834671663).
-define(n, 115792089237316195423570985008687907852837564279074904382605163141518161494337).
base() -> ?Base.
hex2int(N) -> hex2int(N, 0).
hex2int([], N) -> N;
hex2int([H|T], N) -> 
    A = case H<58 of
	    true -> H-48;
	    false -> H-55
	end,
    N2 = (N*16) + A,
    hex2int(T, N2).
make_point(A, B) ->
    #point{x = A, y = B}.

multiply(X, N) when N < 0 -> 
    io:fwrite("mul flip\n"),
    %1=2,
    %N2 = ?p + N,
    %N2 = inverse(-N),
    multiply(X, N + ?p);
multiply(X, 1) -> X;
multiply(P, N) when (N rem 2) == 0 ->
    multiply(add(P, P), N div 2);
multiply(P, N) ->
    add(P, multiply(P, N-1)).
remm(A) -> (((A rem ?p) + ?p) rem ?p).
add(P, Q) ->
    L0 = if
	    (P#point.x == Q#point.x) and
	    (P#point.y == Q#point.y) ->
		(3*P#point.x*P#point.x) * inverse(2*P#point.y);
	    true -> 
		(Q#point.y - P#point.y) * 
		    inverse(Q#point.x - P#point.x)
	end,
    L = remm(L0),
    X = remm((L*L)-P#point.x-Q#point.x),
    Y = remm((L*(P#point.x - X)) - P#point.y),
    make_point(X, Y).
pow(_, 0) -> 1;
pow(A, 1) -> A;
pow(A, B) when (B rem 2) == 0 ->
    pow(A*A, B div 2);
pow(A, B) -> A*pow(A, B-1).
powrem(_, 0) -> 1;
powrem(A, 1) -> A rem ?p;
powrem(A, B) when (B rem 2) == 0 -> 
    powrem(A*A rem ?p, B div 2);
powrem(A, B) -> A*powrem(A, B-1) rem ?p.
inverse(A) ->
    powrem(A, ?p-2).


-define(Base2, {point,71512103163200868719313266481290892478515614256198246112525499383908891547715,
       65334305839683166714752726047646676124964098163470154901472866527750635165535}).

pedersen(M, R) ->
    add(multiply(?Base, R), multiply(?Base2, M)).


test() ->
    One = inverse(5) * 5 rem ?p,
    One = 1,
    N = hex2int("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"),
    N = ?n,
    P = hex2int("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"),
    P = pow(2, 256) - 
	pow(2, 32) -
	pow(2, 9) -
	pow(2, 8) -
	pow(2, 7) -
	pow(2, 6) -
	pow(2, 4) - 
	1,
    P = ?p, %this shows that the hex2int we used to calculate the prime works.
    X = ?Base#point.x,
    X = hex2int("79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"),
    Y = ?Base#point.y,
    Y = hex2int("483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"),
    Two = add(?Base, ?Base),
    Three = add(?Base, Two),
    Three = add(Two, ?Base),
    Four = add(Three, ?Base),
    Four = add(?Base, Three),
    Four = add(Two, Two),
    Two = multiply(?Base, 2),
    Three = multiply(?Base, 3),
    Four = multiply(?Base, 4),
    Base = ?Base,
    Base = multiply(?Base, ?n+1),
    Base = multiply(?Base, (?n+1)*(?n+1)),
    test2().
random_int() ->
    <<Random:264>> = crypto:strong_rand_bytes(33),
    Random rem ?p.
    
test2() ->
    Zero = add(?Base, multiply(?Base, -1)),
    Zero = add(multiply(?Base, 2), multiply(?Base, -2)),
    None = add(multiply(?Base, 2), multiply(?Base, -3)),
    None = add(multiply(?Base, 3), multiply(?Base, -4)),
    test3().
test3() -> 
    R = random_int(),
    R2 = random_int(),
    A = pedersen(5, R),
    B = pedersen(5, R2),
    C = pedersen(10, (R + R2)),
    C = add(A, B),
    D = pedersen(5, 101),
    E = pedersen(6, 1000),
    F = pedersen(11, 1101),
    G = add(D, E),
    %io:fwrite(F, G).
    G = F,
    test4().
    %G.
 
test4() ->
    Q = pedersen(1, 1),
    Q2 = pedersen(1, ?n+1 ),
    Q3 = pedersen(2, ?n + 2),
    Q3 = add(Q,Q2),
    Q = Q2,

    R = random_int(),
    R2 = random_int(),
    DB = dict:new(),
    A = pedersen(100, R),
    %alice has a account with 100, she wants to send 50 to bob's new account.
    B2 = pedersen(50, (R - R2 + ?n) rem ?n),
    B = pedersen(50, R2),
    A = add(B, B2),
    %the sum of the 2 new commitments is equal to the sum of the 2 new commitments.
    success.
     
    
    
