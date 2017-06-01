-module(sign).
-export([test/0,new_key/0,new_key/1,sign/2,verify_sig/3,shared_secret/2]).
en(X) -> base64:encode(X).
de(X) -> base64:decode(X).
params() -> crypto:ec_curve(secp256k1).
shared_secret(Pub, Priv) -> en(crypto:compute_key(ecdh, de(Pub), de(Priv), params())).
new_key() -> 
    {Pub, Priv} = crypto:generate_key(ecdh, params()),
    %crypto:generate_key(ecdh, crypto:ec_curve(secp256k1)) 
    {en(Pub), en(Priv)}.
new_key(P) ->
   {Pub, Priv} = crypto:generate_key(ecdh, params(), de(P)),
    {en(Pub), en(Priv)}.
    
sign(S, Priv) -> en(crypto:sign(ecdsa, sha256, term_to_binary(S), [de(Priv), params()])).
verify_sig(S, Sig, Pub) -> 
    SD = de(Sig),
    PD = de(Pub),
    crypto:verify(ecdsa, sha256, term_to_binary(S), SD, [PD, params()]).

test() ->
    {Pub, Priv} = new_key(),
    {Pub2, Priv2} = new_key(),
    S = <<"abc">>,
    verify_sig(S, sign(S, Priv), Pub),
    SS = shared_secret(Pub, Priv2),
    SS = shared_secret(Pub2, Priv),
    success.
