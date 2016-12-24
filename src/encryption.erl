-module(encryption).
-export([test/0,bin_enc/2,bin_dec/2,send_msg/4,get_msg/2,msg/1,id/1,encrypt/2,decrypt/2]).
-record(msg, {sig = "", msg = "", id = 0}).
-record(emsg, {key = "", msg = ""}).
msg(Msg) -> Msg#msg.msg.
id(Msg) -> Msg#msg.id.

%si(Key) -> crypto:stream_init(rc4, crypto:hmac(sha256, "", Key)).
si(Key) -> crypto:stream_init(aes_ctr, crypto:hmac(sha256, "", Key), <<0:128>>).%because each key is used for encryption at most once, we can use a static initialization vector.

%This version can be used exactly once with each private key.
bin_enc(Key, Bin) ->
    {_, X} = crypto:stream_encrypt(si(Key), Bin),
    X.
bin_dec(Key, Msg) ->
    {_, Y} = crypto:stream_decrypt(si(Key), Msg),
    Y.
sym_enc(Key, Msg) -> bin_enc(Key, term_to_binary(Msg)).
sym_dec(Key, Emsg) -> binary_to_term(bin_dec(Key, Emsg)).
%this version is good for a blockchain situation where you want to send a message to someone who's public key you know.
send_msg(M, ToPub, FromPub, FromPriv) -> 
    {EphPub, EphPriv} = sign:new_key(),
    Msg = #msg{sig=sign:sign(EphPub, FromPriv), msg=M, id = FromPub},
    SS = sign:shared_secret(ToPub, EphPriv),
    Emsg = sym_enc(SS, Msg),
    #emsg{key=EphPub, msg=base64:encode(Emsg)}.
get_msg(Msg, Priv) ->
    Sig = sym_dec(sign:shared_secret(Msg#emsg.key, Priv), base64:decode(Msg#emsg.msg)),
    true = sign:verify_sig(Msg#emsg.key, Sig#msg.sig, Sig#msg.id),
    Sig.
%This version can be used over and over to encrypt data with the same secret.
to_priv(X) ->
    base64:encode(crypto:hash(sha256, X)).
encrypt(File, Secret) ->
    Random = crypto:strong_rand_bytes(32),
    TempPriv = to_priv(Random),
    Priv = to_priv(Secret),
    {TempPub, TempPriv} = sign:new_key(TempPriv),
    {Pub, Priv} = sign:new_key(Priv),
    term_to_binary(send_msg(File, Pub, TempPub, TempPriv)).
decrypt(Encrypted, Secret) ->
    Priv = to_priv(Secret),
    (get_msg(binary_to_term(Encrypted), Priv))#msg.msg.
    
test() ->
    {Pub, Priv} = sign:new_key(),
    {Pub2, Priv2} = sign:new_key(),
    Val = <<"1234">>,
    Binary = <<2,3,4>>,
    true = bin_dec("abc", bin_enc("abc", Val)) == Val,
    true = bin_dec("abc", bin_enc("abc", Binary)) == Binary,
    Record = {f, Binary},
    Sig = get_msg(send_msg(Record, Pub2, Pub, Priv), Priv2),
    true = Sig#msg.msg == Record,
    Secret = <<1,5,3,7,4>>,
    Record = decrypt(encrypt(Record, Secret), Secret), 
    
    success.
