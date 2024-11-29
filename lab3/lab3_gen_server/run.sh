clear
rebar3 compile
cd _build/default/lib/lab3_gen_server/ebin
erl -noshell -s lab3_gen_server_app start -freq 0.5 -w 4 -methods linear lagrange -s init stop
rm -rf erl_crash.dump