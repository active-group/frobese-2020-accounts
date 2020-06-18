# Erlbank Monolithic

Erlbank Legacy System

## Build

```
$ rebar3 compile
```

## Run locally using rebar shell

The service can be run locally including a REPL using

```
$ rebar3 shell
```

The web-frontend is served at http://localhost:8000/

## how to get data (fast)

Name des gen_servers: {global, accounts}

ab jetzt alles: gen_server:cast({global, accounts}, {register, node(), self()})
alles bis jetzt: gen_server:cast({global, accounts}, {replay, node(), self()})

neuer account: {new, map()}
replay-antwort: {replay, list(map())}

map: #{account_number => account_number, amount => amount, firstname => firstname, surname => surname}
