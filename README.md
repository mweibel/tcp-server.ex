# tcp-server.ex

## Purpose
Elixir TCP Server based on OTP principles, guided by the following URLS:

  - [Building a Non-blocking TCP server using OTP Principles - Erlang Central](https://erlangcentral.org/wiki/index.php/Building_a_Non-blocking_TCP_server_using_OTP_principles)
  - [Learn you some erlang - Sockserv supervisor](http://learnyousomeerlang.com/static/erlang/processquest/apps/sockserv-1.0.1/src/sockserv_sup.erl)
  - [Learn you some erlang - Sockserv server](http://learnyousomeerlang.com/static/erlang/processquest/apps/sockserv-1.0.1/src/sockserv_serv.erl)

I built this TCP server in order to learn elixir & learn how to build..a TCP server in elixir. ;)
Therefore, take the code with a grain of salt & **if you have some suggestions, please tell me**. I'm eager to learn more.

Also: you might **not** want to **run it in production**. There are better options like [ranch](https://github.com/extend/ranch).

## What it does
Opening a TCP server on port 5000 (configurable through `config/app.config`) and replying
with the same string as you sent with e.g. `telnet localhost 5000`.

## Installation
1. install elixir (OSX: `brew install elixir`)
2. clone repo: `git clone git@github.com:mweibel/tcp-server.ex.git`
3. get dependencies: `mix get.deps`
4. run: `iex --erl "-config config/app.config" -S mix run`

## License
MIT