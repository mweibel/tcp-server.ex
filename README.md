tcp-server.ex
=============

Elixir TCP Server based on OTP principles

**Note**: Currently doesn't work. Crashes with the following error when doing a `telnet localhost 5000`:

```
[error] Error creating client FSM: {'EXIT',{badarg,[{erlang,apply,['Elixir.TcpServer.TcpClientFsm',start_link,{unknown,{'Elixir.TcpServer.TcpClientFsm',start_link,[]},temporary,2000,worker,['Elixir.TcpServer.TcpClientFsm']}],[]},{supervisor,do_start_child_i,3,[{file,"supervisor.erl"},{line,326}]},{supervisor,handle_call,3,[{file,"supervisor.erl"},{line,351}]},{gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,585}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,239}]}]}}
[info] Terminating listener due to {{badmatch,{error,{'EXIT',{badarg,[{erlang,apply,['Elixir.TcpServer.TcpClientFsm',start_link,{unknown,{'Elixir.TcpServer.TcpClientFsm',start_link,[]},temporary,2000,worker,['Elixir.TcpServer.TcpClientFsm']}],[]},{supervisor,do_start_child_i,3,[{file,"supervisor.erl"},{line,326}]},{supervisor,handle_call,3,[{file,"supervisor.erl"},{line,351}]},{gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,585}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,239}]}]}}}},[{'Elixir.TcpServer.TcpListener',handle_cast,2,[{file,"lib/tcp_server/tcp/listener.ex"},{line,36}]},{gen_server,handle_msg,5,[{file,"gen_server.erl"},{line,604}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,239}]}]}
```