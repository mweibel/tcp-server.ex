defmodule TcpServer.TcpListener do
	use GenServer.Behaviour
	require Lager

	defrecord State, socket: nil

	def start_link do
		port = 5000
		Lager.info(%s(Starting Tcp Listener on port #{port}))
		:gen_server.start_link({ :local, :tcp_listener }, __MODULE__, [port], [])
	end

	def init([port]) do
		Process.flag(:trap_exit, true)
		# TODO: check if {packet, 2} is ok
		opts = [:binary, {:packet, 2}, {:reuseaddr, true},
				{:keepalive, true}, {:backlog, 30}, {:active, false}]
		case :gen_tcp.listen(port, opts) do
			{:ok, ref} ->
				# TODO: check if :prim_inet.async_accept() might be good here
				# see: https://erlangcentral.org/wiki/index.php/Building_a_Non-blocking_TCP_server_using_OTP_principles
				:gen_server.cast(self(), :accept)
				{:ok, State.new(socket: ref)}
			{:error, reason} ->
				{:stop, reason}
		end
	end

	def handle_call(request, _from, state) do
		{:stop, {:unknown_call, request}, state}
	end

	def handle_cast(:accept, state) do
		{:ok, accept_socket} = :gen_tcp.accept(state.socket)
		# FIXME: Why is it not possible to call by atom (:tcp_client_supervisor)?
		{:ok, pid} = TcpServer.TcpClientSupervisor.start_client()
		case :gen_tcp.controlling_process(accept_socket, pid) do
			:ok ->
				pid <- {:socket_ready, accept_socket}
				{:noreply, state}
			{:error, reason} ->
				Lager.error(%s(Error moving controlling process: #{reason}))
				{:stop, reason, state}
		end
	end

	def handle_cast(_msg, state) do
		{:noreply, state}
	end

	def handle_info(_info, state) do
		{:noreply, state}
	end

	def terminate(reason, state) do
		Lager.info("Terminating listener due to ~p", [reason])
		:gen_tcp.close(state.socket)
		:ok
	end

	def code_change(_old_version, state, _extra) do
		{:ok, state}
	end
end