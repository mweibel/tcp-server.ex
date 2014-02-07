defmodule TcpServer.TcpClientFsm do
	use GenFSM.Behaviour
	require Lager

	defrecord State, socket: nil, ip: nil

	@timeout 120000

	def start_link do
		:gen_fsm.start_link(__MODULE__, [], [])
	end

	def init(_args) do
		Process.flag(:trap_exit, true)
		{:ok, :wait_for_socket, State.new()}
	end

	def set_socket(pid, socket) when is_pid(pid) and is_port(socket) do
		:gen_fsm.send_event(pid, {:socket_ready, socket})
	end

	def wait_for_socket({:socket_ready, socket}, state) when is_port(socket) do
		Lager.info("Socket ready: ~p", [socket])
		:inet.setopts(socket, [{:active, :once}, {:packet, 2}, :binary])
		{:ok, {ip, _port}} = :inet.peername(socket)
		{:next_state, :wait_for_data, state.update(socket: socket, ip: ip), @timeout}
	end

	def wait_for_socket(other, state) do
		Lager.error(%s(State 'wait_for_socket'. Unexpected message #{other}))
		{:next_state, :wait_for_socket, state}
	end

	def wait_for_data({:data, data}, State[socket: socket] = state) do
		Lager.info("Got data ~p in state: ~p", [data, state])
		:ok = :gen_tcp.send(socket, data)
		{:next_state, :wait_for_data, state, @timeout}
	end

	def wait_for_data(:timeout, state) do
		Lager.error(%s(#{self()} Client connection timeout - closing.))
		{:stop, :normal, state}
	end

	def wait_for_data(data, state) do
		Lager.info(%s(#{self()} ignoring data: #{data}))
		{:next_state, :wait_for_data, state, @timeout}
	end

	def handle_info({:tcp, socket, data}, state_name, State[socket: socket] = state) do
		:inet.setopts(socket, [{:active, :once}])
		Lager.info("Got tcp data ~p, state_name: ~p, state: ~p", [data, state_name, state])
		apply(self(), state_name, [{:data, data}, state])
	end

	def handle_info({:tcp_closed, socket}, _state_name, State[socket: socket, ip: ip] = state) do
		Lager.info(%s(#{self()} Client #{ip} disconnected.))
		{:stop, :normal, state}
	end

	def handle_info(info, state_name, state) do
		{:noreply, state_name, state}
	end

	def terminate(_reason, _state_name, State[socket: socket] = state) do
		Lager.info("Current state: ~p", [state])
		:ok = :gen_tcp.close(socket)
		:ok
	end
end