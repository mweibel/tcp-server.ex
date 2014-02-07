defmodule TcpServer.TcpClientSupervisor do
	use Supervisor.Behaviour
	require Lager

	def start_link do
		:supervisor.start_link({:local, :tcp_client_supervisor}, __MODULE__, [])
	end

	def init([]) do
		# FIXME: I guess this will result in always having at least one child
		# which is not used at all. Supervise with [] doesn't work though.
		supervise [new_client], strategy: :simple_one_for_one
	end

	def start_client do
		# FIXME: This currently does not work
		case :supervisor.start_child(:tcp_client_supervisor, new_client) do
			{:error, reason} ->
				Lager.error("Error creating client FSM: ~p", [reason])
				{:error, reason}
			{:ok, child} ->
				Lager.info("Created child: ~p", [child])
				{:ok, child}
			{:ok, child, info} ->
				Lager.info("Info on creating client FSM: ~p", [info])
				{:ok, child}
		end
	end

	defp new_client do
		worker(TcpServer.TcpClientFsm, [], id: :unknown, restart: :temporary, shutdown: 2000)
	end
end
