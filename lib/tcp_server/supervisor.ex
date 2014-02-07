defmodule TcpServer.Supervisor do
	use Supervisor.Behaviour
	require Lager

	def start_link do
		Lager.info("Starting supervisor")
		:supervisor.start_link(__MODULE__, [])
	end

	def init([]) do
		children = [
			# Define workers and child supervisors to be supervised
			worker(TcpServer.TcpListener, []),
			supervisor(TcpServer.TcpClientSupervisor, [])
		]

		# See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
		# for other strategies and supported options
		supervise children, strategy: :one_for_one
	end
end
