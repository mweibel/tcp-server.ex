defmodule TcpServer do
	use Application.Behaviour
	require Lager

	# See http://elixir-lang.org/docs/stable/Application.Behaviour.html
	# for more information on OTP Applications
	def start(_type, _args) do
		Lager.info("Starting tcp server")
		TcpServer.Supervisor.start_link
	end
end
