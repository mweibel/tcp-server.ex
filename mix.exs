defmodule TcpServer.Mixfile do
	use Mix.Project

	def project do
		[ app: :tcp_server,
			version: "0.0.1",
			elixir: "~> 0.12.2",
			deps: deps ]
	end

	# Configuration for the OTP application
	def application do
		[
			registered: [:tcp_server, :tcp_listener, :tcp_client_fsm, :tcp_client_supervisor],
			applications: [:exlager],
			mod: { TcpServer, [] }
		]
	end

	# Returns the list of dependencies in the format:
	# { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
	#
	# To specify particular versions, regardless of the tag, do:
	# { :barbat, "~> 0.1", github: "elixir-lang/barbat" }
	defp deps do
		[
			{ :exlager, "~>0.2.1", github: "khia/exlager" }
		]
	end
end
