require Logger

["start", port] = System.argv()

{:ok, apps} = Application.ensure_all_started(:analyzer_service)

port = String.to_integer(port)
{:ok, pid, ^port} = GRPC.Server.start_endpoint(Analyzer.Endpoint, port, [])

Logger.info("Started gRPC server on http://0.0.0.0:#{port}")

System.no_halt(true)

