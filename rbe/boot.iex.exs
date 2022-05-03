require Logger

Logger.info("Starting server...")
Application.ensure_all_started(:http)
