defmodule Analyzer.Server do
  use GRPC.Server, service: Build.Warp.Codedb.AnalyzerService.Service

  require Logger

  def get_interested_extensions(request, _stream) do
    ext = [".hrl", ".erl", ".ex", ".exs", ".config", ".eex"]
    Build.Warp.Codedb.GetInterestedExtensionsResponse.new(ext: ext)
  end

  def analyze_file(req, stream)  do
    ext = Path.extname(req.file)
    do_analyze_file(ext, req)
  end

  defp do_analyze_file(".erl", req), do: run_analysis(req)
  defp do_analyze_file(".hrl", req), do: run_analysis(req)
  defp do_analyze_file(_, req), do: Build.Warp.Codedb.AnalyzeFileResponse.new(skipped: true, file: req.file)

  defp run_analysis(req) do
    Logger.info("Analyzing #{req.file}")

    file = req.file
    {:ok, %{ ^file => result }} = :erl_analyzer.analyze([file], _ModMap=%{}, _IncludePaths=[])

    Build.Warp.Codedb.AnalyzeFileResponse.new(
      file: req.file,
      skipped: false,
    )
  end
end
