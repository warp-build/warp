defmodule Analyzer.Server do
  use GRPC.Server, service: Build.Warp.Codedb.AnalyzerService.Service

  require Logger

  defdelegate generate_signature(_request, _stream), to: Analyzer.GenerateSignature
  defdelegate get_ast(_request, _stream), to: Analyzer.GetAst
  defdelegate get_dependencies(_request, _stream), to: Analyzer.GetDependencies
  defdelegate get_interested_paths(_request, _stream), to: Analyzer.GetInterestedPaths
  defdelegate get_provided_symbols(_request, _stream), to: Analyzer.GetProvidedSymbols
end
