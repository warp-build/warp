defmodule Analyzer.GetInterestedPaths do
  require Logger

  def get_interested_paths(req, _stream) do
    Build.Warp.Codedb.GetInterestedPathsResponse.new(
      build_files: [
        ".eex",
        ".erl",
        ".ex",
        ".exs",
        ".hrl",
        "rebar.config",
        "rebar.lock",
        "mix.lock",
        "mix.exs"
      ],
      test_files: ["*_SUITE.erl", "prop_*.erl"]
    )
  end
end
