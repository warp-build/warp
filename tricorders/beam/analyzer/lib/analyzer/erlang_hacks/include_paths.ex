defmodule Analyzer.ErlangHacks.IncludePaths do
  @moduledoc """

  This is a hack to make the source anlysis find headers cheaply.

  """
  require Logger

  def from_file_and_deps(file, dependencies) do
    # NOTE(@ostera): we don't want to discover things on the FS root
    parts =
      case file |> Path.dirname() |> Path.split() do
        ["/" | parts] -> parts
        parts -> parts
      end

    include_paths =
      [
        "include",
        Enum.map(dependencies, fn dep ->
          [
            Path.dirname(Path.dirname(dep.store_path)),
            Path.dirname(dep.store_path)
          ]
        end),
        for i <- 0..(Enum.count(parts) - 1) do
          path =
            case Enum.take(parts, i) do
              [] -> "."
              parts -> parts |> Path.join()
            end

          [path, Path.join(path, "include")]
        end
      ]
      |> List.flatten()
      |> Enum.sort()
      |> Enum.uniq()

    Logger.info("splitting ast using include paths:")

    for p <- include_paths do
      IO.inspect(p)
    end

    include_paths
  end
end
