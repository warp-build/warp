defmodule Tricorder.Analysis.Paths do
  @moduledoc """

  Create the paths that will be used for preprocessing and compiling files on
  the BEAM. This typically includes `include` files, and paths to `.beam`
  files.

  """

  def build_paths(file, store_paths) do
    include_paths = include_paths(file, store_paths)

    code_paths = store_paths |> Enum.map(&Path.dirname/1) |> Enum.uniq()

    %{include_paths: include_paths, code_paths: code_paths}
  end

  defp include_paths(file, dependencies) do
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

    include_paths
  end
end
