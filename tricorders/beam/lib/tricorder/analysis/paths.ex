defmodule Tricorder.Analysis.Paths do
  @moduledoc """

  Create the paths that will be used for preprocessing and compiling files on
  the BEAM. This typically includes `include` files, and paths to `.beam`
  files.

  """

  def build_paths(file, deps) do
    %{
      include_paths: include_paths(file, deps),
      code_paths: code_paths(file, deps)
    }
  end

  defp code_paths(file, deps) do
    deps
    |> Enum.flat_map(fn dep ->
      Enum.flat_map(dep.outputs, fn out ->
        full_path = Path.join(dep.store_path, out)

        if File.dir?(full_path) do
          [
            Path.join(full_path, "ebin"),
            Path.join(full_path, "src")
          ]
        else
          [Path.dirname(full_path)]
        end
      end)
    end)
    |> Enum.sort()
    |> Enum.uniq()
  end

  defp include_paths(file, deps) do
    Enum.flat_map(deps, fn dep ->
      Enum.flat_map(dep.outputs, fn out ->
        [
          Path.join([dep.store_path, out]) |> Path.dirname(),
          Path.join([dep.store_path, out, "include"])
        ]
      end)
    end)
    |> Enum.sort()
    |> Enum.uniq()
  end
end
