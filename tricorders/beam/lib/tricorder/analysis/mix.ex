defmodule Tricorder.Analysis.Mix do
  def analyze(file, paths) do
    [{mod, _}] = Code.compile_file(file)
    Mix.ProjectStack.pop()

    name = mod.project()[:app] |> Atom.to_string()

    :code.delete(mod)
    :code.purge(mod)
    false = :code.is_loaded(mod)

    {:ok, {:escript, name}}
  end
end
