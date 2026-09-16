defmodule Tricorder.Analysis.Mix do
  alias Tricorder.Signatures

  def analyze(file, paths) do
    [{mod, _} | _] = Code.compile_file(file)
    Mix.ProjectStack.pop()

    name = mod.project()[:app] |> Atom.to_string()

    :code.delete(mod)
    :code.purge(mod)
    false = :code.is_loaded(mod)

    signature = Signatures.mix_escript(name, file)

    {:ok, {:completed, [signature]}}
  end
end
