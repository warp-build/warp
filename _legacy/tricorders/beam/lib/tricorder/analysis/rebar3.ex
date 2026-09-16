defmodule Tricorder.Analysis.Rebar3 do
  alias Tricorder.Signatures

  def analyze(file, paths) do
    {:ok, conf} = :file.consult(file)

    name = file |> Path.dirname() |> Path.basename()

    signature = Signatures.rebar3_library(name, file)

    {:ok, {:completed, [signature]}}
  end
end
