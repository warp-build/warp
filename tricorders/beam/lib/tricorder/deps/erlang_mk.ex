defmodule Tricorder.Deps.ErlangMk do
  def load(file) do
    with {:ok, conf} <- :file.consult(file),
         conf <- :proplists.to_map(conf) do
      deps(conf)
    else
      _ -> []
    end
  end

  def deps(conf) do
    []
  end
end
