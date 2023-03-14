defmodule Tricorder.Deps do
  alias __MODULE__

  require Logger

  defmodule Dep do
    defstruct [:name]
  end

  def scan(root) do
    Logger.info("Scanning #{root}")

    gitignore = Gitignore.read(root)

    (Gitignore.find(gitignore, root, "mix.lock", &Deps.MixLock.load/1) ++
       Gitignore.find(gitignore, root, "rebar.lock", &Deps.RebarLock.load/1))
    |> Enum.reduce(%{}, &Map.merge/2)
  end
end
