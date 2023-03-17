defmodule Tricorder.Deps.Server do
  use GenServer

  def start_link(state) do
    GenServer.start_link(__MODULE__, state, name: __MODULE__)
  end

  ## Callbacks

  @impl true
  def init(root) do
    gitignore = Gitignore.read(root)

    {:ok,
     %{
       root: root,
       gitignore: gitignore,
       deps: %{},
       finish_scanning: false
     }}
  end

  @impl true
  def handle_call(:loaded?, _from, state) do
    {:reply, state.finish_scanning, state}
  end

  @impl true
  def handle_call(:get_all, _from, state) do
    {:reply, state.deps, state}
  end

  @impl true
  def handle_call({:get, name}, _from, state) do
    res =
      case state.deps[name] do
        nil -> {:error, :not_found}
        dep -> {:ok, dep}
      end

    {:reply, res, state}
  end

  @impl true
  def handle_cast(:scan, state) do
    deps =
      Gitignore.find(
        state.gitignore,
        state.root,
        fn file ->
          case Path.basename(file) do
            "mix.lock" -> {:keep, Tricorder.Deps.MixLock.load(file)}
            "rebar.lock" -> {:keep, Tricorder.Deps.RebarLock.load(file)}
            "rebar.config" -> {:keep, Tricorder.Deps.RebarConfig.load(file)}
            _ -> :skip
          end
        end
      )
      |> Enum.reduce(state.deps, &Map.merge/2)

    state = %{state | deps: deps, finish_scanning: true}

    {:noreply, state}
  end

  @impl true
  def handle_cast(_msg, state), do: {:noreply, state}
end
