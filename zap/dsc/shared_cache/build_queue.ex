defmodule Zap.Dsc.SharedCache do

  require Logger
  use GenServer

  alias __MODULE__.State
  alias __MODULE__.Build

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def put(pid, %Build{}=b), do: GenServer.call(pid, {:queue_build, b})

  def get(pid), do: GenServer.call(pid, :execute_next_build)

  @build_frequency 1_000

  @impl true
  def init(args) do
    {:ok, %{ impl: args[:impl] }}
  end

  @impl true
  def handle_info(:pull, state) do
    Logger.info("Pulling from build queue...")
    execute_next_build(self())
    {:noreply, state}
  end

  @impl true
  def handle_call({:queue_build, build}, _from, state) do
    {:ok, id, state} = State.queue_build(state, build)
    {:reply, {:ok, id}, state}
  end

  @impl true
  def handle_cast(:execute_next_build, state) do
    {:ok, state} = do_execute_next_build(state)
    {:noreply, state}
  end


  def do_execute_next_build(state) do
    {:ok, opt_build, state} = State.next_build(state)
    case opt_build do
      :empty -> {:ok, state}
      %Build{} = build ->
        case Zap.Rbe.BuildExecutor.execute(build) do
          {:ok, result} ->
            {:ok, state} = State.save_result(state, build, result)
            {:ok, result, state}
          error -> error
        end
    end
  end
end
