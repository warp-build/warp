defmodule Zap.Rbe.BuildQueue do

  require Logger
  use GenServer

  alias __MODULE__.State
  alias __MODULE__.Build

  def start_link(default) when is_list(default) do
    GenServer.start_link(__MODULE__, default)
  end

  def queue_build(pid, %Build{}=b), do: GenServer.call(pid, {:queue_build, b})

  def execute_next_build(pid), do: GenServer.cast(pid, :execute_next_build)

  @build_frequency 1_000

  @impl true
  def init(_args) do
    {:ok, timer} = :timer.send_interval(@build_frequency, self(), :pull)
    {:ok, State.new(timer)}
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
