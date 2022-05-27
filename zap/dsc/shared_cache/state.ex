defmodule Zap.Rbe.BuildQueue.State do
  defstruct [ :queue, :timer, :results ]

  def new(timer), do: %__MODULE__{ queue: :queue.new(), timer: timer, results: %{} }

  def queue_build(t, build) do
    t = %__MODULE__{ t | queue: :queue.in(t.queue, build) }
    {:ok, t}
  end

  def next_build(t) do
    {item, queue} = :queue.out(t.queue)
    t = %__MODULE__{ t | queue: queue }
    {:ok, item, t}
  end

  def save_result(t, build, result) do
    results = Map.put(t.results, build.uri, result)
    t = %__MODULE__{ t | results: results }
    {:ok, t}
  end
end
