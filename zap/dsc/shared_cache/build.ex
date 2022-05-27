defmodule Zap.Rbe.BuildQueue.Build do
  defstruct [
    :uri,
    :source
  ]

  def new, do: %__MODULE__{
    uri: "zap:build:#{UUID.uuid4()}",
  }
end
