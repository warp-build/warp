defmodule Tricorder.Deps.MixLock do
  def load(file) do
    with {conf, _} <- Code.eval_file(file) do
      conf |> Enum.map(&clean/1) |> Enum.into(%{})
    else
      _ -> %{}
    end
  end

  def clean({name, spec}), do: {name, Tricorder.Deps.Spec.parse(spec)}
end
