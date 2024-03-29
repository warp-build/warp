defmodule Tricorder.Deps.RebarLock do
  def load(file) do
    with {:ok, [{'1.2.0', deps} | _]} <- :file.consult(file) do
      deps |> Enum.map(&clean/1) |> Enum.into(%{})
    else
      _ -> %{}
    end
  end

  def clean({name, spec, _}) do
    name = String.to_atom(name)
    {name, Tricorder.Deps.Spec.parse(name, spec)}
  end
end
