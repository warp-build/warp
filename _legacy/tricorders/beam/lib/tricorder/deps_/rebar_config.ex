defmodule Tricorder.Deps.RebarConfig do
  def load(file) do
    with {:ok, conf} <- :file.consult(file) do
      conf = conf |> :proplists.to_map()

      all_deps = profile_deps(conf) ++ deps(conf)

      all_deps |> Enum.map(&clean/1) |> Enum.into(%{})
    else
      _ -> %{}
    end
  end

  defp clean(name) when is_atom(name), do: {name, Tricorder.Deps.Spec.parse(name)}
  defp clean({name, spec}), do: {name, Tricorder.Deps.Spec.parse(name, spec)}
  defp clean({name, _, spec}), do: {name, Tricorder.Deps.Spec.parse(name, spec)}

  defp deps(conf) do
    conf |> Map.get(:deps, [])
  end

  defp profile_deps(conf) do
    profiles = conf |> Map.get(:profiles, [])

    profiles
    |> Enum.flat_map(fn {_name, profile} ->
      profile = profile |> :proplists.to_map()
      Map.get(profile, :deps, [])
    end)
  end
end
