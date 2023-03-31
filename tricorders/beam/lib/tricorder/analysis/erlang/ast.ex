defmodule Tricorder.Analysis.Erlang.Ast do
  require Logger

  alias Tricorder.Analysis.Erlang

  def parse(file, include_paths) do
    Logger.info("Parsing Erlang file: #{file}")
    {:ok, ast} = :epp.parse_file(:binary.bin_to_list(file), includes: include_paths)

    case has_include_errors?(ast) do
      {true, missing_includes} ->
        Logger.error("Error parsing Erlang file, missing includes: #{missing_includes}")
        {:missing_dependencies, %{includes: missing_includes}}

      false ->
        Logger.info("Parsed Erlang file: #{file}")
        {:ok, ast}
    end
  end

  def has_include_errors?(ast) do
    include_errors = missing_includes(ast)

    if Enum.empty?(include_errors) do
      false
    else
      {true, include_errors}
    end
  end

  def scan(file, ast) do
    current_mod = Erlang.module_name_for_file(file)

    %{
      includes: includes(file, ast),
      missing_includes: missing_includes(ast),
      parse_transforms: parse_transforms(ast),
      remote_functions: remote_functions(current_mod, ast),
      remote_types: remote_types(current_mod, ast),
      imported_mods: imported_mods(current_mod, ast)
    }
    |> Enum.map(fn {k, v} -> {k, v |> Enum.uniq() |> Enum.sort()} end)
    |> Enum.into(%{})
  end

  def includes(current_file, ast) do
    current_file = String.to_charlist(current_file)

    :erl_visitor.walk(
      ast,
      [],
      fn
        # NOTE(@ostera): .hrl trees include themselves!
        {:attribute, _, :file, {file, _}}, acc when file != current_file ->
          case Path.extname(file) do
            ".hrl" -> [file | acc]
            _ -> acc
          end

        {:attribute, _, :include, file}, acc ->
          [file | acc]

        {:attribute, _, :include_lib, file}, acc ->
          [file | acc]

        _, acc ->
          acc
      end
    )
    |> Enum.map(&ensure_string/1)
  end

  def missing_includes(ast) do
    :erl_visitor.walk(
      ast,
      [],
      fn
        {:error, {_, :epp, {:include, :file, file}}}, acc -> [file | acc]
        {:error, {_, :epp, {:include, :lib, file}}}, acc -> [file | acc]
        _, acc -> acc
      end
    )
    |> Enum.map(&ensure_string/1)
  end

  def remote_functions(current_mod, ast) do
    :erl_visitor.walk(
      ast,
      _acc = [],
      fn
        {:remote, _, {:atom, _, mod}, _}, acc when mod != current_mod -> [mod | acc]
        _, acc -> acc
      end
    )
  end

  def remote_types(current_mod, ast) do
    :erl_visitor.walk(
      ast,
      _acc = [],
      fn
        {:attribute, _, :type, {_, {:remote_type, _, [{:atom, _, mod}, _, _]}, _}}, acc
        when mod != current_mod ->
          [mod | acc]

        _, acc ->
          acc
      end
    )
  end

  defp imported_mods(current_mod, ast) do
    :erl_visitor.walk(
      ast,
      _Acc = [],
      fn
        {:attribute, _, :import, {mod, _}}, acc when mod != current_mod -> [mod | acc]
        _, acc -> acc
      end
    )
  end

  defp parse_transforms(ast) do
    :erl_visitor.walk(
      ast,
      _Acc = [],
      fn
        # NOTE(@ostera): sometimes the compile directive has a bunch of things, including the parse transforms
        {:attribute, _, :compile, args}, acc when is_list(args) ->
          case :proplists.lookup(:parse_transform, args) do
            :none -> acc
            {:parse_transform, mod} -> [mod | acc]
          end

        {:attribute, _, :compile, {:parse_transform, mod}}, acc ->
          [mod | acc]

        _, acc ->
          acc
      end
    )
  end

  defp ensure_string(bin) when is_binary(bin), do: bin
  defp ensure_string(bin) when is_list(bin), do: :binary.list_to_bin(bin)
end
