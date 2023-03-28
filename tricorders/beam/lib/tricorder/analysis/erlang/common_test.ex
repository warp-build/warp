defmodule Tricorder.Analysis.Erlang.CommonTest do
  require Logger
  require Tricorder.Analysis.Erlang.Cerl

  alias Tricorder.Signatures

  def suites(file, test_matcher, modules, includes, src_analysis) do
    if String.ends_with?(file, "_SUITE.erl") do
      {:ok, mod} = :compile.noenv_file(:binary.bin_to_list(file), @default_compile_opts)
      cases = mod.all() || []

      groups =
        try do
          mod.groups()
        rescue
          _ -> []
        end

      cases = flatten(cases, groups, [])

      signatures = extract_suites(file, test_matcher, cases, modules, includes)

      :code.delete(mod)
      :code.purge(mod)
      false = :code.is_loaded(mod)

      Logger.info("Found #{signatures |> Enum.count()} tests")
      IO.inspect(signatures)

      signatures
    else
      []
    end
  end

  def extract_suites(file, :all, cases, modules, includes) do
    for case_name <- cases do
      Signatures.erlang_test(file, case_name, modules, includes)
    end
  end

  def extract_suites(file, {:match, name}, cases, modules, includes) do
    for case_name <- cases do
      if String.starts_with?(case_name, name) do
        Signatures.erlang_test(file, case_name, modules, includes)
      else
        []
      end
    end
    |> List.flatten()
  end

  def flatten([], _groups, acc), do: acc

  def flatten([{:group, name, opts} | cases], groups, acc) do
    tests =
      groups
      |> Enum.filter(fn {group_name, _opts, _tests} -> group_name == name end)
      |> Enum.flat_map(fn {_group_name, _opts, tests} -> tests end)
      |> Enum.map(fn test -> "#{name}:#{test}" end)

    flatten(cases, groups, tests ++ acc)
  end

  def flatten([case_ | cases], groups, acc) when is_atom(case_) do
    flatten(cases, groups, [Atom.to_string(case_) | acc])
  end
end
