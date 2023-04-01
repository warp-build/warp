defmodule Tricorder.Analysis.Erlang.CommonTest do
  require Logger
  require Tricorder.Analysis.Erlang.Cerl

  alias Tricorder.Analysis.Erlang
  alias Tricorder.Analysis.TestMatcher
  alias Tricorder.Signatures

  def is_test_suite?(file), do: String.ends_with?(file, "_SUITE.erl")

  def suites(file, test_matcher, modules, includes, paths) do
    with {:ok, mod, bytecode, _core} <- Erlang.Cerl.compile(file, paths) do
      Logger.info("Extracting CommonTest suites from #{mod}")
      {:module, mod} = :code.load_binary(mod, :binary.bin_to_list(file), bytecode)

      cases = mod.all()

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
    end
  end

  def extract_suites(file, :all, cases, modules, includes) do
    for case_desc <- cases do
      Signatures.erlang_test(file, case_desc, modules, includes)
    end
  end

  def extract_suites(file, matcher, cases, modules, includes) do
    for case_desc <- cases do
      if TestMatcher.matches?(matcher, case_desc.name) do
        Signatures.erlang_test(file, case_desc, modules, includes)
      else
        []
      end
    end
    |> List.flatten()
  end

  def flatten([], _groups, acc), do: acc

  def flatten([{:group, name} | cases], groups, acc) do
    tests =
      groups
      |> Enum.filter(fn {group_name, _opts, _tests} -> group_name == name end)
      |> Enum.flat_map(fn {_group_name, _opts, tests} -> tests end)
      |> Enum.map(fn test ->
        %{
          name: "#{name}:#{test}",
          group_name: Atom.to_string(name),
          test_name: Atom.to_string(test),
          group_opts: [],
          test_opts: []
        }
      end)

    flatten(cases, groups, tests ++ acc)
  end

  def flatten([{:group, name, opts} | cases], groups, acc) do
    tests =
      groups
      |> Enum.filter(fn {group_name, _opts, _tests} -> group_name == name end)
      |> Enum.flat_map(fn {_group_name, _opts, tests} -> tests end)
      |> Enum.map(fn test ->
        %{
          name: "#{name}:#{test}",
          group_name: Atom.to_string(name),
          test_name: Atom.to_string(test),
          group_opts: opts,
          test_opts: []
        }
      end)

    flatten(cases, groups, tests ++ acc)
  end

  def flatten([{:group, name, opts, subgroup_opts} | cases], groups, acc) do
    tests =
      groups
      |> Enum.filter(fn {group_name, _opts, _tests} -> group_name == name end)
      |> Enum.map(fn
        {_group_name, _opts, subgroups_or_tests} ->
          subgroups_or_tests
          |> Enum.map(fn
            {:group, subgroup_name} ->
              for {group_name, _opts, tests} <- groups do
                if group_name == subgroup_name do
                  tests
                else
                  []
                end
              end

            tests ->
              tests
          end)
      end)
      |> List.flatten()
      |> Enum.map(fn test ->
        %{
          name: "#{name}:#{test}",
          group_name: Atom.to_string(name),
          test_name: Atom.to_string(test),
          group_opts: opts,
          test_opts: Keyword.get(subgroup_opts, test, [])
        }
      end)

    flatten(cases, groups, tests ++ acc)
  end

  def flatten([case_ | cases], groups, acc) when is_atom(case_) do
    flatten(cases, groups, [
      %{
        name: Atom.to_string(case_),
        group_name: nil,
        test_name: Atom.to_string(case_),
        group_opts: [],
        test_opts: []
      }
      | acc
    ])
  end
end
