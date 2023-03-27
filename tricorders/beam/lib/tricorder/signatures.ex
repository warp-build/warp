defmodule Tricorder.Signatures do
  def mix_escript(name, file) do
    %{
      name: file,
      bin: name,
      rule: "mix_escript"
    }
  end

  def rebar3_library(name, file) do
    %{
      name: file,
      lib: name,
      rule: "rebar3_library"
    }
  end

  def elixir_library(file) do
    %{
      name: file,
      rule: "elixir_library",
      srcs: [Path.basename(file)]
    }
  end

  def elixir_script(file) do
    %{
      name: file,
      rule: "elixir_script",
      main: Path.basename(file)
    }
  end

  def erlang_library(file, modules, includes) do
    %{
      name: file,
      rule: "erlang_library",
      srcs: [Path.basename(file)],
      includes: includes,
      modules: modules
    }
  end

  def erlang_header_library(file, include_deps) do
    %{
      name: file,
      rule: "erlang_library",
      srcs: [Path.basename(file)],
      includes: include_deps
    }
  end

  def erlang_test(file, case_name, modules, includes) do
    %{
      name: case_name,
      rule: "erlang_test",
      test: file,
      modules: modules,
      includes: includes,
      cases: [case_name]
    }
  end
end
