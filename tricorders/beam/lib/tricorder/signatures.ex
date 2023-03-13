defmodule Tricorder.Signatures do
  def erlang_library(file, modules, includes) do
    %{
      name: file,
      rule: "erlang_library",
      srcs: [Path.filename(file)],
      includes: includes,
      modules: modules
    }
  end

  def erlang_header_library(file, include_deps) do
    %{
      name: file,
      rule: "erlang_library",
      srcs: [Path.filename(file)],
      includes: include_deps
    }
  end
end
