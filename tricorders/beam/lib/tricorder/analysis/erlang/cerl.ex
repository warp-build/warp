defmodule Tricorder.Analysis.Erlang.Cerl do
  @default_compile_opts [
    :no_copt,
    :return_errors,
    :no_inline,
    :strict_record_tests,
    :strict_record_updates,
    :no_spawn_compiler_process,
    :binary,
    :to_core
  ]

  def compile(file, include_paths, code_paths) do
    for code_path <- code_paths do
      :code.add_path(code_path)
    end

    compile_opts =
      @default_compile_opts ++
        for path <- include_paths, do: {:i, path}

    file = :binary.bin_to_list(file)

    with {:ok, mod, core} <- :compile.noenv_file(file, compile_opts) do
      tree = :cerl.from_records(core)
      attrs = :cerl.module_attrs(core)
      {:ok, tree, attrs}
    end
  end
end
