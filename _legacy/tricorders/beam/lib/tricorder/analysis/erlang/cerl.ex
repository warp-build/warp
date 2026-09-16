defmodule Tricorder.Analysis.Erlang.Cerl do
  require Logger

  @default_compile_opts [
    :no_copt,
    :return_errors,
    :no_inline,
    :strict_record_tests,
    :strict_record_updates,
    :no_spawn_compiler_process,
    :binary
  ]

  def compile(file, %{include_paths: include_paths, code_paths: code_paths}) do
    for code_path <- code_paths do
      true = code_path |> :binary.bin_to_list() |> :code.add_path()
    end

    compile_opts =
      @default_compile_opts ++
        for path <- include_paths, do: {:i, path |> :binary.bin_to_list()}

    file = :binary.bin_to_list(file)

    Logger.info(
      "Compiling #{file} with opts #{inspect(compile_opts)} and paths #{inspect(code_paths)}"
    )

    with {:ok, mod, bytecode} <- :compile.noenv_file(file, compile_opts),
         {:ok, mod, core} <- :compile.noenv_file(file, compile_opts ++ [:to_core]) do
      Logger.info("Compilation succeeded: #{mod}")
      # tree = :cerl.from_records(core)
      # attrs = :cerl.module_attrs(core)
      {:ok, mod, bytecode, core}
    else
      {:error, err, _} ->
        Logger.error("Compilation error: #{inspect(err, pretty: true)}")
        {:missing_dependencies, %{modules: parse_transforms(err, [])}}
    end
  end

  defp parse_transforms([], acc), do: acc
  defp parse_transforms([h | t], acc), do: parse_transforms(t, [extract_mod(h) | acc])

  defp extract_mod(
         {_file,
          [
            {:none, :compile, {:undef_parse_transform, mod}}
          ]}
       ),
       do: mod
end
