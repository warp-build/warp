defmodule Gitignore do
  defstruct [:root, :patterns]

  @gitignore ".gitignore"
  @hash "#"

  def should_ignore?(%__MODULE__{patterns: patterns}, file) do
    try_match(patterns, file)
  end

  defp try_match([], _file), do: false

  defp try_match([pat | pattern], file) do
    if :glob.run(file, pat) == :match do
      true
    else
      try_match(pattern, file)
    end
  end

  def read(root) do
    case do_read(root) do
      {:ok, file} -> %__MODULE__{root: root, patterns: parse(file)}
      _ -> %__MODULE__{root: root, patterns: []}
    end
  end

  defp do_read(root) do
    case File.read(root) do
      {:ok, file} -> {:ok, file}
      _ -> File.read(Path.join(root, @gitignore))
    end
  end

  defp parse(src) do
    lines = String.split(src, "\n", trim: true, parts: :infinity)
    parse(lines, [])
  end

  defp parse([], acc), do: acc

  defp parse([line | lines], acc) do
    if String.starts_with?(line, @hash) do
      parse(lines, acc)
    else
      lien =
        if String.starts_with?("/", line) do
          ".#{line}"
        else
          line
        end

      {:ok, glob} = :glob.compile(line)
      parse(lines, [glob | acc])
    end
  end

  def find(gitignore, root, pattern, on_match) do
    do_find([root], root, pattern, on_match, gitignore, [])
  end

  defp do_find([], _, _, _, _, acc), do: acc

  defp do_find([file | files], root, pattern, on_match, gitignore, acc) do
    # if we match the gitignore we skip things
    if Gitignore.should_ignore?(gitignore, file) do
      do_find(files, root, pattern, on_match, gitignore, acc)
    else
      # if this file is a dir, we recurse
      if File.dir?(file) do
        {:ok, more_files} = File.ls(file)
        more_files = more_files |> Enum.map(&Path.join(file, &1))
        files = List.flatten(files ++ more_files)
        do_find(files, root, pattern, on_match, gitignore, acc)
      else
        if String.ends_with?(file, pattern) do
          # if this isn't ignored, and isn't a dir, we handle it
          acc = [on_match.(file) | acc]
          do_find(files, root, pattern, on_match, gitignore, acc)
        else
          do_find(files, root, pattern, on_match, gitignore, acc)
        end
      end
    end
  end
end
