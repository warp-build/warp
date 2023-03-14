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
    {:ok, file} = do_read(root)
    %__MODULE__{root: root, patterns: parse(file)}
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
      {:ok, glob} = :glob.compile(line)
      parse(lines, [glob | acc])
    end
  end
end
