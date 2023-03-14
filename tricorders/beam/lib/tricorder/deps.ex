defmodule Tricorder.Deps do
  alias __MODULE__

  require Logger

  defmodule Dep do
    defstruct [:name]
  end

  def scan(root) do
    Logger.info("Scanning #{root}")

    find_files(root, "mix.lock", &Deps.MixLock.load/1) ++
      find_files(root, "rebar.lock", &Deps.RebarLock.load/1)
  end

  def find_files(root, pattern, handler) do
    gitignore = Gitignore.read(root)
    {:ok, files} = File.ls(root)
    find_files(files, root, pattern, handler, gitignore, [])
  end

  def find_files([], _, _, _, _, acc), do: acc

  def find_files([file | files], root, pattern, handler, gitignore, acc) do
    # if we match the gitignore we skip things
    if Gitignore.should_ignore?(gitignore, file) do
      find_files(files, root, pattern, handler, gitignore, acc)
    else
      # if this file is a dir, we recurse
      if File.dir?(file) do
        {:ok, more_files} = File.ls(file)
        files = List.flatten(files ++ more_files)
        find_files(files, root, pattern, handler, gitignore, acc)
      else
        if String.ends_with?(file, pattern) do
          # if this isn't ignored, and isn't a dir, we handle it
          acc = [handler.(file) | acc]
          find_files(files, root, pattern, handler, gitignore, acc)
        else
          find_files(files, root, pattern, handler, gitignore, acc)
        end
      end
    end
  end
end
