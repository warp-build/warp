defmodule Zap.Rbe.BuildExecutor do
  require Logger

  def execute(%{
    author_email: _author_email,
    author_name: _author_name,
    ref: ref,
    repo_name: repo_name,
    repo_url: repo_url,
  }) do

    workspace = "/tmp/dev.abstractmachines.zap/repositories/#{repo_name}"

    if workspace_exists?(workspace) do
      :ok = fetch(ref)
    else
      :ok = clone(repo_url, workspace)
    end

    :ok = build(workspace)

    :ok
  end

  def workspace_exists?(path) do
    File.exists?(path)
  end

  def clone(repo, dir) do
    Logger.info("Cloning #{repo} into #{dir}")
    {_, 0} = System.cmd("git", ["clone", repo, dir])
    :ok
  end

  def fetch(ref) do
    Logger.info("Updating repo to #{ref}")
    {_, 0} = System.cmd("git", ["fetch", ref])
    {_, 0} = System.cmd("git", ["reset", "--hard", ref])
    :ok
  end

  def build(dir) do
    Logger.info("Running zap on #{dir}")
    {_, 0} = System.cmd("zap", ["build", "//..."], cd: dir)
    :ok
  end

end
