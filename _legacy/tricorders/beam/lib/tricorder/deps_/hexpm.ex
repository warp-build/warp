defmodule Tricorder.Deps.Hexpm do
  require Logger

  def get_package(pkg_name) do
    GenServer.call(Tricorder.Deps.Hexpm.Server, {:get_package, pkg_name})
  end
end
