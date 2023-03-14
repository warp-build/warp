defmodule Tricorder.Deps.Spec do
  def parse({:hex, pkg_name, vsn, _hash, tools, opts, host, _hash2}),
    do: %{
      protocol: :hexpm,
      host: host,
      package: Atom.to_string(pkg_name),
      version: vsn
    }

  def parse({:pkg, pkg_name, vsn}),
    do: %{
      protocol: :hexpm,
      package: pkg_name,
      version: vsn
    }

  def parse({:git, repo, ref, opts}) do
    ref = clean_ref(ref)
    url = clean_url(repo)

    %{
      protocol: :git,
      url: url,
      ref: ref,
      opts: opts
    }
  end

  def parse({:git, repo, ref}) do
    ref = clean_ref(ref)

    %{
      protocol: :git,
      url: :binary.list_to_bin(repo),
      ref: ref
    }
  end

  defp clean_url(url) when is_binary(url), do: url
  defp clean_url(url) when is_list(url), do: :binary.list_to_bin(url)

  defp clean_ref(sha) when is_binary(sha), do: sha
  defp clean_ref({:ref, sha}) when is_list(sha), do: :binary.list_to_bin(sha)
end
