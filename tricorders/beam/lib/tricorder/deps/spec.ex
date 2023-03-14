defmodule Tricorder.Deps.Spec do
  defstruct [:protocol, :host, :package, :version, :url, :ref, :opts]

  def parse(name, {:hex, pkg_name, vsn, _hash, tools, opts, host, _hash2}),
    do: %__MODULE__{
      protocol: :https,
      host: host,
      package: Atom.to_string(pkg_name),
      version: vsn,
      url: "https://repo.#{host}/tarballs/#{pkg_name}-#{vsn}.tar"
    }

  def parse(name, {:pkg, pkg_name, vsn}),
    do: %__MODULE__{
      protocol: :https,
      package: pkg_name,
      version: vsn,
      url: "https://repo.hex.pm/tarballs/#{pkg_name}-#{vsn}.tar"
    }

  def parse(name, {:git, repo, ref, opts}) do
    ref = clean_ref(ref)
    url = clean_url(repo)

    %__MODULE__{
      protocol: :git,
      url: url,
      ref: ref,
      opts: opts
    }
  end

  def parse(name, {:git, repo, ref}) do
    ref = clean_ref(ref)

    %__MODULE__{
      protocol: :git,
      url: :binary.list_to_bin(repo),
      ref: ref
    }
  end

  def parse(name, vsn) when is_list(vsn) do
    vsn = :binary.list_to_bin(vsn)

    %__MODULE__{
      protocol: :hexpm,
      version: vsn,
      url: "https://repo.hex.pm/tarballs/#{name}-#{vsn}.tar"
    }
  end

  defp clean_url(url) when is_binary(url), do: url
  defp clean_url(url) when is_list(url), do: :binary.list_to_bin(url)

  defp clean_ref(sha) when is_binary(sha), do: sha
  defp clean_ref({:ref, sha}) when is_list(sha), do: :binary.list_to_bin(sha)
  defp clean_ref({:tag, tag}) when is_list(tag), do: :binary.list_to_bin(tag)
end
