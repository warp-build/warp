defmodule Tricorder.Deps.Hexpm.Resolver do
  require Logger

  def request(method, uri, req_headers, body, _config) do
    uri = URI.parse(uri)

    {:ok, conn} =
      Mint.HTTP.connect(
        :https,
        uri.host,
        uri.port,
        transport_opts: [cacerts: :public_key.cacerts_get()]
      )

    body =
      case body do
        :undefined -> nil
        _ -> body
      end

    {:ok, conn, request_ref} =
      Mint.HTTP.request(
        conn,
        method |> Atom.to_string() |> String.upcase(),
        uri.path,
        req_headers |> Map.to_list(),
        body
      )

    {conn, status, headers, body} = await(conn, request_ref)
    {:ok, _conn} = Mint.HTTP.close(conn)

    Logger.debug("-> #{inspect(method)} #{inspect(uri)}")
    Logger.debug("<- #{inspect(status)} #{inspect(body)}")

    {:ok, {status, headers, body}}
  end

  defp await(conn, request_ref, status \\ nil, headers \\ [], body \\ []) do
    receive do
      message ->
        {:ok, conn, responses} = Mint.HTTP.stream(conn, message)

        {ctrl, status, headers, body} =
          responses
          |> Enum.reduce(
            {:continue, status, headers, body},
            fn
              _, {:complete, status, headers, body} ->
                {:complete, status, headers, body}

              {:status, ^request_ref, status_code}, {:continue, _status, headers, body} ->
                {:continue, status_code, headers, body}

              {:headers, ^request_ref, headers0}, {:continue, status, headers1, body} ->
                {:continue, status, headers0 ++ headers1, body}

              {:data, ^request_ref, data}, {:continue, status, headers, body} ->
                {:continue, status, headers, [data | body]}

              {:done, ^request_ref}, {:continue, status, headers, body} ->
                {:complete, status, headers |> Map.new(), :binary.list_to_bin(Enum.reverse(body))}
            end
          )

        case ctrl do
          :complete -> {conn, status, headers, body}
          _ -> await(conn, request_ref, status, headers, body)
        end
    end
  end
end
