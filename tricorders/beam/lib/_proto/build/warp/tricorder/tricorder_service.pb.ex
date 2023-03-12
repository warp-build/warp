defmodule Build.Warp.Tricorder.TricorderService.Service do
  @moduledoc false
  use GRPC.Service,
    name: "build.warp.tricorder.TricorderService",
    protoc_gen_elixir_version: "0.11.0"

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.ServiceDescriptorProto{
      __unknown_fields__: [],
      method: [
        %Google.Protobuf.MethodDescriptorProto{
          __unknown_fields__: [],
          client_streaming: false,
          input_type: ".build.warp.tricorder.EnsureReadyRequest",
          name: "EnsureReady",
          options: nil,
          output_type: ".build.warp.tricorder.EnsureReadyResponse",
          server_streaming: false
        },
        %Google.Protobuf.MethodDescriptorProto{
          __unknown_fields__: [],
          client_streaming: false,
          input_type: ".build.warp.tricorder.GenerateSignatureRequest",
          name: "GenerateSignature",
          options: nil,
          output_type: ".build.warp.tricorder.GenerateSignatureResponse",
          server_streaming: false
        }
      ],
      name: "TricorderService",
      options: nil
    }
  end

  rpc(
    :EnsureReady,
    Build.Warp.Tricorder.EnsureReadyRequest,
    Build.Warp.Tricorder.EnsureReadyResponse
  )

  rpc(
    :GenerateSignature,
    Build.Warp.Tricorder.GenerateSignatureRequest,
    Build.Warp.Tricorder.GenerateSignatureResponse
  )
end

defmodule Build.Warp.Tricorder.TricorderService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Build.Warp.Tricorder.TricorderService.Service
end
