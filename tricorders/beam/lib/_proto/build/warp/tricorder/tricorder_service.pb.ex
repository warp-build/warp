defmodule Build.Warp.Tricorder.TricorderService.Service do
  @moduledoc false
  use GRPC.Service,
    name: "build.warp.tricorder.TricorderService",
    protoc_gen_elixir_version: "0.11.0"

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.ServiceDescriptorProto{
      name: "TricorderService",
      method: [
        %Google.Protobuf.MethodDescriptorProto{
          name: "EnsureReady",
          input_type: ".build.warp.tricorder.EnsureReadyRequest",
          output_type: ".build.warp.tricorder.EnsureReadyResponse",
          options: nil,
          client_streaming: false,
          server_streaming: false,
          __unknown_fields__: []
        },
        %Google.Protobuf.MethodDescriptorProto{
          name: "GenerateSignature",
          input_type: ".build.warp.tricorder.GenerateSignatureRequest",
          output_type: ".build.warp.tricorder.GenerateSignatureResponse",
          options: nil,
          client_streaming: false,
          server_streaming: false,
          __unknown_fields__: []
        },
        %Google.Protobuf.MethodDescriptorProto{
          name: "GetAst",
          input_type: ".build.warp.tricorder.GetAstRequest",
          output_type: ".build.warp.tricorder.GetAstResponse",
          options: nil,
          client_streaming: false,
          server_streaming: false,
          __unknown_fields__: []
        },
        %Google.Protobuf.MethodDescriptorProto{
          name: "PrepareDependency",
          input_type: ".build.warp.tricorder.PrepareDependencyRequest",
          output_type: ".build.warp.tricorder.PrepareDependencyResponse",
          options: nil,
          client_streaming: false,
          server_streaming: false,
          __unknown_fields__: []
        }
      ],
      options: nil,
      __unknown_fields__: []
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

  rpc(:GetAst, Build.Warp.Tricorder.GetAstRequest, Build.Warp.Tricorder.GetAstResponse)

  rpc(
    :PrepareDependency,
    Build.Warp.Tricorder.PrepareDependencyRequest,
    Build.Warp.Tricorder.PrepareDependencyResponse
  )
end

defmodule Build.Warp.Tricorder.TricorderService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Build.Warp.Tricorder.TricorderService.Service
end
