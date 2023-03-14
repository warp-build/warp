defmodule Build.Warp.Dependency.ResolverService.Service do
  @moduledoc false
  use GRPC.Service,
    name: "build.warp.dependency.ResolverService",
    protoc_gen_elixir_version: "0.11.0"

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.ServiceDescriptorProto{
      __unknown_fields__: [],
      method: [
        %Google.Protobuf.MethodDescriptorProto{
          __unknown_fields__: [],
          client_streaming: false,
          input_type: ".build.warp.dependency.ResolveDependencyRequest",
          name: "ResolveDependency",
          options: nil,
          output_type: ".build.warp.dependency.ResolveDependencyResponse",
          server_streaming: false
        },
        %Google.Protobuf.MethodDescriptorProto{
          __unknown_fields__: [],
          client_streaming: false,
          input_type: ".build.warp.dependency.PrepareDependencyRequest",
          name: "PrepareDependency",
          options: nil,
          output_type: ".build.warp.dependency.PrepareDependencyResponse",
          server_streaming: false
        }
      ],
      name: "ResolverService",
      options: nil
    }
  end

  rpc(
    :ResolveDependency,
    Build.Warp.Dependency.ResolveDependencyRequest,
    Build.Warp.Dependency.ResolveDependencyResponse
  )

  rpc(
    :PrepareDependency,
    Build.Warp.Dependency.PrepareDependencyRequest,
    Build.Warp.Dependency.PrepareDependencyResponse
  )
end

defmodule Build.Warp.Dependency.ResolverService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Build.Warp.Dependency.ResolverService.Service
end
