defmodule Build.Warp.Codedb.AnalyzerService.Service do
  @moduledoc false
  use GRPC.Service, name: "build.warp.codedb.AnalyzerService", protoc_gen_elixir_version: "0.11.0"

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.ServiceDescriptorProto{
      __unknown_fields__: [],
      method: [
        %Google.Protobuf.MethodDescriptorProto{
          __unknown_fields__: [],
          client_streaming: false,
          input_type: ".build.warp.codedb.GetDependenciesRequest",
          name: "GetDependencies",
          options: nil,
          output_type: ".build.warp.codedb.GetDependenciesResponse",
          server_streaming: false
        },
        %Google.Protobuf.MethodDescriptorProto{
          __unknown_fields__: [],
          client_streaming: false,
          input_type: ".build.warp.codedb.GetInterestedPathsRequest",
          name: "GetInterestedPaths",
          options: nil,
          output_type: ".build.warp.codedb.GetInterestedPathsResponse",
          server_streaming: false
        },
        %Google.Protobuf.MethodDescriptorProto{
          __unknown_fields__: [],
          client_streaming: false,
          input_type: ".build.warp.codedb.GetProvidedSymbolsRequest",
          name: "GetProvidedSymbols",
          options: nil,
          output_type: ".build.warp.codedb.GetProvidedSymbolsResponse",
          server_streaming: false
        },
        %Google.Protobuf.MethodDescriptorProto{
          __unknown_fields__: [],
          client_streaming: false,
          input_type: ".build.warp.codedb.GetAstRequest",
          name: "GetAst",
          options: nil,
          output_type: ".build.warp.codedb.GetAstResponse",
          server_streaming: false
        },
        %Google.Protobuf.MethodDescriptorProto{
          __unknown_fields__: [],
          client_streaming: false,
          input_type: ".build.warp.codedb.GenerateSignatureRequest",
          name: "GenerateSignature",
          options: nil,
          output_type: ".build.warp.codedb.GenerateSignatureResponse",
          server_streaming: false
        }
      ],
      name: "AnalyzerService",
      options: nil
    }
  end

  rpc :GetDependencies,
      Build.Warp.Codedb.GetDependenciesRequest,
      Build.Warp.Codedb.GetDependenciesResponse

  rpc :GetInterestedPaths,
      Build.Warp.Codedb.GetInterestedPathsRequest,
      Build.Warp.Codedb.GetInterestedPathsResponse

  rpc :GetProvidedSymbols,
      Build.Warp.Codedb.GetProvidedSymbolsRequest,
      Build.Warp.Codedb.GetProvidedSymbolsResponse

  rpc :GetAst, Build.Warp.Codedb.GetAstRequest, Build.Warp.Codedb.GetAstResponse

  rpc :GenerateSignature,
      Build.Warp.Codedb.GenerateSignatureRequest,
      Build.Warp.Codedb.GenerateSignatureResponse
end

defmodule Build.Warp.Codedb.AnalyzerService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Build.Warp.Codedb.AnalyzerService.Service
end