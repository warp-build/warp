defmodule Build.Warp.Codedb.Analyzer.AnalyzerService.Service do
  @moduledoc false
  use GRPC.Service,
    name: "build.warp.codedb.analyzer.AnalyzerService",
    protoc_gen_elixir_version: "0.11.0"

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.ServiceDescriptorProto{
      __unknown_fields__: [],
      method: [
        %Google.Protobuf.MethodDescriptorProto{
          __unknown_fields__: [],
          client_streaming: false,
          input_type: ".build.warp.codedb.analyzer.GetInterestedExtensionsRequest",
          name: "GetInterestedExtensions",
          options: nil,
          output_type: ".build.warp.codedb.analyzer.GetInterestedExtensionsResponse",
          server_streaming: false
        },
        %Google.Protobuf.MethodDescriptorProto{
          __unknown_fields__: [],
          client_streaming: false,
          input_type: ".build.warp.codedb.analyzer.AnalyzeFileRequest",
          name: "AnalyzeFile",
          options: nil,
          output_type: ".build.warp.codedb.analyzer.AnalyzeFileResponse",
          server_streaming: false
        },
        %Google.Protobuf.MethodDescriptorProto{
          __unknown_fields__: [],
          client_streaming: false,
          input_type: ".build.warp.codedb.analyzer.HelloWorldRequest",
          name: "HelloWorld",
          options: nil,
          output_type: ".build.warp.codedb.analyzer.HelloWorldResponse",
          server_streaming: false
        }
      ],
      name: "AnalyzerService",
      options: nil
    }
  end

  rpc :GetInterestedExtensions,
      Build.Warp.Codedb.Analyzer.GetInterestedExtensionsRequest,
      Build.Warp.Codedb.Analyzer.GetInterestedExtensionsResponse

  rpc :AnalyzeFile,
      Build.Warp.Codedb.Analyzer.AnalyzeFileRequest,
      Build.Warp.Codedb.Analyzer.AnalyzeFileResponse

  rpc :HelloWorld,
      Build.Warp.Codedb.Analyzer.HelloWorldRequest,
      Build.Warp.Codedb.Analyzer.HelloWorldResponse
end

defmodule Build.Warp.Codedb.Analyzer.AnalyzerService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Build.Warp.Codedb.Analyzer.AnalyzerService.Service
end