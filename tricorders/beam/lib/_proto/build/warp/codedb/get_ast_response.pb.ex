defmodule Build.Warp.Codedb.GetAstResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      __unknown_fields__: [],
      enum_type: [],
      extension: [],
      extension_range: [],
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "ok",
          label: :LABEL_OPTIONAL,
          name: "ok",
          number: 1,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.codedb.GetAstSuccessResponse"
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "missingDeps",
          label: :LABEL_OPTIONAL,
          name: "missing_deps",
          number: 3,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.codedb.GetAstMissingDepsResponse"
        }
      ],
      name: "GetAstResponse",
      nested_type: [],
      oneof_decl: [
        %Google.Protobuf.OneofDescriptorProto{
          __unknown_fields__: [],
          name: "response",
          options: nil
        }
      ],
      options: nil,
      reserved_name: [],
      reserved_range: []
    }
  end

  oneof(:response, 0)

  field(:ok, 1, type: Build.Warp.Codedb.GetAstSuccessResponse, oneof: 0)

  field(:missing_deps, 3,
    type: Build.Warp.Codedb.GetAstMissingDepsResponse,
    json_name: "missingDeps",
    oneof: 0
  )
end
