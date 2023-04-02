defmodule Build.Warp.Tricorder.GetAstResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      name: "GetAstResponse",
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          name: "ok",
          extendee: nil,
          number: 1,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.tricorder.GetAstSuccessResponse",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "ok",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "missing_deps",
          extendee: nil,
          number: 3,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.tricorder.GetAstMissingDepsResponse",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "missingDeps",
          proto3_optional: nil,
          __unknown_fields__: []
        }
      ],
      nested_type: [],
      enum_type: [],
      extension_range: [],
      extension: [],
      options: nil,
      oneof_decl: [
        %Google.Protobuf.OneofDescriptorProto{
          name: "response",
          options: nil,
          __unknown_fields__: []
        }
      ],
      reserved_range: [],
      reserved_name: [],
      __unknown_fields__: []
    }
  end

  oneof(:response, 0)

  field(:ok, 1, type: Build.Warp.Tricorder.GetAstSuccessResponse, oneof: 0)

  field(:missing_deps, 3,
    type: Build.Warp.Tricorder.GetAstMissingDepsResponse,
    json_name: "missingDeps",
    oneof: 0
  )
end
