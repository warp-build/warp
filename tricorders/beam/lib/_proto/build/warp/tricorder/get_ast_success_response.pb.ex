defmodule Build.Warp.Tricorder.GetAstSuccessResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      name: "GetAstSuccessResponse",
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          name: "subtrees",
          extendee: nil,
          number: 5,
          label: :LABEL_REPEATED,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.tricorder.AstSubtree",
          default_value: nil,
          options: nil,
          oneof_index: nil,
          json_name: "subtrees",
          proto3_optional: nil,
          __unknown_fields__: []
        }
      ],
      nested_type: [],
      enum_type: [],
      extension_range: [],
      extension: [],
      options: nil,
      oneof_decl: [],
      reserved_range: [],
      reserved_name: [],
      __unknown_fields__: []
    }
  end

  field(:subtrees, 5, repeated: true, type: Build.Warp.Tricorder.AstSubtree)
end
