defmodule Build.Warp.Tricorder.PrepareDependencyResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      name: "PrepareDependencyResponse",
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          name: "signatures",
          extendee: nil,
          number: 2,
          label: :LABEL_REPEATED,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.Signature",
          default_value: nil,
          options: nil,
          oneof_index: nil,
          json_name: "signatures",
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

  field(:signatures, 2, repeated: true, type: Build.Warp.Signature)
end
