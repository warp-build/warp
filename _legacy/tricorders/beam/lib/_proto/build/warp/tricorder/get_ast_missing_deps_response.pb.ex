defmodule Build.Warp.Tricorder.GetAstMissingDepsResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      name: "GetAstMissingDepsResponse",
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          name: "file",
          extendee: nil,
          number: 1,
          label: :LABEL_OPTIONAL,
          type: :TYPE_STRING,
          type_name: nil,
          default_value: nil,
          options: nil,
          oneof_index: nil,
          json_name: "file",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "dependencies",
          extendee: nil,
          number: 4,
          label: :LABEL_REPEATED,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.Dependency",
          default_value: nil,
          options: nil,
          oneof_index: nil,
          json_name: "dependencies",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "requirements",
          extendee: nil,
          number: 5,
          label: :LABEL_REPEATED,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.Requirement",
          default_value: nil,
          options: nil,
          oneof_index: nil,
          json_name: "requirements",
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

  field(:file, 1, type: :string)
  field(:dependencies, 4, repeated: true, type: Build.Warp.Dependency)
  field(:requirements, 5, repeated: true, type: Build.Warp.Requirement)
end
