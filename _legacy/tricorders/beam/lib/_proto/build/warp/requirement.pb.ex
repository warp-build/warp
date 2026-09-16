defmodule Build.Warp.Requirement do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.DescriptorProto{
      name: "Requirement",
      field: [
        %Google.Protobuf.FieldDescriptorProto{
          name: "file",
          extendee: nil,
          number: 1,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.FileRequirement",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "file",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "symbol",
          extendee: nil,
          number: 2,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.SymbolRequirement",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "symbol",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "url",
          extendee: nil,
          number: 3,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.UrlRequirement",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "url",
          proto3_optional: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.FieldDescriptorProto{
          name: "dependency",
          extendee: nil,
          number: 4,
          label: :LABEL_OPTIONAL,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.DependencyRequirement",
          default_value: nil,
          options: nil,
          oneof_index: 0,
          json_name: "dependency",
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
          name: "requirement",
          options: nil,
          __unknown_fields__: []
        }
      ],
      reserved_range: [],
      reserved_name: [],
      __unknown_fields__: []
    }
  end

  oneof(:requirement, 0)

  field(:file, 1, type: Build.Warp.FileRequirement, oneof: 0)
  field(:symbol, 2, type: Build.Warp.SymbolRequirement, oneof: 0)
  field(:url, 3, type: Build.Warp.UrlRequirement, oneof: 0)
  field(:dependency, 4, type: Build.Warp.DependencyRequirement, oneof: 0)
end
