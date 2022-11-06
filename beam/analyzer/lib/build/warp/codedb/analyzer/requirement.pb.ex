defmodule Build.Warp.Codedb.Analyzer.Requirement do
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
          json_name: "file",
          label: :LABEL_OPTIONAL,
          name: "file",
          number: 1,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.codedb.analyzer.FileRequirement"
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "module",
          label: :LABEL_OPTIONAL,
          name: "module",
          number: 2,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.codedb.analyzer.ModuleRequirement"
        },
        %Google.Protobuf.FieldDescriptorProto{
          __unknown_fields__: [],
          default_value: nil,
          extendee: nil,
          json_name: "symbol",
          label: :LABEL_OPTIONAL,
          name: "symbol",
          number: 3,
          oneof_index: 0,
          options: nil,
          proto3_optional: nil,
          type: :TYPE_MESSAGE,
          type_name: ".build.warp.codedb.analyzer.SymbolRequirement"
        }
      ],
      name: "Requirement",
      nested_type: [],
      oneof_decl: [
        %Google.Protobuf.OneofDescriptorProto{
          __unknown_fields__: [],
          name: "requirement",
          options: nil
        }
      ],
      options: nil,
      reserved_name: [],
      reserved_range: []
    }
  end

  oneof :requirement, 0

  field :file, 1, type: Build.Warp.Codedb.Analyzer.FileRequirement, oneof: 0
  field :module, 2, type: Build.Warp.Codedb.Analyzer.ModuleRequirement, oneof: 0
  field :symbol, 3, type: Build.Warp.Codedb.Analyzer.SymbolRequirement, oneof: 0
end