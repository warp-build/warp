defmodule Build.Warp.Goal do
  @moduledoc false
  use Protobuf, enum: true, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  def descriptor do
    # credo:disable-for-next-line
    %Google.Protobuf.EnumDescriptorProto{
      name: "Goal",
      value: [
        %Google.Protobuf.EnumValueDescriptorProto{
          name: "GOAL_UNKNOWN",
          number: 0,
          options: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.EnumValueDescriptorProto{
          name: "GOAL_BUILD",
          number: 1,
          options: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.EnumValueDescriptorProto{
          name: "GOAL_TEST",
          number: 2,
          options: nil,
          __unknown_fields__: []
        },
        %Google.Protobuf.EnumValueDescriptorProto{
          name: "GOAL_RUN",
          number: 3,
          options: nil,
          __unknown_fields__: []
        }
      ],
      options: nil,
      reserved_range: [],
      reserved_name: [],
      __unknown_fields__: []
    }
  end

  field(:GOAL_UNKNOWN, 0)
  field(:GOAL_BUILD, 1)
  field(:GOAL_TEST, 2)
  field(:GOAL_RUN, 3)
end
