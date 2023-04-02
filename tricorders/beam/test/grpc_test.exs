defmodule Tricorder.Grpc.ServerTest do
  use ExUnit.Case

  setup_all do
    :ok = Application.stop(:tricorder)
    Application.put_env(:tricorder, :start_server, true)
    {:ok, _} = Application.ensure_all_started(:tricorder)
    {:ok, channel} = GRPC.Stub.connect("localhost:21000")
    [channel: channel]
  end

  test "generate signatures", %{channel: channel} do
    req =
      Build.Warp.Tricorder.GenerateSignatureRequest.new(
        workspace_root: ".",
        file: "./test/fixtures/prop_verl.erl",
        dependencies: [
          Build.Warp.Dependency.new(
            store_path: "./test/fixtures",
            outputs: [
              "proper_unused_imports_remover.beam",
              "proper_transformer.beam"
            ]
          )
        ]
      )

    assert {
             :ok,
             %Build.Warp.Tricorder.GenerateSignatureResponse{
               __unknown_fields__: [],
               response: {
                 :ok,
                 %Build.Warp.Tricorder.GenerateSignatureSuccessResponse{
                   __unknown_fields__: [],
                   file: "./test/fixtures/prop_verl.erl",
                   signatures: [
                     %Build.Warp.Signature{
                       __unknown_fields__: [],
                       config: %Google.Protobuf.Struct{
                         __unknown_fields__: [],
                         fields: %{
                           "modules" => %Google.Protobuf.Value{
                             __unknown_fields__: [],
                             kind: {
                               :list_value,
                               %Google.Protobuf.ListValue{
                                 __unknown_fields__: [],
                                 values: [
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {:string_value, "proper"}
                                   },
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {:string_value, "re"}
                                   },
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {:string_value, "verl"}
                                   },
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {:string_value, "proper_statem"}
                                   },
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {:string_value, "proper_symb"}
                                   },
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {:string_value, "proper_types"}
                                   },
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {:string_value, "proper_unicode"}
                                   },
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {
                                       :string_value,
                                       "proper_unused_imports_remover"
                                     }
                                   },
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {:string_value, "proper_transformer"}
                                   }
                                 ]
                               }
                             }
                           },
                           "srcs" => %Google.Protobuf.Value{
                             __unknown_fields__: [],
                             kind: {
                               :list_value,
                               %Google.Protobuf.ListValue{
                                 __unknown_fields__: [],
                                 values: [
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {:string_value, "prop_verl.erl"}
                                   }
                                 ]
                               }
                             }
                           }
                         }
                       },
                       deps: [
                         %Build.Warp.Requirement{
                           __unknown_fields__: [],
                           requirement: {
                             :file,
                             %Build.Warp.FileRequirement{
                               __unknown_fields__: [],
                               path: "./test/fixtures/proper.hrl"
                             }
                           }
                         },
                         %Build.Warp.Requirement{
                           __unknown_fields__: [],
                           requirement: {
                             :file,
                             %Build.Warp.FileRequirement{
                               __unknown_fields__: [],
                               path: "./test/fixtures/proper_common.hrl"
                             }
                           }
                         }
                       ],
                       name: "./test/fixtures/prop_verl.erl",
                       rule: "erlang_library",
                       runtime_deps: [
                         %Build.Warp.Requirement{
                           __unknown_fields__: [],
                           requirement: {
                             :symbol,
                             %Build.Warp.SymbolRequirement{
                               __unknown_fields__: [],
                               kind: "module",
                               raw: "proper"
                             }
                           }
                         },
                         %Build.Warp.Requirement{
                           __unknown_fields__: [],
                           requirement: {
                             :symbol,
                             %Build.Warp.SymbolRequirement{
                               __unknown_fields__: [],
                               kind: "module",
                               raw: "verl"
                             }
                           }
                         },
                         %Build.Warp.Requirement{
                           __unknown_fields__: [],
                           requirement: {
                             :symbol,
                             %Build.Warp.SymbolRequirement{
                               __unknown_fields__: [],
                               kind: "module",
                               raw: "proper_statem"
                             }
                           }
                         },
                         %Build.Warp.Requirement{
                           __unknown_fields__: [],
                           requirement: {
                             :symbol,
                             %Build.Warp.SymbolRequirement{
                               __unknown_fields__: [],
                               kind: "module",
                               raw: "proper_symb"
                             }
                           }
                         },
                         %Build.Warp.Requirement{
                           __unknown_fields__: [],
                           requirement: {
                             :symbol,
                             %Build.Warp.SymbolRequirement{
                               __unknown_fields__: [],
                               kind: "module",
                               raw: "proper_types"
                             }
                           }
                         },
                         %Build.Warp.Requirement{
                           __unknown_fields__: [],
                           requirement: {
                             :symbol,
                             %Build.Warp.SymbolRequirement{
                               __unknown_fields__: [],
                               kind: "module",
                               raw: "proper_unicode"
                             }
                           }
                         },
                         %Build.Warp.Requirement{
                           __unknown_fields__: [],
                           requirement: {
                             :file,
                             %Build.Warp.FileRequirement{
                               __unknown_fields__: [],
                               path: "test/fixtures/proper_unused_imports_remover.erl"
                             }
                           }
                         },
                         %Build.Warp.Requirement{
                           __unknown_fields__: [],
                           requirement: {
                             :file,
                             %Build.Warp.FileRequirement{
                               __unknown_fields__: [],
                               path: "test/fixtures/proper_transformer.erl"
                             }
                           }
                         }
                       ]
                     }
                   ],
                   workspace_root: ""
                 }
               }
             }
           } = Build.Warp.Tricorder.TricorderService.Stub.generate_signature(channel, req)
  end

  test "get ast", %{channel: channel} do
    req =
      Build.Warp.Tricorder.GetAstRequest.new(
        workspace_root: ".",
        file: "./test/fixtures/prop_verl.erl",
        test_matcher: Build.Warp.TestMatcher.new(raw: ["prop_basic"]),
        dependencies: [
          Build.Warp.Dependency.new(
            store_path: "./test/fixtures",
            outputs: [
              "proper_unused_imports_remover.beam",
              "proper_transformer.beam"
            ]
          )
        ]
      )

    assert {:ok,
            %Build.Warp.Tricorder.GetAstResponse{
              __unknown_fields__: [],
              response:
                {:ok,
                 %Build.Warp.Tricorder.GetAstSuccessResponse{
                   __unknown_fields__: [],
                   subtrees: [
                     %Build.Warp.Tricorder.AstSubtree{
                       __unknown_fields__: [],
                       ast: _,
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_invalid_semver_more2",
                       source_chunk: _,
                       workspace_root: ""
                     },
                     %Build.Warp.Tricorder.AstSubtree{
                       __unknown_fields__: [],
                       ast: _,
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_invalid_semver_more",
                       source_chunk: _,
                       workspace_root: ""
                     },
                     %Build.Warp.Tricorder.AstSubtree{
                       __unknown_fields__: [],
                       ast: _,
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_invalid_semver",
                       source_chunk: _,
                       workspace_root: ""
                     },
                     %Build.Warp.Tricorder.AstSubtree{
                       __unknown_fields__: [],
                       ast: _,
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_valid_semver",
                       source_chunk: _,
                       workspace_root: ""
                     },
                     %Build.Warp.Tricorder.AstSubtree{
                       __unknown_fields__: [],
                       ast: _,
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_valid_semver0",
                       source_chunk: _,
                       workspace_root: ""
                     }
                   ]
                 }}
            }} = Build.Warp.Tricorder.TricorderService.Stub.get_ast(channel, req)
  end
end
