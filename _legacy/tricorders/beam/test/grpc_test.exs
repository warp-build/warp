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

    assert {:ok,
            %Build.Warp.Tricorder.GenerateSignatureResponse{
              response:
                {:ok,
                 %Build.Warp.Tricorder.GenerateSignatureSuccessResponse{
                   workspace_root: "",
                   file: "./test/fixtures/prop_verl.erl",
                   signatures: [
                     %Build.Warp.Signature{
                       name: "./test/fixtures/prop_verl.erl",
                       rule: "erlang_library",
                       deps: [
                         %Build.Warp.Requirement{
                           requirement:
                             {:file,
                              %Build.Warp.FileRequirement{
                                path: "./test/fixtures/proper.hrl"
                              }}
                         },
                         %Build.Warp.Requirement{
                           requirement:
                             {:file,
                              %Build.Warp.FileRequirement{
                                path: "./test/fixtures/proper_common.hrl"
                              }}
                         }
                       ],
                       runtime_deps: [
                         %Build.Warp.Requirement{
                           requirement:
                             {:symbol,
                              %Build.Warp.SymbolRequirement{
                                raw: "proper",
                                kind: "module"
                              }}
                         },
                         %Build.Warp.Requirement{
                           requirement:
                             {:symbol,
                              %Build.Warp.SymbolRequirement{
                                raw: "verl",
                                kind: "module"
                              }}
                         },
                         %Build.Warp.Requirement{
                           requirement:
                             {:symbol,
                              %Build.Warp.SymbolRequirement{
                                raw: "proper_statem",
                                kind: "module"
                              }}
                         },
                         %Build.Warp.Requirement{
                           requirement:
                             {:symbol,
                              %Build.Warp.SymbolRequirement{
                                raw: "proper_symb",
                                kind: "module"
                              }}
                         },
                         %Build.Warp.Requirement{
                           requirement:
                             {:symbol,
                              %Build.Warp.SymbolRequirement{
                                raw: "proper_types",
                                kind: "module"
                              }}
                         },
                         %Build.Warp.Requirement{
                           requirement:
                             {:symbol,
                              %Build.Warp.SymbolRequirement{
                                raw: "proper_unicode",
                                kind: "module"
                              }}
                         },
                         %Build.Warp.Requirement{
                           requirement:
                             {:file,
                              %Build.Warp.FileRequirement{
                                path: "test/fixtures/proper_unused_imports_remover.erl"
                              }}
                         },
                         %Build.Warp.Requirement{
                           requirement:
                             {:file,
                              %Build.Warp.FileRequirement{
                                path: "test/fixtures/proper_transformer.erl"
                              }}
                         }
                       ],
                       config: %Google.Protobuf.Struct{
                         fields: %{
                           "modules" => %Google.Protobuf.Value{
                             kind:
                               {:list_value,
                                %Google.Protobuf.ListValue{
                                  values: [
                                    %Google.Protobuf.Value{
                                      kind: {:string_value, "proper"}
                                    },
                                    %Google.Protobuf.Value{
                                      kind: {:string_value, "re"}
                                    },
                                    %Google.Protobuf.Value{
                                      kind: {:string_value, "verl"}
                                    },
                                    %Google.Protobuf.Value{
                                      kind: {:string_value, "proper_statem"}
                                    },
                                    %Google.Protobuf.Value{
                                      kind: {:string_value, "proper_symb"}
                                    },
                                    %Google.Protobuf.Value{
                                      kind: {:string_value, "proper_types"}
                                    },
                                    %Google.Protobuf.Value{
                                      kind: {:string_value, "proper_unicode"}
                                    },
                                    %Google.Protobuf.Value{
                                      kind: {:string_value, "proper_unused_imports_remover"}
                                    },
                                    %Google.Protobuf.Value{
                                      kind: {:string_value, "proper_transformer"}
                                    }
                                  ]
                                }}
                           },
                           "srcs" => %Google.Protobuf.Value{
                             kind:
                               {:list_value,
                                %Google.Protobuf.ListValue{
                                  values: [
                                    %Google.Protobuf.Value{
                                      kind: {:string_value, "prop_verl.erl"}
                                    }
                                  ]
                                }}
                           }
                         }
                       }
                     }
                   ]
                 }}
            }} = Build.Warp.Tricorder.TricorderService.Stub.generate_signature(channel, req)
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
              response:
                {:ok,
                 %Build.Warp.Tricorder.GetAstSuccessResponse{
                   subtrees: [
                     %Build.Warp.Tricorder.AstSubtree{
                       ast: _,
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_invalid_semver_more2",
                       source_chunk: _,
                       workspace_root: ""
                     },
                     %Build.Warp.Tricorder.AstSubtree{
                       ast: _,
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_invalid_semver_more",
                       source_chunk: _,
                       workspace_root: ""
                     },
                     %Build.Warp.Tricorder.AstSubtree{
                       ast: _,
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_invalid_semver",
                       source_chunk: _,
                       workspace_root: ""
                     },
                     %Build.Warp.Tricorder.AstSubtree{
                       ast: _,
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_valid_semver",
                       source_chunk: _,
                       workspace_root: ""
                     },
                     %Build.Warp.Tricorder.AstSubtree{
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
