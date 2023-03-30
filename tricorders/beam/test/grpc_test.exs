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
                                     kind: {:string_value, "verl"}
                                   },
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
                                     kind: {
                                       :string_value,
                                       "proper_unused_imports_remover"
                                     }
                                   },
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {
                                       :string_value,
                                       "proper_statem"
                                     }
                                   },
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {:string_value, "proper_symb"}
                                   },
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {
                                       :string_value,
                                       "proper_types"
                                     }
                                   },
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {
                                       :string_value,
                                       "proper_unicode"
                                     }
                                   },
                                   %Google.Protobuf.Value{
                                     __unknown_fields__: [],
                                     kind: {
                                       :string_value,
                                       "proper_transformer"
                                     }
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
                                     kind: {
                                       :string_value,
                                       "prop_verl.erl"
                                     }
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
                               raw: "proper"
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
                               path: "test/fixtures/proper_transformer.erl"
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
                               raw: "proper"
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
                       ast:
                         "[{:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 1}}, {:attribute, 0, :module, :prop_verl}, {:attribute, 0, :file, {'./test/fixtures/proper.hrl', 1}}, {:attribute, 0, :compile, :debug_info}, {:attribute, 0, :file, {'./test/fixtures/proper_common.hrl', 1}}, {:attribute, 0, :file, {'./test/fixtures/proper.hrl', 32}}, {:attribute, 0, :import, {:proper, [numtests: 2, fails: 1, on_output: 2, conjunction: 1]}}, {:attribute, 0, :import, {:proper, [collect: 2, collect: 3, aggregate: 2, aggregate: 3, classify: 3, measure: 3, with_title: 1, equals: 2]}}, {:attribute, 0, :import, {:proper_types, [integer: 2, float: 2, atom: 0, binary: 0, binary: 1, bitstring: 0, bitstring: 1, list: 1, vector: 2, union: 1, weighted_union: 1, tuple: 1, loose_tuple: 1, exactly: 1, fixed_list: 1, function: 2, map: 2, any: 0]}}, {:attribute, 0, :import, {:proper_types, [integer: 0, non_neg_integer: 0, pos_integer: 0, neg_integer: 0, range: 2, float: 0, non_neg_float: 0, number: 0, boolean: 0, byte: 0, char: 0, list: 0, tuple: 0, string: 0, wunion: 1, term: 0, timeout: 0, arity: 0]}}, {:attribute, 0, :import, {:proper_types, [int: 0, nat: 0, largeint: 0, real: 0, bool: 0, choose: 2, elements: 1, oneof: 1, frequency: 1, return: 1, default: 2, orderedlist: 1, function0: 1, function1: 1, function2: 1, function3: 1, function4: 1, weighted_default: 2, parameter: 1, parameter: 2, with_parameter: 3, with_parameters: 2]}}, {:attribute, 0, :import, {:proper_unicode, [utf8: 0, utf8: 1, utf8: 2]}}, {:attribute, 0, :import, {:proper_types, [resize: 2, non_empty: 1, noshrink: 1]}}, {:attribute, 0, :import, {:proper_symb, [eval: 1, eval: 2, defined: 1, well_defined: 1, pretty_print: 1, pretty_print: 2]}}, {:attribute, 0, :import, {:proper_statem, [commands: 1, commands: 2, parallel_commands: 1, parallel_commands: 2, more_commands: 2]}}, {:attribute, 0, :import, {:proper_statem, [run_commands: 2, run_commands: 3, state_after: 2, command_names: 1, zip: 2, run_parallel_commands: 2, run_parallel_commands: 3]}}, {:attribute, 0, :import, {:proper_unused_imports_remover, []}}, {:attribute, 0, :compile, {:parse_transform, :proper_unused_imports_remover}}, {:attribute, 0, :compile, {:parse_transform, :proper_transformer}}, {:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 4}}, {:attribute, 0, :file, {'/nix/store/i2bnhvrvd28jcifmj1w9adk78rifhgd8-erlang-24.3.4.5/lib/erlang/lib/stdlib-3.17.2.1/include/assert.hrl', 1}}, {:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 5}}, {:attribute, 0, :dialyzer, {:no_opaque, {:prop_basic_valid_semver0, 0}}}, {:attribute, 0, :dialyzer, {:no_opaque, {:prop_basic_valid_semver, 0}}}, {:function, 0, :prop_basic_invalid_semver_more2, 0, [{:clause, 0, [], [], [{:call, 0, {:remote, 0, {:atom, 0, :proper}, {:atom, 0, :forall}}, [{:tuple, 0, [{:call, 0, {:atom, 0, :binary}, []}, {:call, 0, {:atom, 0, :binary}, []}, {:call, 0, {:atom, 0, :binary}, []}, {:call, 0, {:atom, 0, :binary}, []}]}, {:fun, 0, {:clauses, [{:clause, 0, [{:tuple, 0, [{:var, 0, :Maj}, {:var, 0, :Min}, {:var, 0, :P}, {:var, 0, :Pre}]}], [], [{:block, 0, [{:match, 0, {:var, 0, :V}, {:bin, 0, [{:bin_element, 98, {:var, 0, :Maj}, :default, [:binary]}, {:bin_element, 98, {:bin, 0, [{:bin_element, 98, {:string, 0, '.'}, :default, :default}]}, :default, [:binary]}, {:bin_element, 98, {:var, 0, :Min}, :default, [:binary]}, {:bin_element, 98, {:bin, 0, [{:bin_element, 98, {:string, 0, '.'}, :default, :default}]}, :default, [:binary]}, {:bin_element, 98, {:var, 0, :P}, :default, [:binary]}, {:bin_element, 98, {:bin, 0, [{:bin_element, 98, {:string, 0, '-'}, :default, :default}]}, :default, [:binary]}, {:bin_element, 99, {:var, 0, :Pre}, :default, [:binary]}]}}, {:op, 0, :\"=:=\", {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :invalid_version}]}, {:call, 0, {:remote, 0, {:atom, 0, :verl}, {:atom, 0, :parse}}, [{:var, 0, :V}]}}]}]}]}}]}]}]}, {:eof, 0}]",
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_invalid_semver_more2",
                       source_chunk:
                         "-file(\"./test/fixtures/prop_verl.erl\", 1).\n\n-module(prop_verl).\n\n-file(\"./test/fixtures/proper.hrl\", 1).\n\n-compile(debug_info).\n\n-file(\"./test/fixtures/proper_common.hrl\", 1).\n\n-file(\"./test/fixtures/proper.hrl\", 32).\n\n-import(proper,\n        [numtests/2, fails/1, on_output/2, conjunction/1]).\n\n-import(proper,\n        [collect/2,\n         collect/3,\n         aggregate/2,\n         aggregate/3,\n         classify/3,\n         measure/3,\n         with_title/1,\n         equals/2]).\n\n-import(proper_types,\n        [integer/2,\n         float/2,\n         atom/0,\n         binary/0,\n         binary/1,\n         bitstring/0,\n         bitstring/1,\n         list/1,\n         vector/2,\n         union/1,\n         weighted_union/1,\n         tuple/1,\n         loose_tuple/1,\n         exactly/1,\n         fixed_list/1,\n         function/2,\n         map/2,\n         any/0]).\n\n-import(proper_types,\n        [integer/0,\n         non_neg_integer/0,\n         pos_integer/0,\n         neg_integer/0,\n         range/2,\n         float/0,\n         non_neg_float/0,\n         number/0,\n         boolean/0,\n         byte/0,\n         char/0,\n         list/0,\n         tuple/0,\n         string/0,\n         wunion/1,\n         term/0,\n         timeout/0,\n         arity/0]).\n\n-import(proper_types,\n        [int/0,\n         nat/0,\n         largeint/0,\n         real/0,\n         bool/0,\n         choose/2,\n         elements/1,\n         oneof/1,\n         frequency/1,\n         return/1,\n         default/2,\n         orderedlist/1,\n         function0/1,\n         function1/1,\n         function2/1,\n         function3/1,\n         function4/1,\n         weighted_default/2,\n         parameter/1,\n         parameter/2,\n         with_parameter/3,\n         with_parameters/2]).\n\n-import(proper_unicode, [utf8/0, utf8/1, utf8/2]).\n\n-import(proper_types,\n        [resize/2, non_empty/1, noshrink/1]).\n\n-import(proper_symb,\n        [eval/1,\n         eval/2,\n         defined/1,\n         well_defined/1,\n         pretty_print/1,\n         pretty_print/2]).\n\n-import(proper_statem,\n        [commands/1,\n         commands/2,\n         parallel_commands/1,\n         parallel_commands/2,\n         more_commands/2]).\n\n-import(proper_statem,\n        [run_commands/2,\n         run_commands/3,\n         state_after/2,\n         command_names/1,\n         zip/2,\n         run_parallel_commands/2,\n         run_parallel_commands/3]).\n\n-import(proper_unused_imports_remover, []).\n\n-compile({parse_transform,\n          proper_unused_imports_remover}).\n\n-compile({parse_transform, proper_transformer}).\n\n-file(\"./test/fixtures/prop_verl.erl\", 4).\n\n-file(\"/nix/store/i2bnhvrvd28jcifmj1w9adk78rifhgd8-e\"\n      \"rlang-24.3.4.5/lib/erlang/lib/stdlib-3.17.2.1\"\n      \"/include/assert.hrl\",\n      1).\n\n-file(\"./test/fixtures/prop_verl.erl\", 5).\n\n-dialyzer({no_opaque, {prop_basic_valid_semver0, 0}}).\n\n-dialyzer({no_opaque, {prop_basic_valid_semver, 0}}).\n\nprop_basic_invalid_semver_more2() ->\n    proper:forall({binary(), binary(), binary(), binary()},\n                  fun ({Maj, Min, P, Pre}) ->\n                          begin\n                              V = <<Maj/binary, <<\".\">>/binary, Min/binary,\n                                    <<\".\">>/binary, P/binary, <<\"-\">>/binary,\n                                    Pre/binary>>,\n                              {error, invalid_version} =:= verl:parse(V)\n                          end\n                  end).\n\n",
                       workspace_root: ""
                     },
                     %Build.Warp.Tricorder.AstSubtree{
                       __unknown_fields__: [],
                       ast:
                         "[{:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 1}}, {:attribute, 0, :module, :prop_verl}, {:attribute, 0, :file, {'./test/fixtures/proper.hrl', 1}}, {:attribute, 0, :compile, :debug_info}, {:attribute, 0, :file, {'./test/fixtures/proper_common.hrl', 1}}, {:attribute, 0, :file, {'./test/fixtures/proper.hrl', 32}}, {:attribute, 0, :import, {:proper, [numtests: 2, fails: 1, on_output: 2, conjunction: 1]}}, {:attribute, 0, :import, {:proper, [collect: 2, collect: 3, aggregate: 2, aggregate: 3, classify: 3, measure: 3, with_title: 1, equals: 2]}}, {:attribute, 0, :import, {:proper_types, [integer: 2, float: 2, atom: 0, binary: 0, binary: 1, bitstring: 0, bitstring: 1, list: 1, vector: 2, union: 1, weighted_union: 1, tuple: 1, loose_tuple: 1, exactly: 1, fixed_list: 1, function: 2, map: 2, any: 0]}}, {:attribute, 0, :import, {:proper_types, [integer: 0, non_neg_integer: 0, pos_integer: 0, neg_integer: 0, range: 2, float: 0, non_neg_float: 0, number: 0, boolean: 0, byte: 0, char: 0, list: 0, tuple: 0, string: 0, wunion: 1, term: 0, timeout: 0, arity: 0]}}, {:attribute, 0, :import, {:proper_types, [int: 0, nat: 0, largeint: 0, real: 0, bool: 0, choose: 2, elements: 1, oneof: 1, frequency: 1, return: 1, default: 2, orderedlist: 1, function0: 1, function1: 1, function2: 1, function3: 1, function4: 1, weighted_default: 2, parameter: 1, parameter: 2, with_parameter: 3, with_parameters: 2]}}, {:attribute, 0, :import, {:proper_unicode, [utf8: 0, utf8: 1, utf8: 2]}}, {:attribute, 0, :import, {:proper_types, [resize: 2, non_empty: 1, noshrink: 1]}}, {:attribute, 0, :import, {:proper_symb, [eval: 1, eval: 2, defined: 1, well_defined: 1, pretty_print: 1, pretty_print: 2]}}, {:attribute, 0, :import, {:proper_statem, [commands: 1, commands: 2, parallel_commands: 1, parallel_commands: 2, more_commands: 2]}}, {:attribute, 0, :import, {:proper_statem, [run_commands: 2, run_commands: 3, state_after: 2, command_names: 1, zip: 2, run_parallel_commands: 2, run_parallel_commands: 3]}}, {:attribute, 0, :import, {:proper_unused_imports_remover, []}}, {:attribute, 0, :compile, {:parse_transform, :proper_unused_imports_remover}}, {:attribute, 0, :compile, {:parse_transform, :proper_transformer}}, {:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 4}}, {:attribute, 0, :file, {'/nix/store/i2bnhvrvd28jcifmj1w9adk78rifhgd8-erlang-24.3.4.5/lib/erlang/lib/stdlib-3.17.2.1/include/assert.hrl', 1}}, {:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 5}}, {:attribute, 0, :dialyzer, {:no_opaque, {:prop_basic_valid_semver0, 0}}}, {:attribute, 0, :dialyzer, {:no_opaque, {:prop_basic_valid_semver, 0}}}, {:function, 0, :prop_basic_invalid_semver_more, 0, [{:clause, 0, [], [], [{:call, 0, {:remote, 0, {:atom, 0, :proper}, {:atom, 0, :forall}}, [{:tuple, 0, [{:call, 0, {:atom, 0, :any}, []}, {:call, 0, {:atom, 0, :any}, []}, {:call, 0, {:atom, 0, :any}, []}]}, {:fun, 0, {:clauses, [{:clause, 0, [{:tuple, 0, [{:var, 0, :Maj}, {:var, 0, :Min}, {:var, 0, :P}]}], [], [{:block, 0, [{:match, 0, {:var, 0, :Major}, {:call, 0, {:atom, 0, :term_to_binary}, [{:var, 0, :Maj}]}}, {:match, 0, {:var, 0, :Minor}, {:call, 0, {:atom, 0, :term_to_binary}, [{:var, 0, :Min}]}}, {:match, 0, {:var, 0, :Patch}, {:call, 0, {:atom, 0, :term_to_binary}, [{:var, 0, :P}]}}, {:match, 0, {:var, 0, :V}, {:bin, 0, [{:bin_element, 87, {:var, 0, :Major}, :default, [:binary]}, {:bin_element, 87, {:bin, 0, [{:bin_element, 87, {:string, 0, '.'}, :default, :default}]}, :default, [:binary]}, {:bin_element, 87, {:var, 0, :Minor}, :default, [:binary]}, {:bin_element, 87, {:bin, 0, [{:bin_element, 87, {:string, 0, '.'}, :default, :default}]}, :default, [:binary]}, {:bin_element, 87, {:var, 0, :Patch}, :default, [:binary]}]}}, {:op, 0, :\"=:=\", {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :invalid_version}]}, {:call, 0, {:remote, 0, {:atom, 0, :verl}, {:atom, 0, :parse}}, [{:var, 0, :V}]}}]}]}]}}]}]}]}, {:eof, 0}]",
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_invalid_semver_more",
                       source_chunk:
                         "-file(\"./test/fixtures/prop_verl.erl\", 1).\n\n-module(prop_verl).\n\n-file(\"./test/fixtures/proper.hrl\", 1).\n\n-compile(debug_info).\n\n-file(\"./test/fixtures/proper_common.hrl\", 1).\n\n-file(\"./test/fixtures/proper.hrl\", 32).\n\n-import(proper,\n        [numtests/2, fails/1, on_output/2, conjunction/1]).\n\n-import(proper,\n        [collect/2,\n         collect/3,\n         aggregate/2,\n         aggregate/3,\n         classify/3,\n         measure/3,\n         with_title/1,\n         equals/2]).\n\n-import(proper_types,\n        [integer/2,\n         float/2,\n         atom/0,\n         binary/0,\n         binary/1,\n         bitstring/0,\n         bitstring/1,\n         list/1,\n         vector/2,\n         union/1,\n         weighted_union/1,\n         tuple/1,\n         loose_tuple/1,\n         exactly/1,\n         fixed_list/1,\n         function/2,\n         map/2,\n         any/0]).\n\n-import(proper_types,\n        [integer/0,\n         non_neg_integer/0,\n         pos_integer/0,\n         neg_integer/0,\n         range/2,\n         float/0,\n         non_neg_float/0,\n         number/0,\n         boolean/0,\n         byte/0,\n         char/0,\n         list/0,\n         tuple/0,\n         string/0,\n         wunion/1,\n         term/0,\n         timeout/0,\n         arity/0]).\n\n-import(proper_types,\n        [int/0,\n         nat/0,\n         largeint/0,\n         real/0,\n         bool/0,\n         choose/2,\n         elements/1,\n         oneof/1,\n         frequency/1,\n         return/1,\n         default/2,\n         orderedlist/1,\n         function0/1,\n         function1/1,\n         function2/1,\n         function3/1,\n         function4/1,\n         weighted_default/2,\n         parameter/1,\n         parameter/2,\n         with_parameter/3,\n         with_parameters/2]).\n\n-import(proper_unicode, [utf8/0, utf8/1, utf8/2]).\n\n-import(proper_types,\n        [resize/2, non_empty/1, noshrink/1]).\n\n-import(proper_symb,\n        [eval/1,\n         eval/2,\n         defined/1,\n         well_defined/1,\n         pretty_print/1,\n         pretty_print/2]).\n\n-import(proper_statem,\n        [commands/1,\n         commands/2,\n         parallel_commands/1,\n         parallel_commands/2,\n         more_commands/2]).\n\n-import(proper_statem,\n        [run_commands/2,\n         run_commands/3,\n         state_after/2,\n         command_names/1,\n         zip/2,\n         run_parallel_commands/2,\n         run_parallel_commands/3]).\n\n-import(proper_unused_imports_remover, []).\n\n-compile({parse_transform,\n          proper_unused_imports_remover}).\n\n-compile({parse_transform, proper_transformer}).\n\n-file(\"./test/fixtures/prop_verl.erl\", 4).\n\n-file(\"/nix/store/i2bnhvrvd28jcifmj1w9adk78rifhgd8-e\"\n      \"rlang-24.3.4.5/lib/erlang/lib/stdlib-3.17.2.1\"\n      \"/include/assert.hrl\",\n      1).\n\n-file(\"./test/fixtures/prop_verl.erl\", 5).\n\n-dialyzer({no_opaque, {prop_basic_valid_semver0, 0}}).\n\n-dialyzer({no_opaque, {prop_basic_valid_semver, 0}}).\n\nprop_basic_invalid_semver_more() ->\n    proper:forall({any(), any(), any()},\n                  fun ({Maj, Min, P}) ->\n                          begin\n                              Major = term_to_binary(Maj),\n                              Minor = term_to_binary(Min),\n                              Patch = term_to_binary(P),\n                              V = <<Major/binary, <<\".\">>/binary, Minor/binary,\n                                    <<\".\">>/binary, Patch/binary>>,\n                              {error, invalid_version} =:= verl:parse(V)\n                          end\n                  end).\n\n",
                       workspace_root: ""
                     },
                     %Build.Warp.Tricorder.AstSubtree{
                       __unknown_fields__: [],
                       ast:
                         "[{:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 1}}, {:attribute, 0, :module, :prop_verl}, {:attribute, 0, :file, {'./test/fixtures/proper.hrl', 1}}, {:attribute, 0, :compile, :debug_info}, {:attribute, 0, :file, {'./test/fixtures/proper_common.hrl', 1}}, {:attribute, 0, :file, {'./test/fixtures/proper.hrl', 32}}, {:attribute, 0, :import, {:proper, [numtests: 2, fails: 1, on_output: 2, conjunction: 1]}}, {:attribute, 0, :import, {:proper, [collect: 2, collect: 3, aggregate: 2, aggregate: 3, classify: 3, measure: 3, with_title: 1, equals: 2]}}, {:attribute, 0, :import, {:proper_types, [integer: 2, float: 2, atom: 0, binary: 0, binary: 1, bitstring: 0, bitstring: 1, list: 1, vector: 2, union: 1, weighted_union: 1, tuple: 1, loose_tuple: 1, exactly: 1, fixed_list: 1, function: 2, map: 2, any: 0]}}, {:attribute, 0, :import, {:proper_types, [integer: 0, non_neg_integer: 0, pos_integer: 0, neg_integer: 0, range: 2, float: 0, non_neg_float: 0, number: 0, boolean: 0, byte: 0, char: 0, list: 0, tuple: 0, string: 0, wunion: 1, term: 0, timeout: 0, arity: 0]}}, {:attribute, 0, :import, {:proper_types, [int: 0, nat: 0, largeint: 0, real: 0, bool: 0, choose: 2, elements: 1, oneof: 1, frequency: 1, return: 1, default: 2, orderedlist: 1, function0: 1, function1: 1, function2: 1, function3: 1, function4: 1, weighted_default: 2, parameter: 1, parameter: 2, with_parameter: 3, with_parameters: 2]}}, {:attribute, 0, :import, {:proper_unicode, [utf8: 0, utf8: 1, utf8: 2]}}, {:attribute, 0, :import, {:proper_types, [resize: 2, non_empty: 1, noshrink: 1]}}, {:attribute, 0, :import, {:proper_symb, [eval: 1, eval: 2, defined: 1, well_defined: 1, pretty_print: 1, pretty_print: 2]}}, {:attribute, 0, :import, {:proper_statem, [commands: 1, commands: 2, parallel_commands: 1, parallel_commands: 2, more_commands: 2]}}, {:attribute, 0, :import, {:proper_statem, [run_commands: 2, run_commands: 3, state_after: 2, command_names: 1, zip: 2, run_parallel_commands: 2, run_parallel_commands: 3]}}, {:attribute, 0, :import, {:proper_unused_imports_remover, []}}, {:attribute, 0, :compile, {:parse_transform, :proper_unused_imports_remover}}, {:attribute, 0, :compile, {:parse_transform, :proper_transformer}}, {:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 4}}, {:attribute, 0, :file, {'/nix/store/i2bnhvrvd28jcifmj1w9adk78rifhgd8-erlang-24.3.4.5/lib/erlang/lib/stdlib-3.17.2.1/include/assert.hrl', 1}}, {:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 5}}, {:attribute, 0, :dialyzer, {:no_opaque, {:prop_basic_valid_semver0, 0}}}, {:attribute, 0, :dialyzer, {:no_opaque, {:prop_basic_valid_semver, 0}}}, {:function, 0, :prop_basic_invalid_semver, 0, [{:clause, 0, [], [], [{:call, 0, {:remote, 0, {:atom, 0, :proper}, {:atom, 0, :forall}}, [{:tuple, 0, [{:call, 0, {:atom, 0, :neg_integer}, []}, {:call, 0, {:atom, 0, :neg_integer}, []}, {:call, 0, {:atom, 0, :neg_integer}, []}]}, {:fun, 0, {:clauses, [{:clause, 0, [{:tuple, 0, [{:var, 0, :Maj}, {:var, 0, :Min}, {:var, 0, :P}]}], [], [{:block, 0, [{:match, 0, {:var, 0, :Major}, {:call, 0, {:atom, 0, :integer_to_binary}, [{:var, 0, :Maj}]}}, {:match, 0, {:var, 0, :Minor}, {:call, 0, {:atom, 0, :integer_to_binary}, [{:var, 0, :Min}]}}, {:match, 0, {:var, 0, :Patch}, {:call, 0, {:atom, 0, :integer_to_binary}, [{:var, 0, :P}]}}, {:match, 0, {:var, 0, :V}, {:bin, 0, [{:bin_element, 74, {:var, 0, :Major}, :default, [:binary]}, {:bin_element, 74, {:bin, 0, [{:bin_element, 74, {:string, 0, '.'}, :default, :default}]}, :default, [:binary]}, {:bin_element, 74, {:var, 0, :Minor}, :default, [:binary]}, {:bin_element, 74, {:bin, 0, [{:bin_element, 74, {:string, 0, '.'}, :default, :default}]}, :default, [:binary]}, {:bin_element, 74, {:var, 0, :Patch}, :default, [:binary]}]}}, {:op, 0, :\"=:=\", {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :invalid_version}]}, {:call, 0, {:remote, 0, {:atom, 0, :verl}, {:atom, 0, :parse}}, [{:var, 0, :V}]}}]}]}]}}]}]}]}, {:eof, 0}]",
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_invalid_semver",
                       source_chunk:
                         "-file(\"./test/fixtures/prop_verl.erl\", 1).\n\n-module(prop_verl).\n\n-file(\"./test/fixtures/proper.hrl\", 1).\n\n-compile(debug_info).\n\n-file(\"./test/fixtures/proper_common.hrl\", 1).\n\n-file(\"./test/fixtures/proper.hrl\", 32).\n\n-import(proper,\n        [numtests/2, fails/1, on_output/2, conjunction/1]).\n\n-import(proper,\n        [collect/2,\n         collect/3,\n         aggregate/2,\n         aggregate/3,\n         classify/3,\n         measure/3,\n         with_title/1,\n         equals/2]).\n\n-import(proper_types,\n        [integer/2,\n         float/2,\n         atom/0,\n         binary/0,\n         binary/1,\n         bitstring/0,\n         bitstring/1,\n         list/1,\n         vector/2,\n         union/1,\n         weighted_union/1,\n         tuple/1,\n         loose_tuple/1,\n         exactly/1,\n         fixed_list/1,\n         function/2,\n         map/2,\n         any/0]).\n\n-import(proper_types,\n        [integer/0,\n         non_neg_integer/0,\n         pos_integer/0,\n         neg_integer/0,\n         range/2,\n         float/0,\n         non_neg_float/0,\n         number/0,\n         boolean/0,\n         byte/0,\n         char/0,\n         list/0,\n         tuple/0,\n         string/0,\n         wunion/1,\n         term/0,\n         timeout/0,\n         arity/0]).\n\n-import(proper_types,\n        [int/0,\n         nat/0,\n         largeint/0,\n         real/0,\n         bool/0,\n         choose/2,\n         elements/1,\n         oneof/1,\n         frequency/1,\n         return/1,\n         default/2,\n         orderedlist/1,\n         function0/1,\n         function1/1,\n         function2/1,\n         function3/1,\n         function4/1,\n         weighted_default/2,\n         parameter/1,\n         parameter/2,\n         with_parameter/3,\n         with_parameters/2]).\n\n-import(proper_unicode, [utf8/0, utf8/1, utf8/2]).\n\n-import(proper_types,\n        [resize/2, non_empty/1, noshrink/1]).\n\n-import(proper_symb,\n        [eval/1,\n         eval/2,\n         defined/1,\n         well_defined/1,\n         pretty_print/1,\n         pretty_print/2]).\n\n-import(proper_statem,\n        [commands/1,\n         commands/2,\n         parallel_commands/1,\n         parallel_commands/2,\n         more_commands/2]).\n\n-import(proper_statem,\n        [run_commands/2,\n         run_commands/3,\n         state_after/2,\n         command_names/1,\n         zip/2,\n         run_parallel_commands/2,\n         run_parallel_commands/3]).\n\n-import(proper_unused_imports_remover, []).\n\n-compile({parse_transform,\n          proper_unused_imports_remover}).\n\n-compile({parse_transform, proper_transformer}).\n\n-file(\"./test/fixtures/prop_verl.erl\", 4).\n\n-file(\"/nix/store/i2bnhvrvd28jcifmj1w9adk78rifhgd8-e\"\n      \"rlang-24.3.4.5/lib/erlang/lib/stdlib-3.17.2.1\"\n      \"/include/assert.hrl\",\n      1).\n\n-file(\"./test/fixtures/prop_verl.erl\", 5).\n\n-dialyzer({no_opaque, {prop_basic_valid_semver0, 0}}).\n\n-dialyzer({no_opaque, {prop_basic_valid_semver, 0}}).\n\nprop_basic_invalid_semver() ->\n    proper:forall({neg_integer(),\n                   neg_integer(),\n                   neg_integer()},\n                  fun ({Maj, Min, P}) ->\n                          begin\n                              Major = integer_to_binary(Maj),\n                              Minor = integer_to_binary(Min),\n                              Patch = integer_to_binary(P),\n                              V = <<Major/binary, <<\".\">>/binary, Minor/binary,\n                                    <<\".\">>/binary, Patch/binary>>,\n                              {error, invalid_version} =:= verl:parse(V)\n                          end\n                  end).\n\n",
                       workspace_root: ""
                     },
                     %Build.Warp.Tricorder.AstSubtree{
                       __unknown_fields__: [],
                       ast:
                         "[{:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 1}}, {:attribute, 0, :module, :prop_verl}, {:attribute, 0, :file, {'./test/fixtures/proper.hrl', 1}}, {:attribute, 0, :compile, :debug_info}, {:attribute, 0, :file, {'./test/fixtures/proper_common.hrl', 1}}, {:attribute, 0, :file, {'./test/fixtures/proper.hrl', 32}}, {:attribute, 0, :import, {:proper, [numtests: 2, fails: 1, on_output: 2, conjunction: 1]}}, {:attribute, 0, :import, {:proper, [collect: 2, collect: 3, aggregate: 2, aggregate: 3, classify: 3, measure: 3, with_title: 1, equals: 2]}}, {:attribute, 0, :import, {:proper_types, [integer: 2, float: 2, atom: 0, binary: 0, binary: 1, bitstring: 0, bitstring: 1, list: 1, vector: 2, union: 1, weighted_union: 1, tuple: 1, loose_tuple: 1, exactly: 1, fixed_list: 1, function: 2, map: 2, any: 0]}}, {:attribute, 0, :import, {:proper_types, [integer: 0, non_neg_integer: 0, pos_integer: 0, neg_integer: 0, range: 2, float: 0, non_neg_float: 0, number: 0, boolean: 0, byte: 0, char: 0, list: 0, tuple: 0, string: 0, wunion: 1, term: 0, timeout: 0, arity: 0]}}, {:attribute, 0, :import, {:proper_types, [int: 0, nat: 0, largeint: 0, real: 0, bool: 0, choose: 2, elements: 1, oneof: 1, frequency: 1, return: 1, default: 2, orderedlist: 1, function0: 1, function1: 1, function2: 1, function3: 1, function4: 1, weighted_default: 2, parameter: 1, parameter: 2, with_parameter: 3, with_parameters: 2]}}, {:attribute, 0, :import, {:proper_unicode, [utf8: 0, utf8: 1, utf8: 2]}}, {:attribute, 0, :import, {:proper_types, [resize: 2, non_empty: 1, noshrink: 1]}}, {:attribute, 0, :import, {:proper_symb, [eval: 1, eval: 2, defined: 1, well_defined: 1, pretty_print: 1, pretty_print: 2]}}, {:attribute, 0, :import, {:proper_statem, [commands: 1, commands: 2, parallel_commands: 1, parallel_commands: 2, more_commands: 2]}}, {:attribute, 0, :import, {:proper_statem, [run_commands: 2, run_commands: 3, state_after: 2, command_names: 1, zip: 2, run_parallel_commands: 2, run_parallel_commands: 3]}}, {:attribute, 0, :import, {:proper_unused_imports_remover, []}}, {:attribute, 0, :compile, {:parse_transform, :proper_unused_imports_remover}}, {:attribute, 0, :compile, {:parse_transform, :proper_transformer}}, {:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 4}}, {:attribute, 0, :file, {'/nix/store/i2bnhvrvd28jcifmj1w9adk78rifhgd8-erlang-24.3.4.5/lib/erlang/lib/stdlib-3.17.2.1/include/assert.hrl', 1}}, {:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 5}}, {:attribute, 0, :dialyzer, {:no_opaque, {:prop_basic_valid_semver0, 0}}}, {:attribute, 0, :dialyzer, {:no_opaque, {:prop_basic_valid_semver, 0}}}, {:function, 0, :prop_basic_valid_semver, 0, [{:clause, 0, [], [], [{:call, 0, {:remote, 0, {:atom, 0, :proper}, {:atom, 0, :forall}}, [{:tuple, 0, [{:call, 0, {:atom, 0, :non_neg_integer}, []}, {:call, 0, {:atom, 0, :non_neg_integer}, []}, {:call, 0, {:atom, 0, :non_neg_integer}, []}]}, {:fun, 0, {:clauses, [{:clause, 0, [{:tuple, 0, [{:var, 0, :Maj}, {:var, 0, :Min}, {:var, 0, :P}]}], [], [{:block, 0, [{:match, 0, {:var, 0, :Major}, {:call, 0, {:atom, 0, :integer_to_binary}, [{:var, 0, :Maj}]}}, {:match, 0, {:var, 0, :Minor}, {:call, 0, {:atom, 0, :integer_to_binary}, [{:var, 0, :Min}]}}, {:match, 0, {:var, 0, :Patch}, {:call, 0, {:atom, 0, :integer_to_binary}, [{:var, 0, :P}]}}, {:match, 0, {:var, 0, :V}, {:bin, 0, [{:bin_element, 59, {:var, 0, :Major}, :default, [:binary]}, {:bin_element, 59, {:bin, 0, [{:bin_element, 59, {:string, 0, '.'}, :default, :default}]}, :default, [:binary]}, {:bin_element, 59, {:var, 0, :Minor}, :default, [:binary]}, {:bin_element, 59, {:bin, 0, [{:bin_element, 59, {:string, 0, '.'}, :default, :default}]}, :default, [:binary]}, {:bin_element, 59, {:var, 0, :Patch}, :default, [:binary]}]}}, {:match, 0, {:tuple, 0, [{:atom, 0, :ok}, {:var, 0, :Parsed}]}, {:call, 0, {:remote, 0, {:atom, 0, :verl}, {:atom, 0, :parse}}, [{:var, 0, :V}]}}, {:match, 0, {:var, 0, :Exp}, {:map, 0, [{:map_field_assoc, 0, {:atom, 0, :build}, {:atom, 0, :undefined}}, {:map_field_assoc, 0, {:atom, 0, :major}, {:var, 0, :Maj}}, {:map_field_assoc, 0, {:atom, 0, :minor}, {:var, 0, :Min}}, {:map_field_assoc, 0, {:atom, 0, :patch}, {:var, 0, :P}}, {:map_field_assoc, 0, {:atom, 0, :pre}, {nil, 0}}]}}, {:op, 0, :\"=:=\", {:var, 0, :Exp}, {:var, 0, :Parsed}}]}]}]}}]}]}]}, {:eof, 0}]",
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_valid_semver",
                       source_chunk:
                         "-file(\"./test/fixtures/prop_verl.erl\", 1).\n\n-module(prop_verl).\n\n-file(\"./test/fixtures/proper.hrl\", 1).\n\n-compile(debug_info).\n\n-file(\"./test/fixtures/proper_common.hrl\", 1).\n\n-file(\"./test/fixtures/proper.hrl\", 32).\n\n-import(proper,\n        [numtests/2, fails/1, on_output/2, conjunction/1]).\n\n-import(proper,\n        [collect/2,\n         collect/3,\n         aggregate/2,\n         aggregate/3,\n         classify/3,\n         measure/3,\n         with_title/1,\n         equals/2]).\n\n-import(proper_types,\n        [integer/2,\n         float/2,\n         atom/0,\n         binary/0,\n         binary/1,\n         bitstring/0,\n         bitstring/1,\n         list/1,\n         vector/2,\n         union/1,\n         weighted_union/1,\n         tuple/1,\n         loose_tuple/1,\n         exactly/1,\n         fixed_list/1,\n         function/2,\n         map/2,\n         any/0]).\n\n-import(proper_types,\n        [integer/0,\n         non_neg_integer/0,\n         pos_integer/0,\n         neg_integer/0,\n         range/2,\n         float/0,\n         non_neg_float/0,\n         number/0,\n         boolean/0,\n         byte/0,\n         char/0,\n         list/0,\n         tuple/0,\n         string/0,\n         wunion/1,\n         term/0,\n         timeout/0,\n         arity/0]).\n\n-import(proper_types,\n        [int/0,\n         nat/0,\n         largeint/0,\n         real/0,\n         bool/0,\n         choose/2,\n         elements/1,\n         oneof/1,\n         frequency/1,\n         return/1,\n         default/2,\n         orderedlist/1,\n         function0/1,\n         function1/1,\n         function2/1,\n         function3/1,\n         function4/1,\n         weighted_default/2,\n         parameter/1,\n         parameter/2,\n         with_parameter/3,\n         with_parameters/2]).\n\n-import(proper_unicode, [utf8/0, utf8/1, utf8/2]).\n\n-import(proper_types,\n        [resize/2, non_empty/1, noshrink/1]).\n\n-import(proper_symb,\n        [eval/1,\n         eval/2,\n         defined/1,\n         well_defined/1,\n         pretty_print/1,\n         pretty_print/2]).\n\n-import(proper_statem,\n        [commands/1,\n         commands/2,\n         parallel_commands/1,\n         parallel_commands/2,\n         more_commands/2]).\n\n-import(proper_statem,\n        [run_commands/2,\n         run_commands/3,\n         state_after/2,\n         command_names/1,\n         zip/2,\n         run_parallel_commands/2,\n         run_parallel_commands/3]).\n\n-import(proper_unused_imports_remover, []).\n\n-compile({parse_transform,\n          proper_unused_imports_remover}).\n\n-compile({parse_transform, proper_transformer}).\n\n-file(\"./test/fixtures/prop_verl.erl\", 4).\n\n-file(\"/nix/store/i2bnhvrvd28jcifmj1w9adk78rifhgd8-e\"\n      \"rlang-24.3.4.5/lib/erlang/lib/stdlib-3.17.2.1\"\n      \"/include/assert.hrl\",\n      1).\n\n-file(\"./test/fixtures/prop_verl.erl\", 5).\n\n-dialyzer({no_opaque, {prop_basic_valid_semver0, 0}}).\n\n-dialyzer({no_opaque, {prop_basic_valid_semver, 0}}).\n\nprop_basic_valid_semver() ->\n    proper:forall({non_neg_integer(),\n                   non_neg_integer(),\n                   non_neg_integer()},\n                  fun ({Maj, Min, P}) ->\n                          begin\n                              Major = integer_to_binary(Maj),\n                              Minor = integer_to_binary(Min),\n                              Patch = integer_to_binary(P),\n                              V = <<Major/binary, <<\".\">>/binary, Minor/binary,\n                                    <<\".\">>/binary, Patch/binary>>,\n                              {ok, Parsed} = verl:parse(V),\n                              Exp = \#{build => undefined, major => Maj,\n                                      minor => Min, patch => P, pre => []},\n                              Exp =:= Parsed\n                          end\n                  end).\n\n",
                       workspace_root: ""
                     },
                     %Build.Warp.Tricorder.AstSubtree{
                       __unknown_fields__: [],
                       ast:
                         "[{:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 1}}, {:attribute, 0, :module, :prop_verl}, {:attribute, 0, :file, {'./test/fixtures/proper.hrl', 1}}, {:attribute, 0, :compile, :debug_info}, {:attribute, 0, :file, {'./test/fixtures/proper_common.hrl', 1}}, {:attribute, 0, :file, {'./test/fixtures/proper.hrl', 32}}, {:attribute, 0, :import, {:proper, [numtests: 2, fails: 1, on_output: 2, conjunction: 1]}}, {:attribute, 0, :import, {:proper, [collect: 2, collect: 3, aggregate: 2, aggregate: 3, classify: 3, measure: 3, with_title: 1, equals: 2]}}, {:attribute, 0, :import, {:proper_types, [integer: 2, float: 2, atom: 0, binary: 0, binary: 1, bitstring: 0, bitstring: 1, list: 1, vector: 2, union: 1, weighted_union: 1, tuple: 1, loose_tuple: 1, exactly: 1, fixed_list: 1, function: 2, map: 2, any: 0]}}, {:attribute, 0, :import, {:proper_types, [integer: 0, non_neg_integer: 0, pos_integer: 0, neg_integer: 0, range: 2, float: 0, non_neg_float: 0, number: 0, boolean: 0, byte: 0, char: 0, list: 0, tuple: 0, string: 0, wunion: 1, term: 0, timeout: 0, arity: 0]}}, {:attribute, 0, :import, {:proper_types, [int: 0, nat: 0, largeint: 0, real: 0, bool: 0, choose: 2, elements: 1, oneof: 1, frequency: 1, return: 1, default: 2, orderedlist: 1, function0: 1, function1: 1, function2: 1, function3: 1, function4: 1, weighted_default: 2, parameter: 1, parameter: 2, with_parameter: 3, with_parameters: 2]}}, {:attribute, 0, :import, {:proper_unicode, [utf8: 0, utf8: 1, utf8: 2]}}, {:attribute, 0, :import, {:proper_types, [resize: 2, non_empty: 1, noshrink: 1]}}, {:attribute, 0, :import, {:proper_symb, [eval: 1, eval: 2, defined: 1, well_defined: 1, pretty_print: 1, pretty_print: 2]}}, {:attribute, 0, :import, {:proper_statem, [commands: 1, commands: 2, parallel_commands: 1, parallel_commands: 2, more_commands: 2]}}, {:attribute, 0, :import, {:proper_statem, [run_commands: 2, run_commands: 3, state_after: 2, command_names: 1, zip: 2, run_parallel_commands: 2, run_parallel_commands: 3]}}, {:attribute, 0, :import, {:proper_unused_imports_remover, []}}, {:attribute, 0, :compile, {:parse_transform, :proper_unused_imports_remover}}, {:attribute, 0, :compile, {:parse_transform, :proper_transformer}}, {:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 4}}, {:attribute, 0, :file, {'/nix/store/i2bnhvrvd28jcifmj1w9adk78rifhgd8-erlang-24.3.4.5/lib/erlang/lib/stdlib-3.17.2.1/include/assert.hrl', 1}}, {:attribute, 0, :file, {'./test/fixtures/prop_verl.erl', 5}}, {:attribute, 0, :dialyzer, {:no_opaque, {:prop_basic_valid_semver0, 0}}}, {:function, 0, :prop_basic_valid_semver0, 0, [{:clause, 0, [], [], [{:call, 0, {:remote, 0, {:atom, 0, :proper}, {:atom, 0, :forall}}, [{:tuple, 0, [{:call, 0, {:atom, 0, :non_neg_integer}, []}, {:call, 0, {:atom, 0, :non_neg_integer}, []}, {:call, 0, {:atom, 0, :non_neg_integer}, []}, {:call, 0, {:atom, 0, :non_empty}, [{:call, 0, {:atom, 0, :binary}, []}]}]}, {:fun, 0, {:clauses, [{:clause, 0, [{:tuple, 0, [{:var, 0, :Maj}, {:var, 0, :Min}, {:var, 0, :P}, {:var, 0, :Pre}]}], [], [{:block, 0, [{:match, 0, {:var, 0, :Major}, {:call, 0, {:atom, 0, :integer_to_binary}, [{:var, 0, :Maj}]}}, {:match, 0, {:var, 0, :Minor}, {:call, 0, {:atom, 0, :integer_to_binary}, [{:var, 0, :Min}]}}, {:match, 0, {:var, 0, :Patch}, {:call, 0, {:atom, 0, :integer_to_binary}, [{:var, 0, :P}]}}, {:match, 0, {:var, 0, :V}, {:bin, 0, [{:bin_element, 21, {:var, 0, :Major}, :default, [:binary]}, {:bin_element, 21, {:bin, 0, [{:bin_element, 21, {:string, 0, '.'}, :default, :default}]}, :default, [:binary]}, {:bin_element, 21, {:var, 0, :Minor}, :default, [:binary]}, {:bin_element, 21, {:bin, 0, [{:bin_element, 21, {:string, 0, '.'}, :default, :default}]}, :default, [:binary]}, {:bin_element, 21, {:var, 0, :Patch}, :default, [:binary]}, {:bin_element, 22, {:bin, 0, [{:bin_element, 22, {:string, 0, '-'}, :default, :default}]}, :default, [:binary]}, {:bin_element, 22, {:var, 0, :Pre}, :default, [:binary]}]}}, {:case, 0, {:tuple, 0, [{:call, 0, {:remote, 0, {:atom, 0, :re}, {:atom, 0, :run}}, [{:var, 0, :Pre}, {:string, 0, '^[0-9A-Za-z-+]+$'}]}, {:call, 0, {:remote, 0, {:atom, 0, :re}, {:atom, 0, :run}}, [{:var, 0, :Pre}, {:string, 0, '(^0[0-9]+)|(^[+]$)|[\\r\\n]'}]}]}, [{:clause, 0, [{:tuple, 0, [{:atom, 0, :nomatch}, {:var, 0, :_}]}], [], [{:op, 0, :\"=:=\", {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :invalid_version}]}, {:call, 0, {:remote, 0, {:atom, 0, :verl}, {:atom, 0, :parse}}, [{:var, 0, :V}]}}]}, {:clause, 0, [{:tuple, 0, [{:var, 0, :_}, {:tuple, 0, [{:atom, 0, :match}, {:var, 0, :_}]}]}], [], [{:op, 0, :\"=:=\", {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :invalid_version}]}, {:call, 0, {:remote, 0, {:atom, 0, :verl}, {:atom, 0, :parse}}, [{:var, 0, :V}]}}]}, {:clause, 0, [{:var, 0, :_}], [], [{:match, 0, {:tuple, 0, [{:atom, 0, :ok}, {:var, 0, :Parsed}]}, {:call, 0, {:remote, 0, {:atom, 0, :verl}, {:atom, 0, :parse}}, [{:var, 0, :V}]}}, {:match, 0, {:var, 0, :PreEl}, {:case, 0, {:call, 0, {:remote, 0, {:atom, 0, :re}, {:atom, 0, :run}}, [{:var, 0, :Pre}, {:string, 0, '^[0-9]+$'}]}, [{:clause, 0, [{:atom, 0, :nomatch}], [], [{:cons, 0, {:var, 0, :Pre}, {nil, 0}}]}, {:clause, 0, [{:var, 0, :_}], [], [{:cons, 0, {:call, 0, {:atom, 0, :binary_to_integer}, [{:var, 0, :Pre}]}, {nil, 0}}]}]}}, {:match, 0, {:var, 0, :Exp}, {:map, 0, [{:map_field_assoc, 0, {:atom, 0, :build}, {:atom, 0, :undefined}}, {:map_field_assoc, 0, {:atom, 0, :major}, {:var, 0, :Maj}}, {:map_field_assoc, 0, {:atom, 0, :minor}, {:var, 0, :Min}}, {:map_field_assoc, 0, {:atom, 0, :patch}, {:var, 0, :P}}, {:map_field_assoc, 0, {:atom, 0, :pre}, {:var, 0, :PreEl}}]}}, {:op, 0, :\"=:=\", {:var, 0, :Exp}, {:var, 0, :Parsed}}]}]}]}]}]}}]}]}]}, {:attribute, 0, :dialyzer, {:no_opaque, {:prop_basic_valid_semver, 0}}}, {:eof, 0}]",
                       file: "./test/fixtures/prop_verl.erl",
                       signature_name: "prop_basic_valid_semver0",
                       source_chunk:
                         "-file(\"./test/fixtures/prop_verl.erl\", 1).\n\n-module(prop_verl).\n\n-file(\"./test/fixtures/proper.hrl\", 1).\n\n-compile(debug_info).\n\n-file(\"./test/fixtures/proper_common.hrl\", 1).\n\n-file(\"./test/fixtures/proper.hrl\", 32).\n\n-import(proper,\n        [numtests/2, fails/1, on_output/2, conjunction/1]).\n\n-import(proper,\n        [collect/2,\n         collect/3,\n         aggregate/2,\n         aggregate/3,\n         classify/3,\n         measure/3,\n         with_title/1,\n         equals/2]).\n\n-import(proper_types,\n        [integer/2,\n         float/2,\n         atom/0,\n         binary/0,\n         binary/1,\n         bitstring/0,\n         bitstring/1,\n         list/1,\n         vector/2,\n         union/1,\n         weighted_union/1,\n         tuple/1,\n         loose_tuple/1,\n         exactly/1,\n         fixed_list/1,\n         function/2,\n         map/2,\n         any/0]).\n\n-import(proper_types,\n        [integer/0,\n         non_neg_integer/0,\n         pos_integer/0,\n         neg_integer/0,\n         range/2,\n         float/0,\n         non_neg_float/0,\n         number/0,\n         boolean/0,\n         byte/0,\n         char/0,\n         list/0,\n         tuple/0,\n         string/0,\n         wunion/1,\n         term/0,\n         timeout/0,\n         arity/0]).\n\n-import(proper_types,\n        [int/0,\n         nat/0,\n         largeint/0,\n         real/0,\n         bool/0,\n         choose/2,\n         elements/1,\n         oneof/1,\n         frequency/1,\n         return/1,\n         default/2,\n         orderedlist/1,\n         function0/1,\n         function1/1,\n         function2/1,\n         function3/1,\n         function4/1,\n         weighted_default/2,\n         parameter/1,\n         parameter/2,\n         with_parameter/3,\n         with_parameters/2]).\n\n-import(proper_unicode, [utf8/0, utf8/1, utf8/2]).\n\n-import(proper_types,\n        [resize/2, non_empty/1, noshrink/1]).\n\n-import(proper_symb,\n        [eval/1,\n         eval/2,\n         defined/1,\n         well_defined/1,\n         pretty_print/1,\n         pretty_print/2]).\n\n-import(proper_statem,\n        [commands/1,\n         commands/2,\n         parallel_commands/1,\n         parallel_commands/2,\n         more_commands/2]).\n\n-import(proper_statem,\n        [run_commands/2,\n         run_commands/3,\n         state_after/2,\n         command_names/1,\n         zip/2,\n         run_parallel_commands/2,\n         run_parallel_commands/3]).\n\n-import(proper_unused_imports_remover, []).\n\n-compile({parse_transform,\n          proper_unused_imports_remover}).\n\n-compile({parse_transform, proper_transformer}).\n\n-file(\"./test/fixtures/prop_verl.erl\", 4).\n\n-file(\"/nix/store/i2bnhvrvd28jcifmj1w9adk78rifhgd8-e\"\n      \"rlang-24.3.4.5/lib/erlang/lib/stdlib-3.17.2.1\"\n      \"/include/assert.hrl\",\n      1).\n\n-file(\"./test/fixtures/prop_verl.erl\", 5).\n\n-dialyzer({no_opaque, {prop_basic_valid_semver0, 0}}).\n\nprop_basic_valid_semver0() ->\n    proper:forall({non_neg_integer(),\n                   non_neg_integer(),\n                   non_neg_integer(),\n                   non_empty(binary())},\n                  fun ({Maj, Min, P, Pre}) ->\n                          begin\n                              Major = integer_to_binary(Maj),\n                              Minor = integer_to_binary(Min),\n                              Patch = integer_to_binary(P),\n                              V = <<Major/binary, <<\".\">>/binary, Minor/binary,\n                                    <<\".\">>/binary, Patch/binary,\n                                    <<\"-\">>/binary, Pre/binary>>,\n                              case {re:run(Pre, \"^[0-9A-Za-z-+]+$\"),\n                                    re:run(Pre, \"(^0[0-9]+)|(^[+]$)|[\\r\\n]\")}\n                                  of\n                                  {nomatch, _} ->\n                                      {error, invalid_version} =:=\n                                          verl:parse(V);\n                                  {_, {match, _}} ->\n                                      {error, invalid_version} =:=\n                                          verl:parse(V);\n                                  _ ->\n                                      {ok, Parsed} = verl:parse(V),\n                                      PreEl = case re:run(Pre, \"^[0-9]+$\") of\n                                                  nomatch -> [Pre];\n                                                  _ -> [binary_to_integer(Pre)]\n                                              end,\n                                      Exp = \#{build => undefined, major => Maj,\n                                              minor => Min, patch => P,\n                                              pre => PreEl},\n                                      Exp =:= Parsed\n                              end\n                          end\n                  end).\n\n-dialyzer({no_opaque, {prop_basic_valid_semver, 0}}).\n\n",
                       workspace_root: ""
                     }
                   ]
                 }}
            }} = Build.Warp.Tricorder.TricorderService.Stub.get_ast(channel, req)
  end
end
