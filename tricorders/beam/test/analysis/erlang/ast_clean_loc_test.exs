defmodule Tricorder.Analysis.Erlang.AstCleanLocTest do
  use ExUnit.Case
  require Logger

  alias Tricorder.Analysis.Erlang.Ast
  alias Tricorder.Analysis.Erlang.Ast.CleanLoc

  test "zeroes all position information" do
    {:ok, ast} = Ast.parse("./test/fixtures/tree_split_stress.erl", [])

    clean_tree = CleanLoc.clean(ast)

    assert [
             {:attribute, 0, :file, {'./test/fixtures/tree_split_stress.erl', 1}},
             {:attribute, 0, :module, :tree_splitter_stress},
             {:attribute, 0, :file, {_, 1}},
             {:attribute, 0, :file, {'./test/fixtures/tree_split_stress.erl', 23}},
             {:attribute, 0, :export,
              [
                start: 3,
                start: 4,
                start_link: 3,
                start_link: 4,
                start_monitor: 3,
                start_monitor: 4,
                stop: 1,
                stop: 3,
                cast: 2,
                call: 2,
                call: 3,
                send_request: 2,
                send_request: 4,
                wait_response: 1,
                wait_response: 2,
                wait_response: 3,
                receive_response: 1,
                receive_response: 2,
                receive_response: 3,
                check_response: 2,
                check_response: 3,
                reqids_new: 0,
                reqids_size: 1,
                reqids_add: 3,
                reqids_to_list: 1,
                enter_loop: 4,
                enter_loop: 5,
                enter_loop: 6,
                reply: 1,
                reply: 2
              ]},
             {:attribute, 0, :export, [init_it: 6]},
             {:attribute, 0, :export,
              [
                system_continue: 3,
                system_terminate: 4,
                system_code_change: 4,
                system_get_state: 1,
                system_replace_state: 2,
                format_status: 2
              ]},
             {:attribute, 0, :export, [wakeup_from_hibernate: 3]},
             {:attribute, 0, :export, [format_log: 1, format_log: 2]},
             {:attribute, 0, :export_type,
              [
                event_type: 0,
                from: 0,
                reply_tag: 0,
                callback_mode_result: 0,
                init_result: 1,
                init_result: 2,
                state_enter_result: 1,
                state_enter_result: 2,
                event_handler_result: 1,
                event_handler_result: 2,
                reply_action: 0,
                enter_action: 0,
                action: 0,
                request_id: 0,
                request_id_collection: 0,
                format_status: 0
              ]},
             {:attribute, 0, :export_type, [state_function_result: 0, handle_event_result: 0]},
             {:attribute, 0, :export_type, [transition_option: 0]},
             {:attribute, 0, :export_type,
              [
                server_name: 0,
                server_ref: 0,
                start_opt: 0,
                enter_loop_opt: 0,
                start_ret: 0,
                start_mon_ret: 0
              ]},
             {:attribute, 0, :type,
              {:from,
               {:type, 108, :tuple,
                [
                  {:ann_type, 108, [{:var, 108, :To}, {:type, 108, :pid, []}]},
                  {:ann_type, 108, [{:var, 108, :Tag}, {:user_type, 108, :reply_tag, []}]}
                ]}, []}},
             {:attribute, 0, :opaque,
              {:reply_tag,
               {:remote_type, 109, [{:atom, 109, :gen}, {:atom, 109, :reply_tag}, []]}, []}},
             {:attribute, 0, :type,
              {:state,
               {:type, 112, :union,
                [{:user_type, 112, :state_name, []}, {:type, 113, :term, []}]}, []}},
             {:attribute, 0, :type, {:state_name, {:type, 115, :atom, []}, []}},
             {:attribute, 0, :type, {:data, {:type, 117, :term, []}, []}},
             {:attribute, 0, :type,
              {:event_type,
               {:type, 120, :union,
                [
                  {:user_type, 120, :external_event_type, []},
                  {:user_type, 120, :timeout_event_type, []},
                  {:atom, 120, :internal}
                ]}, []}},
             {:attribute, 0, :type,
              {:external_event_type,
               {:type, 122, :union,
                [
                  {:type, 122, :tuple,
                   [
                     {:atom, 122, :call},
                     {:ann_type, 122, [{:var, 122, :From}, {:user_type, 122, :from, []}]}
                   ]},
                  {:atom, 122, :cast},
                  {:atom, 122, :info}
                ]}, []}},
             {:attribute, 0, :type,
              {:timeout_event_type,
               {:type, 124, :union,
                [
                  {:atom, 124, :timeout},
                  {:type, 124, :tuple,
                   [
                     {:atom, 124, :timeout},
                     {:ann_type, 124, [{:var, 124, :Name}, {:type, 124, :term, []}]}
                   ]},
                  {:atom, 124, :state_timeout}
                ]}, []}},
             {:attribute, 0, :type, {:event_content, {:type, 126, :term, []}, []}},
             {:attribute, 0, :type,
              {:callback_mode_result,
               {:type, 129, :union,
                [
                  {:user_type, 129, :callback_mode, []},
                  {:type, 129, :list,
                   [
                     {:type, 129, :union,
                      [
                        {:user_type, 129, :callback_mode, []},
                        {:user_type, 129, :state_enter, []}
                      ]}
                   ]}
                ]}, []}},
             {:attribute, 0, :type,
              {:callback_mode,
               {:type, 130, :union,
                [{:atom, 130, :state_functions}, {:atom, 130, :handle_event_function}]}, []}},
             {:attribute, 0, :type, {:state_enter, {:atom, 131, :state_enter}, []}},
             {:attribute, 0, :type,
              {:transition_option,
               {:type, 134, :union,
                [
                  {:user_type, 134, :postpone, []},
                  {:user_type, 134, :hibernate, []},
                  {:user_type, 135, :event_timeout, []},
                  {:user_type, 135, :generic_timeout, []},
                  {:user_type, 135, :state_timeout, []}
                ]}, []}},
             {:attribute, 0, :type, {:postpone, {:type, 139, :boolean, []}, []}},
             {:attribute, 0, :type, {:hibernate, {:type, 142, :boolean, []}, []}},
             {:attribute, 0, :type,
              {:event_timeout,
               {:ann_type, 146,
                [
                  {:var, 146, :Time},
                  {:type, 146, :union, [{:type, 146, :timeout, []}, {:type, 146, :integer, []}]}
                ]}, []}},
             {:attribute, 0, :type,
              {:generic_timeout,
               {:ann_type, 149,
                [
                  {:var, 149, :Time},
                  {:type, 149, :union, [{:type, 149, :timeout, []}, {:type, 149, :integer, []}]}
                ]}, []}},
             {:attribute, 0, :type,
              {:state_timeout,
               {:ann_type, 153,
                [
                  {:var, 153, :Time},
                  {:type, 153, :union, [{:type, 153, :timeout, []}, {:type, 153, :integer, []}]}
                ]}, []}},
             {:attribute, 0, :type,
              {:timeout_option,
               {:type, 154, :tuple,
                [
                  {:atom, 154, :abs},
                  {:ann_type, 154, [{:var, 154, :Abs}, {:type, 154, :boolean, []}]}
                ]}, []}},
             {:attribute, 0, :type,
              {:action,
               {:type, 171, :union,
                [
                  {:atom, 171, :postpone},
                  {:type, 172, :tuple,
                   [
                     {:atom, 172, :postpone},
                     {:ann_type, 172, [{:var, 172, :Postpone}, {:user_type, 172, :postpone, []}]}
                   ]},
                  {:type, 177, :tuple,
                   [
                     {:atom, 177, :next_event},
                     {:ann_type, 178,
                      [{:var, 178, :EventType}, {:user_type, 178, :event_type, []}]},
                     {:ann_type, 179,
                      [{:var, 179, :EventContent}, {:user_type, 179, :event_content, []}]}
                   ]},
                  {:type, 180, :tuple,
                   [
                     {:atom, 180, :change_callback_module},
                     {:ann_type, 180, [{:var, 180, :NewModule}, {:type, 180, :module, []}]}
                   ]},
                  {:type, 181, :tuple,
                   [
                     {:atom, 181, :push_callback_module},
                     {:ann_type, 181, [{:var, 181, :NewModule}, {:type, 181, :module, []}]}
                   ]},
                  {:atom, 182, :pop_callback_module},
                  {:user_type, 183, :enter_action, []}
                ]}, []}},
             {:attribute, 0, :type,
              {:enter_action,
               {:type, 185, :union,
                [
                  {:atom, 185, :hibernate},
                  {:type, 186, :tuple,
                   [
                     {:atom, 186, :hibernate},
                     {:ann_type, 186,
                      [{:var, 186, :Hibernate}, {:user_type, 186, :hibernate, []}]}
                   ]},
                  {:user_type, 187, :timeout_action, []},
                  {:user_type, 188, :reply_action, []}
                ]}, []}},
             {:attribute, 0, :type,
              {:timeout_action,
               {:type, 190, :union,
                [
                  {:ann_type, 190, [{:var, 190, :Time}, {:user_type, 190, :event_timeout, []}]},
                  {:type, 191, :tuple,
                   [
                     {:atom, 191, :timeout},
                     {:ann_type, 192,
                      [{:var, 192, :Time}, {:user_type, 192, :event_timeout, []}]},
                     {:ann_type, 192,
                      [{:var, 192, :EventContent}, {:user_type, 192, :event_content, []}]}
                   ]},
                  {:type, 193, :tuple,
                   [
                     {:atom, 193, :timeout},
                     {:ann_type, 194,
                      [{:var, 194, :Time}, {:user_type, 194, :event_timeout, []}]},
                     {:ann_type, 195,
                      [{:var, 195, :EventContent}, {:user_type, 195, :event_content, []}]},
                     {:ann_type, 196,
                      [
                        {:var, 196, :Options},
                        {:type, 196, :union,
                         [
                           {:user_type, 196, :timeout_option, []},
                           {:type, 196, :list, [{:user_type, 196, :timeout_option, []}]}
                         ]}
                      ]}
                   ]},
                  {:type, 198, :tuple,
                   [
                     {:type, 198, :tuple,
                      [
                        {:atom, 198, :timeout},
                        {:ann_type, 198, [{:var, 198, :Name}, {:type, 198, :term, []}]}
                      ]},
                     {:ann_type, 199,
                      [{:var, 199, :Time}, {:user_type, 199, :generic_timeout, []}]},
                     {:ann_type, 199,
                      [{:var, 199, :EventContent}, {:user_type, 199, :event_content, []}]}
                   ]},
                  {:type, 200, :tuple,
                   [
                     {:type, 200, :tuple,
                      [
                        {:atom, 200, :timeout},
                        {:ann_type, 200, [{:var, 200, :Name}, {:type, 200, :term, []}]}
                      ]},
                     {:ann_type, 201,
                      [{:var, 201, :Time}, {:user_type, 201, :generic_timeout, []}]},
                     {:ann_type, 202,
                      [{:var, 202, :EventContent}, {:user_type, 202, :event_content, []}]},
                     {:ann_type, 203,
                      [
                        {:var, 203, :Options},
                        {:type, 203, :union,
                         [
                           {:user_type, 203, :timeout_option, []},
                           {:type, 203, :list, [{:user_type, 203, :timeout_option, []}]}
                         ]}
                      ]}
                   ]},
                  {:type, 205, :tuple,
                   [
                     {:atom, 205, :state_timeout},
                     {:ann_type, 206,
                      [{:var, 206, :Time}, {:user_type, 206, :state_timeout, []}]},
                     {:ann_type, 206,
                      [{:var, 206, :EventContent}, {:user_type, 206, :event_content, []}]}
                   ]},
                  {:type, 207, :tuple,
                   [
                     {:atom, 207, :state_timeout},
                     {:ann_type, 208,
                      [{:var, 208, :Time}, {:user_type, 208, :state_timeout, []}]},
                     {:ann_type, 209,
                      [{:var, 209, :EventContent}, {:user_type, 209, :event_content, []}]},
                     {:ann_type, 210,
                      [
                        {:var, 210, :Options},
                        {:type, 210, :union,
                         [
                           {:user_type, 210, :timeout_option, []},
                           {:type, 210, :list, [{:user_type, 210, :timeout_option, []}]}
                         ]}
                      ]}
                   ]},
                  {:user_type, 211, :timeout_cancel_action, []},
                  {:user_type, 212, :timeout_update_action, []}
                ]}, []}},
             {:attribute, 0, :type,
              {:timeout_cancel_action,
               {:type, 214, :union,
                [
                  {:type, 214, :tuple, [{:atom, 214, :timeout}, {:atom, 214, :cancel}]},
                  {:type, 215, :tuple,
                   [
                     {:type, 215, :tuple,
                      [
                        {:atom, 215, :timeout},
                        {:ann_type, 215, [{:var, 215, :Name}, {:type, 215, :term, []}]}
                      ]},
                     {:atom, 215, :cancel}
                   ]},
                  {:type, 216, :tuple, [{:atom, 216, :state_timeout}, {:atom, 216, :cancel}]}
                ]}, []}},
             {:attribute, 0, :type,
              {:timeout_update_action,
               {:type, 218, :union,
                [
                  {:type, 218, :tuple,
                   [
                     {:atom, 218, :timeout},
                     {:atom, 218, :update},
                     {:ann_type, 218,
                      [{:var, 218, :EventContent}, {:user_type, 218, :event_content, []}]}
                   ]},
                  {:type, 219, :tuple,
                   [
                     {:type, 219, :tuple,
                      [
                        {:atom, 219, :timeout},
                        {:ann_type, 219, [{:var, 219, :Name}, {:type, 219, :term, []}]}
                      ]},
                     {:atom, 220, :update},
                     {:ann_type, 220,
                      [{:var, 220, :EventContent}, {:user_type, 220, :event_content, []}]}
                   ]},
                  {:type, 221, :tuple,
                   [
                     {:atom, 221, :state_timeout},
                     {:atom, 221, :update},
                     {:ann_type, 221,
                      [{:var, 221, :EventContent}, {:user_type, 221, :event_content, []}]}
                   ]}
                ]}, []}},
             {:attribute, 0, :type,
              {:reply_action,
               {:type, 223, :tuple,
                [
                  {:atom, 223, :reply},
                  {:ann_type, 224, [{:var, 224, :From}, {:user_type, 224, :from, []}]},
                  {:ann_type, 224, [{:var, 224, :Reply}, {:type, 224, :term, []}]}
                ]}, []}},
             {:attribute, 0, :type,
              {:init_result,
               {:user_type, 226, :init_result,
                [{:var, 226, :StateType}, {:type, 226, :term, []}]}, [{:var, 226, :StateType}]}},
             {:attribute, 0, :type,
              {:init_result,
               {:type, 228, :union,
                [
                  {:type, 228, :tuple,
                   [
                     {:atom, 228, :ok},
                     {:ann_type, 228, [{:var, 228, :State}, {:var, 228, :StateType}]},
                     {:ann_type, 228, [{:var, 228, :Data}, {:var, 228, :DataType}]}
                   ]},
                  {:type, 229, :tuple,
                   [
                     {:atom, 229, :ok},
                     {:ann_type, 229, [{:var, 229, :State}, {:var, 229, :StateType}]},
                     {:ann_type, 229, [{:var, 229, :Data}, {:var, 229, :DataType}]},
                     {:ann_type, 230,
                      [
                        {:var, 230, :Actions},
                        {:type, 230, :union,
                         [
                           {:type, 230, :list, [{:user_type, 230, :action, []}]},
                           {:user_type, 230, :action, []}
                         ]}
                      ]}
                   ]},
                  {:atom, 231, :ignore},
                  {:type, 232, :tuple,
                   [
                     {:atom, 232, :stop},
                     {:ann_type, 232, [{:var, 232, :Reason}, {:type, 232, :term, []}]}
                   ]},
                  {:type, 233, :tuple,
                   [
                     {:atom, 233, :error},
                     {:ann_type, 233, [{:var, 233, :Reason}, {:type, 233, :term, []}]}
                   ]}
                ]}, [{:var, 227, :StateType}, {:var, 227, :DataType}]}},
             {:attribute, 0, :type,
              {:state_function_result,
               {:user_type, 237, :event_handler_result, [{:user_type, 237, :state_name, []}]},
               []}},
             {:attribute, 0, :type,
              {:handle_event_result,
               {:user_type, 239, :event_handler_result, [{:user_type, 239, :state, []}]}, []}},
             {:attribute, 0, :type,
              {:state_enter_result,
               {:user_type, 241, :state_enter_result,
                [{:var, 241, :State}, {:type, 241, :term, []}]}, [{:var, 241, :State}]}},
             {:attribute, 0, :type,
              {:state_enter_result,
               {:type, 243, :union,
                [
                  {:type, 243, :tuple,
                   [
                     {:atom, 243, :next_state},
                     {:var, 244, :State},
                     {:ann_type, 245, [{:var, 245, :NewData}, {:var, 245, :DataType}]}
                   ]},
                  {:type, 246, :tuple,
                   [
                     {:atom, 246, :next_state},
                     {:var, 247, :State},
                     {:ann_type, 248, [{:var, 248, :NewData}, {:var, 248, :DataType}]},
                     {:ann_type, 249,
                      [
                        {:var, 249, :Actions},
                        {:type, 249, :union,
                         [
                           {:type, 249, :list, [{:user_type, 249, :enter_action, []}]},
                           {:user_type, 249, :enter_action, []}
                         ]}
                      ]}
                   ]},
                  {:user_type, 250, :state_callback_result,
                   [{:user_type, 250, :enter_action, []}]}
                ]}, [{:var, 242, :State}, {:var, 242, :DataType}]}},
             {:attribute, 0, :type,
              {:event_handler_result,
               {:user_type, 252, :event_handler_result,
                [{:var, 252, :StateType}, {:type, 252, :term, []}]}, [{:var, 251, :StateType}]}},
             {:attribute, 0, :type,
              {:event_handler_result,
               {:type, 254, :union,
                [
                  {:type, 254, :tuple,
                   [
                     {:atom, 254, :next_state},
                     {:ann_type, 255, [{:var, 255, :NextState}, {:var, 255, :StateType}]},
                     {:ann_type, 256, [{:var, 256, :NewData}, {:var, 256, :DataType}]}
                   ]},
                  {:type, 257, :tuple,
                   [
                     {:atom, 257, :next_state},
                     {:ann_type, 258, [{:var, 258, :NextState}, {:var, 258, :StateType}]},
                     {:ann_type, 259, [{:var, 259, :NewData}, {:var, 259, :DataType}]},
                     {:ann_type, 260,
                      [
                        {:var, 260, :Actions},
                        {:type, 260, :union,
                         [
                           {:type, 260, :list, [{:user_type, 260, :action, []}]},
                           {:user_type, 260, :action, []}
                         ]}
                      ]}
                   ]},
                  {:user_type, 261, :state_callback_result, [{:user_type, 261, :action, []}]}
                ]}, [{:var, 253, :StateType}, {:var, 253, :DataType}]}},
             {:attribute, 0, :type,
              {:state_callback_result,
               {:user_type, 263, :state_callback_result,
                [{:var, 263, :ActionType}, {:type, 263, :term, []}]},
               [{:var, 262, :ActionType}]}},
             {:attribute, 0, :type,
              {:state_callback_result,
               {:type, 265, :union,
                [
                  {:type, 265, :tuple,
                   [
                     {:atom, 265, :keep_state},
                     {:ann_type, 266, [{:var, 266, :NewData}, {:var, 266, :DataType}]}
                   ]},
                  {:type, 267, :tuple,
                   [
                     {:atom, 267, :keep_state},
                     {:ann_type, 268, [{:var, 268, :NewData}, {:var, 268, :DataType}]},
                     {:ann_type, 269,
                      [
                        {:var, 269, :Actions},
                        {:type, 269, :union,
                         [
                           {:type, 269, :list, [{:var, 269, :ActionType}]},
                           {:var, 269, :ActionType}
                         ]}
                      ]}
                   ]},
                  {:atom, 270, :keep_state_and_data},
                  {:type, 271, :tuple,
                   [
                     {:atom, 271, :keep_state_and_data},
                     {:ann_type, 272,
                      [
                        {:var, 272, :Actions},
                        {:type, 272, :union,
                         [
                           {:type, 272, :list, [{:var, 272, :ActionType}]},
                           {:var, 272, :ActionType}
                         ]}
                      ]}
                   ]},
                  {:type, 274, :tuple,
                   [
                     {:atom, 274, :repeat_state},
                     {:ann_type, 275, [{:var, 275, :NewData}, {:var, 275, :DataType}]}
                   ]},
                  {:type, 276, :tuple,
                   [
                     {:atom, 276, :repeat_state},
                     {:ann_type, 277, [{:var, 277, :NewData}, {:var, 277, :DataType}]},
                     {:ann_type, 278,
                      [
                        {:var, 278, :Actions},
                        {:type, 278, :union,
                         [
                           {:type, 278, :list, [{:var, 278, :ActionType}]},
                           {:var, 278, :ActionType}
                         ]}
                      ]}
                   ]},
                  {:atom, 279, :repeat_state_and_data},
                  {:type, 280, :tuple,
                   [
                     {:atom, 280, :repeat_state_and_data},
                     {:ann_type, 281,
                      [
                        {:var, 281, :Actions},
                        {:type, 281, :union,
                         [
                           {:type, 281, :list, [{:var, 281, :ActionType}]},
                           {:var, 281, :ActionType}
                         ]}
                      ]}
                   ]},
                  {:atom, 283, :stop},
                  {:type, 284, :tuple,
                   [
                     {:atom, 284, :stop},
                     {:ann_type, 285, [{:var, 285, :Reason}, {:type, 285, :term, []}]}
                   ]},
                  {:type, 286, :tuple,
                   [
                     {:atom, 286, :stop},
                     {:ann_type, 287, [{:var, 287, :Reason}, {:type, 287, :term, []}]},
                     {:ann_type, 288, [{:var, 288, :NewData}, {:var, 288, :DataType}]}
                   ]},
                  {:type, 290, :tuple,
                   [
                     {:atom, 290, :stop_and_reply},
                     {:ann_type, 291, [{:var, 291, :Reason}, {:type, 291, :term, []}]},
                     {:ann_type, 292,
                      [
                        {:var, 292, :Replies},
                        {:type, 292, :union,
                         [
                           {:type, 292, :list, [{:user_type, 292, :reply_action, []}]},
                           {:user_type, 292, :reply_action, []}
                         ]}
                      ]}
                   ]},
                  {:type, 293, :tuple,
                   [
                     {:atom, 293, :stop_and_reply},
                     {:ann_type, 294, [{:var, 294, :Reason}, {:type, 294, :term, []}]},
                     {:ann_type, 295,
                      [
                        {:var, 295, :Replies},
                        {:type, 295, :union,
                         [
                           {:type, 295, :list, [{:user_type, 295, :reply_action, []}]},
                           {:user_type, 295, :reply_action, []}
                         ]}
                      ]},
                     {:ann_type, 296, [{:var, 296, :NewData}, {:var, 296, :DataType}]}
                   ]}
                ]}, [{:var, 264, :ActionType}, {:var, 264, :DataType}]}},
             {:attribute, 0, :opaque,
              {:request_id,
               {:remote_type, 298, [{:atom, 298, :gen}, {:atom, 298, :request_id}, []]}, []}},
             {:attribute, 0, :opaque,
              {:request_id_collection,
               {:remote_type, 300,
                [{:atom, 300, :gen}, {:atom, 300, :request_id_collection}, []]}, []}},
             {:attribute, 0, :type,
              {:response_timeout,
               {:type, 303, :union,
                [
                  {:type, 303, :timeout, []},
                  {:type, 303, :tuple, [{:atom, 303, :abs}, {:type, 303, :integer, []}]}
                ]}, []}},
             {:attribute, 0, :callback,
              {{:init, 1},
               [
                 {:type, 309, :fun,
                  [
                    {:type, 309, :product,
                     [{:ann_type, 309, [{:var, 309, :Args}, {:type, 309, :term, []}]}]},
                    {:user_type, 309, :init_result, [{:user_type, 309, :state, []}]}
                  ]}
               ]}},
             {:attribute, 0, :callback,
              {{:callback_mode, 0},
               [
                 {:type, 315, :fun,
                  [
                    {:type, 315, :product, []},
                    {:user_type, 315, :callback_mode_result, []}
                  ]}
               ]}},
             {:attribute, 0, :callback,
              {{:state_name, 3},
               [
                 {:type, 325, :fun,
                  [
                    {:type, 325, :product,
                     [
                       {:atom, 326, :enter},
                       {:ann_type, 327,
                        [{:var, 327, :OldStateName}, {:user_type, 327, :state_name, []}]},
                       {:user_type, 328, :data, []}
                     ]},
                    {:user_type, 329, :state_enter_result, [{:atom, 329, :state_name}]}
                  ]},
                 {:type, 330, :fun,
                  [
                    {:type, 330, :product,
                     [
                       {:user_type, 330, :event_type, []},
                       {:user_type, 331, :event_content, []},
                       {:user_type, 332, :data, []}
                     ]},
                    {:user_type, 333, :event_handler_result, [{:user_type, 333, :state_name, []}]}
                  ]}
               ]}},
             {:attribute, 0, :callback,
              {{:handle_event, 4},
               [
                 {:type, 337, :fun,
                  [
                    {:type, 337, :product,
                     [
                       {:atom, 338, :enter},
                       {:ann_type, 339, [{:var, 339, :OldState}, {:user_type, 339, :state, []}]},
                       {:var, 340, :CurrentState},
                       {:user_type, 341, :data, []}
                     ]},
                    {:user_type, 342, :state_enter_result, [{:var, 342, :CurrentState}]}
                  ]},
                 {:type, 343, :fun,
                  [
                    {:type, 343, :product,
                     [
                       {:user_type, 343, :event_type, []},
                       {:user_type, 344, :event_content, []},
                       {:ann_type, 345,
                        [{:var, 345, :CurrentState}, {:user_type, 345, :state, []}]},
                       {:user_type, 346, :data, []}
                     ]},
                    {:user_type, 347, :event_handler_result, [{:user_type, 347, :state, []}]}
                  ]}
               ]}},
             {:attribute, 0, :callback,
              {{:terminate, 3},
               [
                 {:type, 350, :fun,
                  [
                    {:type, 350, :product,
                     [
                       {:ann_type, 351,
                        [
                          {:var, 351, :Reason},
                          {:type, 351, :union,
                           [
                             {:atom, 351, :normal},
                             {:atom, 351, :shutdown},
                             {:type, 351, :tuple,
                              [{:atom, 351, :shutdown}, {:type, 351, :term, []}]},
                             {:type, 352, :term, []}
                           ]}
                        ]},
                       {:ann_type, 353,
                        [{:var, 353, :CurrentState}, {:user_type, 353, :state, []}]},
                       {:user_type, 354, :data, []}
                     ]},
                    {:type, 355, :any, []}
                  ]}
               ]}},
             {:attribute, 0, :callback,
              {{:code_change, 4},
               [
                 {:type, 360, :fun,
                  [
                    {:type, 360, :product,
                     [
                       {:ann_type, 361,
                        [
                          {:var, 361, :OldVsn},
                          {:type, 361, :union,
                           [
                             {:type, 361, :term, []},
                             {:type, 361, :tuple, [{:atom, 361, :down}, {:type, 361, :term, []}]}
                           ]}
                        ]},
                       {:ann_type, 362, [{:var, 362, :OldState}, {:user_type, 362, :state, []}]},
                       {:ann_type, 363, [{:var, 363, :OldData}, {:user_type, 363, :data, []}]},
                       {:ann_type, 364, [{:var, 364, :Extra}, {:type, 364, :term, []}]}
                     ]},
                    {:type, 365, :union,
                     [
                       {:type, 365, :tuple,
                        [
                          {:atom, 365, :ok},
                          {:ann_type, 365,
                           [{:var, 365, :NewState}, {:user_type, 365, :state, []}]},
                          {:ann_type, 365, [{:var, 365, :NewData}, {:user_type, 365, :data, []}]}
                        ]},
                       {:ann_type, 366, [{:var, 366, :Reason}, {:type, 366, :term, []}]}
                     ]}
                  ]}
               ]}},
             {:attribute, 0, :callback,
              {{:format_status, 2},
               [
                 {:type, 374, :bounded_fun,
                  [
                    {:type, 374, :fun,
                     [
                       {:type, 374, :product,
                        [
                          {:var, 375, :StatusOption},
                          {:type, 376, :list,
                           [
                             {:type, 376, :union,
                              [
                                {:type, 376, :list,
                                 [
                                   {:type, 376, :tuple,
                                    [
                                      {:ann_type, 376,
                                       [{:var, 376, :Key}, {:type, 376, :term, []}]},
                                      {:ann_type, 376,
                                       [{:var, 376, :Value}, {:type, 376, :term, []}]}
                                    ]}
                                 ]},
                                {:user_type, 377, :state, []},
                                {:user_type, 378, :data, []}
                              ]}
                           ]}
                        ]},
                       {:ann_type, 379, [{:var, 379, :Status}, {:type, 379, :term, []}]}
                     ]},
                    [
                      {:type, 380, :constraint,
                       [
                         {:atom, 380, :is_subtype},
                         [
                           {:var, 380, :StatusOption},
                           {:type, 380, :union, [{:atom, 380, :normal}, {:atom, 380, :terminate}]}
                         ]
                       ]}
                    ]
                  ]}
               ]}},
             {:attribute, 0, :type,
              {:format_status,
               {:type, 383, :map,
                [
                  {:type, 383, :map_field_assoc,
                   [{:atom, 383, :state}, {:user_type, 383, :state, []}]},
                  {:type, 384, :map_field_assoc,
                   [{:atom, 384, :data}, {:user_type, 384, :data, []}]},
                  {:type, 385, :map_field_assoc,
                   [{:atom, 385, :reason}, {:type, 385, :term, []}]},
                  {:type, 386, :map_field_assoc,
                   [
                     {:atom, 386, :queue},
                     {:type, 386, :list,
                      [
                        {:type, 386, :tuple,
                         [
                           {:user_type, 386, :event_type, []},
                           {:user_type, 386, :event_content, []}
                         ]}
                      ]}
                   ]},
                  {:type, 387, :map_field_assoc,
                   [
                     {:atom, 387, :postponed},
                     {:type, 387, :list,
                      [
                        {:type, 387, :tuple,
                         [
                           {:user_type, 387, :event_type, []},
                           {:user_type, 387, :event_content, []}
                         ]}
                      ]}
                   ]},
                  {:type, 388, :map_field_assoc,
                   [
                     {:atom, 388, :timeouts},
                     {:type, 388, :list,
                      [
                        {:type, 388, :tuple,
                         [
                           {:user_type, 388, :timeout_event_type, []},
                           {:user_type, 388, :event_content, []}
                         ]}
                      ]}
                   ]},
                  {:type, 389, :map_field_assoc,
                   [
                     {:atom, 389, :log},
                     {:type, 389, :list,
                      [
                        {:remote_type, 389, [{:atom, 389, :sys}, {:atom, 389, :system_event}, []]}
                      ]}
                   ]}
                ]}, []}},
             {:attribute, 0, :callback,
              {{:format_status, 1},
               [
                 {:type, 393, :bounded_fun,
                  [
                    {:type, 393, :fun,
                     [
                       {:type, 393, :product, [{:var, 393, :Status}]},
                       {:var, 393, :NewStatus}
                     ]},
                    [
                      {:type, 394, :constraint,
                       [
                         {:atom, 394, :is_subtype},
                         [{:var, 394, :Status}, {:user_type, 394, :format_status, []}]
                       ]},
                      {:type, 395, :constraint,
                       [
                         {:atom, 395, :is_subtype},
                         [{:var, 395, :NewStatus}, {:user_type, 395, :format_status, []}]
                       ]}
                    ]
                  ]}
               ]}},
             {:attribute, 0, :optional_callbacks,
              [
                format_status: 1,
                format_status: 2,
                terminate: 3,
                code_change: 4,
                state_name: 3,
                handle_event: 4
              ]},
             {:attribute, 0, :compile,
              {:inline,
               [
                 callback_mode: 1,
                 state_enter: 1,
                 event_type: 1,
                 from: 1,
                 timeout_event_type: 1
               ]}},
             {:function, 0, :callback_mode, 1,
              [
                {:clause, 0, [{:var, 0, :CallbackMode}], [],
                 [
                   {:case, 0, {:var, 0, :CallbackMode},
                    [
                      {:clause, 0, [{:atom, 0, :state_functions}], [], [{:atom, 0, true}]},
                      {:clause, 0, [{:atom, 0, :handle_event_function}], [], [{:atom, 0, true}]},
                      {:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, false}]}
                    ]}
                 ]}
              ]},
             {:function, 0, :state_enter, 1,
              [
                {:clause, 0, [{:var, 0, :StateEnter}], [],
                 [
                   {:case, 0, {:var, 0, :StateEnter},
                    [
                      {:clause, 0, [{:atom, 0, :state_enter}], [], [{:atom, 0, true}]},
                      {:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, false}]}
                    ]}
                 ]}
              ]},
             {:function, 0, :timeout_event_type, 1,
              [
                {:clause, 0, [{:var, 0, :Type}], [],
                 [
                   {:case, 0, {:var, 0, :Type},
                    [
                      {:clause, 0, [{:atom, 0, :timeout}], [], [{:atom, 0, true}]},
                      {:clause, 0, [{:atom, 0, :state_timeout}], [], [{:atom, 0, true}]},
                      {:clause, 0, [{:tuple, 0, [{:atom, 0, :timeout}, {:var, 0, :_}]}], [],
                       [{:atom, 0, true}]},
                      {:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, false}]}
                    ]}
                 ]}
              ]},
             {:function, 0, :from, 1,
              [
                {:clause, 0, [{:var, 0, :From}], [],
                 [
                   {:case, 0, {:var, 0, :From},
                    [
                      {:clause, 0, [{:tuple, 0, [{:var, 0, :_}, {:var, 0, :_}]}],
                       [
                         [
                           {:call, 0, {:atom, 0, :is_pid},
                            [
                              {:call, 0, {:atom, 0, :element},
                               [{:integer, 0, 1}, {:var, 0, :From}]}
                            ]}
                         ]
                       ], [{:atom, 0, true}]},
                      {:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, false}]}
                    ]}
                 ]}
              ]},
             {:function, 0, :event_type, 1,
              [
                {:clause, 0, [{:var, 0, :Type}], [],
                 [
                   {:case, 0, {:var, 0, :Type},
                    [
                      {:clause, 0, [{:tuple, 0, [{:atom, 0, :call}, {:var, 0, :From}]}], [],
                       [
                         {:case, 0, {:var, 0, :From},
                          [
                            {:clause, 0, [{:tuple, 0, [{:var, 0, :_}, {:var, 0, :_}]}],
                             [
                               [
                                 {:call, 0, {:atom, 0, :is_pid},
                                  [
                                    {:call, 0, {:atom, 0, :element},
                                     [{:integer, 0, 1}, {:var, 0, :From}]}
                                  ]}
                               ]
                             ], [{:atom, 0, true}]},
                            {:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, false}]}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, :cast}], [], [{:atom, 0, true}]},
                      {:clause, 0, [{:atom, 0, :info}], [], [{:atom, 0, true}]},
                      {:clause, 0, [{:atom, 0, :internal}], [], [{:atom, 0, true}]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:case, 0, {:var, 0, :Type},
                          [
                            {:clause, 0, [{:atom, 0, :timeout}], [], [{:atom, 0, true}]},
                            {:clause, 0, [{:atom, 0, :state_timeout}], [], [{:atom, 0, true}]},
                            {:clause, 0, [{:tuple, 0, [{:atom, 0, :timeout}, {:var, 0, :_}]}], [],
                             [{:atom, 0, true}]},
                            {:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, false}]}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :record,
              {:params,
               [
                 {:typed_record_field,
                  {:record_field, 495, {:atom, 495, :callback_mode},
                   {:atom, 495, :state_functions}}, {:user_type, 495, :callback_mode, []}},
                 {:typed_record_field,
                  {:record_field, 496, {:atom, 496, :state_enter}, {:atom, 496, false}},
                  {:type, 496, :boolean, []}},
                 {:typed_record_field, {:record_field, 497, {:atom, 497, :parent}},
                  {:type, 497, :pid, []}},
                 {:typed_record_field,
                  {:record_field, 498, {:atom, 498, :modules},
                   {:cons, 498, {:atom, 498, :tree_splitter_stress}, {nil, 498}}},
                  {:type, 498, :nonempty_list, [{:type, 498, :module, []}]}},
                 {:typed_record_field, {:record_field, 499, {:atom, 499, :name}},
                  {:type, 499, :union, [{:type, 499, :atom, []}, {:type, 499, :pid, []}]}},
                 {:typed_record_field,
                  {:record_field, 500, {:atom, 500, :hibernate_after}, {:atom, 500, :infinity}},
                  {:type, 500, :timeout, []}}
               ]}},
             {:attribute, 0, :record,
              {:state,
               [
                 {:typed_record_field,
                  {:record_field, 504, {:atom, 504, :state_data},
                   {:tuple, 504, [{:atom, 504, :undefined}, {:atom, 504, :undefined}]}},
                  {:type, 505, :tuple,
                   [
                     {:ann_type, 505, [{:var, 505, :State}, {:type, 505, :term, []}]},
                     {:ann_type, 505, [{:var, 505, :Data}, {:type, 505, :term, []}]}
                   ]}},
                 {:typed_record_field, {:record_field, 506, {:atom, 506, :postponed}, {nil, 506}},
                  {:type, 506, :list,
                   [
                     {:type, 506, :tuple,
                      [
                        {:user_type, 506, :event_type, []},
                        {:user_type, 506, :event_content, []}
                      ]}
                   ]}},
                 {:typed_record_field,
                  {:record_field, 507, {:atom, 507, :timers},
                   {:map, 507, [{:map_field_assoc, 507, {:atom, 507, :t0q}, {nil, 507}}]}},
                  {:type, 508, :map,
                   [
                     {:type, 513, :map_field_exact,
                      [
                        {:atom, 513, :t0q},
                        {:type, 513, :list, [{:user_type, 513, :timeout_event_type, []}]}
                      ]},
                     {:type, 515, :map_field_assoc,
                      [
                        {:ann_type, 515,
                         [
                           {:var, 515, :TimeoutType},
                           {:user_type, 515, :timeout_event_type, []}
                         ]},
                        {:type, 516, :tuple,
                         [
                           {:ann_type, 516,
                            [
                              {:var, 516, :TimerRef},
                              {:type, 516, :union,
                               [{:type, 516, :reference, []}, {:integer, 516, 0}]}
                            ]},
                           {:ann_type, 517,
                            [
                              {:var, 517, :TimeoutMsg},
                              {:user_type, 517, :event_content, []}
                            ]}
                         ]}
                      ]}
                   ]}},
                 {:typed_record_field,
                  {:record_field, 518, {:atom, 518, :hibernate}, {:atom, 518, false}},
                  {:type, 518, :boolean, []}}
               ]}},
             {:attribute, 0, :type,
              {:server_name,
               {:type, 525, :union,
                [
                  {:type, 525, :tuple, [{:atom, 525, :local}, {:type, 525, :atom, []}]},
                  {:type, 526, :tuple,
                   [
                     {:atom, 526, :global},
                     {:ann_type, 526, [{:var, 526, :GlobalName}, {:type, 526, :term, []}]}
                   ]},
                  {:type, 527, :tuple,
                   [
                     {:atom, 527, :via},
                     {:ann_type, 527, [{:var, 527, :RegMod}, {:type, 527, :module, []}]},
                     {:ann_type, 527, [{:var, 527, :Name}, {:type, 527, :term, []}]}
                   ]}
                ]}, []}},
             {:attribute, 0, :type,
              {:server_ref,
               {:type, 530, :union,
                [
                  {:type, 530, :pid, []},
                  {:ann_type, 531, [{:var, 531, :LocalName}, {:type, 531, :atom, []}]},
                  {:type, 532, :tuple,
                   [
                     {:ann_type, 532, [{:var, 532, :Name}, {:type, 532, :atom, []}]},
                     {:ann_type, 532, [{:var, 532, :Node}, {:type, 532, :atom, []}]}
                   ]},
                  {:type, 533, :tuple,
                   [
                     {:atom, 533, :global},
                     {:ann_type, 533, [{:var, 533, :GlobalName}, {:type, 533, :term, []}]}
                   ]},
                  {:type, 534, :tuple,
                   [
                     {:atom, 534, :via},
                     {:ann_type, 534, [{:var, 534, :RegMod}, {:type, 534, :module, []}]},
                     {:ann_type, 534, [{:var, 534, :ViaName}, {:type, 534, :term, []}]}
                   ]}
                ]}, []}},
             {:attribute, 0, :type,
              {:start_opt,
               {:type, 537, :union,
                [
                  {:type, 537, :tuple,
                   [
                     {:atom, 537, :timeout},
                     {:ann_type, 537, [{:var, 537, :Time}, {:type, 537, :timeout, []}]}
                   ]},
                  {:type, 538, :tuple,
                   [
                     {:atom, 538, :spawn_opt},
                     {:type, 538, :list,
                      [
                        {:remote_type, 538,
                         [{:atom, 538, :proc_lib}, {:atom, 538, :spawn_option}, []]}
                      ]}
                   ]},
                  {:user_type, 539, :enter_loop_opt, []}
                ]}, []}},
             {:attribute, 0, :type,
              {:enter_loop_opt,
               {:type, 542, :union,
                [
                  {:type, 542, :tuple,
                   [
                     {:atom, 542, :hibernate_after},
                     {:ann_type, 542,
                      [{:var, 542, :HibernateAfterTimeout}, {:type, 542, :timeout, []}]}
                   ]},
                  {:type, 543, :tuple,
                   [
                     {:atom, 543, :debug},
                     {:ann_type, 543,
                      [
                        {:var, 543, :Dbgs},
                        {:type, 543, :list,
                         [
                           {:remote_type, 543,
                            [{:atom, 543, :sys}, {:atom, 543, :debug_option}, []]}
                         ]}
                      ]}
                   ]}
                ]}, []}},
             {:attribute, 0, :type,
              {:start_ret,
               {:type, 546, :union,
                [
                  {:type, 546, :tuple, [{:atom, 546, :ok}, {:type, 546, :pid, []}]},
                  {:atom, 547, :ignore},
                  {:type, 548, :tuple, [{:atom, 548, :error}, {:type, 548, :term, []}]}
                ]}, []}},
             {:attribute, 0, :type,
              {:start_mon_ret,
               {:type, 551, :union,
                [
                  {:type, 551, :tuple,
                   [
                     {:atom, 551, :ok},
                     {:type, 551, :tuple, [{:type, 551, :pid, []}, {:type, 551, :reference, []}]}
                   ]},
                  {:atom, 552, :ignore},
                  {:type, 553, :tuple, [{:atom, 553, :error}, {:type, 553, :term, []}]}
                ]}, []}},
             {:attribute, 0, :spec,
              {{:start, 3},
               [
                 {:type, 559, :fun,
                  [
                    {:type, 559, :product,
                     [
                       {:ann_type, 560, [{:var, 560, :Module}, {:type, 560, :module, []}]},
                       {:ann_type, 560, [{:var, 560, :Args}, {:type, 560, :term, []}]},
                       {:ann_type, 560,
                        [
                          {:var, 560, :Opts},
                          {:type, 560, :list, [{:user_type, 560, :start_opt, []}]}
                        ]}
                     ]},
                    {:user_type, 561, :start_ret, []}
                  ]}
               ]}},
             {:function, 0, :start, 3,
              [
                {:clause, 0, [{:var, 0, :Module}, {:var, 0, :Args}, {:var, 0, :Opts}], [],
                 [
                   {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :start}},
                    [
                      {:atom, 0, :tree_splitter_stress},
                      {:atom, 0, :nolink},
                      {:var, 0, :Module},
                      {:var, 0, :Args},
                      {:var, 0, :Opts}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:start, 4},
               [
                 {:type, 565, :fun,
                  [
                    {:type, 565, :product,
                     [
                       {:ann_type, 566,
                        [{:var, 566, :ServerName}, {:user_type, 566, :server_name, []}]},
                       {:ann_type, 567, [{:var, 567, :Module}, {:type, 567, :module, []}]},
                       {:ann_type, 567, [{:var, 567, :Args}, {:type, 567, :term, []}]},
                       {:ann_type, 567,
                        [
                          {:var, 567, :Opts},
                          {:type, 567, :list, [{:user_type, 567, :start_opt, []}]}
                        ]}
                     ]},
                    {:user_type, 568, :start_ret, []}
                  ]}
               ]}},
             {:function, 0, :start, 4,
              [
                {:clause, 0,
                 [
                   {:var, 0, :ServerName},
                   {:var, 0, :Module},
                   {:var, 0, :Args},
                   {:var, 0, :Opts}
                 ], [],
                 [
                   {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :start}},
                    [
                      {:atom, 0, :tree_splitter_stress},
                      {:atom, 0, :nolink},
                      {:var, 0, :ServerName},
                      {:var, 0, :Module},
                      {:var, 0, :Args},
                      {:var, 0, :Opts}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:start_link, 3},
               [
                 {:type, 573, :fun,
                  [
                    {:type, 573, :product,
                     [
                       {:ann_type, 574, [{:var, 574, :Module}, {:type, 574, :module, []}]},
                       {:ann_type, 574, [{:var, 574, :Args}, {:type, 574, :term, []}]},
                       {:ann_type, 574,
                        [
                          {:var, 574, :Opts},
                          {:type, 574, :list, [{:user_type, 574, :start_opt, []}]}
                        ]}
                     ]},
                    {:user_type, 575, :start_ret, []}
                  ]}
               ]}},
             {:function, 0, :start_link, 3,
              [
                {:clause, 0, [{:var, 0, :Module}, {:var, 0, :Args}, {:var, 0, :Opts}], [],
                 [
                   {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :start}},
                    [
                      {:atom, 0, :tree_splitter_stress},
                      {:atom, 0, :link},
                      {:var, 0, :Module},
                      {:var, 0, :Args},
                      {:var, 0, :Opts}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:start_link, 4},
               [
                 {:type, 579, :fun,
                  [
                    {:type, 579, :product,
                     [
                       {:ann_type, 580,
                        [{:var, 580, :ServerName}, {:user_type, 580, :server_name, []}]},
                       {:ann_type, 581, [{:var, 581, :Module}, {:type, 581, :module, []}]},
                       {:ann_type, 581, [{:var, 581, :Args}, {:type, 581, :term, []}]},
                       {:ann_type, 581,
                        [
                          {:var, 581, :Opts},
                          {:type, 581, :list, [{:user_type, 581, :start_opt, []}]}
                        ]}
                     ]},
                    {:user_type, 582, :start_ret, []}
                  ]}
               ]}},
             {:function, 0, :start_link, 4,
              [
                {:clause, 0,
                 [
                   {:var, 0, :ServerName},
                   {:var, 0, :Module},
                   {:var, 0, :Args},
                   {:var, 0, :Opts}
                 ], [],
                 [
                   {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :start}},
                    [
                      {:atom, 0, :tree_splitter_stress},
                      {:atom, 0, :link},
                      {:var, 0, :ServerName},
                      {:var, 0, :Module},
                      {:var, 0, :Args},
                      {:var, 0, :Opts}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:start_monitor, 3},
               [
                 {:type, 587, :fun,
                  [
                    {:type, 587, :product,
                     [
                       {:ann_type, 588, [{:var, 588, :Module}, {:type, 588, :module, []}]},
                       {:ann_type, 588, [{:var, 588, :Args}, {:type, 588, :term, []}]},
                       {:ann_type, 588,
                        [
                          {:var, 588, :Opts},
                          {:type, 588, :list, [{:user_type, 588, :start_opt, []}]}
                        ]}
                     ]},
                    {:user_type, 589, :start_mon_ret, []}
                  ]}
               ]}},
             {:function, 0, :start_monitor, 3,
              [
                {:clause, 0, [{:var, 0, :Module}, {:var, 0, :Args}, {:var, 0, :Opts}], [],
                 [
                   {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :start}},
                    [
                      {:atom, 0, :tree_splitter_stress},
                      {:atom, 0, :monitor},
                      {:var, 0, :Module},
                      {:var, 0, :Args},
                      {:var, 0, :Opts}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:start_monitor, 4},
               [
                 {:type, 593, :fun,
                  [
                    {:type, 593, :product,
                     [
                       {:ann_type, 594,
                        [{:var, 594, :ServerName}, {:user_type, 594, :server_name, []}]},
                       {:ann_type, 595, [{:var, 595, :Module}, {:type, 595, :module, []}]},
                       {:ann_type, 595, [{:var, 595, :Args}, {:type, 595, :term, []}]},
                       {:ann_type, 595,
                        [
                          {:var, 595, :Opts},
                          {:type, 595, :list, [{:user_type, 595, :start_opt, []}]}
                        ]}
                     ]},
                    {:user_type, 596, :start_mon_ret, []}
                  ]}
               ]}},
             {:function, 0, :start_monitor, 4,
              [
                {:clause, 0,
                 [
                   {:var, 0, :ServerName},
                   {:var, 0, :Module},
                   {:var, 0, :Args},
                   {:var, 0, :Opts}
                 ], [],
                 [
                   {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :start}},
                    [
                      {:atom, 0, :tree_splitter_stress},
                      {:atom, 0, :monitor},
                      {:var, 0, :ServerName},
                      {:var, 0, :Module},
                      {:var, 0, :Args},
                      {:var, 0, :Opts}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:stop, 1},
               [
                 {:type, 601, :fun,
                  [
                    {:type, 601, :product,
                     [
                       {:ann_type, 601,
                        [{:var, 601, :ServerRef}, {:user_type, 601, :server_ref, []}]}
                     ]},
                    {:atom, 601, :ok}
                  ]}
               ]}},
             {:function, 0, :stop, 1,
              [
                {:clause, 0, [{:var, 0, :ServerRef}], [],
                 [
                   {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :stop}},
                    [{:var, 0, :ServerRef}]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:stop, 3},
               [
                 {:type, 605, :fun,
                  [
                    {:type, 605, :product,
                     [
                       {:ann_type, 606,
                        [{:var, 606, :ServerRef}, {:user_type, 606, :server_ref, []}]},
                       {:ann_type, 607, [{:var, 607, :Reason}, {:type, 607, :term, []}]},
                       {:ann_type, 608, [{:var, 608, :Timeout}, {:type, 608, :timeout, []}]}
                     ]},
                    {:atom, 608, :ok}
                  ]}
               ]}},
             {:function, 0, :stop, 3,
              [
                {:clause, 0, [{:var, 0, :ServerRef}, {:var, 0, :Reason}, {:var, 0, :Timeout}], [],
                 [
                   {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :stop}},
                    [{:var, 0, :ServerRef}, {:var, 0, :Reason}, {:var, 0, :Timeout}]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:cast, 2},
               [
                 {:type, 613, :fun,
                  [
                    {:type, 613, :product,
                     [
                       {:ann_type, 613,
                        [{:var, 613, :ServerRef}, {:user_type, 613, :server_ref, []}]},
                       {:ann_type, 613, [{:var, 613, :Msg}, {:type, 613, :term, []}]}
                     ]},
                    {:atom, 613, :ok}
                  ]}
               ]}},
             {:function, 0, :cast, 2,
              [
                {:clause, 0, [{:var, 0, :ServerRef}, {:var, 0, :Msg}],
                 [[{:call, 0, {:atom, 0, :is_pid}, [{:var, 0, :ServerRef}]}]],
                 [
                   {:call, 0, {:atom, 0, :send},
                    [
                      {:var, 0, :ServerRef},
                      {:call, 0, {:atom, 0, :wrap_cast}, [{:var, 0, :Msg}]}
                    ]}
                 ]},
                {:clause, 0, [{:var, 0, :ServerRef}, {:var, 0, :Msg}],
                 [[{:call, 0, {:atom, 0, :is_atom}, [{:var, 0, :ServerRef}]}]],
                 [
                   {:call, 0, {:atom, 0, :send},
                    [
                      {:var, 0, :ServerRef},
                      {:call, 0, {:atom, 0, :wrap_cast}, [{:var, 0, :Msg}]}
                    ]}
                 ]},
                {:clause, 0,
                 [{:tuple, 0, [{:atom, 0, :global}, {:var, 0, :Name}]}, {:var, 0, :Msg}], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :global}, {:atom, 0, :send}},
                       [
                         {:var, 0, :Name},
                         {:call, 0, {:atom, 0, :wrap_cast}, [{:var, 0, :Msg}]}
                       ]}
                    ], [{:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :ok}]}],
                    [
                      {:clause, 0, [{:tuple, 0, [{:var, 0, :_}, {:var, 0, :_}, {:var, 0, :_}]}],
                       [], [{:atom, 0, :ok}]}
                    ], []}
                 ]},
                {:clause, 0,
                 [
                   {:tuple, 0, [{:atom, 0, :via}, {:var, 0, :RegMod}, {:var, 0, :Name}]},
                   {:var, 0, :Msg}
                 ], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:var, 0, :RegMod}, {:atom, 0, :send}},
                       [
                         {:var, 0, :Name},
                         {:call, 0, {:atom, 0, :wrap_cast}, [{:var, 0, :Msg}]}
                       ]}
                    ], [{:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :ok}]}],
                    [
                      {:clause, 0, [{:tuple, 0, [{:var, 0, :_}, {:var, 0, :_}, {:var, 0, :_}]}],
                       [], [{:atom, 0, :ok}]}
                    ], []}
                 ]},
                {:clause, 0,
                 [
                   {:match, 0, {:tuple, 0, [{:var, 0, :Name}, {:var, 0, :Node}]},
                    {:var, 0, :ServerRef}},
                   {:var, 0, :Msg}
                 ],
                 [
                   [
                     {:call, 0, {:atom, 0, :is_atom}, [{:var, 0, :Name}]},
                     {:call, 0, {:atom, 0, :is_atom}, [{:var, 0, :Node}]}
                   ]
                 ],
                 [
                   {:call, 0, {:atom, 0, :send},
                    [
                      {:var, 0, :ServerRef},
                      {:call, 0, {:atom, 0, :wrap_cast}, [{:var, 0, :Msg}]}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:call, 2},
               [
                 {:type, 635, :fun,
                  [
                    {:type, 635, :product,
                     [
                       {:ann_type, 635,
                        [{:var, 635, :ServerRef}, {:user_type, 635, :server_ref, []}]},
                       {:ann_type, 635, [{:var, 635, :Request}, {:type, 635, :term, []}]}
                     ]},
                    {:ann_type, 635, [{:var, 635, :Reply}, {:type, 635, :term, []}]}
                  ]}
               ]}},
             {:function, 0, :call, 2,
              [
                {:clause, 0, [{:var, 0, :ServerRef}, {:var, 0, :Request}], [],
                 [
                   {:call, 0, {:atom, 0, :call},
                    [{:var, 0, :ServerRef}, {:var, 0, :Request}, {:atom, 0, :infinity}]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:call, 3},
               [
                 {:type, 639, :fun,
                  [
                    {:type, 639, :product,
                     [
                       {:ann_type, 640,
                        [{:var, 640, :ServerRef}, {:user_type, 640, :server_ref, []}]},
                       {:ann_type, 641, [{:var, 641, :Request}, {:type, 641, :term, []}]},
                       {:ann_type, 642,
                        [
                          {:var, 642, :Timeout},
                          {:type, 643, :union,
                           [
                             {:type, 643, :timeout, []},
                             {:type, 644, :tuple,
                              [
                                {:atom, 644, :clean_timeout},
                                {:ann_type, 644, [{:var, 644, :T}, {:type, 644, :timeout, []}]}
                              ]},
                             {:type, 645, :tuple,
                              [
                                {:atom, 645, :dirty_timeout},
                                {:ann_type, 645, [{:var, 645, :T}, {:type, 645, :timeout, []}]}
                              ]}
                           ]}
                        ]}
                     ]},
                    {:ann_type, 646, [{:var, 646, :Reply}, {:type, 646, :term, []}]}
                  ]}
               ]}},
             {:function, 0, :call, 3,
              [
                {:clause, 0,
                 [
                   {:var, 0, :ServerRef},
                   {:var, 0, :Request},
                   {:match, 0, {:atom, 0, :infinity},
                    {:match, 0, {:var, 0, :T}, {:var, 0, :Timeout}}}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :call_dirty},
                    [
                      {:var, 0, :ServerRef},
                      {:var, 0, :Request},
                      {:var, 0, :Timeout},
                      {:var, 0, :T}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :ServerRef},
                   {:var, 0, :Request},
                   {:match, 0, {:tuple, 0, [{:atom, 0, :dirty_timeout}, {:var, 0, :T}]},
                    {:var, 0, :Timeout}}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :call_dirty},
                    [
                      {:var, 0, :ServerRef},
                      {:var, 0, :Request},
                      {:var, 0, :Timeout},
                      {:var, 0, :T}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :ServerRef},
                   {:var, 0, :Request},
                   {:match, 0, {:tuple, 0, [{:atom, 0, :clean_timeout}, {:var, 0, :T}]},
                    {:var, 0, :Timeout}}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :call_clean},
                    [
                      {:var, 0, :ServerRef},
                      {:var, 0, :Request},
                      {:var, 0, :Timeout},
                      {:var, 0, :T}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :ServerRef},
                   {:var, 0, :Request},
                   {:match, 0, {:tuple, 0, [{:var, 0, :_}, {:var, 0, :_}]}, {:var, 0, :Timeout}}
                 ], [],
                 [
                   {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :error}},
                    [
                      {:atom, 0, :badarg},
                      {:cons, 0, {:var, 0, :ServerRef},
                       {:cons, 0, {:var, 0, :Request}, {:cons, 0, {:var, 0, :Timeout}, {nil, 0}}}}
                    ]}
                 ]},
                {:clause, 0, [{:var, 0, :ServerRef}, {:var, 0, :Request}, {:var, 0, :Timeout}],
                 [],
                 [
                   {:call, 0, {:atom, 0, :call_clean},
                    [
                      {:var, 0, :ServerRef},
                      {:var, 0, :Request},
                      {:var, 0, :Timeout},
                      {:var, 0, :Timeout}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:send_request, 2},
               [
                 {:type, 658, :fun,
                  [
                    {:type, 658, :product,
                     [
                       {:ann_type, 658,
                        [{:var, 658, :ServerRef}, {:user_type, 658, :server_ref, []}]},
                       {:ann_type, 658, [{:var, 658, :Request}, {:type, 658, :term, []}]}
                     ]},
                    {:ann_type, 659, [{:var, 659, :ReqId}, {:user_type, 659, :request_id, []}]}
                  ]}
               ]}},
             {:function, 0, :send_request, 2,
              [
                {:clause, 0, [{:var, 0, :Name}, {:var, 0, :Request}], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :send_request}},
                       [{:var, 0, :Name}, {:atom, 0, :"$gen_call"}, {:var, 0, :Request}]}
                    ], [],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :badarg}, {:var, 0, :_}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :error},
                          [
                            {:atom, 0, :badarg},
                            {:cons, 0, {:var, 0, :Name},
                             {:cons, 0, {:var, 0, :Request}, {nil, 0}}}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:send_request, 4},
               [
                 {:type, 668, :fun,
                  [
                    {:type, 668, :product,
                     [
                       {:ann_type, 668,
                        [{:var, 668, :ServerRef}, {:user_type, 668, :server_ref, []}]},
                       {:ann_type, 669, [{:var, 669, :Request}, {:type, 669, :term, []}]},
                       {:ann_type, 670, [{:var, 670, :Label}, {:type, 670, :term, []}]},
                       {:ann_type, 671,
                        [
                          {:var, 671, :ReqIdCollection},
                          {:user_type, 671, :request_id_collection, []}
                        ]}
                     ]},
                    {:ann_type, 672,
                     [
                       {:var, 672, :NewReqIdCollection},
                       {:user_type, 672, :request_id_collection, []}
                     ]}
                  ]}
               ]}},
             {:function, 0, :send_request, 4,
              [
                {:clause, 0,
                 [
                   {:var, 0, :ServerRef},
                   {:var, 0, :Request},
                   {:var, 0, :Label},
                   {:var, 0, :ReqIdCol}
                 ], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :send_request}},
                       [
                         {:var, 0, :ServerRef},
                         {:atom, 0, :"$gen_call"},
                         {:var, 0, :Request},
                         {:var, 0, :Label},
                         {:var, 0, :ReqIdCol}
                       ]}
                    ], [],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :badarg}, {:var, 0, :_}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :error},
                          [
                            {:atom, 0, :badarg},
                            {:cons, 0, {:var, 0, :ServerRef},
                             {:cons, 0, {:var, 0, :Request},
                              {:cons, 0, {:var, 0, :Label},
                               {:cons, 0, {:var, 0, :ReqIdCol}, {nil, 0}}}}}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:wait_response, 1},
               [
                 {:type, 683, :bounded_fun,
                  [
                    {:type, 683, :fun,
                     [{:type, 683, :product, [{:var, 683, :ReqId}]}, {:var, 683, :Result}]},
                    [
                      {:type, 684, :constraint,
                       [
                         {:atom, 684, :is_subtype},
                         [{:var, 684, :ReqId}, {:user_type, 684, :request_id, []}]
                       ]},
                      {:type, 685, :constraint,
                       [
                         {:atom, 685, :is_subtype},
                         [
                           {:var, 685, :Response},
                           {:type, 685, :union,
                            [
                              {:type, 685, :tuple,
                               [
                                 {:atom, 685, :reply},
                                 {:ann_type, 685, [{:var, 685, :Reply}, {:type, 685, :term, []}]}
                               ]},
                              {:type, 686, :tuple,
                               [
                                 {:atom, 686, :error},
                                 {:type, 686, :tuple,
                                  [
                                    {:ann_type, 686,
                                     [{:var, 686, :Reason}, {:type, 686, :term, []}]},
                                    {:user_type, 686, :server_ref, []}
                                  ]}
                               ]}
                            ]}
                         ]
                       ]},
                      {:type, 687, :constraint,
                       [
                         {:atom, 687, :is_subtype},
                         [
                           {:var, 687, :Result},
                           {:type, 687, :union, [{:var, 687, :Response}, {:atom, 687, :timeout}]}
                         ]
                       ]}
                    ]
                  ]}
               ]}},
             {:function, 0, :wait_response, 1,
              [
                {:clause, 0, [{:var, 0, :ReqId}], [],
                 [
                   {:call, 0, {:atom, 0, :wait_response},
                    [{:var, 0, :ReqId}, {:atom, 0, :infinity}]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:wait_response, 2},
               [
                 {:type, 692, :bounded_fun,
                  [
                    {:type, 692, :fun,
                     [
                       {:type, 692, :product, [{:var, 692, :ReqId}, {:var, 692, :WaitTime}]},
                       {:var, 692, :Result}
                     ]},
                    [
                      {:type, 693, :constraint,
                       [
                         {:atom, 693, :is_subtype},
                         [{:var, 693, :ReqId}, {:user_type, 693, :request_id, []}]
                       ]},
                      {:type, 694, :constraint,
                       [
                         {:atom, 694, :is_subtype},
                         [{:var, 694, :WaitTime}, {:user_type, 694, :response_timeout, []}]
                       ]},
                      {:type, 695, :constraint,
                       [
                         {:atom, 695, :is_subtype},
                         [
                           {:var, 695, :Response},
                           {:type, 695, :union,
                            [
                              {:type, 695, :tuple,
                               [
                                 {:atom, 695, :reply},
                                 {:ann_type, 695, [{:var, 695, :Reply}, {:type, 695, :term, []}]}
                               ]},
                              {:type, 696, :tuple,
                               [
                                 {:atom, 696, :error},
                                 {:type, 696, :tuple,
                                  [
                                    {:ann_type, 696,
                                     [{:var, 696, :Reason}, {:type, 696, :term, []}]},
                                    {:user_type, 696, :server_ref, []}
                                  ]}
                               ]}
                            ]}
                         ]
                       ]},
                      {:type, 697, :constraint,
                       [
                         {:atom, 697, :is_subtype},
                         [
                           {:var, 697, :Result},
                           {:type, 697, :union, [{:var, 697, :Response}, {:atom, 697, :timeout}]}
                         ]
                       ]}
                    ]
                  ]}
               ]}},
             {:function, 0, :wait_response, 2,
              [
                {:clause, 0, [{:var, 0, :ReqId}, {:var, 0, :WaitTime}], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :wait_response}},
                       [{:var, 0, :ReqId}, {:var, 0, :WaitTime}]}
                    ], [],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :badarg}, {:var, 0, :_}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :error},
                          [
                            {:atom, 0, :badarg},
                            {:cons, 0, {:var, 0, :ReqId},
                             {:cons, 0, {:var, 0, :WaitTime}, {nil, 0}}}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:wait_response, 3},
               [
                 {:type, 707, :bounded_fun,
                  [
                    {:type, 707, :fun,
                     [
                       {:type, 707, :product,
                        [
                          {:var, 707, :ReqIdCollection},
                          {:var, 707, :WaitTime},
                          {:var, 707, :Delete}
                        ]},
                       {:var, 707, :Result}
                     ]},
                    [
                      {:type, 708, :constraint,
                       [
                         {:atom, 708, :is_subtype},
                         [
                           {:var, 708, :ReqIdCollection},
                           {:user_type, 708, :request_id_collection, []}
                         ]
                       ]},
                      {:type, 709, :constraint,
                       [
                         {:atom, 709, :is_subtype},
                         [{:var, 709, :WaitTime}, {:user_type, 709, :response_timeout, []}]
                       ]},
                      {:type, 710, :constraint,
                       [
                         {:atom, 710, :is_subtype},
                         [{:var, 710, :Delete}, {:type, 710, :boolean, []}]
                       ]},
                      {:type, 711, :constraint,
                       [
                         {:atom, 711, :is_subtype},
                         [
                           {:var, 711, :Response},
                           {:type, 711, :union,
                            [
                              {:type, 711, :tuple,
                               [
                                 {:atom, 711, :reply},
                                 {:ann_type, 711, [{:var, 711, :Reply}, {:type, 711, :term, []}]}
                               ]},
                              {:type, 712, :tuple,
                               [
                                 {:atom, 712, :error},
                                 {:type, 712, :tuple,
                                  [
                                    {:ann_type, 712,
                                     [{:var, 712, :Reason}, {:type, 712, :term, []}]},
                                    {:user_type, 712, :server_ref, []}
                                  ]}
                               ]}
                            ]}
                         ]
                       ]},
                      {:type, 713, :constraint,
                       [
                         {:atom, 713, :is_subtype},
                         [
                           {:var, 713, :Result},
                           {:type, 713, :union,
                            [
                              {:type, 713, :tuple,
                               [
                                 {:var, 713, :Response},
                                 {:ann_type, 714, [{:var, 714, :Label}, {:type, 714, :term, []}]},
                                 {:ann_type, 715,
                                  [
                                    {:var, 715, :NewReqIdCollection},
                                    {:user_type, 715, :request_id_collection, []}
                                  ]}
                               ]},
                              {:atom, 716, :no_request},
                              {:atom, 717, :timeout}
                            ]}
                         ]
                       ]}
                    ]
                  ]}
               ]}},
             {:function, 0, :wait_response, 3,
              [
                {:clause, 0, [{:var, 0, :ReqIdCol}, {:var, 0, :WaitTime}, {:var, 0, :Delete}], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :wait_response}},
                       [{:var, 0, :ReqIdCol}, {:var, 0, :WaitTime}, {:var, 0, :Delete}]}
                    ], [],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :badarg}, {:var, 0, :_}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :error},
                          [
                            {:atom, 0, :badarg},
                            {:cons, 0, {:var, 0, :ReqIdCol},
                             {:cons, 0, {:var, 0, :WaitTime},
                              {:cons, 0, {:var, 0, :Delete}, {nil, 0}}}}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:receive_response, 1},
               [
                 {:type, 727, :bounded_fun,
                  [
                    {:type, 727, :fun,
                     [{:type, 727, :product, [{:var, 727, :ReqId}]}, {:var, 727, :Result}]},
                    [
                      {:type, 728, :constraint,
                       [
                         {:atom, 728, :is_subtype},
                         [{:var, 728, :ReqId}, {:user_type, 728, :request_id, []}]
                       ]},
                      {:type, 729, :constraint,
                       [
                         {:atom, 729, :is_subtype},
                         [
                           {:var, 729, :Response},
                           {:type, 729, :union,
                            [
                              {:type, 729, :tuple,
                               [
                                 {:atom, 729, :reply},
                                 {:ann_type, 729, [{:var, 729, :Reply}, {:type, 729, :term, []}]}
                               ]},
                              {:type, 730, :tuple,
                               [
                                 {:atom, 730, :error},
                                 {:type, 730, :tuple,
                                  [
                                    {:ann_type, 730,
                                     [{:var, 730, :Reason}, {:type, 730, :term, []}]},
                                    {:user_type, 730, :server_ref, []}
                                  ]}
                               ]}
                            ]}
                         ]
                       ]},
                      {:type, 731, :constraint,
                       [
                         {:atom, 731, :is_subtype},
                         [
                           {:var, 731, :Result},
                           {:type, 731, :union, [{:var, 731, :Response}, {:atom, 731, :timeout}]}
                         ]
                       ]}
                    ]
                  ]}
               ]}},
             {:function, 0, :receive_response, 1,
              [
                {:clause, 0, [{:var, 0, :ReqId}], [],
                 [
                   {:call, 0, {:atom, 0, :receive_response},
                    [{:var, 0, :ReqId}, {:atom, 0, :infinity}]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:receive_response, 2},
               [
                 {:type, 736, :bounded_fun,
                  [
                    {:type, 736, :fun,
                     [
                       {:type, 736, :product, [{:var, 736, :ReqId}, {:var, 736, :Timeout}]},
                       {:var, 736, :Result}
                     ]},
                    [
                      {:type, 737, :constraint,
                       [
                         {:atom, 737, :is_subtype},
                         [{:var, 737, :ReqId}, {:user_type, 737, :request_id, []}]
                       ]},
                      {:type, 738, :constraint,
                       [
                         {:atom, 738, :is_subtype},
                         [{:var, 738, :Timeout}, {:user_type, 738, :response_timeout, []}]
                       ]},
                      {:type, 739, :constraint,
                       [
                         {:atom, 739, :is_subtype},
                         [
                           {:var, 739, :Response},
                           {:type, 739, :union,
                            [
                              {:type, 739, :tuple,
                               [
                                 {:atom, 739, :reply},
                                 {:ann_type, 739, [{:var, 739, :Reply}, {:type, 739, :term, []}]}
                               ]},
                              {:type, 740, :tuple,
                               [
                                 {:atom, 740, :error},
                                 {:type, 740, :tuple,
                                  [
                                    {:ann_type, 740,
                                     [{:var, 740, :Reason}, {:type, 740, :term, []}]},
                                    {:user_type, 740, :server_ref, []}
                                  ]}
                               ]}
                            ]}
                         ]
                       ]},
                      {:type, 741, :constraint,
                       [
                         {:atom, 741, :is_subtype},
                         [
                           {:var, 741, :Result},
                           {:type, 741, :union, [{:var, 741, :Response}, {:atom, 741, :timeout}]}
                         ]
                       ]}
                    ]
                  ]}
               ]}},
             {:function, 0, :receive_response, 2,
              [
                {:clause, 0, [{:var, 0, :ReqId}, {:var, 0, :Timeout}], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :receive_response}},
                       [{:var, 0, :ReqId}, {:var, 0, :Timeout}]}
                    ], [],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :badarg}, {:var, 0, :_}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :error},
                          [
                            {:atom, 0, :badarg},
                            {:cons, 0, {:var, 0, :ReqId},
                             {:cons, 0, {:var, 0, :Timeout}, {nil, 0}}}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:receive_response, 3},
               [
                 {:type, 751, :bounded_fun,
                  [
                    {:type, 751, :fun,
                     [
                       {:type, 751, :product,
                        [
                          {:var, 751, :ReqIdCollection},
                          {:var, 751, :Timeout},
                          {:var, 751, :Delete}
                        ]},
                       {:var, 751, :Result}
                     ]},
                    [
                      {:type, 752, :constraint,
                       [
                         {:atom, 752, :is_subtype},
                         [
                           {:var, 752, :ReqIdCollection},
                           {:user_type, 752, :request_id_collection, []}
                         ]
                       ]},
                      {:type, 753, :constraint,
                       [
                         {:atom, 753, :is_subtype},
                         [{:var, 753, :Timeout}, {:user_type, 753, :response_timeout, []}]
                       ]},
                      {:type, 754, :constraint,
                       [
                         {:atom, 754, :is_subtype},
                         [{:var, 754, :Delete}, {:type, 754, :boolean, []}]
                       ]},
                      {:type, 755, :constraint,
                       [
                         {:atom, 755, :is_subtype},
                         [
                           {:var, 755, :Response},
                           {:type, 755, :union,
                            [
                              {:type, 755, :tuple,
                               [
                                 {:atom, 755, :reply},
                                 {:ann_type, 755, [{:var, 755, :Reply}, {:type, 755, :term, []}]}
                               ]},
                              {:type, 756, :tuple,
                               [
                                 {:atom, 756, :error},
                                 {:type, 756, :tuple,
                                  [
                                    {:ann_type, 756,
                                     [{:var, 756, :Reason}, {:type, 756, :term, []}]},
                                    {:user_type, 756, :server_ref, []}
                                  ]}
                               ]}
                            ]}
                         ]
                       ]},
                      {:type, 757, :constraint,
                       [
                         {:atom, 757, :is_subtype},
                         [
                           {:var, 757, :Result},
                           {:type, 757, :union,
                            [
                              {:type, 757, :tuple,
                               [
                                 {:var, 757, :Response},
                                 {:ann_type, 758, [{:var, 758, :Label}, {:type, 758, :term, []}]},
                                 {:ann_type, 759,
                                  [
                                    {:var, 759, :NewReqIdCollection},
                                    {:user_type, 759, :request_id_collection, []}
                                  ]}
                               ]},
                              {:atom, 760, :no_request},
                              {:atom, 761, :timeout}
                            ]}
                         ]
                       ]}
                    ]
                  ]}
               ]}},
             {:function, 0, :receive_response, 3,
              [
                {:clause, 0, [{:var, 0, :ReqIdCol}, {:var, 0, :Timeout}, {:var, 0, :Delete}], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :receive_response}},
                       [{:var, 0, :ReqIdCol}, {:var, 0, :Timeout}, {:var, 0, :Delete}]}
                    ], [],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :badarg}, {:var, 0, :_}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :error},
                          [
                            {:atom, 0, :badarg},
                            {:cons, 0, {:var, 0, :ReqIdCol},
                             {:cons, 0, {:var, 0, :Timeout},
                              {:cons, 0, {:var, 0, :Delete}, {nil, 0}}}}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:check_response, 2},
               [
                 {:type, 771, :bounded_fun,
                  [
                    {:type, 771, :fun,
                     [
                       {:type, 771, :product, [{:var, 771, :Msg}, {:var, 771, :ReqId}]},
                       {:var, 771, :Result}
                     ]},
                    [
                      {:type, 772, :constraint,
                       [
                         {:atom, 772, :is_subtype},
                         [{:var, 772, :Msg}, {:type, 772, :term, []}]
                       ]},
                      {:type, 773, :constraint,
                       [
                         {:atom, 773, :is_subtype},
                         [{:var, 773, :ReqId}, {:user_type, 773, :request_id, []}]
                       ]},
                      {:type, 774, :constraint,
                       [
                         {:atom, 774, :is_subtype},
                         [
                           {:var, 774, :Response},
                           {:type, 774, :union,
                            [
                              {:type, 774, :tuple,
                               [
                                 {:atom, 774, :reply},
                                 {:ann_type, 774, [{:var, 774, :Reply}, {:type, 774, :term, []}]}
                               ]},
                              {:type, 775, :tuple,
                               [
                                 {:atom, 775, :error},
                                 {:type, 775, :tuple,
                                  [
                                    {:ann_type, 775,
                                     [{:var, 775, :Reason}, {:type, 775, :term, []}]},
                                    {:user_type, 775, :server_ref, []}
                                  ]}
                               ]}
                            ]}
                         ]
                       ]},
                      {:type, 776, :constraint,
                       [
                         {:atom, 776, :is_subtype},
                         [
                           {:var, 776, :Result},
                           {:type, 776, :union, [{:var, 776, :Response}, {:atom, 776, :no_reply}]}
                         ]
                       ]}
                    ]
                  ]}
               ]}},
             {:function, 0, :check_response, 2,
              [
                {:clause, 0, [{:var, 0, :Msg}, {:var, 0, :ReqId}], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :check_response}},
                       [{:var, 0, :Msg}, {:var, 0, :ReqId}]}
                    ], [],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :badarg}, {:var, 0, :_}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :error},
                          [
                            {:atom, 0, :badarg},
                            {:cons, 0, {:var, 0, :Msg}, {:cons, 0, {:var, 0, :ReqId}, {nil, 0}}}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:check_response, 3},
               [
                 {:type, 786, :bounded_fun,
                  [
                    {:type, 786, :fun,
                     [
                       {:type, 786, :product,
                        [
                          {:var, 786, :Msg},
                          {:var, 786, :ReqIdCollection},
                          {:var, 786, :Delete}
                        ]},
                       {:var, 786, :Result}
                     ]},
                    [
                      {:type, 787, :constraint,
                       [
                         {:atom, 787, :is_subtype},
                         [{:var, 787, :Msg}, {:type, 787, :term, []}]
                       ]},
                      {:type, 788, :constraint,
                       [
                         {:atom, 788, :is_subtype},
                         [
                           {:var, 788, :ReqIdCollection},
                           {:user_type, 788, :request_id_collection, []}
                         ]
                       ]},
                      {:type, 789, :constraint,
                       [
                         {:atom, 789, :is_subtype},
                         [{:var, 789, :Delete}, {:type, 789, :boolean, []}]
                       ]},
                      {:type, 790, :constraint,
                       [
                         {:atom, 790, :is_subtype},
                         [
                           {:var, 790, :Response},
                           {:type, 790, :union,
                            [
                              {:type, 790, :tuple,
                               [
                                 {:atom, 790, :reply},
                                 {:ann_type, 790, [{:var, 790, :Reply}, {:type, 790, :term, []}]}
                               ]},
                              {:type, 791, :tuple,
                               [
                                 {:atom, 791, :error},
                                 {:type, 791, :tuple,
                                  [
                                    {:ann_type, 791,
                                     [{:var, 791, :Reason}, {:type, 791, :term, []}]},
                                    {:user_type, 791, :server_ref, []}
                                  ]}
                               ]}
                            ]}
                         ]
                       ]},
                      {:type, 792, :constraint,
                       [
                         {:atom, 792, :is_subtype},
                         [
                           {:var, 792, :Result},
                           {:type, 792, :union,
                            [
                              {:type, 792, :tuple,
                               [
                                 {:var, 792, :Response},
                                 {:ann_type, 793, [{:var, 793, :Label}, {:type, 793, :term, []}]},
                                 {:ann_type, 794,
                                  [
                                    {:var, 794, :NewReqIdCollection},
                                    {:user_type, 794, :request_id_collection, []}
                                  ]}
                               ]},
                              {:atom, 795, :no_request},
                              {:atom, 796, :no_reply}
                            ]}
                         ]
                       ]}
                    ]
                  ]}
               ]}},
             {:function, 0, :check_response, 3,
              [
                {:clause, 0, [{:var, 0, :Msg}, {:var, 0, :ReqIdCol}, {:var, 0, :Delete}], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :check_response}},
                       [{:var, 0, :Msg}, {:var, 0, :ReqIdCol}, {:var, 0, :Delete}]}
                    ], [],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :badarg}, {:var, 0, :_}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :error},
                          [
                            {:atom, 0, :badarg},
                            {:cons, 0, {:var, 0, :Msg},
                             {:cons, 0, {:var, 0, :ReqIdCol},
                              {:cons, 0, {:var, 0, :Delete}, {nil, 0}}}}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:reqids_new, 0},
               [
                 {:type, 806, :fun,
                  [
                    {:type, 806, :product, []},
                    {:ann_type, 807,
                     [
                       {:var, 807, :NewReqIdCollection},
                       {:user_type, 807, :request_id_collection, []}
                     ]}
                  ]}
               ]}},
             {:function, 0, :reqids_new, 0,
              [
                {:clause, 0, [], [],
                 [{:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :reqids_new}}, []}]}
              ]},
             {:attribute, 0, :spec,
              {{:reqids_size, 1},
               [
                 {:type, 812, :fun,
                  [
                    {:type, 812, :product,
                     [
                       {:ann_type, 812,
                        [
                          {:var, 812, :ReqIdCollection},
                          {:user_type, 812, :request_id_collection, []}
                        ]}
                     ]},
                    {:type, 813, :non_neg_integer, []}
                  ]}
               ]}},
             {:function, 0, :reqids_size, 1,
              [
                {:clause, 0, [{:var, 0, :ReqIdCollection}], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :reqids_size}},
                       [{:var, 0, :ReqIdCollection}]}
                    ], [],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :badarg}, {:var, 0, :_}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :error},
                          [
                            {:atom, 0, :badarg},
                            {:cons, 0, {:var, 0, :ReqIdCollection}, {nil, 0}}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:reqids_add, 3},
               [
                 {:type, 822, :fun,
                  [
                    {:type, 822, :product,
                     [
                       {:ann_type, 822,
                        [{:var, 822, :ReqId}, {:user_type, 822, :request_id, []}]},
                       {:ann_type, 822, [{:var, 822, :Label}, {:type, 822, :term, []}]},
                       {:ann_type, 823,
                        [
                          {:var, 823, :ReqIdCollection},
                          {:user_type, 823, :request_id_collection, []}
                        ]}
                     ]},
                    {:ann_type, 824,
                     [
                       {:var, 824, :NewReqIdCollection},
                       {:user_type, 824, :request_id_collection, []}
                     ]}
                  ]}
               ]}},
             {:function, 0, :reqids_add, 3,
              [
                {:clause, 0, [{:var, 0, :ReqId}, {:var, 0, :Label}, {:var, 0, :ReqIdCollection}],
                 [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :reqids_add}},
                       [{:var, 0, :ReqId}, {:var, 0, :Label}, {:var, 0, :ReqIdCollection}]}
                    ], [],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :badarg}, {:var, 0, :_}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :error},
                          [
                            {:atom, 0, :badarg},
                            {:cons, 0, {:var, 0, :ReqId},
                             {:cons, 0, {:var, 0, :Label},
                              {:cons, 0, {:var, 0, :ReqIdCollection}, {nil, 0}}}}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:reqids_to_list, 1},
               [
                 {:type, 833, :fun,
                  [
                    {:type, 833, :product,
                     [
                       {:ann_type, 833,
                        [
                          {:var, 833, :ReqIdCollection},
                          {:user_type, 833, :request_id_collection, []}
                        ]}
                     ]},
                    {:type, 834, :list,
                     [
                       {:type, 834, :tuple,
                        [
                          {:ann_type, 834,
                           [{:var, 834, :ReqId}, {:user_type, 834, :request_id, []}]},
                          {:ann_type, 834, [{:var, 834, :Label}, {:type, 834, :term, []}]}
                        ]}
                     ]}
                  ]}
               ]}},
             {:function, 0, :reqids_to_list, 1,
              [
                {:clause, 0, [{:var, 0, :ReqIdCollection}], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :reqids_to_list}},
                       [{:var, 0, :ReqIdCollection}]}
                    ], [],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :error}, {:atom, 0, :badarg}, {:var, 0, :_}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :error},
                          [
                            {:atom, 0, :badarg},
                            {:cons, 0, {:var, 0, :ReqIdCollection}, {nil, 0}}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:reply, 1},
               [
                 {:type, 844, :fun,
                  [
                    {:type, 844, :product,
                     [
                       {:type, 844, :union,
                        [
                          {:type, 844, :list, [{:user_type, 844, :reply_action, []}]},
                          {:user_type, 844, :reply_action, []}
                        ]}
                     ]},
                    {:atom, 844, :ok}
                  ]}
               ]}},
             {:function, 0, :reply, 1,
              [
                {:clause, 0,
                 [{:tuple, 0, [{:atom, 0, :reply}, {:var, 0, :From}, {:var, 0, :Reply}]}], [],
                 [{:call, 0, {:atom, 0, :reply}, [{:var, 0, :From}, {:var, 0, :Reply}]}]},
                {:clause, 0, [{:var, 0, :Replies}],
                 [[{:call, 0, {:atom, 0, :is_list}, [{:var, 0, :Replies}]}]],
                 [{:call, 0, {:atom, 0, :replies}, [{:var, 0, :Replies}]}]}
              ]},
             {:attribute, 0, :compile, {:inline, [reply: 2]}},
             {:attribute, 0, :spec,
              {{:reply, 2},
               [
                 {:type, 851, :fun,
                  [
                    {:type, 851, :product,
                     [
                       {:ann_type, 851, [{:var, 851, :From}, {:user_type, 851, :from, []}]},
                       {:ann_type, 851, [{:var, 851, :Reply}, {:type, 851, :term, []}]}
                     ]},
                    {:atom, 851, :ok}
                  ]}
               ]}},
             {:function, 0, :reply, 2,
              [
                {:clause, 0, [{:var, 0, :From}, {:var, 0, :Reply}], [],
                 [
                   {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :reply}},
                    [{:var, 0, :From}, {:var, 0, :Reply}]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:enter_loop, 4},
               [
                 {:type, 859, :fun,
                  [
                    {:type, 859, :product,
                     [
                       {:ann_type, 860, [{:var, 860, :Module}, {:type, 860, :module, []}]},
                       {:ann_type, 860,
                        [
                          {:var, 860, :Opts},
                          {:type, 860, :list, [{:user_type, 860, :enter_loop_opt, []}]}
                        ]},
                       {:ann_type, 861, [{:var, 861, :State}, {:user_type, 861, :state, []}]},
                       {:ann_type, 861, [{:var, 861, :Data}, {:user_type, 861, :data, []}]}
                     ]},
                    {:type, 862, :no_return, []}
                  ]}
               ]}},
             {:function, 0, :enter_loop, 4,
              [
                {:clause, 0,
                 [
                   {:var, 0, :Module},
                   {:var, 0, :Opts},
                   {:var, 0, :State},
                   {:var, 0, :Data}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :enter_loop},
                    [
                      {:var, 0, :Module},
                      {:var, 0, :Opts},
                      {:var, 0, :State},
                      {:var, 0, :Data},
                      {:call, 0, {:atom, 0, :self}, []}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:enter_loop, 5},
               [
                 {:type, 866, :fun,
                  [
                    {:type, 866, :product,
                     [
                       {:ann_type, 867, [{:var, 867, :Module}, {:type, 867, :module, []}]},
                       {:ann_type, 867,
                        [
                          {:var, 867, :Opts},
                          {:type, 867, :list, [{:user_type, 867, :enter_loop_opt, []}]}
                        ]},
                       {:ann_type, 868, [{:var, 868, :State}, {:user_type, 868, :state, []}]},
                       {:ann_type, 868, [{:var, 868, :Data}, {:user_type, 868, :data, []}]},
                       {:ann_type, 869,
                        [
                          {:var, 869, :Server_or_Actions},
                          {:type, 870, :union,
                           [
                             {:user_type, 870, :server_name, []},
                             {:type, 870, :pid, []},
                             {:type, 870, :list, [{:user_type, 870, :action, []}]}
                           ]}
                        ]}
                     ]},
                    {:type, 871, :no_return, []}
                  ]}
               ]}},
             {:function, 0, :enter_loop, 5,
              [
                {:clause, 0,
                 [
                   {:var, 0, :Module},
                   {:var, 0, :Opts},
                   {:var, 0, :State},
                   {:var, 0, :Data},
                   {:var, 0, :Server_or_Actions}
                 ], [],
                 [
                   {:if, 0,
                    [
                      {:clause, 0, [],
                       [
                         [
                           {:call, 0, {:atom, 0, :is_list}, [{:var, 0, :Server_or_Actions}]}
                         ]
                       ],
                       [
                         {:call, 0, {:atom, 0, :enter_loop},
                          [
                            {:var, 0, :Module},
                            {:var, 0, :Opts},
                            {:var, 0, :State},
                            {:var, 0, :Data},
                            {:call, 0, {:atom, 0, :self}, []},
                            {:var, 0, :Server_or_Actions}
                          ]}
                       ]},
                      {:clause, 0, [], [[{:atom, 0, true}]],
                       [
                         {:call, 0, {:atom, 0, :enter_loop},
                          [
                            {:var, 0, :Module},
                            {:var, 0, :Opts},
                            {:var, 0, :State},
                            {:var, 0, :Data},
                            {:var, 0, :Server_or_Actions},
                            {nil, 0}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :spec,
              {{:enter_loop, 6},
               [
                 {:type, 880, :fun,
                  [
                    {:type, 880, :product,
                     [
                       {:ann_type, 881, [{:var, 881, :Module}, {:type, 881, :module, []}]},
                       {:ann_type, 881,
                        [
                          {:var, 881, :Opts},
                          {:type, 881, :list, [{:user_type, 881, :enter_loop_opt, []}]}
                        ]},
                       {:ann_type, 882, [{:var, 882, :State}, {:user_type, 882, :state, []}]},
                       {:ann_type, 882, [{:var, 882, :Data}, {:user_type, 882, :data, []}]},
                       {:ann_type, 883,
                        [
                          {:var, 883, :Server},
                          {:type, 883, :union,
                           [{:user_type, 883, :server_name, []}, {:type, 883, :pid, []}]}
                        ]},
                       {:ann_type, 884,
                        [
                          {:var, 884, :Actions},
                          {:type, 884, :union,
                           [
                             {:type, 884, :list, [{:user_type, 884, :action, []}]},
                             {:user_type, 884, :action, []}
                           ]}
                        ]}
                     ]},
                    {:type, 885, :no_return, []}
                  ]}
               ]}},
             {:function, 0, :enter_loop, 6,
              [
                {:clause, 0,
                 [
                   {:var, 0, :Module},
                   {:var, 0, :Opts},
                   {:var, 0, :State},
                   {:var, 0, :Data},
                   {:var, 0, :Server},
                   {:var, 0, :Actions}
                 ], [],
                 [
                   {:op, 0, :orelse, {:call, 0, {:atom, 0, :is_atom}, [{:var, 0, :Module}]},
                    {:call, 0, {:atom, 0, :error},
                     [{:tuple, 0, [{:atom, 0, :atom}, {:var, 0, :Module}]}]}},
                   {:match, 0, {:var, 0, :Parent},
                    {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :get_parent}}, []}},
                   {:match, 0, {:var, 0, :Name},
                    {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :get_proc_name}},
                     [{:var, 0, :Server}]}},
                   {:match, 0, {:var, 0, :Debug},
                    {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :debug_options}},
                     [{:var, 0, :Name}, {:var, 0, :Opts}]}},
                   {:match, 0, {:var, 0, :HibernateAfterTimeout},
                    {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :hibernate_after}},
                     [{:var, 0, :Opts}]}},
                   {:call, 0, {:atom, 0, :enter},
                    [
                      {:var, 0, :Parent},
                      {:var, 0, :Debug},
                      {:var, 0, :Module},
                      {:var, 0, :Name},
                      {:var, 0, :HibernateAfterTimeout},
                      {:var, 0, :State},
                      {:var, 0, :Data},
                      {:var, 0, :Actions}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :compile, {:inline, [wrap_cast: 1]}},
             {:function, 0, :wrap_cast, 1,
              [
                {:clause, 0, [{:var, 0, :Event}], [],
                 [{:tuple, 0, [{:atom, 0, :"$gen_cast"}, {:var, 0, :Event}]}]}
              ]},
             {:function, 0, :call_dirty, 4,
              [
                {:clause, 0,
                 [
                   {:var, 0, :ServerRef},
                   {:var, 0, :Request},
                   {:var, 0, :Timeout},
                   {:var, 0, :T}
                 ], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :call}},
                       [
                         {:var, 0, :ServerRef},
                         {:atom, 0, :"$gen_call"},
                         {:var, 0, :Request},
                         {:var, 0, :T}
                       ]}
                    ],
                    [
                      {:clause, 0, [{:tuple, 0, [{:atom, 0, :ok}, {:var, 0, :Reply}]}], [],
                       [{:var, 0, :Reply}]}
                    ],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}]}
                       ], [[{:op, 0, :"=:=", {:var, 0, :Class}, {:atom, 0, :exit}}]],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :raise}},
                          [
                            {:var, 0, :Class},
                            {:tuple, 0,
                             [
                               {:var, 0, :Reason},
                               {:tuple, 0,
                                [
                                  {:atom, 0, :tree_splitter_stress},
                                  {:atom, 0, :call},
                                  {:cons, 0, {:var, 0, :ServerRef},
                                   {:cons, 0, {:var, 0, :Request},
                                    {:cons, 0, {:var, 0, :Timeout}, {nil, 0}}}}
                                ]}
                             ]},
                            {:var, 0, :Stacktrace}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:function, 0, :call_clean, 4,
              [
                {:clause, 0,
                 [
                   {:var, 0, :ServerRef},
                   {:var, 0, :Request},
                   {:var, 0, :Timeout},
                   {:var, 0, :T}
                 ],
                 [
                   [
                     {:op, 0, :orelse,
                      {:op, 0, :andalso, {:call, 0, {:atom, 0, :is_pid}, [{:var, 0, :ServerRef}]},
                       {:op, 0, :==, {:call, 0, {:atom, 0, :node}, [{:var, 0, :ServerRef}]},
                        {:call, 0, {:atom, 0, :node}, []}}},
                      {:op, 0, :andalso,
                       {:op, 0, :==,
                        {:call, 0, {:atom, 0, :element},
                         [{:integer, 0, 2}, {:var, 0, :ServerRef}]},
                        {:call, 0, {:atom, 0, :node}, []}},
                       {:op, 0, :andalso,
                        {:call, 0, {:atom, 0, :is_atom},
                         [
                           {:call, 0, {:atom, 0, :element},
                            [{:integer, 0, 1}, {:var, 0, :ServerRef}]}
                         ]},
                        {:op, 0, :"=:=",
                         {:call, 0, {:atom, 0, :tuple_size}, [{:var, 0, :ServerRef}]},
                         {:integer, 0, 2}}}}}
                   ]
                 ],
                 [
                   {:call, 0, {:atom, 0, :call_dirty},
                    [
                      {:var, 0, :ServerRef},
                      {:var, 0, :Request},
                      {:var, 0, :Timeout},
                      {:var, 0, :T}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :ServerRef},
                   {:var, 0, :Request},
                   {:var, 0, :Timeout},
                   {:var, 0, :T}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :Ref}, {:call, 0, {:atom, 0, :make_ref}, []}},
                   {:match, 0, {:var, 0, :Self}, {:call, 0, {:atom, 0, :self}, []}},
                   {:match, 0, {:var, 0, :Pid},
                    {:call, 0, {:atom, 0, :spawn},
                     [
                       {:fun, 0,
                        {:clauses,
                         [
                           {:clause, 0, [], [],
                            [
                              {:op, 0, :!, {:var, 0, :Self},
                               {:try, 0,
                                [
                                  {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :call}},
                                   [
                                     {:var, 0, :ServerRef},
                                     {:atom, 0, :"$gen_call"},
                                     {:var, 0, :Request},
                                     {:var, 0, :T}
                                   ]}
                                ],
                                [
                                  {:clause, 0, [{:var, 0, :Result}], [],
                                   [{:tuple, 0, [{:var, 0, :Ref}, {:var, 0, :Result}]}]}
                                ],
                                [
                                  {:clause, 0,
                                   [
                                     {:tuple, 0,
                                      [
                                        {:var, 0, :Class},
                                        {:var, 0, :Reason},
                                        {:var, 0, :Stacktrace}
                                      ]}
                                   ], [],
                                   [
                                     {:tuple, 0,
                                      [
                                        {:var, 0, :Ref},
                                        {:var, 0, :Class},
                                        {:var, 0, :Reason},
                                        {:var, 0, :Stacktrace}
                                      ]}
                                   ]}
                                ], []}}
                            ]}
                         ]}}
                     ]}},
                   {:match, 0, {:var, 0, :Mref},
                    {:call, 0, {:atom, 0, :monitor}, [{:atom, 0, :process}, {:var, 0, :Pid}]}},
                   {:receive, 0,
                    [
                      {:clause, 0, [{:tuple, 0, [{:var, 0, :Ref}, {:var, 0, :Result}]}], [],
                       [
                         {:call, 0, {:atom, 0, :demonitor},
                          [{:var, 0, :Mref}, {:cons, 0, {:atom, 0, :flush}, {nil, 0}}]},
                         {:case, 0, {:var, 0, :Result},
                          [
                            {:clause, 0, [{:tuple, 0, [{:atom, 0, :ok}, {:var, 0, :Reply}]}], [],
                             [{:var, 0, :Reply}]}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:var, 0, :Ref},
                            {:var, 0, :Class},
                            {:var, 0, :Reason},
                            {:var, 0, :Stacktrace}
                          ]}
                       ], [[{:op, 0, :"=:=", {:var, 0, :Class}, {:atom, 0, :exit}}]],
                       [
                         {:call, 0, {:atom, 0, :demonitor},
                          [{:var, 0, :Mref}, {:cons, 0, {:atom, 0, :flush}, {nil, 0}}]},
                         {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :raise}},
                          [
                            {:var, 0, :Class},
                            {:tuple, 0,
                             [
                               {:var, 0, :Reason},
                               {:tuple, 0,
                                [
                                  {:atom, 0, :tree_splitter_stress},
                                  {:atom, 0, :call},
                                  {:cons, 0, {:var, 0, :ServerRef},
                                   {:cons, 0, {:var, 0, :Request},
                                    {:cons, 0, {:var, 0, :Timeout}, {nil, 0}}}}
                                ]}
                             ]},
                            {:var, 0, :Stacktrace}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:var, 0, :Ref},
                            {:var, 0, :Class},
                            {:var, 0, :Reason},
                            {:var, 0, :Stacktrace}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :demonitor},
                          [{:var, 0, :Mref}, {:cons, 0, {:atom, 0, :flush}, {nil, 0}}]},
                         {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :raise}},
                          [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :DOWN},
                            {:var, 0, :Mref},
                            {:var, 0, :_},
                            {:var, 0, :_},
                            {:var, 0, :Reason}
                          ]}
                       ], [], [{:call, 0, {:atom, 0, :exit}, [{:var, 0, :Reason}]}]}
                    ]}
                 ]}
              ]},
             {:function, 0, :replies, 1,
              [
                {:clause, 0,
                 [
                   {:cons, 0,
                    {:tuple, 0, [{:atom, 0, :reply}, {:var, 0, :From}, {:var, 0, :Reply}]},
                    {:var, 0, :Replies}}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :reply}, [{:var, 0, :From}, {:var, 0, :Reply}]},
                   {:call, 0, {:atom, 0, :replies}, [{:var, 0, :Replies}]}
                 ]},
                {:clause, 0, [nil: 0], [], [{:atom, 0, :ok}]}
              ]},
             {:function, 0, :send, 2,
              [
                {:clause, 0, [{:var, 0, :Proc}, {:var, 0, :Msg}], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :send}},
                       [{:var, 0, :Proc}, {:var, 0, :Msg}]}
                    ], [],
                    [
                      {:clause, 0,
                       [{:tuple, 0, [{:atom, 0, :error}, {:var, 0, :_}, {:var, 0, :_}]}], [],
                       [{:atom, 0, :ok}]}
                    ], []},
                   {:atom, 0, :ok}
                 ]}
              ]},
             {:function, 0, :enter, 8,
              [
                {:clause, 0,
                 [
                   {:var, 0, :Parent},
                   {:var, 0, :Debug},
                   {:var, 0, :Module},
                   {:var, 0, :Name},
                   {:var, 0, :HibernateAfterTimeout},
                   {:var, 0, :State},
                   {:var, 0, :Data},
                   {:var, 0, :Actions}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :Q},
                    {:cons, 0, {:tuple, 0, [{:atom, 0, :internal}, {:atom, 0, :init_state}]},
                     {nil, 0}}},
                   {:match, 0, {:var, 0, :Actions_1},
                    {:op, 0, :++, {:call, 0, {:atom, 0, :listify}, [{:var, 0, :Actions}]},
                     {:cons, 0, {:tuple, 0, [{:atom, 0, :postpone}, {:atom, 0, false}]}, {nil, 0}}}},
                   {:match, 0, {:var, 0, :Modules}, {:cons, 0, {:var, 0, :Module}, {nil, 0}}},
                   {:match, 0, {:var, 0, :P},
                    {:record, 0, :params,
                     [
                       {:record_field, 0, {:atom, 0, :parent}, {:var, 0, :Parent}},
                       {:record_field, 0, {:atom, 0, :name}, {:var, 0, :Name}},
                       {:record_field, 0, {:atom, 0, :hibernate_after},
                        {:var, 0, :HibernateAfterTimeout}}
                     ]}},
                   {:match, 0, {:var, 0, :S},
                    {:record, 0, :state,
                     [
                       {:record_field, 0, {:atom, 0, :state_data},
                        {:tuple, 0, [{:var, 0, :State}, {:var, 0, :Data}]}}
                     ]}},
                   {:case, 0,
                    {:call, 0, {:atom, 0, :get_callback_mode},
                     [{:var, 0, :P}, {:var, 0, :Modules}]},
                    [
                      {:clause, 0, [{:match, 0, {:record, 0, :params, []}, {:var, 0, :P_1}}], [],
                       [
                         {:match, 0, {:var, 0, :Debug_1},
                          {:case, 0, {:block, 0, [{:var, 0, :Debug}]},
                           [
                             {:clause, 0, [nil: 0], [], [{:block, 0, [{:var, 0, :Debug}]}]},
                             {:clause, 0, [{:var, 0, :_}], [],
                              [
                                {:call, 0, {:atom, 0, :sys_debug},
                                 [
                                   {:block, 0, [{:var, 0, :Debug}]},
                                   {:block, 0, [{:var, 0, :Name}]},
                                   {:block, 0,
                                    [
                                      {:tuple, 0,
                                       [
                                         {:atom, 0, :enter},
                                         {:var, 0, :Module},
                                         {:var, 0, :State}
                                       ]}
                                    ]}
                                 ]}
                              ]}
                           ]}},
                         {:call, 0, {:atom, 0, :loop_enter},
                          [
                            {:var, 0, :P_1},
                            {:var, 0, :Debug_1},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:tuple, 0, [{:var, 0, :State}, {:var, 0, :Data}]},
                            {:var, 0, :Actions_1}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}]}
                       ], [],
                       [
                         {:match, 0, {:var, 0, :P_1},
                          {:record, 0, {:var, 0, :P}, :params,
                           [{:record_field, 0, {:atom, 0, :modules}, {:var, 0, :Modules}}]}},
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:var, 0, :Class},
                            {:var, 0, :Reason},
                            {:var, 0, :Stacktrace},
                            {:var, 0, :P_1},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :init_it, 6,
              [
                {:clause, 0,
                 [
                   {:var, 0, :Starter},
                   {:atom, 0, :self},
                   {:var, 0, :ServerRef},
                   {:var, 0, :Module},
                   {:var, 0, :Args},
                   {:var, 0, :Opts}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :init_it},
                    [
                      {:var, 0, :Starter},
                      {:call, 0, {:atom, 0, :self}, []},
                      {:var, 0, :ServerRef},
                      {:var, 0, :Module},
                      {:var, 0, :Args},
                      {:var, 0, :Opts}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :Starter},
                   {:var, 0, :Parent},
                   {:var, 0, :ServerRef},
                   {:var, 0, :Module},
                   {:var, 0, :Args},
                   {:var, 0, :Opts}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :Name},
                    {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :get_proc_name}},
                     [{:var, 0, :ServerRef}]}},
                   {:match, 0, {:var, 0, :Debug},
                    {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :debug_options}},
                     [{:var, 0, :Name}, {:var, 0, :Opts}]}},
                   {:match, 0, {:var, 0, :HibernateAfterTimeout},
                    {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :hibernate_after}},
                     [{:var, 0, :Opts}]}},
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:var, 0, :Module}, {:atom, 0, :init}},
                       [{:var, 0, :Args}]}
                    ],
                    [
                      {:clause, 0, [{:var, 0, :Result}], [],
                       [
                         {:call, 0, {:atom, 0, :init_result},
                          [
                            {:var, 0, :Starter},
                            {:var, 0, :Parent},
                            {:var, 0, :ServerRef},
                            {:var, 0, :Module},
                            {:var, 0, :Result},
                            {:var, 0, :Name},
                            {:var, 0, :Debug},
                            {:var, 0, :HibernateAfterTimeout}
                          ]}
                       ]}
                    ],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :throw}, {:var, 0, :Result}, {:var, 0, :_}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :init_result},
                          [
                            {:var, 0, :Starter},
                            {:var, 0, :Parent},
                            {:var, 0, :ServerRef},
                            {:var, 0, :Module},
                            {:var, 0, :Result},
                            {:var, 0, :Name},
                            {:var, 0, :Debug},
                            {:var, 0, :HibernateAfterTimeout}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}]}
                       ], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :unregister_name}},
                          [{:var, 0, :ServerRef}]},
                         {:call, 0, {:atom, 0, :error_info},
                          [
                            {:var, 0, :Class},
                            {:var, 0, :Reason},
                            {:var, 0, :Stacktrace},
                            {:var, 0, :Debug},
                            {:record, 0, :params,
                             [
                               {:record_field, 0, {:atom, 0, :parent}, {:var, 0, :Parent}},
                               {:record_field, 0, {:atom, 0, :name}, {:var, 0, :Name}},
                               {:record_field, 0, {:atom, 0, :modules},
                                {:cons, 0, {:var, 0, :Module}, {nil, 0}}}
                             ]},
                            {:record, 0, :state, []},
                            {nil, 0}
                          ]},
                         {:call, 0, {:remote, 0, {:atom, 0, :proc_lib}, {:atom, 0, :init_fail}},
                          [
                            {:var, 0, :Starter},
                            {:tuple, 0, [{:atom, 0, :error}, {:var, 0, :Reason}]},
                            {:tuple, 0,
                             [
                               {:var, 0, :Class},
                               {:var, 0, :Reason},
                               {:var, 0, :Stacktrace}
                             ]}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:function, 0, :init_result, 8,
              [
                {:clause, 0,
                 [
                   {:var, 0, :Starter},
                   {:var, 0, :Parent},
                   {:var, 0, :ServerRef},
                   {:var, 0, :Module},
                   {:var, 0, :Result},
                   {:var, 0, :Name},
                   {:var, 0, :Debug},
                   {:var, 0, :HibernateAfterTimeout}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :Result},
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :ok}, {:var, 0, :State}, {:var, 0, :Data}]}
                       ], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :proc_lib}, {:atom, 0, :init_ack}},
                          [
                            {:var, 0, :Starter},
                            {:tuple, 0, [{:atom, 0, :ok}, {:call, 0, {:atom, 0, :self}, []}]}
                          ]},
                         {:call, 0, {:atom, 0, :enter},
                          [
                            {:var, 0, :Parent},
                            {:var, 0, :Debug},
                            {:var, 0, :Module},
                            {:var, 0, :Name},
                            {:var, 0, :HibernateAfterTimeout},
                            {:var, 0, :State},
                            {:var, 0, :Data},
                            {nil, 0}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :ok},
                            {:var, 0, :State},
                            {:var, 0, :Data},
                            {:var, 0, :Actions}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :proc_lib}, {:atom, 0, :init_ack}},
                          [
                            {:var, 0, :Starter},
                            {:tuple, 0, [{:atom, 0, :ok}, {:call, 0, {:atom, 0, :self}, []}]}
                          ]},
                         {:call, 0, {:atom, 0, :enter},
                          [
                            {:var, 0, :Parent},
                            {:var, 0, :Debug},
                            {:var, 0, :Module},
                            {:var, 0, :Name},
                            {:var, 0, :HibernateAfterTimeout},
                            {:var, 0, :State},
                            {:var, 0, :Data},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0, [{:tuple, 0, [{:atom, 0, :stop}, {:var, 0, :Reason}]}], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :unregister_name}},
                          [{:var, 0, :ServerRef}]},
                         {:call, 0, {:atom, 0, :exit}, [{:var, 0, :Reason}]}
                       ]},
                      {:clause, 0,
                       [
                         {:match, 0, {:tuple, 0, [{:atom, 0, :error}, {:var, 0, :_Reason}]},
                          {:var, 0, :ERROR}}
                       ], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :unregister_name}},
                          [{:var, 0, :ServerRef}]},
                         {:call, 0, {:remote, 0, {:atom, 0, :proc_lib}, {:atom, 0, :init_fail}},
                          [
                            {:var, 0, :Starter},
                            {:var, 0, :ERROR},
                            {:tuple, 0, [{:atom, 0, :exit}, {:atom, 0, :normal}]}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, :ignore}], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :unregister_name}},
                          [{:var, 0, :ServerRef}]},
                         {:call, 0, {:remote, 0, {:atom, 0, :proc_lib}, {:atom, 0, :init_fail}},
                          [
                            {:var, 0, :Starter},
                            {:atom, 0, :ignore},
                            {:tuple, 0, [{:atom, 0, :exit}, {:atom, 0, :normal}]}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :unregister_name}},
                          [{:var, 0, :ServerRef}]},
                         {:match, 0, {:var, 0, :Reason},
                          {:tuple, 0, [{:atom, 0, :bad_return_from_init}, {:var, 0, :Result}]}},
                         {:call, 0, {:atom, 0, :error_info},
                          [
                            {:atom, 0, :error},
                            {:var, 0, :Reason},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :Debug},
                            {:record, 0, :params,
                             [
                               {:record_field, 0, {:atom, 0, :parent}, {:var, 0, :Parent}},
                               {:record_field, 0, {:atom, 0, :name}, {:var, 0, :Name}},
                               {:record_field, 0, {:atom, 0, :modules},
                                {:cons, 0, {:var, 0, :Module}, {nil, 0}}}
                             ]},
                            {:record, 0, :state, []},
                            {nil, 0}
                          ]},
                         {:call, 0, {:atom, 0, :exit}, [{:var, 0, :Reason}]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :system_continue, 3,
              [
                {:clause, 0,
                 [
                   {:var, 0, :Parent},
                   {:var, 0, :Debug},
                   {:tuple, 0, [{:var, 0, :P}, {:var, 0, :S}]}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :loop},
                    [
                      {:call, 0, {:atom, 0, :update_parent}, [{:var, 0, :P}, {:var, 0, :Parent}]},
                      {:var, 0, :Debug},
                      {:var, 0, :S}
                    ]}
                 ]}
              ]},
             {:function, 0, :system_terminate, 4,
              [
                {:clause, 0,
                 [
                   {:var, 0, :Reason},
                   {:var, 0, :Parent},
                   {:var, 0, :Debug},
                   {:tuple, 0, [{:var, 0, :P}, {:var, 0, :S}]}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :terminate},
                    [
                      {:atom, 0, :exit},
                      {:var, 0, :Reason},
                      {:call, 0, {:atom, 0, :element},
                       [
                         {:integer, 0, 2},
                         {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                          [
                            {:call, 0, {:atom, 0, :self}, []},
                            {:atom, 0, :current_stacktrace}
                          ]}
                       ]},
                      {:call, 0, {:atom, 0, :update_parent}, [{:var, 0, :P}, {:var, 0, :Parent}]},
                      {:var, 0, :Debug},
                      {:var, 0, :S},
                      {nil, 0}
                    ]}
                 ]}
              ]},
             {:function, 0, :system_code_change, 4,
              [
                {:clause, 0,
                 [
                   {:tuple, 0,
                    [
                      {:match, 0,
                       {:record, 0, :params,
                        [
                          {:record_field, 0, {:atom, 0, :modules},
                           {:match, 0, {:cons, 0, {:var, 0, :Module}, {:var, 0, :_}},
                            {:var, 0, :Modules}}}
                        ]}, {:var, 0, :P}},
                      {:match, 0,
                       {:record, 0, :state,
                        [
                          {:record_field, 0, {:atom, 0, :state_data},
                           {:tuple, 0, [{:var, 0, :State}, {:var, 0, :Data}]}}
                        ]}, {:var, 0, :S}}
                    ]},
                   {:var, 0, :_Mod},
                   {:var, 0, :OldVsn},
                   {:var, 0, :Extra}
                 ], [],
                 [
                   {:case, 0,
                    {:try, 0,
                     [
                       {:call, 0, {:remote, 0, {:var, 0, :Module}, {:atom, 0, :code_change}},
                        [
                          {:var, 0, :OldVsn},
                          {:var, 0, :State},
                          {:var, 0, :Data},
                          {:var, 0, :Extra}
                        ]}
                     ], [],
                     [
                       {:clause, 0,
                        [
                          {:tuple, 0, [{:atom, 0, :throw}, {:var, 0, :Result}, {:var, 0, :_}]}
                        ], [], [{:var, 0, :Result}]}
                     ], []},
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :ok}, {:var, 0, :NewState}, {:var, 0, :NewData}]}
                       ], [],
                       [
                         {:case, 0,
                          {:call, 0, {:atom, 0, :get_callback_mode},
                           [{:var, 0, :P}, {:var, 0, :Modules}]},
                          [
                            {:clause, 0,
                             [{:match, 0, {:record, 0, :params, []}, {:var, 0, :P_1}}], [],
                             [
                               {:tuple, 0,
                                [
                                  {:atom, 0, :ok},
                                  {:tuple, 0,
                                   [
                                     {:var, 0, :P_1},
                                     {:record, 0, {:var, 0, :S}, :state,
                                      [
                                        {:record_field, 0, {:atom, 0, :state_data},
                                         {:tuple, 0, [{:var, 0, :NewState}, {:var, 0, :NewData}]}}
                                      ]}
                                   ]}
                                ]}
                             ]},
                            {:clause, 0,
                             [
                               {:tuple, 0,
                                [
                                  {:var, 0, :Class},
                                  {:var, 0, :Reason},
                                  {:var, 0, :Stacktrace}
                                ]}
                             ], [],
                             [
                               {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :raise}},
                                [
                                  {:var, 0, :Class},
                                  {:var, 0, :Reason},
                                  {:var, 0, :Stacktrace}
                                ]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:match, 0, {:tuple, 0, [{:atom, 0, :ok}, {:var, 0, :_}]},
                          {:var, 0, :Error}}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :error},
                          [{:tuple, 0, [{:atom, 0, :case_clause}, {:var, 0, :Error}]}]}
                       ]},
                      {:clause, 0, [{:var, 0, :Error}], [], [{:var, 0, :Error}]}
                    ]}
                 ]}
              ]},
             {:function, 0, :system_get_state, 1,
              [
                {:clause, 0,
                 [
                   {:tuple, 0,
                    [
                      {:var, 0, :_P},
                      {:record, 0, :state,
                       [
                         {:record_field, 0, {:atom, 0, :state_data}, {:var, 0, :State_Data}}
                       ]}
                    ]}
                 ], [], [{:tuple, 0, [{:atom, 0, :ok}, {:var, 0, :State_Data}]}]}
              ]},
             {:function, 0, :system_replace_state, 2,
              [
                {:clause, 0,
                 [
                   {:var, 0, :StateFun},
                   {:tuple, 0,
                    [
                      {:var, 0, :P},
                      {:match, 0,
                       {:record, 0, :state,
                        [
                          {:record_field, 0, {:atom, 0, :state_data}, {:var, 0, :State_Data}}
                        ]}, {:var, 0, :S}}
                    ]}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :NewState_NewData},
                    {:call, 0, {:var, 0, :StateFun}, [{:var, 0, :State_Data}]}},
                   {:tuple, 0,
                    [
                      {:atom, 0, :ok},
                      {:var, 0, :NewState_NewData},
                      {:tuple, 0,
                       [
                         {:var, 0, :P},
                         {:record, 0, {:var, 0, :S}, :state,
                          [
                            {:record_field, 0, {:atom, 0, :state_data},
                             {:var, 0, :NewState_NewData}}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :format_status, 2,
              [
                {:clause, 0,
                 [
                   {:var, 0, :Opt},
                   {:cons, 0, {:var, 0, :PDict},
                    {:cons, 0, {:var, 0, :SysState},
                     {:cons, 0, {:var, 0, :Parent},
                      {:cons, 0, {:var, 0, :Debug},
                       {:cons, 0,
                        {:tuple, 0,
                         [
                           {:record, 0, :params,
                            [
                              {:record_field, 0, {:atom, 0, :name}, {:var, 0, :Name}},
                              {:record_field, 0, {:atom, 0, :modules},
                               {:match, 0, {:cons, 0, {:var, 0, :Mod}, {:var, 0, :_}},
                                {:var, 0, :Modules}}}
                            ]},
                           {:record, 0, :state,
                            [
                              {:record_field, 0, {:atom, 0, :postponed}, {:var, 0, :Postponed}},
                              {:record_field, 0, {:atom, 0, :timers}, {:var, 0, :Timers}},
                              {:record_field, 0, {:atom, 0, :state_data},
                               {:tuple, 0, [{:var, 0, :State}, {:var, 0, :Data}]}}
                            ]}
                         ]}, {nil, 0}}}}}}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :Header},
                    {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :format_status_header}},
                     [{:string, 0, 'Status for state machine'}, {:var, 0, :Name}]}},
                   {:match, 0, {:tuple, 0, [{:var, 0, :NumTimers}, {:var, 0, :ListTimers}]},
                    {:call, 0, {:atom, 0, :list_timeouts}, [{:var, 0, :Timers}]}},
                   {:match, 0, {:var, 0, :StatusMap},
                    {:map, 0,
                     [
                       {:map_field_assoc, 0, {:atom, 0, :state}, {:var, 0, :State}},
                       {:map_field_assoc, 0, {:atom, 0, :data}, {:var, 0, :Data}},
                       {:map_field_assoc, 0, {:atom, 0, :postponed}, {:var, 0, :Postponed}},
                       {:map_field_assoc, 0, {:atom, 0, :log},
                        {:call, 0, {:remote, 0, {:atom, 0, :sys}, {:atom, 0, :get_log}},
                         [{:var, 0, :Debug}]}},
                       {:map_field_assoc, 0, {:atom, 0, :timeouts}, {:var, 0, :ListTimers}}
                     ]}},
                   {:match, 0, {:var, 0, :NewStatusMap},
                    {:case, 0,
                     {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :format_status}},
                      [
                        {:var, 0, :Mod},
                        {:var, 0, :Opt},
                        {:var, 0, :StatusMap},
                        {:cons, 0, {:var, 0, :PDict},
                         {:cons, 0, {:var, 0, :State}, {:cons, 0, {:var, 0, :Data}, {nil, 0}}}}
                      ]},
                     [
                       {:clause, 0,
                        [
                          {:map, 0, [{:map_field_exact, 0, {:atom, 0, :EXIT}, {:var, 0, :R}}]}
                        ], [],
                        [
                          {:match, 0, {:var, 0, :Crashed},
                           {:cons, 0,
                            {:tuple, 0,
                             [
                               {:atom, 0, :data},
                               {:cons, 0,
                                {:tuple, 0,
                                 [
                                   {:string, 0, 'State'},
                                   {:tuple, 0, [{:var, 0, :State}, {:var, 0, :R}]}
                                 ]}, {nil, 0}}
                             ]}, {nil, 0}}},
                          {:map, 0, {:var, 0, :StatusMap},
                           [
                             {:map_field_assoc, 0, {:atom, 0, :"$status"}, {:var, 0, :Crashed}}
                           ]}
                        ]},
                       {:clause, 0,
                        [
                          {:match, 0,
                           {:map, 0,
                            [{:map_field_exact, 0, {:atom, 0, :"$status"}, {:var, 0, :L}}]},
                           {:var, 0, :SM}}
                        ], [[{:call, 0, {:atom, 0, :is_list}, [{:var, 0, :L}]}]],
                        [{:var, 0, :SM}]},
                       {:clause, 0,
                        [
                          {:match, 0,
                           {:map, 0,
                            [{:map_field_exact, 0, {:atom, 0, :"$status"}, {:var, 0, :T}}]},
                           {:var, 0, :SM}}
                        ], [],
                        [
                          {:map, 0, {:var, 0, :SM},
                           [
                             {:map_field_exact, 0, {:atom, 0, :"$status"},
                              {:cons, 0, {:var, 0, :T}, {nil, 0}}}
                           ]}
                        ]},
                       {:clause, 0,
                        [
                          {:match, 0,
                           {:map, 0,
                            [
                              {:map_field_exact, 0, {:atom, 0, :state}, {:var, 0, :S}},
                              {:map_field_exact, 0, {:atom, 0, :data}, {:var, 0, :D}}
                            ]}, {:var, 0, :SM}}
                        ], [],
                        [
                          {:map, 0, {:var, 0, :SM},
                           [
                             {:map_field_assoc, 0, {:atom, 0, :"$status"},
                              {:cons, 0,
                               {:tuple, 0,
                                [
                                  {:atom, 0, :data},
                                  {:cons, 0,
                                   {:tuple, 0,
                                    [
                                      {:string, 0, 'State'},
                                      {:tuple, 0, [{:var, 0, :S}, {:var, 0, :D}]}
                                    ]}, {nil, 0}}
                                ]}, {nil, 0}}}
                           ]}
                        ]}
                     ]}},
                   {:cons, 0, {:tuple, 0, [{:atom, 0, :header}, {:var, 0, :Header}]},
                    {:cons, 0,
                     {:tuple, 0,
                      [
                        {:atom, 0, :data},
                        {:cons, 0, {:tuple, 0, [{:string, 0, 'Status'}, {:var, 0, :SysState}]},
                         {:cons, 0, {:tuple, 0, [{:string, 0, 'Parent'}, {:var, 0, :Parent}]},
                          {:cons, 0, {:tuple, 0, [{:string, 0, 'Modules'}, {:var, 0, :Modules}]},
                           {:cons, 0,
                            {:tuple, 0,
                             [
                               {:string, 0, 'Time-outs'},
                               {:tuple, 0,
                                [
                                  {:var, 0, :NumTimers},
                                  {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :get}},
                                   [{:atom, 0, :timeouts}, {:var, 0, :NewStatusMap}]}
                                ]}
                             ]},
                            {:cons, 0,
                             {:tuple, 0,
                              [
                                {:string, 0, 'Logged Events'},
                                {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :get}},
                                 [{:atom, 0, :log}, {:var, 0, :NewStatusMap}]}
                              ]},
                             {:cons, 0,
                              {:tuple, 0,
                               [
                                 {:string, 0, 'Postponed'},
                                 {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :get}},
                                  [{:atom, 0, :postponed}, {:var, 0, :NewStatusMap}]}
                               ]}, {nil, 0}}}}}}}
                      ]},
                     {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :get}},
                      [{:atom, 0, :"$status"}, {:var, 0, :NewStatusMap}]}}}
                 ]}
              ]},
             {:attribute, 0, :compile, {:inline, {:update_parent, 2}}},
             {:function, 0, :update_parent, 2,
              [
                {:clause, 0, [{:var, 0, :P}, {:var, 0, :Parent}], [],
                 [
                   {:case, 0, {:var, 0, :P},
                    [
                      {:clause, 0,
                       [
                         {:record, 0, :params,
                          [{:record_field, 0, {:atom, 0, :parent}, {:var, 0, :Parent}}]}
                       ], [], [{:var, 0, :P}]},
                      {:clause, 0, [{:record, 0, :params, []}], [],
                       [
                         {:record, 0, {:var, 0, :P}, :params,
                          [{:record_field, 0, {:atom, 0, :parent}, {:var, 0, :Parent}}]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :sys_debug, 3,
              [
                {:clause, 0, [{:var, 0, :Debug}, {:var, 0, :NameState}, {:var, 0, :Entry}], [],
                 [
                   {:call, 0, {:remote, 0, {:atom, 0, :sys}, {:atom, 0, :handle_debug}},
                    [
                      {:var, 0, :Debug},
                      {:fun, 0, {:function, :print_event, 3}},
                      {:var, 0, :NameState},
                      {:var, 0, :Entry}
                    ]}
                 ]}
              ]},
             {:function, 0, :print_event, 3,
              [
                {:clause, 0, [{:var, 0, :Dev}, {:var, 0, :SystemEvent}, {:var, 0, :Name}], [],
                 [
                   {:case, 0, {:var, 0, :SystemEvent},
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :in}, {:var, 0, :Event}, {:var, 0, :State}]}
                       ], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :io}, {:atom, 0, :format}},
                          [
                            {:var, 0, :Dev},
                            {:string, 0, '*DBG* ~tp receive ~ts in state ~tp~n'},
                            {:cons, 0, {:var, 0, :Name},
                             {:cons, 0,
                              {:call, 0, {:atom, 0, :event_string}, [{:var, 0, :Event}]},
                              {:cons, 0, {:var, 0, :State}, {nil, 0}}}}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :out},
                            {:var, 0, :Reply},
                            {:tuple, 0, [{:var, 0, :To}, {:var, 0, :_Tag}]}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :io}, {:atom, 0, :format}},
                          [
                            {:var, 0, :Dev},
                            {:string, 0, '*DBG* ~tp send ~tp to ~tw~n'},
                            {:cons, 0, {:var, 0, :Name},
                             {:cons, 0, {:var, 0, :Reply}, {:cons, 0, {:var, 0, :To}, {nil, 0}}}}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :enter}, {:var, 0, :Module}, {:var, 0, :State}]}
                       ], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :io}, {:atom, 0, :format}},
                          [
                            {:var, 0, :Dev},
                            {:string, 0, '*DBG* ~tp enter ~tp in state ~tp~n'},
                            {:cons, 0, {:var, 0, :Name},
                             {:cons, 0, {:var, 0, :Module},
                              {:cons, 0, {:var, 0, :State}, {nil, 0}}}}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :module}, {:var, 0, :Module}, {:var, 0, :State}]}
                       ], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :io}, {:atom, 0, :format}},
                          [
                            {:var, 0, :Dev},
                            {:string, 0, '*DBG* ~tp module ~tp in state ~tp~n'},
                            {:cons, 0, {:var, 0, :Name},
                             {:cons, 0, {:var, 0, :Module},
                              {:cons, 0, {:var, 0, :State}, {nil, 0}}}}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [{:atom, 0, :start_timer}, {:var, 0, :Action}, {:var, 0, :State}]}
                       ], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :io}, {:atom, 0, :format}},
                          [
                            {:var, 0, :Dev},
                            {:string, 0, '*DBG* ~tp start_timer ~tp in state ~tp~n'},
                            {:cons, 0, {:var, 0, :Name},
                             {:cons, 0, {:var, 0, :Action},
                              {:cons, 0, {:var, 0, :State}, {nil, 0}}}}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :insert_timeout},
                            {:var, 0, :Event},
                            {:var, 0, :State}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :io}, {:atom, 0, :format}},
                          [
                            {:var, 0, :Dev},
                            {:string, 0, '*DBG* ~tp insert_timeout ~tp in state ~tp~n'},
                            {:cons, 0, {:var, 0, :Name},
                             {:cons, 0, {:var, 0, :Event},
                              {:cons, 0, {:var, 0, :State}, {nil, 0}}}}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [{:atom, 0, :terminate}, {:var, 0, :Reason}, {:var, 0, :State}]}
                       ], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :io}, {:atom, 0, :format}},
                          [
                            {:var, 0, :Dev},
                            {:string, 0, '*DBG* ~tp terminate ~tp in state ~tp~n'},
                            {:cons, 0, {:var, 0, :Name},
                             {:cons, 0, {:var, 0, :Reason},
                              {:cons, 0, {:var, 0, :State}, {nil, 0}}}}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:var, 0, :Tag},
                            {:var, 0, :Event},
                            {:var, 0, :State},
                            {:var, 0, :NextState}
                          ]}
                       ],
                       [
                         [{:op, 0, :"=:=", {:var, 0, :Tag}, {:atom, 0, :postpone}}],
                         [{:op, 0, :"=:=", {:var, 0, :Tag}, {:atom, 0, :consume}}]
                       ],
                       [
                         {:match, 0, {:var, 0, :StateString},
                          {:case, 0, {:var, 0, :NextState},
                           [
                             {:clause, 0, [{:var, 0, :State}], [],
                              [
                                {:call, 0, {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :format}},
                                 [
                                   {:string, 0, '~tp'},
                                   {:cons, 0, {:var, 0, :State}, {nil, 0}}
                                 ]}
                              ]},
                             {:clause, 0, [{:var, 0, :_}], [],
                              [
                                {:call, 0, {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :format}},
                                 [
                                   {:string, 0, '~tp => ~tp'},
                                   {:cons, 0, {:var, 0, :State},
                                    {:cons, 0, {:var, 0, :NextState}, {nil, 0}}}
                                 ]}
                              ]}
                           ]}},
                         {:call, 0, {:remote, 0, {:atom, 0, :io}, {:atom, 0, :format}},
                          [
                            {:var, 0, :Dev},
                            {:string, 0, '*DBG* ~tp ~tw ~ts in state ~ts~n'},
                            {:cons, 0, {:var, 0, :Name},
                             {:cons, 0, {:var, 0, :Tag},
                              {:cons, 0,
                               {:call, 0, {:atom, 0, :event_string}, [{:var, 0, :Event}]},
                               {:cons, 0, {:var, 0, :StateString}, {nil, 0}}}}}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :event_string, 1,
              [
                {:clause, 0, [{:var, 0, :Event}], [],
                 [
                   {:case, 0, {:var, 0, :Event},
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:tuple, 0,
                             [
                               {:atom, 0, :call},
                               {:tuple, 0, [{:var, 0, :Pid}, {:var, 0, :_Tag}]}
                             ]},
                            {:var, 0, :Request}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :format}},
                          [
                            {:string, 0, 'call ~tp from ~tw'},
                            {:cons, 0, {:var, 0, :Request}, {:cons, 0, {:var, 0, :Pid}, {nil, 0}}}
                          ]}
                       ]},
                      {:clause, 0,
                       [{:tuple, 0, [{:var, 0, :EventType}, {:var, 0, :EventContent}]}], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :format}},
                          [
                            {:string, 0, '~tw ~tp'},
                            {:cons, 0, {:var, 0, :EventType},
                             {:cons, 0, {:var, 0, :EventContent}, {nil, 0}}}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :wakeup_from_hibernate, 3,
              [
                {:clause, 0, [{:var, 0, :P}, {:var, 0, :Debug}, {:var, 0, :S}], [],
                 [
                   {:call, 0, {:atom, 0, :loop_receive},
                    [{:var, 0, :P}, {:var, 0, :Debug}, {:var, 0, :S}]}
                 ]}
              ]},
             {:function, 0, :loop, 3,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:match, 0,
                    {:record, 0, :state,
                     [{:record_field, 0, {:atom, 0, :hibernate}, {:atom, 0, true}}]},
                    {:var, 0, :S}}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :loop_hibernate},
                    [{:var, 0, :P}, {:var, 0, :Debug}, {:var, 0, :S}]}
                 ]},
                {:clause, 0, [{:var, 0, :P}, {:var, 0, :Debug}, {:var, 0, :S}], [],
                 [
                   {:call, 0, {:atom, 0, :loop_receive},
                    [{:var, 0, :P}, {:var, 0, :Debug}, {:var, 0, :S}]}
                 ]}
              ]},
             {:function, 0, :loop_hibernate, 3,
              [
                {:clause, 0, [{:var, 0, :P}, {:var, 0, :Debug}, {:var, 0, :S}], [],
                 [
                   {:call, 0, {:remote, 0, {:atom, 0, :proc_lib}, {:atom, 0, :hibernate}},
                    [
                      {:atom, 0, :tree_splitter_stress},
                      {:atom, 0, :wakeup_from_hibernate},
                      {:cons, 0, {:var, 0, :P},
                       {:cons, 0, {:var, 0, :Debug}, {:cons, 0, {:var, 0, :S}, {nil, 0}}}}
                    ]},
                   {:call, 0, {:atom, 0, :error},
                    [
                      {:tuple, 0,
                       [
                         {:atom, 0, :should_not_have_arrived_here_but_instead_in},
                         {:tuple, 0,
                          [
                            {:atom, 0, :tree_splitter_stress},
                            {:atom, 0, :wakeup_from_hibernate},
                            {:integer, 0, 3}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_receive, 3,
              [
                {:clause, 0,
                 [
                   {:match, 0,
                    {:record, 0, :params,
                     [
                       {:record_field, 0, {:atom, 0, :hibernate_after},
                        {:var, 0, :HibernateAfterTimeout}}
                     ]}, {:var, 0, :P}},
                   {:var, 0, :Debug},
                   {:var, 0, :S}
                 ], [],
                 [
                   {:receive, 0,
                    [
                      {:clause, 0, [{:var, 0, :Msg}], [],
                       [
                         {:case, 0, {:var, 0, :Msg},
                          [
                            {:clause, 0,
                             [
                               {:tuple, 0,
                                [
                                  {:atom, 0, :"$gen_call"},
                                  {:var, 0, :From},
                                  {:var, 0, :Request}
                                ]}
                             ], [],
                             [
                               {:call, 0, {:atom, 0, :loop_receive_result},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:var, 0, :S},
                                  {:tuple, 0,
                                   [
                                     {:tuple, 0, [{:atom, 0, :call}, {:var, 0, :From}]},
                                     {:var, 0, :Request}
                                   ]}
                                ]}
                             ]},
                            {:clause, 0,
                             [{:tuple, 0, [{:atom, 0, :"$gen_cast"}, {:var, 0, :Cast}]}], [],
                             [
                               {:call, 0, {:atom, 0, :loop_receive_result},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:var, 0, :S},
                                  {:tuple, 0, [{:atom, 0, :cast}, {:var, 0, :Cast}]}
                                ]}
                             ]},
                            {:clause, 0,
                             [
                               {:tuple, 0,
                                [
                                  {:atom, 0, :timeout},
                                  {:var, 0, :TimerRef},
                                  {:var, 0, :TimeoutType}
                                ]}
                             ], [],
                             [
                               {:case, 0,
                                {:record_field, 0, {:var, 0, :S}, :state, {:atom, 0, :timers}},
                                [
                                  {:clause, 0,
                                   [
                                     {:match, 0,
                                      {:map, 0,
                                       [
                                         {:map_field_exact, 0, {:var, 0, :TimeoutType},
                                          {:tuple, 0,
                                           [{:var, 0, :TimerRef}, {:var, 0, :TimeoutMsg}]}}
                                       ]}, {:var, 0, :Timers}}
                                   ],
                                   [
                                     [
                                       {:op, 0, :"=/=", {:var, 0, :TimeoutType}, {:atom, 0, :t0q}}
                                     ]
                                   ],
                                   [
                                     {:match, 0, {:var, 0, :Timers_1},
                                      {:call, 0,
                                       {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :remove}},
                                       [{:var, 0, :TimeoutType}, {:var, 0, :Timers}]}},
                                     {:match, 0, {:var, 0, :S_1},
                                      {:record, 0, {:var, 0, :S}, :state,
                                       [
                                         {:record_field, 0, {:atom, 0, :timers},
                                          {:var, 0, :Timers_1}}
                                       ]}},
                                     {:call, 0, {:atom, 0, :loop_receive_result},
                                      [
                                        {:var, 0, :P},
                                        {:var, 0, :Debug},
                                        {:var, 0, :S_1},
                                        {:tuple, 0,
                                         [{:var, 0, :TimeoutType}, {:var, 0, :TimeoutMsg}]}
                                      ]}
                                   ]},
                                  {:clause, 0, [{:map, 0, []}], [],
                                   [
                                     {:call, 0, {:atom, 0, :loop_receive_result},
                                      [
                                        {:var, 0, :P},
                                        {:var, 0, :Debug},
                                        {:var, 0, :S},
                                        {:tuple, 0, [{:atom, 0, :info}, {:var, 0, :Msg}]}
                                      ]}
                                   ]}
                                ]}
                             ]},
                            {:clause, 0,
                             [
                               {:tuple, 0,
                                [{:atom, 0, :system}, {:var, 0, :Pid}, {:var, 0, :Req}]}
                             ], [],
                             [
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :sys}, {:atom, 0, :handle_system_msg}},
                                [
                                  {:var, 0, :Req},
                                  {:var, 0, :Pid},
                                  {:record_field, 0, {:var, 0, :P}, :params, {:atom, 0, :parent}},
                                  {:atom, 0, :tree_splitter_stress},
                                  {:var, 0, :Debug},
                                  {:tuple, 0, [{:var, 0, :P}, {:var, 0, :S}]},
                                  {:record_field, 0, {:var, 0, :S}, :state,
                                   {:atom, 0, :hibernate}}
                                ]}
                             ]},
                            {:clause, 0,
                             [
                               {:tuple, 0,
                                [{:atom, 0, :EXIT}, {:var, 0, :Pid}, {:var, 0, :Reason}]}
                             ], [],
                             [
                               {:case, 0,
                                {:record_field, 0, {:var, 0, :P}, :params, {:atom, 0, :parent}},
                                [
                                  {:clause, 0, [{:var, 0, :Pid}], [],
                                   [
                                     {:call, 0, {:atom, 0, :terminate},
                                      [
                                        {:atom, 0, :exit},
                                        {:var, 0, :Reason},
                                        {:call, 0, {:atom, 0, :element},
                                         [
                                           {:integer, 0, 2},
                                           {:call, 0,
                                            {:remote, 0, {:atom, 0, :erlang},
                                             {:atom, 0, :process_info}},
                                            [
                                              {:call, 0, {:atom, 0, :self}, []},
                                              {:atom, 0, :current_stacktrace}
                                            ]}
                                         ]},
                                        {:var, 0, :P},
                                        {:var, 0, :Debug},
                                        {:var, 0, :S},
                                        {nil, 0}
                                      ]}
                                   ]},
                                  {:clause, 0, [{:var, 0, :_}], [],
                                   [
                                     {:call, 0, {:atom, 0, :loop_receive_result},
                                      [
                                        {:var, 0, :P},
                                        {:var, 0, :Debug},
                                        {:var, 0, :S},
                                        {:tuple, 0, [{:atom, 0, :info}, {:var, 0, :Msg}]}
                                      ]}
                                   ]}
                                ]}
                             ]},
                            {:clause, 0, [{:var, 0, :_}], [],
                             [
                               {:call, 0, {:atom, 0, :loop_receive_result},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:var, 0, :S},
                                  {:tuple, 0, [{:atom, 0, :info}, {:var, 0, :Msg}]}
                                ]}
                             ]}
                          ]}
                       ]}
                    ], {:var, 0, :HibernateAfterTimeout},
                    [
                      {:call, 0, {:atom, 0, :loop_hibernate},
                       [{:var, 0, :P}, {:var, 0, :Debug}, {:var, 0, :S}]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_receive_result, 4,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:match, 0, {nil, 0}, {:var, 0, :Debug}},
                   {:var, 0, :S},
                   {:var, 0, :Event}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :Events}, {nil, 0}},
                   {:call, 0, {:atom, 0, :loop_event},
                    [
                      {:var, 0, :P},
                      {:var, 0, :Debug},
                      {:var, 0, :S},
                      {:var, 0, :Event},
                      {:var, 0, :Events}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:match, 0,
                    {:record, 0, :params,
                     [{:record_field, 0, {:atom, 0, :name}, {:var, 0, :Name}}]}, {:var, 0, :P}},
                   {:var, 0, :Debug},
                   {:match, 0,
                    {:record, 0, :state,
                     [
                       {:record_field, 0, {:atom, 0, :state_data},
                        {:tuple, 0, [{:var, 0, :State}, {:var, 0, :_Data}]}}
                     ]}, {:var, 0, :S}},
                   {:var, 0, :Event}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :Debug_1},
                    {:call, 0, {:atom, 0, :sys_debug},
                     [
                       {:var, 0, :Debug},
                       {:var, 0, :Name},
                       {:tuple, 0, [{:atom, 0, :in}, {:var, 0, :Event}, {:var, 0, :State}]}
                     ]}},
                   {:match, 0, {:var, 0, :Events}, {nil, 0}},
                   {:call, 0, {:atom, 0, :loop_event},
                    [
                      {:var, 0, :P},
                      {:var, 0, :Debug_1},
                      {:var, 0, :S},
                      {:var, 0, :Event},
                      {:var, 0, :Events}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_event, 5,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:match, 0,
                    {:record, 0, :state,
                     [{:record_field, 0, {:atom, 0, :hibernate}, {:atom, 0, true}}]},
                    {:var, 0, :S}},
                   {:var, 0, :Event},
                   {:var, 0, :Events}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :_}, {:call, 0, {:atom, 0, :garbage_collect}, []}},
                   {:match, 0, {:var, 0, :Q}, {:cons, 0, {:var, 0, :Event}, {:var, 0, :Events}}},
                   {:call, 0, {:atom, 0, :loop_state_callback},
                    [
                      {:var, 0, :P},
                      {:var, 0, :Debug},
                      {:var, 0, :S},
                      {:var, 0, :Q},
                      {:record_field, 0, {:var, 0, :S}, :state, {:atom, 0, :state_data}},
                      {:var, 0, :Event}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Event},
                   {:var, 0, :Events}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :Q}, {:cons, 0, {:var, 0, :Event}, {:var, 0, :Events}}},
                   {:call, 0, {:atom, 0, :loop_state_callback},
                    [
                      {:var, 0, :P},
                      {:var, 0, :Debug},
                      {:var, 0, :S},
                      {:var, 0, :Q},
                      {:record_field, 0, {:var, 0, :S}, :state, {:atom, 0, :state_data}},
                      {:var, 0, :Event}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :compile, {:inline, [loop_state_enter: 9]}},
             {:function, 0, :loop_state_enter, 9,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:match, 0,
                    {:record, 0, :state,
                     [
                       {:record_field, 0, {:atom, 0, :state_data},
                        {:tuple, 0, [{:var, 0, :PrevState}, {:var, 0, :_PrevData}]}}
                     ]}, {:var, 0, :S}},
                   {:var, 0, :Q},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :StateCall}, {:atom, 0, false}},
                   {:match, 0, {:var, 0, :CallbackEvent},
                    {:tuple, 0, [{:atom, 0, :enter}, {:var, 0, :PrevState}]}},
                   {:call, 0, {:atom, 0, :loop_state_callback},
                    [
                      {:var, 0, :P},
                      {:var, 0, :Debug},
                      {:var, 0, :S},
                      {:var, 0, :Q},
                      {:var, 0, :NextState_NewData},
                      {:var, 0, :NextEventsR},
                      {:var, 0, :Hibernate},
                      {:var, 0, :TimeoutsR},
                      {:var, 0, :Postpone},
                      {:var, 0, :StateCall},
                      {:var, 0, :CallbackEvent}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :compile, {:inline, [loop_enter: 6]}},
             {:function, 0, :loop_enter, 6,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :State_Data},
                   {:var, 0, :Actions}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :NextEventsR}, {nil, 0}},
                   {:match, 0, {:var, 0, :Hibernate}, {:atom, 0, false}},
                   {:match, 0, {:var, 0, :TimeoutsR}, {nil, 0}},
                   {:match, 0, {:var, 0, :Postpone}, {:atom, 0, false}},
                   {:match, 0, {:var, 0, :CallEnter}, {:atom, 0, true}},
                   {:match, 0, {:var, 0, :StateCall}, {:atom, 0, true}},
                   {:call, 0, {:atom, 0, :loop_actions_list},
                    [
                      {:var, 0, :P},
                      {:var, 0, :Debug},
                      {:var, 0, :S},
                      {:var, 0, :Q},
                      {:var, 0, :State_Data},
                      {:var, 0, :NextEventsR},
                      {:var, 0, :Hibernate},
                      {:var, 0, :TimeoutsR},
                      {:var, 0, :Postpone},
                      {:var, 0, :CallEnter},
                      {:var, 0, :StateCall},
                      {:var, 0, :Actions}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :compile, {:inline, [loop_state_callback: 6]}},
             {:function, 0, :loop_state_callback, 6,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :State_Data},
                   {:var, 0, :CallbackEvent}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :NextEventsR}, {nil, 0}},
                   {:match, 0, {:var, 0, :Hibernate}, {:atom, 0, false}},
                   {:match, 0, {:var, 0, :TimeoutsR}, {nil, 0}},
                   {:match, 0, {:var, 0, :Postpone}, {:atom, 0, false}},
                   {:match, 0, {:var, 0, :StateCall}, {:atom, 0, true}},
                   {:call, 0, {:atom, 0, :loop_state_callback},
                    [
                      {:var, 0, :P},
                      {:var, 0, :Debug},
                      {:var, 0, :S},
                      {:var, 0, :Q},
                      {:var, 0, :State_Data},
                      {:var, 0, :NextEventsR},
                      {:var, 0, :Hibernate},
                      {:var, 0, :TimeoutsR},
                      {:var, 0, :Postpone},
                      {:var, 0, :StateCall},
                      {:var, 0, :CallbackEvent}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_state_callback, 11,
              [
                {:clause, 0,
                 [
                   {:match, 0,
                    {:record, 0, :params,
                     [
                       {:record_field, 0, {:atom, 0, :callback_mode}, {:var, 0, :CallbackMode}},
                       {:record_field, 0, {:atom, 0, :modules},
                        {:cons, 0, {:var, 0, :Module}, {:var, 0, :_}}}
                     ]}, {:var, 0, :P}},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:match, 0, {:tuple, 0, [{:var, 0, :State}, {:var, 0, :Data}]},
                    {:var, 0, :State_Data}},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone},
                   {:var, 0, :StateCall},
                   {:tuple, 0, [{:var, 0, :Type}, {:var, 0, :Content}]}
                 ], [],
                 [
                   {:try, 0,
                    [
                      {:case, 0, {:var, 0, :CallbackMode},
                       [
                         {:clause, 0, [{:atom, 0, :state_functions}], [],
                          [
                            {:call, 0, {:remote, 0, {:var, 0, :Module}, {:var, 0, :State}},
                             [{:var, 0, :Type}, {:var, 0, :Content}, {:var, 0, :Data}]}
                          ]},
                         {:clause, 0, [{:atom, 0, :handle_event_function}], [],
                          [
                            {:call, 0,
                             {:remote, 0, {:var, 0, :Module}, {:atom, 0, :handle_event}},
                             [
                               {:var, 0, :Type},
                               {:var, 0, :Content},
                               {:var, 0, :State},
                               {:var, 0, :Data}
                             ]}
                          ]}
                       ]}
                    ],
                    [
                      {:clause, 0, [{:var, 0, :Result}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_state_callback_result},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :State_Data},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:var, 0, :StateCall},
                            {:var, 0, :Result}
                          ]}
                       ]}
                    ],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :throw}, {:var, 0, :Result}, {:var, 0, :_}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_state_callback_result},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :State_Data},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:var, 0, :StateCall},
                            {:var, 0, :Result}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:var, 0, :Class},
                            {:var, 0, :Reason},
                            {:var, 0, :Stacktrace},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q}
                          ]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:function, 0, :loop_state_callback_result, 11,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:match, 0, {:tuple, 0, [{:var, 0, :State}, {:var, 0, :_Data}]},
                    {:var, 0, :State_Data}},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone},
                   {:var, 0, :StateCall},
                   {:var, 0, :Result}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :Result},
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [{:atom, 0, :next_state}, {:var, 0, :State}, {:var, 0, :NewData}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:tuple, 0, [{:var, 0, :State}, {:var, 0, :NewData}]},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:atom, 0, false}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :next_state},
                            {:var, 0, :NextState},
                            {:var, 0, :NewData}
                          ]}
                       ], [[{:var, 0, :StateCall}]],
                       [
                         {:call, 0, {:atom, 0, :loop_actions},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:tuple, 0, [{:var, 0, :NextState}, {:var, 0, :NewData}]},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:atom, 0, true}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :next_state},
                            {:var, 0, :_NextState},
                            {:var, 0, :_NewData}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:atom, 0, :error},
                            {:tuple, 0,
                             [
                               {:atom, 0, :bad_state_enter_return_from_state_function},
                               {:var, 0, :Result}
                             ]},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:var, 0, :State_Data}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :next_state},
                            {:var, 0, :State},
                            {:var, 0, :NewData},
                            {:var, 0, :Actions}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:tuple, 0, [{:var, 0, :State}, {:var, 0, :NewData}]},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:atom, 0, false},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :next_state},
                            {:var, 0, :NextState},
                            {:var, 0, :NewData},
                            {:var, 0, :Actions}
                          ]}
                       ], [[{:var, 0, :StateCall}]],
                       [
                         {:call, 0, {:atom, 0, :loop_actions},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:tuple, 0, [{:var, 0, :NextState}, {:var, 0, :NewData}]},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:atom, 0, true},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :next_state},
                            {:var, 0, :_NextState},
                            {:var, 0, :_NewData},
                            {:var, 0, :_Actions}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:atom, 0, :error},
                            {:tuple, 0,
                             [
                               {:atom, 0, :bad_state_enter_return_from_state_function},
                               {:var, 0, :Result}
                             ]},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:var, 0, :State_Data}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q}
                          ]}
                       ]},
                      {:clause, 0, [{:tuple, 0, [{:atom, 0, :keep_state}, {:var, 0, :NewData}]}],
                       [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:tuple, 0, [{:var, 0, :State}, {:var, 0, :NewData}]},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:atom, 0, false}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :keep_state},
                            {:var, 0, :NewData},
                            {:var, 0, :Actions}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:tuple, 0, [{:var, 0, :State}, {:var, 0, :NewData}]},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:atom, 0, false},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, :keep_state_and_data}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :State_Data},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:atom, 0, false}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :keep_state_and_data}, {:var, 0, :Actions}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :State_Data},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:atom, 0, false},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0,
                       [{:tuple, 0, [{:atom, 0, :repeat_state}, {:var, 0, :NewData}]}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:tuple, 0, [{:var, 0, :State}, {:var, 0, :NewData}]},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:atom, 0, true}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :repeat_state},
                            {:var, 0, :NewData},
                            {:var, 0, :Actions}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:tuple, 0, [{:var, 0, :State}, {:var, 0, :NewData}]},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:atom, 0, true},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, :repeat_state_and_data}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :State_Data},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:atom, 0, true}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :repeat_state_and_data}, {:var, 0, :Actions}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :State_Data},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:atom, 0, true},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, :stop}], [],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:atom, 0, :exit},
                            {:atom, 0, :normal},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:var, 0, :State_Data}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q}
                          ]}
                       ]},
                      {:clause, 0, [{:tuple, 0, [{:atom, 0, :stop}, {:var, 0, :Reason}]}], [],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:atom, 0, :exit},
                            {:var, 0, :Reason},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:var, 0, :State_Data}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :stop}, {:var, 0, :Reason}, {:var, 0, :NewData}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:atom, 0, :exit},
                            {:var, 0, :Reason},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:tuple, 0, [{:var, 0, :State}, {:var, 0, :NewData}]}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :stop_and_reply},
                            {:var, 0, :Reason},
                            {:var, 0, :Replies}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :reply_then_terminate},
                          [
                            {:atom, 0, :exit},
                            {:var, 0, :Reason},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:var, 0, :State_Data}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q},
                            {:var, 0, :Replies}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :stop_and_reply},
                            {:var, 0, :Reason},
                            {:var, 0, :Replies},
                            {:var, 0, :NewData}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :reply_then_terminate},
                          [
                            {:atom, 0, :exit},
                            {:var, 0, :Reason},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:tuple, 0, [{:var, 0, :State}, {:var, 0, :NewData}]}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q},
                            {:var, 0, :Replies}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:atom, 0, :error},
                            {:tuple, 0,
                             [
                               {:atom, 0, :bad_return_from_state_function},
                               {:var, 0, :Result}
                             ]},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:var, 0, :State_Data}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_actions, 12,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone},
                   {:var, 0, :CallEnter},
                   {:var, 0, :_StateCall},
                   {nil, 0}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :loop_actions},
                    [
                      {:var, 0, :P},
                      {:var, 0, :Debug},
                      {:var, 0, :S},
                      {:var, 0, :Q},
                      {:var, 0, :NextState_NewData},
                      {:var, 0, :NextEventsR},
                      {:var, 0, :Hibernate},
                      {:var, 0, :TimeoutsR},
                      {:var, 0, :Postpone},
                      {:var, 0, :CallEnter}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone},
                   {:var, 0, :CallEnter},
                   {:var, 0, :StateCall},
                   {:var, 0, :Actions}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :loop_actions_list},
                    [
                      {:var, 0, :P},
                      {:var, 0, :Debug},
                      {:var, 0, :S},
                      {:var, 0, :Q},
                      {:var, 0, :NextState_NewData},
                      {:var, 0, :NextEventsR},
                      {:var, 0, :Hibernate},
                      {:var, 0, :TimeoutsR},
                      {:var, 0, :Postpone},
                      {:var, 0, :CallEnter},
                      {:var, 0, :StateCall},
                      {:call, 0, {:atom, 0, :listify}, [{:var, 0, :Actions}]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_actions, 10,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone},
                   {:var, 0, :CallEnter}
                 ], [],
                 [
                   {:case, 0,
                    {:op, 0, :andalso, {:var, 0, :CallEnter},
                     {:record_field, 0, {:var, 0, :P}, :params, {:atom, 0, :state_enter}}},
                    [
                      {:clause, 0, [{:atom, 0, true}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_state_enter},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, false}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_state_transition},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_actions_list, 12,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone},
                   {:var, 0, :CallEnter},
                   {:var, 0, :_StateCall},
                   {nil, 0}
                 ], [],
                 [
                   {:case, 0,
                    {:record_field, 0, {:var, 0, :P}, :params, {:atom, 0, :state_enter}},
                    [
                      {:clause, 0, [{:atom, 0, true}], [[{:var, 0, :CallEnter}]],
                       [
                         {:call, 0, {:atom, 0, :loop_state_enter},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_state_transition},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone}
                          ]}
                       ]}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone},
                   {:var, 0, :CallEnter},
                   {:var, 0, :StateCall},
                   {:cons, 0, {:var, 0, :Action}, {:var, 0, :Actions}}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :Action},
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :reply}, {:var, 0, :From}, {:var, 0, :Reply}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions_reply},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:var, 0, :CallEnter},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions},
                            {:var, 0, :From},
                            {:var, 0, :Reply}
                          ]}
                       ]},
                      {:clause, 0,
                       [{:tuple, 0, [{:atom, 0, :hibernate}, {:var, 0, :Hibernate_1}]}],
                       [[{:call, 0, {:atom, 0, :is_boolean}, [{:var, 0, :Hibernate_1}]}]],
                       [
                         {:call, 0, {:atom, 0, :loop_actions_list},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate_1},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:var, 0, :CallEnter},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, :hibernate}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions_list},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:atom, 0, true},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:var, 0, :CallEnter},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0, [{:tuple, 0, [{:atom, 0, :postpone}, {:var, 0, :Postpone_1}]}],
                       [
                         [
                           {:op, 0, :orelse, {:op, 0, :not, {:var, 0, :Postpone_1}},
                            {:var, 0, :StateCall}}
                         ]
                       ],
                       [
                         {:call, 0, {:atom, 0, :loop_actions_list},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone_1},
                            {:var, 0, :CallEnter},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, :postpone}], [[{:var, 0, :StateCall}]],
                       [
                         {:call, 0, {:atom, 0, :loop_actions_list},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:atom, 0, true},
                            {:var, 0, :CallEnter},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, :postpone}], [],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:atom, 0, :error},
                            {:tuple, 0,
                             [
                               {:atom, 0, :bad_state_enter_action_from_state_function},
                               {:var, 0, :Action}
                             ]},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:var, 0, :NextState_NewData}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [{:atom, 0, :next_event}, {:var, 0, :Type}, {:var, 0, :Content}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions_next_event},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:var, 0, :CallEnter},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions},
                            {:var, 0, :Type},
                            {:var, 0, :Content}
                          ]}
                       ]},
                      {:clause, 0, [{:tuple, 0, [{:var, 0, :Tag}, {:var, 0, :NewModule}]}],
                       [
                         [
                           {:op, 0, :"=:=", {:var, 0, :Tag}, {:atom, 0, :change_callback_module}},
                           {:call, 0, {:atom, 0, :is_atom}, [{:var, 0, :NewModule}]}
                         ],
                         [
                           {:op, 0, :"=:=", {:var, 0, :Tag}, {:atom, 0, :push_callback_module}},
                           {:call, 0, {:atom, 0, :is_atom}, [{:var, 0, :NewModule}]}
                         ]
                       ],
                       [
                         {:if, 0,
                          [
                            {:clause, 0, [], [[{:var, 0, :StateCall}]],
                             [
                               {:match, 0, {:var, 0, :NewModules},
                                {:case, 0, {:var, 0, :Tag},
                                 [
                                   {:clause, 0, [{:atom, 0, :change_callback_module}], [],
                                    [
                                      {:cons, 0, {:var, 0, :NewModule},
                                       {:call, 0, {:atom, 0, :tl},
                                        [
                                          {:record_field, 0, {:var, 0, :P}, :params,
                                           {:atom, 0, :modules}}
                                        ]}}
                                    ]},
                                   {:clause, 0, [{:atom, 0, :push_callback_module}], [],
                                    [
                                      {:cons, 0, {:var, 0, :NewModule},
                                       {:record_field, 0, {:var, 0, :P}, :params,
                                        {:atom, 0, :modules}}}
                                    ]}
                                 ]}},
                               {:case, 0,
                                {:call, 0, {:atom, 0, :get_callback_mode},
                                 [{:var, 0, :P}, {:var, 0, :NewModules}]},
                                [
                                  {:clause, 0,
                                   [
                                     {:match, 0, {:record, 0, :params, []}, {:var, 0, :P_1}}
                                   ], [],
                                   [
                                     {:match, 0,
                                      {:tuple, 0, [{:var, 0, :NextState}, {:var, 0, :_NewData}]},
                                      {:var, 0, :NextState_NewData}},
                                     {:match, 0, {:var, 0, :Debug_1},
                                      {:case, 0, {:block, 0, [{:var, 0, :Debug}]},
                                       [
                                         {:clause, 0, [nil: 0], [],
                                          [{:block, 0, [{:var, 0, :Debug}]}]},
                                         {:clause, 0, [{:var, 0, :_}], [],
                                          [
                                            {:call, 0, {:atom, 0, :sys_debug},
                                             [
                                               {:block, 0, [{:var, 0, :Debug}]},
                                               {:block, 0,
                                                [
                                                  {:record_field, 0, {:var, 0, :P}, :params,
                                                   {:atom, 0, :name}}
                                                ]},
                                               {:block, 0,
                                                [
                                                  {:tuple, 0,
                                                   [
                                                     {:atom, 0, :module},
                                                     {:var, 0, :NewModule},
                                                     {:var, 0, :NextState}
                                                   ]}
                                                ]}
                                             ]}
                                          ]}
                                       ]}},
                                     {:call, 0, {:atom, 0, :loop_actions_list},
                                      [
                                        {:var, 0, :P_1},
                                        {:var, 0, :Debug_1},
                                        {:var, 0, :S},
                                        {:var, 0, :Q},
                                        {:var, 0, :NextState_NewData},
                                        {:var, 0, :NextEventsR},
                                        {:var, 0, :Hibernate},
                                        {:var, 0, :TimeoutsR},
                                        {:var, 0, :Postpone},
                                        {:var, 0, :CallEnter},
                                        {:var, 0, :StateCall},
                                        {:var, 0, :Actions}
                                      ]}
                                   ]},
                                  {:clause, 0,
                                   [
                                     {:tuple, 0,
                                      [
                                        {:var, 0, :Class},
                                        {:var, 0, :Reason},
                                        {:var, 0, :Stacktrace}
                                      ]}
                                   ], [],
                                   [
                                     {:call, 0, {:atom, 0, :terminate},
                                      [
                                        {:var, 0, :Class},
                                        {:var, 0, :Reason},
                                        {:var, 0, :Stacktrace},
                                        {:var, 0, :P},
                                        {:var, 0, :Debug},
                                        {:record, 0, {:var, 0, :S}, :state,
                                         [
                                           {:record_field, 0, {:atom, 0, :state_data},
                                            {:var, 0, :NextState_NewData}},
                                           {:record_field, 0, {:atom, 0, :hibernate},
                                            {:var, 0, :Hibernate}}
                                         ]},
                                        {:var, 0, :Q}
                                      ]}
                                   ]}
                                ]}
                             ]},
                            {:clause, 0, [], [[{:atom, 0, true}]],
                             [
                               {:call, 0, {:atom, 0, :terminate},
                                [
                                  {:atom, 0, :error},
                                  {:tuple, 0,
                                   [
                                     {:atom, 0, :bad_state_enter_action_from_state_function},
                                     {:var, 0, :Action}
                                   ]},
                                  {:call, 0, {:atom, 0, :element},
                                   [
                                     {:integer, 0, 2},
                                     {:call, 0,
                                      {:remote, 0, {:atom, 0, :erlang},
                                       {:atom, 0, :process_info}},
                                      [
                                        {:call, 0, {:atom, 0, :self}, []},
                                        {:atom, 0, :current_stacktrace}
                                      ]}
                                   ]},
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:record, 0, {:var, 0, :S}, :state,
                                   [
                                     {:record_field, 0, {:atom, 0, :state_data},
                                      {:var, 0, :NextState_NewData}},
                                     {:record_field, 0, {:atom, 0, :hibernate},
                                      {:var, 0, :Hibernate}}
                                   ]},
                                  {:var, 0, :Q}
                                ]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, :pop_callback_module}],
                       [
                         [
                           {:op, 0, :"=/=",
                            {:call, 0, {:atom, 0, :tl},
                             [
                               {:record_field, 0, {:var, 0, :P}, :params, {:atom, 0, :modules}}
                             ]}, {nil, 0}}
                         ]
                       ],
                       [
                         {:if, 0,
                          [
                            {:clause, 0, [], [[{:var, 0, :StateCall}]],
                             [
                               {:match, 0, {:var, 0, :NewModules},
                                {:call, 0, {:atom, 0, :tl},
                                 [
                                   {:record_field, 0, {:var, 0, :P}, :params,
                                    {:atom, 0, :modules}}
                                 ]}},
                               {:case, 0,
                                {:call, 0, {:atom, 0, :get_callback_mode},
                                 [{:var, 0, :P}, {:var, 0, :NewModules}]},
                                [
                                  {:clause, 0,
                                   [
                                     {:match, 0, {:record, 0, :params, []}, {:var, 0, :P_1}}
                                   ], [],
                                   [
                                     {:match, 0,
                                      {:tuple, 0, [{:var, 0, :NextState}, {:var, 0, :_NewData}]},
                                      {:var, 0, :NextState_NewData}},
                                     {:match, 0, {:var, 0, :Debug_1},
                                      {:case, 0, {:block, 0, [{:var, 0, :Debug}]},
                                       [
                                         {:clause, 0, [nil: 0], [],
                                          [{:block, 0, [{:var, 0, :Debug}]}]},
                                         {:clause, 0, [{:var, 0, :_}], [],
                                          [
                                            {:call, 0, {:atom, 0, :sys_debug},
                                             [
                                               {:block, 0, [{:var, 0, :Debug}]},
                                               {:block, 0,
                                                [
                                                  {:record_field, 0, {:var, 0, :P}, :params,
                                                   {:atom, 0, :name}}
                                                ]},
                                               {:block, 0,
                                                [
                                                  {:tuple, 0,
                                                   [
                                                     {:atom, 0, :module},
                                                     {:call, 0, {:atom, 0, :hd},
                                                      [{:var, 0, :NewModules}]},
                                                     {:var, 0, :NextState}
                                                   ]}
                                                ]}
                                             ]}
                                          ]}
                                       ]}},
                                     {:call, 0, {:atom, 0, :loop_actions_list},
                                      [
                                        {:var, 0, :P_1},
                                        {:var, 0, :Debug_1},
                                        {:var, 0, :S},
                                        {:var, 0, :Q},
                                        {:var, 0, :NextState_NewData},
                                        {:var, 0, :NextEventsR},
                                        {:var, 0, :Hibernate},
                                        {:var, 0, :TimeoutsR},
                                        {:var, 0, :Postpone},
                                        {:var, 0, :CallEnter},
                                        {:var, 0, :StateCall},
                                        {:var, 0, :Actions}
                                      ]}
                                   ]},
                                  {:clause, 0,
                                   [
                                     {:tuple, 0,
                                      [
                                        {:var, 0, :Class},
                                        {:var, 0, :Reason},
                                        {:var, 0, :Stacktrace}
                                      ]}
                                   ], [],
                                   [
                                     {:call, 0, {:atom, 0, :terminate},
                                      [
                                        {:var, 0, :Class},
                                        {:var, 0, :Reason},
                                        {:var, 0, :Stacktrace},
                                        {:var, 0, :P},
                                        {:var, 0, :Debug},
                                        {:record, 0, {:var, 0, :S}, :state,
                                         [
                                           {:record_field, 0, {:atom, 0, :state_data},
                                            {:var, 0, :NextState_NewData}},
                                           {:record_field, 0, {:atom, 0, :hibernate},
                                            {:var, 0, :Hibernate}}
                                         ]},
                                        {:var, 0, :Q}
                                      ]}
                                   ]}
                                ]}
                             ]},
                            {:clause, 0, [], [[{:atom, 0, true}]],
                             [
                               {:call, 0, {:atom, 0, :terminate},
                                [
                                  {:atom, 0, :error},
                                  {:tuple, 0,
                                   [
                                     {:atom, 0, :bad_state_enter_action_from_state_function},
                                     {:var, 0, :Action}
                                   ]},
                                  {:call, 0, {:atom, 0, :element},
                                   [
                                     {:integer, 0, 2},
                                     {:call, 0,
                                      {:remote, 0, {:atom, 0, :erlang},
                                       {:atom, 0, :process_info}},
                                      [
                                        {:call, 0, {:atom, 0, :self}, []},
                                        {:atom, 0, :current_stacktrace}
                                      ]}
                                   ]},
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:record, 0, {:var, 0, :S}, :state,
                                   [
                                     {:record_field, 0, {:atom, 0, :state_data},
                                      {:var, 0, :NextState_NewData}},
                                     {:record_field, 0, {:atom, 0, :hibernate},
                                      {:var, 0, :Hibernate}}
                                   ]},
                                  {:var, 0, :Q}
                                ]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions_list},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:var, 0, :CallEnter},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions},
                            {:var, 0, :Action}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_actions_list, 13,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone},
                   {:var, 0, :CallEnter},
                   {:var, 0, :StateCall},
                   {:var, 0, :Actions},
                   {:match, 0,
                    {:tuple, 0,
                     [
                       {:var, 0, :TimeoutType},
                       {:var, 0, :Time},
                       {:var, 0, :TimeoutMsg},
                       {:var, 0, :TimeoutOpts}
                     ]}, {:var, 0, :Timeout}}
                 ], [],
                 [
                   {:case, 0,
                    {:call, 0, {:atom, 0, :timeout_event_type}, [{:var, 0, :TimeoutType}]},
                    [
                      {:clause, 0, [{:atom, 0, true}], [],
                       [
                         {:case, 0, {:call, 0, {:atom, 0, :listify}, [{:var, 0, :TimeoutOpts}]},
                          [
                            {:clause, 0,
                             [
                               {:cons, 0, {:tuple, 0, [{:atom, 0, :abs}, {:atom, 0, true}]},
                                {nil, 0}}
                             ],
                             [
                               [
                                 {:op, 0, :orelse,
                                  {:call, 0, {:atom, 0, :is_integer}, [{:var, 0, :Time}]},
                                  {:op, 0, :"=:=", {:var, 0, :Time}, {:atom, 0, :infinity}}}
                               ]
                             ],
                             [
                               {:call, 0, {:atom, 0, :loop_actions_list},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:var, 0, :S},
                                  {:var, 0, :Q},
                                  {:var, 0, :NextState_NewData},
                                  {:var, 0, :NextEventsR},
                                  {:var, 0, :Hibernate},
                                  {:cons, 0, {:var, 0, :Timeout}, {:var, 0, :TimeoutsR}},
                                  {:var, 0, :Postpone},
                                  {:var, 0, :CallEnter},
                                  {:var, 0, :StateCall},
                                  {:var, 0, :Actions}
                                ]}
                             ]},
                            {:clause, 0,
                             [
                               {:cons, 0, {:tuple, 0, [{:atom, 0, :abs}, {:atom, 0, false}]},
                                {nil, 0}}
                             ],
                             [
                               [
                                 {:op, 0, :orelse,
                                  {:op, 0, :andalso,
                                   {:call, 0, {:atom, 0, :is_integer}, [{:var, 0, :Time}]},
                                   {:op, 0, :"=<", {:integer, 0, 0}, {:var, 0, :Time}}},
                                  {:op, 0, :"=:=", {:var, 0, :Time}, {:atom, 0, :infinity}}}
                               ]
                             ],
                             [
                               {:match, 0, {:var, 0, :RelativeTimeout},
                                {:tuple, 0,
                                 [
                                   {:var, 0, :TimeoutType},
                                   {:var, 0, :Time},
                                   {:var, 0, :TimeoutMsg}
                                 ]}},
                               {:call, 0, {:atom, 0, :loop_actions_list},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:var, 0, :S},
                                  {:var, 0, :Q},
                                  {:var, 0, :NextState_NewData},
                                  {:var, 0, :NextEventsR},
                                  {:var, 0, :Hibernate},
                                  {:cons, 0, {:var, 0, :RelativeTimeout}, {:var, 0, :TimeoutsR}},
                                  {:var, 0, :Postpone},
                                  {:var, 0, :CallEnter},
                                  {:var, 0, :StateCall},
                                  {:var, 0, :Actions}
                                ]}
                             ]},
                            {:clause, 0, [nil: 0],
                             [
                               [
                                 {:op, 0, :orelse,
                                  {:op, 0, :andalso,
                                   {:call, 0, {:atom, 0, :is_integer}, [{:var, 0, :Time}]},
                                   {:op, 0, :"=<", {:integer, 0, 0}, {:var, 0, :Time}}},
                                  {:op, 0, :"=:=", {:var, 0, :Time}, {:atom, 0, :infinity}}}
                               ]
                             ],
                             [
                               {:match, 0, {:var, 0, :RelativeTimeout},
                                {:tuple, 0,
                                 [
                                   {:var, 0, :TimeoutType},
                                   {:var, 0, :Time},
                                   {:var, 0, :TimeoutMsg}
                                 ]}},
                               {:call, 0, {:atom, 0, :loop_actions_list},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:var, 0, :S},
                                  {:var, 0, :Q},
                                  {:var, 0, :NextState_NewData},
                                  {:var, 0, :NextEventsR},
                                  {:var, 0, :Hibernate},
                                  {:cons, 0, {:var, 0, :RelativeTimeout}, {:var, 0, :TimeoutsR}},
                                  {:var, 0, :Postpone},
                                  {:var, 0, :CallEnter},
                                  {:var, 0, :StateCall},
                                  {:var, 0, :Actions}
                                ]}
                             ]},
                            {:clause, 0, [{:var, 0, :TimeoutOptsList}], [],
                             [
                               {:case, 0,
                                {:call, 0, {:atom, 0, :parse_timeout_opts_abs},
                                 [{:var, 0, :TimeoutOptsList}]},
                                [
                                  {:clause, 0, [{:atom, 0, true}],
                                   [
                                     [
                                       {:op, 0, :orelse,
                                        {:call, 0, {:atom, 0, :is_integer}, [{:var, 0, :Time}]},
                                        {:op, 0, :"=:=", {:var, 0, :Time}, {:atom, 0, :infinity}}}
                                     ]
                                   ],
                                   [
                                     {:call, 0, {:atom, 0, :loop_actions_list},
                                      [
                                        {:var, 0, :P},
                                        {:var, 0, :Debug},
                                        {:var, 0, :S},
                                        {:var, 0, :Q},
                                        {:var, 0, :NextState_NewData},
                                        {:var, 0, :NextEventsR},
                                        {:var, 0, :Hibernate},
                                        {:cons, 0, {:var, 0, :Timeout}, {:var, 0, :TimeoutsR}},
                                        {:var, 0, :Postpone},
                                        {:var, 0, :CallEnter},
                                        {:var, 0, :StateCall},
                                        {:var, 0, :Actions}
                                      ]}
                                   ]},
                                  {:clause, 0, [{:atom, 0, false}],
                                   [
                                     [
                                       {:op, 0, :orelse,
                                        {:op, 0, :andalso,
                                         {:call, 0, {:atom, 0, :is_integer}, [{:var, 0, :Time}]},
                                         {:op, 0, :"=<", {:integer, 0, 0}, {:var, 0, :Time}}},
                                        {:op, 0, :"=:=", {:var, 0, :Time}, {:atom, 0, :infinity}}}
                                     ]
                                   ],
                                   [
                                     {:match, 0, {:var, 0, :RelativeTimeout},
                                      {:tuple, 0,
                                       [
                                         {:var, 0, :TimeoutType},
                                         {:var, 0, :Time},
                                         {:var, 0, :TimeoutMsg}
                                       ]}},
                                     {:call, 0, {:atom, 0, :loop_actions_list},
                                      [
                                        {:var, 0, :P},
                                        {:var, 0, :Debug},
                                        {:var, 0, :S},
                                        {:var, 0, :Q},
                                        {:var, 0, :NextState_NewData},
                                        {:var, 0, :NextEventsR},
                                        {:var, 0, :Hibernate},
                                        {:cons, 0, {:var, 0, :RelativeTimeout},
                                         {:var, 0, :TimeoutsR}},
                                        {:var, 0, :Postpone},
                                        {:var, 0, :CallEnter},
                                        {:var, 0, :StateCall},
                                        {:var, 0, :Actions}
                                      ]}
                                   ]},
                                  {:clause, 0, [{:var, 0, :_}], [],
                                   [
                                     {:call, 0, {:atom, 0, :terminate},
                                      [
                                        {:atom, 0, :error},
                                        {:tuple, 0,
                                         [
                                           {:atom, 0, :bad_action_from_state_function},
                                           {:var, 0, :Timeout}
                                         ]},
                                        {:call, 0, {:atom, 0, :element},
                                         [
                                           {:integer, 0, 2},
                                           {:call, 0,
                                            {:remote, 0, {:atom, 0, :erlang},
                                             {:atom, 0, :process_info}},
                                            [
                                              {:call, 0, {:atom, 0, :self}, []},
                                              {:atom, 0, :current_stacktrace}
                                            ]}
                                         ]},
                                        {:var, 0, :P},
                                        {:var, 0, :Debug},
                                        {:record, 0, {:var, 0, :S}, :state,
                                         [
                                           {:record_field, 0, {:atom, 0, :state_data},
                                            {:var, 0, :NextState_NewData}},
                                           {:record_field, 0, {:atom, 0, :hibernate},
                                            {:var, 0, :Hibernate}}
                                         ]},
                                        {:var, 0, :Q}
                                      ]}
                                   ]}
                                ]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, false}], [],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:atom, 0, :error},
                            {:tuple, 0,
                             [
                               {:atom, 0, :bad_action_from_state_function},
                               {:var, 0, :Timeout}
                             ]},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:var, 0, :NextState_NewData}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q}
                          ]}
                       ]}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone},
                   {:var, 0, :CallEnter},
                   {:var, 0, :StateCall},
                   {:var, 0, :Actions},
                   {:match, 0,
                    {:tuple, 0,
                     [{:var, 0, :TimeoutType}, {:var, 0, :Time}, {:var, 0, :_TimeoutMsg}]},
                    {:var, 0, :Timeout}}
                 ], [],
                 [
                   {:case, 0,
                    {:call, 0, {:atom, 0, :timeout_event_type}, [{:var, 0, :TimeoutType}]},
                    [
                      {:clause, 0, [{:atom, 0, true}],
                       [
                         [
                           {:op, 0, :orelse,
                            {:op, 0, :andalso,
                             {:call, 0, {:atom, 0, :is_integer}, [{:var, 0, :Time}]},
                             {:op, 0, :"=<", {:integer, 0, 0}, {:var, 0, :Time}}},
                            {:op, 0, :"=:=", {:var, 0, :Time}, {:atom, 0, :infinity}}}
                         ],
                         [{:op, 0, :"=:=", {:var, 0, :Time}, {:atom, 0, :update}}]
                       ],
                       [
                         {:call, 0, {:atom, 0, :loop_actions_list},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:cons, 0, {:var, 0, :Timeout}, {:var, 0, :TimeoutsR}},
                            {:var, 0, :Postpone},
                            {:var, 0, :CallEnter},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:atom, 0, :error},
                            {:tuple, 0,
                             [
                               {:atom, 0, :bad_action_from_state_function},
                               {:var, 0, :Timeout}
                             ]},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:var, 0, :NextState_NewData}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q}
                          ]}
                       ]}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone},
                   {:var, 0, :CallEnter},
                   {:var, 0, :StateCall},
                   {:var, 0, :Actions},
                   {:match, 0, {:tuple, 0, [{:var, 0, :TimeoutType}, {:atom, 0, :cancel}]},
                    {:var, 0, :Action}}
                 ], [],
                 [
                   {:case, 0,
                    {:call, 0, {:atom, 0, :timeout_event_type}, [{:var, 0, :TimeoutType}]},
                    [
                      {:clause, 0, [{:atom, 0, true}], [],
                       [
                         {:match, 0, {:var, 0, :Timeout},
                          {:tuple, 0,
                           [
                             {:var, 0, :TimeoutType},
                             {:atom, 0, :infinity},
                             {:atom, 0, :undefined}
                           ]}},
                         {:call, 0, {:atom, 0, :loop_actions_list},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:cons, 0, {:var, 0, :Timeout}, {:var, 0, :TimeoutsR}},
                            {:var, 0, :Postpone},
                            {:var, 0, :CallEnter},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, false}], [],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:atom, 0, :error},
                            {:tuple, 0,
                             [
                               {:atom, 0, :bad_action_from_state_function},
                               {:var, 0, :Action}
                             ]},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:var, 0, :NextState_NewData}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q}
                          ]}
                       ]}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone},
                   {:var, 0, :CallEnter},
                   {:var, 0, :StateCall},
                   {:var, 0, :Actions},
                   {:var, 0, :Time}
                 ], [],
                 [
                   {:if, 0,
                    [
                      {:clause, 0, [],
                       [
                         [
                           {:op, 0, :orelse,
                            {:op, 0, :andalso,
                             {:call, 0, {:atom, 0, :is_integer}, [{:var, 0, :Time}]},
                             {:op, 0, :"=<", {:integer, 0, 0}, {:var, 0, :Time}}},
                            {:op, 0, :"=:=", {:var, 0, :Time}, {:atom, 0, :infinity}}}
                         ]
                       ],
                       [
                         {:match, 0, {:var, 0, :Timeout},
                          {:tuple, 0, [{:atom, 0, :timeout}, {:var, 0, :Time}, {:var, 0, :Time}]}},
                         {:call, 0, {:atom, 0, :loop_actions_list},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:cons, 0, {:var, 0, :Timeout}, {:var, 0, :TimeoutsR}},
                            {:var, 0, :Postpone},
                            {:var, 0, :CallEnter},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0, [], [[{:atom, 0, true}]],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:atom, 0, :error},
                            {:tuple, 0,
                             [
                               {:atom, 0, :bad_action_from_state_function},
                               {:var, 0, :Time}
                             ]},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:var, 0, :NextState_NewData}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_actions_reply, 14,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone},
                   {:var, 0, :CallEnter},
                   {:var, 0, :StateCall},
                   {:var, 0, :Actions},
                   {:var, 0, :From},
                   {:var, 0, :Reply}
                 ], [],
                 [
                   {:case, 0, {:call, 0, {:atom, 0, :from}, [{:var, 0, :From}]},
                    [
                      {:clause, 0, [{:atom, 0, true}], [],
                       [
                         {:call, 0, {:atom, 0, :reply}, [{:var, 0, :From}, {:var, 0, :Reply}]},
                         {:match, 0, {:var, 0, :Debug_1},
                          {:case, 0, {:block, 0, [{:var, 0, :Debug}]},
                           [
                             {:clause, 0, [nil: 0], [], [{:block, 0, [{:var, 0, :Debug}]}]},
                             {:clause, 0, [{:var, 0, :_}], [],
                              [
                                {:call, 0, {:atom, 0, :sys_debug},
                                 [
                                   {:block, 0, [{:var, 0, :Debug}]},
                                   {:block, 0,
                                    [
                                      {:record_field, 0, {:var, 0, :P}, :params,
                                       {:atom, 0, :name}}
                                    ]},
                                   {:block, 0,
                                    [
                                      {:tuple, 0,
                                       [
                                         {:atom, 0, :out},
                                         {:var, 0, :Reply},
                                         {:var, 0, :From}
                                       ]}
                                    ]}
                                 ]}
                              ]}
                           ]}},
                         {:call, 0, {:atom, 0, :loop_actions_list},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug_1},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postpone},
                            {:var, 0, :CallEnter},
                            {:var, 0, :StateCall},
                            {:var, 0, :Actions}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, false}], [],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:atom, 0, :error},
                            {:tuple, 0,
                             [
                               {:atom, 0, :bad_action_from_state_function},
                               {:tuple, 0,
                                [{:atom, 0, :reply}, {:var, 0, :From}, {:var, 0, :Reply}]}
                             ]},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:record, 0, {:var, 0, :S}, :state,
                             [
                               {:record_field, 0, {:atom, 0, :state_data},
                                {:var, 0, :NextState_NewData}},
                               {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                             ]},
                            {:var, 0, :Q}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_actions_next_event, 14,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone},
                   {:var, 0, :CallEnter},
                   {:var, 0, :StateCall},
                   {:var, 0, :Actions},
                   {:var, 0, :Type},
                   {:var, 0, :Content}
                 ], [],
                 [
                   {:case, 0, {:call, 0, {:atom, 0, :event_type}, [{:var, 0, :Type}]},
                    [
                      {:clause, 0, [{:atom, 0, true}], [[{:var, 0, :StateCall}]],
                       [
                         {:match, 0, {:var, 0, :NextEvent},
                          {:tuple, 0, [{:var, 0, :Type}, {:var, 0, :Content}]}},
                         {:if, 0,
                          [
                            {:clause, 0, [], [[{:op, 0, :"=:=", {:var, 0, :Debug}, {nil, 0}}]],
                             [
                               {:call, 0, {:atom, 0, :loop_actions_list},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:var, 0, :S},
                                  {:var, 0, :Q},
                                  {:var, 0, :NextState_NewData},
                                  {:cons, 0, {:var, 0, :NextEvent}, {:var, 0, :NextEventsR}},
                                  {:var, 0, :Hibernate},
                                  {:var, 0, :TimeoutsR},
                                  {:var, 0, :Postpone},
                                  {:var, 0, :CallEnter},
                                  {:var, 0, :StateCall},
                                  {:var, 0, :Actions}
                                ]}
                             ]},
                            {:clause, 0, [], [[{:atom, 0, true}]],
                             [
                               {:match, 0, {:var, 0, :Name},
                                {:record_field, 0, {:var, 0, :P}, :params, {:atom, 0, :name}}},
                               {:match, 0,
                                {:tuple, 0, [{:var, 0, :NextState}, {:var, 0, :_NewData}]},
                                {:var, 0, :NextState_NewData}},
                               {:match, 0, {:var, 0, :Debug_1},
                                {:call, 0, {:atom, 0, :sys_debug},
                                 [
                                   {:var, 0, :Debug},
                                   {:var, 0, :Name},
                                   {:tuple, 0,
                                    [
                                      {:atom, 0, :in},
                                      {:var, 0, :NextEvent},
                                      {:var, 0, :NextState}
                                    ]}
                                 ]}},
                               {:call, 0, {:atom, 0, :loop_actions_list},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug_1},
                                  {:var, 0, :S},
                                  {:var, 0, :Q},
                                  {:var, 0, :NextState_NewData},
                                  {:cons, 0, {:var, 0, :NextEvent}, {:var, 0, :NextEventsR}},
                                  {:var, 0, :Hibernate},
                                  {:var, 0, :TimeoutsR},
                                  {:var, 0, :Postpone},
                                  {:var, 0, :CallEnter},
                                  {:var, 0, :StateCall},
                                  {:var, 0, :Actions}
                                ]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_actions_next_event_bad},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :StateCall},
                            {:var, 0, :Hibernate},
                            {:var, 0, :Type},
                            {:var, 0, :Content}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_actions_next_event_bad, 9,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :StateCall},
                   {:var, 0, :Hibernate},
                   {:var, 0, :Type},
                   {:var, 0, :Content}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :terminate},
                    [
                      {:atom, 0, :error},
                      {:tuple, 0,
                       [
                         {:case, 0, {:var, 0, :StateCall},
                          [
                            {:clause, 0, [{:atom, 0, true}], [],
                             [{:atom, 0, :bad_action_from_state_function}]},
                            {:clause, 0, [{:atom, 0, false}], [],
                             [{:atom, 0, :bad_state_enter_action_from_state_function}]}
                          ]},
                         {:tuple, 0,
                          [{:atom, 0, :next_event}, {:var, 0, :Type}, {:var, 0, :Content}]}
                       ]},
                      {:call, 0, {:atom, 0, :element},
                       [
                         {:integer, 0, 2},
                         {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                          [
                            {:call, 0, {:atom, 0, :self}, []},
                            {:atom, 0, :current_stacktrace}
                          ]}
                       ]},
                      {:var, 0, :P},
                      {:var, 0, :Debug},
                      {:record, 0, {:var, 0, :S}, :state,
                       [
                         {:record_field, 0, {:atom, 0, :state_data},
                          {:var, 0, :NextState_NewData}},
                         {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                       ]},
                      {:var, 0, :Q}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_state_transition, 9,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:match, 0,
                    {:record, 0, :state,
                     [
                       {:record_field, 0, {:atom, 0, :state_data},
                        {:tuple, 0, [{:var, 0, :State}, {:var, 0, :_Data}]}},
                       {:record_field, 0, {:atom, 0, :postponed}, {:var, 0, :Postponed}}
                     ]}, {:var, 0, :S}},
                   {:cons, 0, {:var, 0, :Event}, {:var, 0, :Events}},
                   {:match, 0, {:tuple, 0, [{:var, 0, :NextState}, {:var, 0, :_NewData}]},
                    {:var, 0, :NextState_NewData}},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postpone}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :Postponed_1},
                    {:case, 0, {:var, 0, :Postpone},
                     [
                       {:clause, 0, [{:atom, 0, true}], [],
                        [{:cons, 0, {:var, 0, :Event}, {:var, 0, :Postponed}}]},
                       {:clause, 0, [{:atom, 0, false}], [], [{:var, 0, :Postponed}]}
                     ]}},
                   {:case, 0, {:var, 0, :Debug},
                    [
                      {:clause, 0, [nil: 0], [],
                       [
                         {:if, 0,
                          [
                            {:clause, 0, [],
                             [[{:op, 0, :"=:=", {:var, 0, :NextState}, {:var, 0, :State}}]],
                             [
                               {:call, 0, {:atom, 0, :loop_keep_state},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:var, 0, :S},
                                  {:var, 0, :Events},
                                  {:var, 0, :NextState_NewData},
                                  {:var, 0, :NextEventsR},
                                  {:var, 0, :Hibernate},
                                  {:var, 0, :TimeoutsR},
                                  {:var, 0, :Postponed_1}
                                ]}
                             ]},
                            {:clause, 0, [], [[{:atom, 0, true}]],
                             [
                               {:call, 0, {:atom, 0, :loop_state_change},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:var, 0, :S},
                                  {:var, 0, :Events},
                                  {:var, 0, :NextState_NewData},
                                  {:var, 0, :NextEventsR},
                                  {:var, 0, :Hibernate},
                                  {:var, 0, :TimeoutsR},
                                  {:var, 0, :Postponed_1}
                                ]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:match, 0, {:var, 0, :Name},
                          {:record_field, 0, {:var, 0, :P}, :params, {:atom, 0, :name}}},
                         {:match, 0, {:var, 0, :Debug_1},
                          {:case, 0, {:var, 0, :Postpone},
                           [
                             {:clause, 0, [{:atom, 0, true}], [],
                              [
                                {:call, 0, {:atom, 0, :sys_debug},
                                 [
                                   {:var, 0, :Debug},
                                   {:var, 0, :Name},
                                   {:tuple, 0,
                                    [
                                      {:atom, 0, :postpone},
                                      {:var, 0, :Event},
                                      {:var, 0, :State},
                                      {:var, 0, :NextState}
                                    ]}
                                 ]}
                              ]},
                             {:clause, 0, [{:atom, 0, false}], [],
                              [
                                {:call, 0, {:atom, 0, :sys_debug},
                                 [
                                   {:var, 0, :Debug},
                                   {:var, 0, :Name},
                                   {:tuple, 0,
                                    [
                                      {:atom, 0, :consume},
                                      {:var, 0, :Event},
                                      {:var, 0, :State},
                                      {:var, 0, :NextState}
                                    ]}
                                 ]}
                              ]}
                           ]}},
                         {:if, 0,
                          [
                            {:clause, 0, [],
                             [[{:op, 0, :"=:=", {:var, 0, :NextState}, {:var, 0, :State}}]],
                             [
                               {:call, 0, {:atom, 0, :loop_keep_state},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug_1},
                                  {:var, 0, :S},
                                  {:var, 0, :Events},
                                  {:var, 0, :NextState_NewData},
                                  {:var, 0, :NextEventsR},
                                  {:var, 0, :Hibernate},
                                  {:var, 0, :TimeoutsR},
                                  {:var, 0, :Postponed_1}
                                ]}
                             ]},
                            {:clause, 0, [], [[{:atom, 0, true}]],
                             [
                               {:call, 0, {:atom, 0, :loop_state_change},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug_1},
                                  {:var, 0, :S},
                                  {:var, 0, :Events},
                                  {:var, 0, :NextState_NewData},
                                  {:var, 0, :NextEventsR},
                                  {:var, 0, :Hibernate},
                                  {:var, 0, :TimeoutsR},
                                  {:var, 0, :Postponed_1}
                                ]}
                             ]}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_keep_state, 9,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:match, 0,
                    {:record, 0, :state,
                     [{:record_field, 0, {:atom, 0, :timers}, {:var, 0, :Timers}}]},
                    {:var, 0, :S}},
                   {:var, 0, :Events},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postponed}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :Timers},
                    [
                      {:clause, 0,
                       [
                         {:map, 0,
                          [
                            {:map_field_exact, 0, {:atom, 0, :timeout},
                             {:tuple, 0, [{:var, 0, :TimerRef}, {:var, 0, :_TimeoutMsg}]}}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_next_events},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:call, 0, {:atom, 0, :cancel_timer},
                             [
                               {:atom, 0, :timeout},
                               {:var, 0, :TimerRef},
                               {:var, 0, :Timers}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_next_events},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_state_change, 9,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Events},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postponed}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :Postponed},
                    [
                      {:clause, 0, [nil: 0], [],
                       [
                         {:call, 0, {:atom, 0, :loop_state_change},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR}
                          ]}
                       ]},
                      {:clause, 0, [{:cons, 0, {:var, 0, :E1}, {nil, 0}}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_state_change},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:cons, 0, {:var, 0, :E1}, {:var, 0, :Events}},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR}
                          ]}
                       ]},
                      {:clause, 0,
                       [{:cons, 0, {:var, 0, :E2}, {:cons, 0, {:var, 0, :E1}, {nil, 0}}}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_state_change},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:cons, 0, {:var, 0, :E1},
                             {:cons, 0, {:var, 0, :E2}, {:var, 0, :Events}}},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:cons, 0, {:var, 0, :_}, {:cons, 0, {:var, 0, :_}, {:var, 0, :_}}}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_state_change},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:call, 0, {:remote, 0, {:atom, 0, :lists}, {:atom, 0, :reverse}},
                             [{:var, 0, :Postponed}, {:var, 0, :Events}]},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_state_change, 8,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:match, 0,
                    {:record, 0, :state,
                     [{:record_field, 0, {:atom, 0, :timers}, {:var, 0, :Timers}}]},
                    {:var, 0, :S}},
                   {:var, 0, :Events},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :Timers},
                    [
                      {:clause, 0,
                       [
                         {:map, 0,
                          [
                            {:map_field_exact, 0, {:atom, 0, :state_timeout},
                             {:tuple, 0, [{:var, 0, :TimerRef}, {:var, 0, :_TimeoutMsg}]}}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_next_events},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {nil, 0},
                            {:call, 0, {:atom, 0, :cancel_timer},
                             [
                               {:atom, 0, :timeout},
                               {:call, 0, {:atom, 0, :cancel_timer},
                                [
                                  {:atom, 0, :state_timeout},
                                  {:var, 0, :TimerRef},
                                  {:var, 0, :Timers}
                                ]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:map, 0,
                          [
                            {:map_field_exact, 0, {:atom, 0, :timeout},
                             {:tuple, 0, [{:var, 0, :TimerRef}, {:var, 0, :_TimeoutMsg}]}}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_next_events},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {nil, 0},
                            {:call, 0, {:atom, 0, :cancel_timer},
                             [
                               {:atom, 0, :timeout},
                               {:var, 0, :TimerRef},
                               {:var, 0, :Timers}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_next_events},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {nil, 0},
                            {:var, 0, :Timers}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_next_events, 10,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Events},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {nil, 0},
                   {:var, 0, :Postponed},
                   {:var, 0, :Timers}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :loop_done},
                    [
                      {:var, 0, :P},
                      {:var, 0, :Debug},
                      {:record, 0, {:var, 0, :S}, :state,
                       [
                         {:record_field, 0, {:atom, 0, :state_data},
                          {:var, 0, :NextState_NewData}},
                         {:record_field, 0, {:atom, 0, :postponed}, {:var, 0, :Postponed}},
                         {:record_field, 0, {:atom, 0, :timers}, {:var, 0, :Timers}},
                         {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                       ]},
                      {:var, 0, :Events},
                      {:var, 0, :NextEventsR}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Events},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postponed},
                   {:var, 0, :Timers}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :Seen}, {:map, 0, []}},
                   {:match, 0, {:var, 0, :TimeoutEvents}, {nil, 0}},
                   {:call, 0, {:atom, 0, :loop_timeouts},
                    [
                      {:var, 0, :P},
                      {:var, 0, :Debug},
                      {:var, 0, :S},
                      {:var, 0, :Events},
                      {:var, 0, :NextState_NewData},
                      {:var, 0, :NextEventsR},
                      {:var, 0, :Hibernate},
                      {:var, 0, :TimeoutsR},
                      {:var, 0, :Postponed},
                      {:var, 0, :Timers},
                      {:var, 0, :Seen},
                      {:var, 0, :TimeoutEvents}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_timeouts, 12,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Events},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {nil, 0},
                   {:var, 0, :Postponed},
                   {:var, 0, :Timers},
                   {:var, 0, :_Seen},
                   {:var, 0, :TimeoutEvents}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :TimeoutEvents},
                    [
                      {:clause, 0, [nil: 0], [],
                       [
                         {:match, 0, {:var, 0, :S_1},
                          {:record, 0, {:var, 0, :S}, :state,
                           [
                             {:record_field, 0, {:atom, 0, :state_data},
                              {:var, 0, :NextState_NewData}},
                             {:record_field, 0, {:atom, 0, :postponed}, {:var, 0, :Postponed}},
                             {:record_field, 0, {:atom, 0, :timers}, {:var, 0, :Timers}},
                             {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                           ]}},
                         {:call, 0, {:atom, 0, :loop_done},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S_1},
                            {:var, 0, :Events},
                            {:var, 0, :NextEventsR}
                          ]}
                       ]},
                      {:clause, 0, [{:cons, 0, {:var, 0, :_}, {:var, 0, :_}}], [],
                       [
                         {:match, 0,
                          {:map, 0, [{:map_field_exact, 0, {:atom, 0, :t0q}, {:var, 0, :T0Q}}]},
                          {:var, 0, :Timers}},
                         {:match, 0, {:var, 0, :S_1},
                          {:record, 0, {:var, 0, :S}, :state,
                           [
                             {:record_field, 0, {:atom, 0, :state_data},
                              {:var, 0, :NextState_NewData}},
                             {:record_field, 0, {:atom, 0, :postponed}, {:var, 0, :Postponed}},
                             {:record_field, 0, {:atom, 0, :timers},
                              {:map, 0, {:var, 0, :Timers},
                               [
                                 {:map_field_exact, 0, {:atom, 0, :t0q},
                                  {:op, 0, :++, {:var, 0, :T0Q}, {:var, 0, :TimeoutEvents}}}
                               ]}},
                             {:record_field, 0, {:atom, 0, :hibernate}, {:var, 0, :Hibernate}}
                           ]}},
                         {:call, 0, {:atom, 0, :loop_done},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S_1},
                            {:var, 0, :Events},
                            {:var, 0, :NextEventsR}
                          ]}
                       ]}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Events},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:cons, 0, {:var, 0, :Timeout}, {:var, 0, :TimeoutsR}},
                   {:var, 0, :Postponed},
                   {:var, 0, :Timers},
                   {:var, 0, :Seen},
                   {:var, 0, :TimeoutEvents}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :TimeoutType},
                    {:call, 0, {:atom, 0, :element}, [{:integer, 0, 1}, {:var, 0, :Timeout}]}},
                   {:case, 0, {:var, 0, :Seen},
                    [
                      {:clause, 0,
                       [
                         {:map, 0,
                          [{:map_field_exact, 0, {:var, 0, :TimeoutType}, {:var, 0, :_}}]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_timeouts},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers},
                            {:var, 0, :Seen},
                            {:var, 0, :TimeoutEvents}
                          ]}
                       ]},
                      {:clause, 0, [{:map, 0, []}], [],
                       [
                         {:case, 0, {:var, 0, :Timeout},
                          [
                            {:clause, 0,
                             [
                               {:tuple, 0,
                                [{:var, 0, :_}, {:var, 0, :Time}, {:var, 0, :TimeoutMsg}]}
                             ], [],
                             [
                               {:call, 0, {:atom, 0, :loop_timeouts_start},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:var, 0, :S},
                                  {:var, 0, :Events},
                                  {:var, 0, :NextState_NewData},
                                  {:var, 0, :NextEventsR},
                                  {:var, 0, :Hibernate},
                                  {:var, 0, :TimeoutsR},
                                  {:var, 0, :Postponed},
                                  {:var, 0, :Timers},
                                  {:var, 0, :Seen},
                                  {:var, 0, :TimeoutEvents},
                                  {:var, 0, :TimeoutType},
                                  {:var, 0, :Time},
                                  {:var, 0, :TimeoutMsg},
                                  {nil, 0}
                                ]}
                             ]},
                            {:clause, 0,
                             [
                               {:tuple, 0,
                                [
                                  {:var, 0, :_},
                                  {:var, 0, :Time},
                                  {:var, 0, :TimeoutMsg},
                                  {:var, 0, :TimeoutOpts}
                                ]}
                             ], [],
                             [
                               {:call, 0, {:atom, 0, :loop_timeouts_start},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:var, 0, :S},
                                  {:var, 0, :Events},
                                  {:var, 0, :NextState_NewData},
                                  {:var, 0, :NextEventsR},
                                  {:var, 0, :Hibernate},
                                  {:var, 0, :TimeoutsR},
                                  {:var, 0, :Postponed},
                                  {:var, 0, :Timers},
                                  {:var, 0, :Seen},
                                  {:var, 0, :TimeoutEvents},
                                  {:var, 0, :TimeoutType},
                                  {:var, 0, :Time},
                                  {:var, 0, :TimeoutMsg},
                                  {:call, 0, {:atom, 0, :listify}, [{:var, 0, :TimeoutOpts}]}
                                ]}
                             ]}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_timeouts_start, 16,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Events},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postponed},
                   {:var, 0, :Timers},
                   {:var, 0, :Seen},
                   {:var, 0, :TimeoutEvents},
                   {:var, 0, :TimeoutType},
                   {:var, 0, :Time},
                   {:var, 0, :TimeoutMsg},
                   {:var, 0, :TimeoutOpts}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :Time},
                    [
                      {:clause, 0, [{:atom, 0, :infinity}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_timeouts_cancel},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers},
                            {:var, 0, :Seen},
                            {:var, 0, :TimeoutEvents},
                            {:var, 0, :TimeoutType}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, :update}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_timeouts_update},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers},
                            {:var, 0, :Seen},
                            {:var, 0, :TimeoutEvents},
                            {:var, 0, :TimeoutType},
                            {:var, 0, :TimeoutMsg}
                          ]}
                       ]},
                      {:clause, 0, [{:integer, 0, 0}], [],
                       [
                         {:match, 0, {:var, 0, :TimerRef}, {:integer, 0, 0}},
                         {:match, 0, {:var, 0, :TimeoutEvents_1},
                          {:cons, 0, {:var, 0, :TimeoutType}, {:var, 0, :TimeoutEvents}}},
                         {:call, 0, {:atom, 0, :loop_timeouts_register},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers},
                            {:var, 0, :Seen},
                            {:var, 0, :TimeoutEvents_1},
                            {:var, 0, :TimeoutType},
                            {:var, 0, :Time},
                            {:var, 0, :TimeoutMsg},
                            {:var, 0, :TimeoutOpts},
                            {:var, 0, :TimerRef}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:match, 0, {:var, 0, :TimerRef},
                          {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :start_timer}},
                           [
                             {:var, 0, :Time},
                             {:call, 0, {:atom, 0, :self}, []},
                             {:var, 0, :TimeoutType},
                             {:var, 0, :TimeoutOpts}
                           ]}},
                         {:call, 0, {:atom, 0, :loop_timeouts_register},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers},
                            {:var, 0, :Seen},
                            {:var, 0, :TimeoutEvents},
                            {:var, 0, :TimeoutType},
                            {:var, 0, :Time},
                            {:var, 0, :TimeoutMsg},
                            {:var, 0, :TimeoutOpts},
                            {:var, 0, :TimerRef}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_timeouts_register, 17,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Events},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postponed},
                   {:var, 0, :Timers},
                   {:var, 0, :Seen},
                   {:var, 0, :TimeoutEvents},
                   {:var, 0, :TimeoutType},
                   {:var, 0, :Time},
                   {:var, 0, :TimeoutMsg},
                   {:var, 0, :TimeoutOpts},
                   {:var, 0, :TimerRef}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :Debug},
                    [
                      {:clause, 0, [nil: 0], [],
                       [
                         {:call, 0, {:atom, 0, :loop_timeouts_register},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers},
                            {:var, 0, :Seen},
                            {:var, 0, :TimeoutEvents},
                            {:var, 0, :TimeoutType},
                            {:var, 0, :TimerRef},
                            {:var, 0, :TimeoutMsg}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:match, 0, {:tuple, 0, [{:var, 0, :State}, {:var, 0, :_Data}]},
                          {:var, 0, :NextState_NewData}},
                         {:match, 0, {:var, 0, :Debug_1},
                          {:call, 0, {:atom, 0, :sys_debug},
                           [
                             {:var, 0, :Debug},
                             {:record_field, 0, {:var, 0, :P}, :params, {:atom, 0, :name}},
                             {:tuple, 0,
                              [
                                {:atom, 0, :start_timer},
                                {:tuple, 0,
                                 [
                                   {:var, 0, :TimeoutType},
                                   {:var, 0, :Time},
                                   {:var, 0, :TimeoutMsg},
                                   {:var, 0, :TimeoutOpts}
                                 ]},
                                {:var, 0, :State}
                              ]}
                           ]}},
                         {:call, 0, {:atom, 0, :loop_timeouts_register},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug_1},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers},
                            {:var, 0, :Seen},
                            {:var, 0, :TimeoutEvents},
                            {:var, 0, :TimeoutType},
                            {:var, 0, :TimerRef},
                            {:var, 0, :TimeoutMsg}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_timeouts_register, 15,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Events},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postponed},
                   {:var, 0, :Timers},
                   {:var, 0, :Seen},
                   {:var, 0, :TimeoutEvents},
                   {:var, 0, :TimeoutType},
                   {:var, 0, :TimerRef},
                   {:var, 0, :TimeoutMsg}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :Timers},
                    [
                      {:clause, 0,
                       [
                         {:map, 0,
                          [
                            {:map_field_exact, 0, {:var, 0, :TimeoutType},
                             {:tuple, 0, [{:integer, 0, 0}, {:var, 0, :_OldTimeoutMsg}]}},
                            {:map_field_exact, 0, {:atom, 0, :t0q}, {:var, 0, :T0Q}}
                          ]}
                       ], [],
                       [
                         {:match, 0, {:var, 0, :Timers_1},
                          {:map, 0, {:var, 0, :Timers},
                           [
                             {:map_field_exact, 0, {:var, 0, :TimeoutType},
                              {:tuple, 0, [{:integer, 0, 0}, {:var, 0, :TimeoutMsg}]}},
                             {:map_field_exact, 0, {:atom, 0, :t0q},
                              {:call, 0, {:remote, 0, {:atom, 0, :lists}, {:atom, 0, :delete}},
                               [{:var, 0, :TimeoutType}, {:var, 0, :T0Q}]}}
                           ]}},
                         {:call, 0, {:atom, 0, :loop_timeouts},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers_1},
                            {:map, 0, {:var, 0, :Seen},
                             [
                               {:map_field_assoc, 0, {:var, 0, :TimeoutType}, {:atom, 0, true}}
                             ]},
                            {:var, 0, :TimeoutEvents}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:map, 0,
                          [
                            {:map_field_exact, 0, {:var, 0, :TimeoutType},
                             {:tuple, 0, [{:var, 0, :OldTimerRef}, {:var, 0, :_OldTimeoutMsg}]}}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :cancel_timer}, [{:var, 0, :OldTimerRef}]},
                         {:match, 0, {:var, 0, :Timers_1},
                          {:map, 0, {:var, 0, :Timers},
                           [
                             {:map_field_exact, 0, {:var, 0, :TimeoutType},
                              {:tuple, 0, [{:var, 0, :TimerRef}, {:var, 0, :TimeoutMsg}]}}
                           ]}},
                         {:call, 0, {:atom, 0, :loop_timeouts},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers_1},
                            {:map, 0, {:var, 0, :Seen},
                             [
                               {:map_field_assoc, 0, {:var, 0, :TimeoutType}, {:atom, 0, true}}
                             ]},
                            {:var, 0, :TimeoutEvents}
                          ]}
                       ]},
                      {:clause, 0, [{:map, 0, []}], [],
                       [
                         {:match, 0, {:var, 0, :Timers_1},
                          {:map, 0, {:var, 0, :Timers},
                           [
                             {:map_field_assoc, 0, {:var, 0, :TimeoutType},
                              {:tuple, 0, [{:var, 0, :TimerRef}, {:var, 0, :TimeoutMsg}]}}
                           ]}},
                         {:call, 0, {:atom, 0, :loop_timeouts},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers_1},
                            {:map, 0, {:var, 0, :Seen},
                             [
                               {:map_field_assoc, 0, {:var, 0, :TimeoutType}, {:atom, 0, true}}
                             ]},
                            {:var, 0, :TimeoutEvents}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_timeouts_cancel, 13,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Events},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postponed},
                   {:var, 0, :Timers},
                   {:var, 0, :Seen},
                   {:var, 0, :TimeoutEvents},
                   {:var, 0, :TimeoutType}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :Timers},
                    [
                      {:clause, 0,
                       [
                         {:map, 0,
                          [
                            {:map_field_exact, 0, {:var, 0, :TimeoutType},
                             {:tuple, 0, [{:var, 0, :TimerRef}, {:var, 0, :_TimeoutMsg}]}}
                          ]}
                       ], [],
                       [
                         {:match, 0, {:var, 0, :Timers_1},
                          {:call, 0, {:atom, 0, :cancel_timer},
                           [
                             {:var, 0, :TimeoutType},
                             {:var, 0, :TimerRef},
                             {:var, 0, :Timers}
                           ]}},
                         {:call, 0, {:atom, 0, :loop_timeouts},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers_1},
                            {:map, 0, {:var, 0, :Seen},
                             [
                               {:map_field_assoc, 0, {:var, 0, :TimeoutType}, {:atom, 0, true}}
                             ]},
                            {:var, 0, :TimeoutEvents}
                          ]}
                       ]},
                      {:clause, 0, [{:map, 0, []}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_timeouts},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers},
                            {:map, 0, {:var, 0, :Seen},
                             [
                               {:map_field_assoc, 0, {:var, 0, :TimeoutType}, {:atom, 0, true}}
                             ]},
                            {:var, 0, :TimeoutEvents}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_timeouts_update, 14,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Events},
                   {:var, 0, :NextState_NewData},
                   {:var, 0, :NextEventsR},
                   {:var, 0, :Hibernate},
                   {:var, 0, :TimeoutsR},
                   {:var, 0, :Postponed},
                   {:var, 0, :Timers},
                   {:var, 0, :Seen},
                   {:var, 0, :TimeoutEvents},
                   {:var, 0, :TimeoutType},
                   {:var, 0, :TimeoutMsg}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :Timers},
                    [
                      {:clause, 0,
                       [
                         {:map, 0,
                          [
                            {:map_field_exact, 0, {:var, 0, :TimeoutType},
                             {:tuple, 0, [{:var, 0, :TimerRef}, {:var, 0, :_OldTimeoutMsg}]}}
                          ]}
                       ], [],
                       [
                         {:match, 0, {:var, 0, :Timers_1},
                          {:map, 0, {:var, 0, :Timers},
                           [
                             {:map_field_exact, 0, {:var, 0, :TimeoutType},
                              {:tuple, 0, [{:var, 0, :TimerRef}, {:var, 0, :TimeoutMsg}]}}
                           ]}},
                         {:call, 0, {:atom, 0, :loop_timeouts},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers_1},
                            {:map, 0, {:var, 0, :Seen},
                             [
                               {:map_field_assoc, 0, {:var, 0, :TimeoutType}, {:atom, 0, true}}
                             ]},
                            {:var, 0, :TimeoutEvents}
                          ]}
                       ]},
                      {:clause, 0, [{:map, 0, []}], [],
                       [
                         {:match, 0, {:var, 0, :Timers_1},
                          {:map, 0, {:var, 0, :Timers},
                           [
                             {:map_field_assoc, 0, {:var, 0, :TimeoutType},
                              {:tuple, 0, [{:integer, 0, 0}, {:var, 0, :TimeoutMsg}]}}
                           ]}},
                         {:match, 0, {:var, 0, :TimeoutEvents_1},
                          {:cons, 0, {:var, 0, :TimeoutType}, {:var, 0, :TimeoutEvents}}},
                         {:call, 0, {:atom, 0, :loop_timeouts},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events},
                            {:var, 0, :NextState_NewData},
                            {:var, 0, :NextEventsR},
                            {:var, 0, :Hibernate},
                            {:var, 0, :TimeoutsR},
                            {:var, 0, :Postponed},
                            {:var, 0, :Timers_1},
                            {:map, 0, {:var, 0, :Seen},
                             [
                               {:map_field_assoc, 0, {:var, 0, :TimeoutType}, {:atom, 0, true}}
                             ]},
                            {:var, 0, :TimeoutEvents_1}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_done, 5,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Events},
                   {:var, 0, :NextEventsR}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :NextEventsR},
                    [
                      {:clause, 0, [nil: 0], [],
                       [
                         {:call, 0, {:atom, 0, :loop_done},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Events}
                          ]}
                       ]},
                      {:clause, 0, [{:cons, 0, {:var, 0, :E1}, {nil, 0}}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_done},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:cons, 0, {:var, 0, :E1}, {:var, 0, :Events}}
                          ]}
                       ]},
                      {:clause, 0,
                       [{:cons, 0, {:var, 0, :E2}, {:cons, 0, {:var, 0, :E1}, {nil, 0}}}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_done},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:cons, 0, {:var, 0, :E1},
                             {:cons, 0, {:var, 0, :E2}, {:var, 0, :Events}}}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:cons, 0, {:var, 0, :_}, {:cons, 0, {:var, 0, :_}, {:var, 0, :_}}}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :loop_done},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:call, 0, {:remote, 0, {:atom, 0, :lists}, {:atom, 0, :reverse}},
                             [{:var, 0, :NextEventsR}, {:var, 0, :Events}]}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :loop_done, 4,
              [
                {:clause, 0, [{:var, 0, :P}, {:var, 0, :Debug}, {:var, 0, :S}, {:var, 0, :Q}], [],
                 [
                   {:case, 0, {:var, 0, :Q},
                    [
                      {:clause, 0, [nil: 0], [],
                       [
                         {:case, 0,
                          {:record_field, 0, {:var, 0, :S}, :state, {:atom, 0, :timers}},
                          [
                            {:clause, 0,
                             [
                               {:match, 0,
                                {:map, 0,
                                 [
                                   {:map_field_exact, 0, {:atom, 0, :t0q},
                                    {:cons, 0, {:var, 0, :TimeoutType}, {:var, 0, :_}}}
                                 ]}, {:var, 0, :Timers}}
                             ], [],
                             [
                               {:match, 0,
                                {:map, 0,
                                 [
                                   {:map_field_exact, 0, {:var, 0, :TimeoutType},
                                    {:tuple, 0,
                                     [
                                       {:match, 0, {:integer, 0, 0}, {:var, 0, :TimerRef}},
                                       {:var, 0, :TimeoutMsg}
                                     ]}}
                                 ]}, {:var, 0, :Timers}},
                               {:match, 0, {:var, 0, :Timers_1},
                                {:call, 0, {:atom, 0, :cancel_timer},
                                 [
                                   {:var, 0, :TimeoutType},
                                   {:var, 0, :TimerRef},
                                   {:var, 0, :Timers}
                                 ]}},
                               {:match, 0, {:var, 0, :S_1},
                                {:record, 0, {:var, 0, :S}, :state,
                                 [
                                   {:record_field, 0, {:atom, 0, :timers}, {:var, 0, :Timers_1}}
                                 ]}},
                               {:match, 0, {:var, 0, :Event},
                                {:tuple, 0, [{:var, 0, :TimeoutType}, {:var, 0, :TimeoutMsg}]}},
                               {:call, 0, {:atom, 0, :loop_receive_result},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:var, 0, :S_1},
                                  {:var, 0, :Event}
                                ]}
                             ]},
                            {:clause, 0, [{:map, 0, []}], [],
                             [
                               {:call, 0, {:atom, 0, :loop},
                                [{:var, 0, :P}, {:var, 0, :Debug}, {:var, 0, :S}]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [{:cons, 0, {:var, 0, :Event}, {:var, 0, :Events}}], [],
                       [
                         {:call, 0, {:atom, 0, :loop_event},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Event},
                            {:var, 0, :Events}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :parse_timeout_opts_abs, 1,
              [
                {:clause, 0, [{:var, 0, :Opts}], [],
                 [
                   {:call, 0, {:atom, 0, :parse_timeout_opts_abs},
                    [{:var, 0, :Opts}, {:atom, 0, false}]}
                 ]}
              ]},
             {:function, 0, :parse_timeout_opts_abs, 2,
              [
                {:clause, 0, [{:var, 0, :Opts}, {:var, 0, :Abs}], [],
                 [
                   {:case, 0, {:var, 0, :Opts},
                    [
                      {:clause, 0, [nil: 0], [], [{:var, 0, :Abs}]},
                      {:clause, 0,
                       [
                         {:cons, 0, {:tuple, 0, [{:atom, 0, :abs}, {:var, 0, :Abs_1}]},
                          {:var, 0, :Opts}}
                       ], [[{:call, 0, {:atom, 0, :is_boolean}, [{:var, 0, :Abs_1}]}]],
                       [
                         {:call, 0, {:atom, 0, :parse_timeout_opts_abs},
                          [{:var, 0, :Opts}, {:var, 0, :Abs_1}]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :badarg}]}
                    ]}
                 ]}
              ]},
             {:function, 0, :get_callback_mode, 2,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:match, 0, {:cons, 0, {:var, 0, :Module}, {:var, 0, :_}}, {:var, 0, :Modules}}
                 ], [],
                 [
                   {:try, 0,
                    [
                      {:call, 0, {:remote, 0, {:var, 0, :Module}, {:atom, 0, :callback_mode}}, []}
                    ],
                    [
                      {:clause, 0, [{:var, 0, :CallbackModeResult}], [],
                       [
                         {:call, 0, {:atom, 0, :callback_mode_result},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Modules},
                            {:var, 0, :CallbackModeResult}
                          ]}
                       ]}
                    ],
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :throw},
                            {:var, 0, :CallbackModeResult},
                            {:var, 0, :_}
                          ]}
                       ], [],
                       [
                         {:call, 0, {:atom, 0, :callback_mode_result},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Modules},
                            {:var, 0, :CallbackModeResult}
                          ]}
                       ]},
                      {:clause, 0,
                       [
                         {:tuple, 0,
                          [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}]}
                       ], [],
                       [
                         {:tuple, 0,
                          [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}]}
                       ]}
                    ], []}
                 ]}
              ]},
             {:function, 0, :callback_mode_result, 3,
              [
                {:clause, 0, [{:var, 0, :P}, {:var, 0, :Modules}, {:var, 0, :CallbackModeResult}],
                 [],
                 [
                   {:call, 0, {:atom, 0, :callback_mode_result},
                    [
                      {:var, 0, :P},
                      {:var, 0, :Modules},
                      {:var, 0, :CallbackModeResult},
                      {:call, 0, {:atom, 0, :listify}, [{:var, 0, :CallbackModeResult}]},
                      {:atom, 0, :undefined},
                      {:atom, 0, false}
                    ]}
                 ]}
              ]},
             {:function, 0, :callback_mode_result, 6,
              [
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Modules},
                   {:var, 0, :CallbackModeResult},
                   {:cons, 0, {:var, 0, :H}, {:var, 0, :T}},
                   {:var, 0, :CallbackMode},
                   {:var, 0, :StateEnter}
                 ], [],
                 [
                   {:case, 0, {:call, 0, {:atom, 0, :callback_mode}, [{:var, 0, :H}]},
                    [
                      {:clause, 0, [{:atom, 0, true}], [],
                       [
                         {:call, 0, {:atom, 0, :callback_mode_result},
                          [
                            {:var, 0, :P},
                            {:var, 0, :Modules},
                            {:var, 0, :CallbackModeResult},
                            {:var, 0, :T},
                            {:var, 0, :H},
                            {:var, 0, :StateEnter}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, false}], [],
                       [
                         {:case, 0, {:call, 0, {:atom, 0, :state_enter}, [{:var, 0, :H}]},
                          [
                            {:clause, 0, [{:atom, 0, true}], [],
                             [
                               {:call, 0, {:atom, 0, :callback_mode_result},
                                [
                                  {:var, 0, :P},
                                  {:var, 0, :Modules},
                                  {:var, 0, :CallbackModeResult},
                                  {:var, 0, :T},
                                  {:var, 0, :CallbackMode},
                                  {:atom, 0, true}
                                ]}
                             ]},
                            {:clause, 0, [{:atom, 0, false}], [],
                             [
                               {:tuple, 0,
                                [
                                  {:atom, 0, :error},
                                  {:tuple, 0,
                                   [
                                     {:atom, 0, :bad_return_from_callback_mode},
                                     {:var, 0, :CallbackModeResult}
                                   ]},
                                  {:call, 0, {:atom, 0, :element},
                                   [
                                     {:integer, 0, 2},
                                     {:call, 0,
                                      {:remote, 0, {:atom, 0, :erlang},
                                       {:atom, 0, :process_info}},
                                      [
                                        {:call, 0, {:atom, 0, :self}, []},
                                        {:atom, 0, :current_stacktrace}
                                      ]}
                                   ]}
                                ]}
                             ]}
                          ]}
                       ]}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :P},
                   {:var, 0, :Modules},
                   {:var, 0, :CallbackModeResult},
                   {nil, 0},
                   {:var, 0, :CallbackMode},
                   {:var, 0, :StateEnter}
                 ], [],
                 [
                   {:if, 0,
                    [
                      {:clause, 0, [],
                       [
                         [
                           {:op, 0, :"=:=", {:var, 0, :CallbackMode}, {:atom, 0, :undefined}}
                         ]
                       ],
                       [
                         {:tuple, 0,
                          [
                            {:atom, 0, :error},
                            {:tuple, 0,
                             [
                               {:atom, 0, :bad_return_from_callback_mode},
                               {:var, 0, :CallbackModeResult}
                             ]},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [], [[{:atom, 0, true}]],
                       [
                         {:record, 0, {:var, 0, :P}, :params,
                          [
                            {:record_field, 0, {:atom, 0, :modules}, {:var, 0, :Modules}},
                            {:record_field, 0, {:atom, 0, :callback_mode},
                             {:var, 0, :CallbackMode}},
                            {:record_field, 0, {:atom, 0, :state_enter}, {:var, 0, :StateEnter}}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :reply_then_terminate, 8,
              [
                {:clause, 0,
                 [
                   {:var, 0, :Class},
                   {:var, 0, :Reason},
                   {:var, 0, :Stacktrace},
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:var, 0, :Replies}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :do_reply_then_terminate},
                    [
                      {:var, 0, :Class},
                      {:var, 0, :Reason},
                      {:var, 0, :Stacktrace},
                      {:var, 0, :P},
                      {:var, 0, :Debug},
                      {:var, 0, :S},
                      {:var, 0, :Q},
                      {:call, 0, {:atom, 0, :listify}, [{:var, 0, :Replies}]}
                    ]}
                 ]}
              ]},
             {:function, 0, :do_reply_then_terminate, 8,
              [
                {:clause, 0,
                 [
                   {:var, 0, :Class},
                   {:var, 0, :Reason},
                   {:var, 0, :Stacktrace},
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {nil, 0}
                 ], [],
                 [
                   {:call, 0, {:atom, 0, :terminate},
                    [
                      {:var, 0, :Class},
                      {:var, 0, :Reason},
                      {:var, 0, :Stacktrace},
                      {:var, 0, :P},
                      {:var, 0, :Debug},
                      {:var, 0, :S},
                      {:var, 0, :Q}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:var, 0, :Class},
                   {:var, 0, :Reason},
                   {:var, 0, :Stacktrace},
                   {:var, 0, :P},
                   {:var, 0, :Debug},
                   {:var, 0, :S},
                   {:var, 0, :Q},
                   {:cons, 0, {:var, 0, :R}, {:var, 0, :Rs}}
                 ], [],
                 [
                   {:case, 0, {:var, 0, :R},
                    [
                      {:clause, 0,
                       [
                         {:tuple, 0, [{:atom, 0, :reply}, {:var, 0, :From}, {:var, 0, :Reply}]}
                       ], [],
                       [
                         {:case, 0, {:call, 0, {:atom, 0, :from}, [{:var, 0, :From}]},
                          [
                            {:clause, 0, [{:atom, 0, true}], [],
                             [
                               {:call, 0, {:atom, 0, :reply},
                                [{:var, 0, :From}, {:var, 0, :Reply}]},
                               {:match, 0, {:var, 0, :Debug_1},
                                {:case, 0, {:block, 0, [{:var, 0, :Debug}]},
                                 [
                                   {:clause, 0, [nil: 0], [], [{:block, 0, [{:var, 0, :Debug}]}]},
                                   {:clause, 0, [{:var, 0, :_}], [],
                                    [
                                      {:call, 0, {:atom, 0, :sys_debug},
                                       [
                                         {:block, 0, [{:var, 0, :Debug}]},
                                         {:block, 0,
                                          [
                                            {:record_field, 0, {:var, 0, :P}, :params,
                                             {:atom, 0, :name}}
                                          ]},
                                         {:block, 0,
                                          [
                                            {:tuple, 0,
                                             [
                                               {:atom, 0, :out},
                                               {:var, 0, :Reply},
                                               {:var, 0, :From}
                                             ]}
                                          ]}
                                       ]}
                                    ]}
                                 ]}},
                               {:call, 0, {:atom, 0, :do_reply_then_terminate},
                                [
                                  {:var, 0, :Class},
                                  {:var, 0, :Reason},
                                  {:var, 0, :Stacktrace},
                                  {:var, 0, :P},
                                  {:var, 0, :Debug_1},
                                  {:var, 0, :S},
                                  {:var, 0, :Q},
                                  {:var, 0, :Rs}
                                ]}
                             ]},
                            {:clause, 0, [{:atom, 0, false}], [],
                             [
                               {:call, 0, {:atom, 0, :terminate},
                                [
                                  {:atom, 0, :error},
                                  {:tuple, 0,
                                   [
                                     {:atom, 0, :bad_reply_action_from_state_function},
                                     {:var, 0, :R}
                                   ]},
                                  {:call, 0, {:atom, 0, :element},
                                   [
                                     {:integer, 0, 2},
                                     {:call, 0,
                                      {:remote, 0, {:atom, 0, :erlang},
                                       {:atom, 0, :process_info}},
                                      [
                                        {:call, 0, {:atom, 0, :self}, []},
                                        {:atom, 0, :current_stacktrace}
                                      ]}
                                   ]},
                                  {:var, 0, :P},
                                  {:var, 0, :Debug},
                                  {:var, 0, :S},
                                  {:var, 0, :Q}
                                ]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:call, 0, {:atom, 0, :terminate},
                          [
                            {:atom, 0, :error},
                            {:tuple, 0,
                             [
                               {:atom, 0, :bad_reply_action_from_state_function},
                               {:var, 0, :R}
                             ]},
                            {:call, 0, {:atom, 0, :element},
                             [
                               {:integer, 0, 2},
                               {:call, 0,
                                {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :process_info}},
                                [
                                  {:call, 0, {:atom, 0, :self}, []},
                                  {:atom, 0, :current_stacktrace}
                                ]}
                             ]},
                            {:var, 0, :P},
                            {:var, 0, :Debug},
                            {:var, 0, :S},
                            {:var, 0, :Q}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :terminate, 7,
              [
                {:clause, 0,
                 [
                   {:var, 0, :Class},
                   {:var, 0, :Reason},
                   {:var, 0, :Stacktrace},
                   {:match, 0,
                    {:record, 0, :params,
                     [
                       {:record_field, 0, {:atom, 0, :modules},
                        {:cons, 0, {:var, 0, :Module}, {:var, 0, :_}}}
                     ]}, {:var, 0, :P}},
                   {:var, 0, :Debug},
                   {:match, 0,
                    {:record, 0, :state,
                     [
                       {:record_field, 0, {:atom, 0, :state_data},
                        {:tuple, 0, [{:var, 0, :State}, {:var, 0, :Data}]}}
                     ]}, {:var, 0, :S}},
                   {:var, 0, :Q}
                 ], [],
                 [
                   {:case, 0,
                    {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :function_exported}},
                     [{:var, 0, :Module}, {:atom, 0, :terminate}, {:integer, 0, 3}]},
                    [
                      {:clause, 0, [{:atom, 0, true}], [],
                       [
                         {:try, 0,
                          [
                            {:call, 0, {:remote, 0, {:var, 0, :Module}, {:atom, 0, :terminate}},
                             [{:var, 0, :Reason}, {:var, 0, :State}, {:var, 0, :Data}]}
                          ], [{:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :ok}]}],
                          [
                            {:clause, 0,
                             [
                               {:tuple, 0, [{:atom, 0, :throw}, {:var, 0, :_}, {:var, 0, :_}]}
                             ], [], [{:atom, 0, :ok}]},
                            {:clause, 0,
                             [{:tuple, 0, [{:var, 0, :C}, {:var, 0, :R}, {:var, 0, :ST}]}], [],
                             [
                               {:call, 0, {:atom, 0, :error_info},
                                [
                                  {:var, 0, :C},
                                  {:var, 0, :R},
                                  {:var, 0, :ST},
                                  {:var, 0, :Debug},
                                  {:var, 0, :P},
                                  {:var, 0, :S},
                                  {:var, 0, :Q}
                                ]},
                               {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :raise}},
                                [{:var, 0, :C}, {:var, 0, :R}, {:var, 0, :ST}]}
                             ]}
                          ], []}
                       ]},
                      {:clause, 0, [{:atom, 0, false}], [], [{:atom, 0, :ok}]}
                    ]},
                   {:match, 0, {:var, 0, :_},
                    {:case, 0, {:var, 0, :Reason},
                     [
                       {:clause, 0, [{:atom, 0, :normal}], [],
                        [
                          {:call, 0, {:atom, 0, :terminate_sys_debug},
                           [
                             {:var, 0, :Debug},
                             {:var, 0, :P},
                             {:var, 0, :State},
                             {:var, 0, :Reason}
                           ]}
                        ]},
                       {:clause, 0, [{:atom, 0, :shutdown}], [],
                        [
                          {:call, 0, {:atom, 0, :terminate_sys_debug},
                           [
                             {:var, 0, :Debug},
                             {:var, 0, :P},
                             {:var, 0, :State},
                             {:var, 0, :Reason}
                           ]}
                        ]},
                       {:clause, 0, [{:tuple, 0, [{:atom, 0, :shutdown}, {:var, 0, :_}]}], [],
                        [
                          {:call, 0, {:atom, 0, :terminate_sys_debug},
                           [
                             {:var, 0, :Debug},
                             {:var, 0, :P},
                             {:var, 0, :State},
                             {:var, 0, :Reason}
                           ]}
                        ]},
                       {:clause, 0, [{:var, 0, :_}], [],
                        [
                          {:call, 0, {:atom, 0, :error_info},
                           [
                             {:var, 0, :Class},
                             {:var, 0, :Reason},
                             {:var, 0, :Stacktrace},
                             {:var, 0, :Debug},
                             {:var, 0, :P},
                             {:var, 0, :S},
                             {:var, 0, :Q}
                           ]}
                        ]}
                     ]}},
                   {:case, 0, {:var, 0, :Stacktrace},
                    [
                      {:clause, 0, [nil: 0], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:var, 0, :Class}},
                          [{:var, 0, :Reason}]}
                       ]},
                      {:clause, 0, [{:cons, 0, {:var, 0, :_}, {:var, 0, :_}}], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :raise}},
                          [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :terminate_sys_debug, 4,
              [
                {:clause, 0,
                 [{:var, 0, :Debug}, {:var, 0, :P}, {:var, 0, :State}, {:var, 0, :Reason}], [],
                 [
                   {:case, 0, {:block, 0, [{:var, 0, :Debug}]},
                    [
                      {:clause, 0, [nil: 0], [], [{:block, 0, [{:var, 0, :Debug}]}]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:call, 0, {:atom, 0, :sys_debug},
                          [
                            {:block, 0, [{:var, 0, :Debug}]},
                            {:block, 0,
                             [
                               {:record_field, 0, {:var, 0, :P}, :params, {:atom, 0, :name}}
                             ]},
                            {:block, 0,
                             [
                               {:tuple, 0,
                                [
                                  {:atom, 0, :terminate},
                                  {:var, 0, :Reason},
                                  {:var, 0, :State}
                                ]}
                             ]}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:function, 0, :error_info, 7,
              [
                {:clause, 0,
                 [
                   {:var, 0, :Class},
                   {:var, 0, :Reason},
                   {:var, 0, :Stacktrace},
                   {:var, 0, :Debug},
                   {:record, 0, :params,
                    [
                      {:record_field, 0, {:atom, 0, :name}, {:var, 0, :Name}},
                      {:record_field, 0, {:atom, 0, :modules},
                       {:match, 0, {:cons, 0, {:var, 0, :Mod}, {:var, 0, :_}},
                        {:var, 0, :Modules}}},
                      {:record_field, 0, {:atom, 0, :callback_mode}, {:var, 0, :CallbackMode}},
                      {:record_field, 0, {:atom, 0, :state_enter}, {:var, 0, :StateEnter}}
                    ]},
                   {:record, 0, :state,
                    [
                      {:record_field, 0, {:atom, 0, :postponed}, {:var, 0, :Postponed}},
                      {:record_field, 0, {:atom, 0, :timers}, {:var, 0, :Timers}},
                      {:record_field, 0, {:atom, 0, :state_data},
                       {:tuple, 0, [{:var, 0, :State}, {:var, 0, :Data}]}}
                    ]},
                   {:var, 0, :Q}
                 ], [],
                 [
                   {:match, 0, {:tuple, 0, [{:var, 0, :NumTimers}, {:var, 0, :ListTimers}]},
                    {:call, 0, {:atom, 0, :list_timeouts}, [{:var, 0, :Timers}]}},
                   {:match, 0, {:var, 0, :Status},
                    {:call, 0, {:remote, 0, {:atom, 0, :gen}, {:atom, 0, :format_status}},
                     [
                       {:var, 0, :Mod},
                       {:atom, 0, :terminate},
                       {:map, 0,
                        [
                          {:map_field_assoc, 0, {:atom, 0, :reason}, {:var, 0, :Reason}},
                          {:map_field_assoc, 0, {:atom, 0, :state}, {:var, 0, :State}},
                          {:map_field_assoc, 0, {:atom, 0, :data}, {:var, 0, :Data}},
                          {:map_field_assoc, 0, {:atom, 0, :queue}, {:var, 0, :Q}},
                          {:map_field_assoc, 0, {:atom, 0, :postponed}, {:var, 0, :Postponed}},
                          {:map_field_assoc, 0, {:atom, 0, :timeouts}, {:var, 0, :ListTimers}},
                          {:map_field_assoc, 0, {:atom, 0, :log},
                           {:call, 0, {:remote, 0, {:atom, 0, :sys}, {:atom, 0, :get_log}},
                            [{:var, 0, :Debug}]}}
                        ]},
                       {:cons, 0, {:call, 0, {:atom, 0, :get}, []},
                        {:cons, 0, {:var, 0, :State}, {:cons, 0, {:var, 0, :Data}, {nil, 0}}}}
                     ]}},
                   {:match, 0, {:var, 0, :NewState},
                    {:case, 0,
                     {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :find}},
                      [{:atom, 0, :"$status"}, {:var, 0, :Status}]},
                     [
                       {:clause, 0, [{:atom, 0, :error}], [],
                        [
                          {:tuple, 0,
                           [
                             {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :get}},
                              [{:atom, 0, :state}, {:var, 0, :Status}]},
                             {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :get}},
                              [{:atom, 0, :data}, {:var, 0, :Status}]}
                           ]}
                        ]},
                       {:clause, 0, [{:tuple, 0, [{:atom, 0, :ok}, {:var, 0, :S}]}], [],
                        [{:var, 0, :S}]}
                     ]}},
                   {:case, 0,
                    {:call, 0, {:remote, 0, {:atom, 0, :logger}, {:atom, 0, :allow}},
                     [{:atom, 0, :error}, {:atom, 0, :tree_splitter_stress}]},
                    [
                      {:clause, 0, [{:atom, 0, true}], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :apply}},
                          [
                            {:atom, 0, :logger},
                            {:atom, 0, :macro_log},
                            {:cons, 0,
                             {:map, 0,
                              [
                                {:map_field_assoc, 0, {:atom, 0, :mfa},
                                 {:tuple, 0,
                                  [
                                    {:atom, 0, :tree_splitter_stress},
                                    {:atom, 0, :error_info},
                                    {:integer, 0, 7}
                                  ]}},
                                {:map_field_assoc, 0, {:atom, 0, :line}, {:integer, 0, 2673}},
                                {:map_field_assoc, 0, {:atom, 0, :file},
                                 {:string, 0, './test/fixtures/tree_split_stress.erl'}}
                              ]},
                             {:cons, 0, {:atom, 0, :error},
                              {:cons, 0,
                               {:map, 0,
                                [
                                  {:map_field_assoc, 0, {:atom, 0, :label},
                                   {:tuple, 0, [{:atom, 0, :gen_statem}, {:atom, 0, :terminate}]}},
                                  {:map_field_assoc, 0, {:atom, 0, :name}, {:var, 0, :Name}},
                                  {:map_field_assoc, 0, {:atom, 0, :queue},
                                   {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :get}},
                                    [{:atom, 0, :queue}, {:var, 0, :Status}]}},
                                  {:map_field_assoc, 0, {:atom, 0, :postponed},
                                   {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :get}},
                                    [{:atom, 0, :postponed}, {:var, 0, :Status}]}},
                                  {:map_field_assoc, 0, {:atom, 0, :modules},
                                   {:var, 0, :Modules}},
                                  {:map_field_assoc, 0, {:atom, 0, :callback_mode},
                                   {:var, 0, :CallbackMode}},
                                  {:map_field_assoc, 0, {:atom, 0, :state_enter},
                                   {:var, 0, :StateEnter}},
                                  {:map_field_assoc, 0, {:atom, 0, :state}, {:var, 0, :NewState}},
                                  {:map_field_assoc, 0, {:atom, 0, :timeouts},
                                   {:tuple, 0,
                                    [
                                      {:var, 0, :NumTimers},
                                      {:call, 0,
                                       {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :get}},
                                       [{:atom, 0, :timeouts}, {:var, 0, :Status}]}
                                    ]}},
                                  {:map_field_assoc, 0, {:atom, 0, :log},
                                   {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :get}},
                                    [{:atom, 0, :log}, {:var, 0, :Status}]}},
                                  {:map_field_assoc, 0, {:atom, 0, :reason},
                                   {:tuple, 0,
                                    [
                                      {:var, 0, :Class},
                                      {:call, 0,
                                       {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :get}},
                                       [{:atom, 0, :reason}, {:var, 0, :Status}]},
                                      {:var, 0, :Stacktrace}
                                    ]}},
                                  {:map_field_assoc, 0, {:atom, 0, :client_info},
                                   {:call, 0, {:atom, 0, :client_stacktrace}, [{:var, 0, :Q}]}}
                                ]},
                               {:cons, 0,
                                {:map, 0,
                                 [
                                   {:map_field_assoc, 0, {:atom, 0, :domain},
                                    {:cons, 0, {:atom, 0, :otp}, {nil, 0}}},
                                   {:map_field_assoc, 0, {:atom, 0, :report_cb},
                                    {:fun, 0,
                                     {:function, {:atom, 0, :gen_statem}, {:atom, 0, :format_log},
                                      {:integer, 0, 2}}}},
                                   {:map_field_assoc, 0, {:atom, 0, :error_logger},
                                    {:map, 0,
                                     [
                                       {:map_field_assoc, 0, {:atom, 0, :tag},
                                        {:atom, 0, :error}},
                                       {:map_field_assoc, 0, {:atom, 0, :report_cb},
                                        {:fun, 0,
                                         {:function, {:atom, 0, :gen_statem},
                                          {:atom, 0, :format_log}, {:integer, 0, 1}}}}
                                     ]}}
                                 ]}, {nil, 0}}}}}
                          ]}
                       ]},
                      {:clause, 0, [{:atom, 0, false}], [], [{:atom, 0, :ok}]}
                    ]}
                 ]}
              ]},
             {:function, 0, :client_stacktrace, 1,
              [
                {:clause, 0, [nil: 0], [], [{:atom, 0, :undefined}]},
                {:clause, 0,
                 [
                   {:cons, 0,
                    {:tuple, 0,
                     [
                       {:tuple, 0,
                        [
                          {:atom, 0, :call},
                          {:tuple, 0, [{:var, 0, :Pid}, {:var, 0, :_Tag}]}
                        ]},
                       {:var, 0, :_Req}
                     ]}, {:var, 0, :_}}
                 ], [[{:call, 0, {:atom, 0, :is_pid}, [{:var, 0, :Pid}]}]],
                 [
                   {:if, 0,
                    [
                      {:clause, 0, [],
                       [
                         [
                           {:op, 0, :"=:=", {:call, 0, {:atom, 0, :node}, [{:var, 0, :Pid}]},
                            {:call, 0, {:atom, 0, :node}, []}}
                         ]
                       ],
                       [
                         {:case, 0,
                          {:call, 0, {:atom, 0, :process_info},
                           [
                             {:var, 0, :Pid},
                             {:cons, 0, {:atom, 0, :current_stacktrace},
                              {:cons, 0, {:atom, 0, :registered_name}, {nil, 0}}}
                           ]},
                          [
                            {:clause, 0, [{:atom, 0, :undefined}], [],
                             [{:tuple, 0, [{:var, 0, :Pid}, {:atom, 0, :dead}]}]},
                            {:clause, 0,
                             [
                               {:cons, 0,
                                {:tuple, 0,
                                 [{:atom, 0, :current_stacktrace}, {:var, 0, :Stacktrace}]},
                                {:cons, 0, {:tuple, 0, [{:atom, 0, :registered_name}, {nil, 0}]},
                                 {nil, 0}}}
                             ], [],
                             [
                               {:tuple, 0,
                                [
                                  {:var, 0, :Pid},
                                  {:tuple, 0, [{:var, 0, :Pid}, {:var, 0, :Stacktrace}]}
                                ]}
                             ]},
                            {:clause, 0,
                             [
                               {:cons, 0,
                                {:tuple, 0,
                                 [{:atom, 0, :current_stacktrace}, {:var, 0, :Stacktrace}]},
                                {:cons, 0,
                                 {:tuple, 0, [{:atom, 0, :registered_name}, {:var, 0, :Name}]},
                                 {nil, 0}}}
                             ], [],
                             [
                               {:tuple, 0,
                                [
                                  {:var, 0, :Pid},
                                  {:tuple, 0, [{:var, 0, :Name}, {:var, 0, :Stacktrace}]}
                                ]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [], [[{:atom, 0, true}]],
                       [{:tuple, 0, [{:var, 0, :Pid}, {:atom, 0, :remote}]}]}
                    ]}
                 ]},
                {:clause, 0, [{:cons, 0, {:var, 0, :_}, {:var, 0, :_}}], [],
                 [{:atom, 0, :undefined}]}
              ]},
             {:function, 0, :format_log, 1,
              [
                {:clause, 0, [{:var, 0, :Report}], [],
                 [
                   {:match, 0, {:var, 0, :Depth},
                    {:call, 0,
                     {:remote, 0, {:atom, 0, :error_logger}, {:atom, 0, :get_format_depth}}, []}},
                   {:match, 0, {:var, 0, :FormatOpts},
                    {:map, 0,
                     [
                       {:map_field_assoc, 0, {:atom, 0, :chars_limit}, {:atom, 0, :unlimited}},
                       {:map_field_assoc, 0, {:atom, 0, :depth}, {:var, 0, :Depth}},
                       {:map_field_assoc, 0, {:atom, 0, :single_line}, {:atom, 0, false}},
                       {:map_field_assoc, 0, {:atom, 0, :encoding}, {:atom, 0, :utf8}}
                     ]}},
                   {:call, 0, {:atom, 0, :format_log_multi},
                    [
                      {:call, 0, {:atom, 0, :limit_report},
                       [{:var, 0, :Report}, {:var, 0, :Depth}]},
                      {:var, 0, :FormatOpts}
                    ]}
                 ]}
              ]},
             {:function, 0, :limit_report, 2,
              [
                {:clause, 0, [{:var, 0, :Report}, {:atom, 0, :unlimited}], [],
                 [{:var, 0, :Report}]},
                {:clause, 0,
                 [
                   {:match, 0,
                    {:map, 0,
                     [
                       {:map_field_exact, 0, {:atom, 0, :label},
                        {:tuple, 0, [{:atom, 0, :gen_statem}, {:atom, 0, :terminate}]}},
                       {:map_field_exact, 0, {:atom, 0, :queue}, {:var, 0, :Q}},
                       {:map_field_exact, 0, {:atom, 0, :postponed}, {:var, 0, :Postponed}},
                       {:map_field_exact, 0, {:atom, 0, :modules}, {:var, 0, :Modules}},
                       {:map_field_exact, 0, {:atom, 0, :state}, {:var, 0, :FmtData}},
                       {:map_field_exact, 0, {:atom, 0, :timeouts}, {:var, 0, :Timeouts}},
                       {:map_field_exact, 0, {:atom, 0, :log}, {:var, 0, :Log}},
                       {:map_field_exact, 0, {:atom, 0, :reason},
                        {:tuple, 0,
                         [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}]}},
                       {:map_field_exact, 0, {:atom, 0, :client_info}, {:var, 0, :ClientInfo}}
                     ]}, {:var, 0, :Report}},
                   {:var, 0, :Depth}
                 ], [],
                 [
                   {:map, 0, {:var, 0, :Report},
                    [
                      {:map_field_assoc, 0, {:atom, 0, :queue},
                       {:case, 0, {:var, 0, :Q},
                        [
                          {:clause, 0, [{:cons, 0, {:var, 0, :Event}, {:var, 0, :Events}}], [],
                           [
                             {:cons, 0,
                              {:call, 0,
                               {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :limit_term}},
                               [{:var, 0, :Event}, {:var, 0, :Depth}]},
                              {:call, 0,
                               {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :limit_term}},
                               [{:var, 0, :Events}, {:var, 0, :Depth}]}}
                           ]},
                          {:clause, 0, [{:var, 0, :_}], [], [nil: 0]}
                        ]}},
                      {:map_field_assoc, 0, {:atom, 0, :postponed},
                       {:case, 0, {:var, 0, :Postponed},
                        [
                          {:clause, 0, [nil: 0], [], [nil: 0]},
                          {:clause, 0, [{:var, 0, :_}], [],
                           [
                             {:call, 0,
                              {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :limit_term}},
                              [{:var, 0, :Postponed}, {:var, 0, :Depth}]}
                           ]}
                        ]}},
                      {:map_field_assoc, 0, {:atom, 0, :modules},
                       {:call, 0, {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :limit_term}},
                        [{:var, 0, :Modules}, {:var, 0, :Depth}]}},
                      {:map_field_assoc, 0, {:atom, 0, :state},
                       {:call, 0, {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :limit_term}},
                        [{:var, 0, :FmtData}, {:var, 0, :Depth}]}},
                      {:map_field_assoc, 0, {:atom, 0, :timeouts},
                       {:case, 0, {:var, 0, :Timeouts},
                        [
                          {:clause, 0, [{:tuple, 0, [{:integer, 0, 0}, {:var, 0, :_}]}], [],
                           [{:var, 0, :Timeouts}]},
                          {:clause, 0, [{:var, 0, :_}], [],
                           [
                             {:call, 0,
                              {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :limit_term}},
                              [{:var, 0, :Timeouts}, {:var, 0, :Depth}]}
                           ]}
                        ]}},
                      {:map_field_assoc, 0, {:atom, 0, :log},
                       {:case, 0, {:var, 0, :Log},
                        [
                          {:clause, 0, [nil: 0], [], [nil: 0]},
                          {:clause, 0, [{:var, 0, :_}], [],
                           [
                             {:lc, 0,
                              {:call, 0,
                               {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :limit_term}},
                               [{:var, 0, :T}, {:var, 0, :Depth}]},
                              [{:generate, 0, {:var, 0, :T}, {:var, 0, :Log}}]}
                           ]}
                        ]}},
                      {:map_field_assoc, 0, {:atom, 0, :reason},
                       {:tuple, 0,
                        [
                          {:var, 0, :Class},
                          {:call, 0, {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :limit_term}},
                           [{:var, 0, :Reason}, {:var, 0, :Depth}]},
                          {:call, 0, {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :limit_term}},
                           [{:var, 0, :Stacktrace}, {:var, 0, :Depth}]}
                        ]}},
                      {:map_field_assoc, 0, {:atom, 0, :client_info},
                       {:call, 0, {:atom, 0, :limit_client_info},
                        [{:var, 0, :ClientInfo}, {:var, 0, :Depth}]}}
                    ]}
                 ]}
              ]},
             {:function, 0, :limit_client_info, 2,
              [
                {:clause, 0,
                 [
                   {:tuple, 0,
                    [
                      {:var, 0, :Pid},
                      {:tuple, 0, [{:var, 0, :Name}, {:var, 0, :Stacktrace}]}
                    ]},
                   {:var, 0, :Depth}
                 ], [],
                 [
                   {:tuple, 0,
                    [
                      {:var, 0, :Pid},
                      {:tuple, 0,
                       [
                         {:var, 0, :Name},
                         {:call, 0, {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :limit_term}},
                          [{:var, 0, :Stacktrace}, {:var, 0, :Depth}]}
                       ]}
                    ]}
                 ]},
                {:clause, 0, [{:var, 0, :Client}, {:var, 0, :_Depth}], [], [{:var, 0, :Client}]}
              ]},
             {:function, 0, :format_log, 2,
              [
                {:clause, 0, [{:var, 0, :Report}, {:var, 0, :FormatOpts0}], [],
                 [
                   {:match, 0, {:var, 0, :Default},
                    {:map, 0,
                     [
                       {:map_field_assoc, 0, {:atom, 0, :chars_limit}, {:atom, 0, :unlimited}},
                       {:map_field_assoc, 0, {:atom, 0, :depth}, {:atom, 0, :unlimited}},
                       {:map_field_assoc, 0, {:atom, 0, :single_line}, {:atom, 0, false}},
                       {:map_field_assoc, 0, {:atom, 0, :encoding}, {:atom, 0, :utf8}}
                     ]}},
                   {:match, 0, {:var, 0, :FormatOpts},
                    {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :merge}},
                     [{:var, 0, :Default}, {:var, 0, :FormatOpts0}]}},
                   {:match, 0, {:var, 0, :IoOpts},
                    {:case, 0, {:var, 0, :FormatOpts},
                     [
                       {:clause, 0,
                        [
                          {:map, 0,
                           [
                             {:map_field_exact, 0, {:atom, 0, :chars_limit},
                              {:atom, 0, :unlimited}}
                           ]}
                        ], [], [nil: 0]},
                       {:clause, 0,
                        [
                          {:map, 0,
                           [
                             {:map_field_exact, 0, {:atom, 0, :chars_limit}, {:var, 0, :Limit}}
                           ]}
                        ], [],
                        [
                          {:cons, 0, {:tuple, 0, [{:atom, 0, :chars_limit}, {:var, 0, :Limit}]},
                           {nil, 0}}
                        ]}
                     ]}},
                   {:match, 0, {:tuple, 0, [{:var, 0, :Format}, {:var, 0, :Args}]},
                    {:call, 0, {:atom, 0, :format_log_single},
                     [{:var, 0, :Report}, {:var, 0, :FormatOpts}]}},
                   {:call, 0, {:remote, 0, {:atom, 0, :io_lib}, {:atom, 0, :format}},
                    [{:var, 0, :Format}, {:var, 0, :Args}, {:var, 0, :IoOpts}]}
                 ]}
              ]},
             {:function, 0, :format_log_single, 2,
              [
                {:clause, 0,
                 [
                   {:map, 0,
                    [
                      {:map_field_exact, 0, {:atom, 0, :label},
                       {:tuple, 0, [{:atom, 0, :gen_statem}, {:atom, 0, :terminate}]}},
                      {:map_field_exact, 0, {:atom, 0, :name}, {:var, 0, :Name}},
                      {:map_field_exact, 0, {:atom, 0, :queue}, {:var, 0, :Q}},
                      {:map_field_exact, 0, {:atom, 0, :state}, {:var, 0, :FmtData}},
                      {:map_field_exact, 0, {:atom, 0, :log}, {:var, 0, :Log}},
                      {:map_field_exact, 0, {:atom, 0, :reason},
                       {:tuple, 0,
                        [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}]}},
                      {:map_field_exact, 0, {:atom, 0, :client_info}, {:var, 0, :ClientInfo}}
                    ]},
                   {:match, 0,
                    {:map, 0,
                     [
                       {:map_field_exact, 0, {:atom, 0, :single_line}, {:atom, 0, true}},
                       {:map_field_exact, 0, {:atom, 0, :depth}, {:var, 0, :Depth}}
                     ]}, {:var, 0, :FormatOpts}}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :P},
                    {:call, 0, {:atom, 0, :p}, [{:var, 0, :FormatOpts}]}},
                   {:match, 0,
                    {:tuple, 0, [{:var, 0, :FixedReason}, {:var, 0, :FixedStacktrace}]},
                    {:call, 0, {:atom, 0, :fix_reason},
                     [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}]}},
                   {:match, 0, {:tuple, 0, [{:var, 0, :ClientFmt}, {:var, 0, :ClientArgs}]},
                    {:call, 0, {:atom, 0, :format_client_log_single},
                     [{:var, 0, :ClientInfo}, {:var, 0, :P}, {:var, 0, :Depth}]}},
                   {:match, 0, {:var, 0, :Format},
                    {:call, 0, {:remote, 0, {:atom, 0, :lists}, {:atom, 0, :append}},
                     [
                       {:cons, 0, {:string, 0, 'State machine '},
                        {:cons, 0, {:var, 0, :P},
                         {:cons, 0, {:string, 0, ' terminating. Reason: '},
                          {:cons, 0, {:var, 0, :P},
                           {:cons, 0,
                            {:case, 0, {:var, 0, :FixedStacktrace},
                             [
                               {:clause, 0, [nil: 0], [], [{:string, 0, []}]},
                               {:clause, 0, [{:var, 0, :_}], [],
                                [{:op, 0, :++, {:string, 0, '. Stack: '}, {:var, 0, :P}}]}
                             ]},
                            {:cons, 0,
                             {:case, 0, {:var, 0, :Q},
                              [
                                {:clause, 0, [nil: 0], [], [{:string, 0, []}]},
                                {:clause, 0, [{:var, 0, :_}], [],
                                 [
                                   {:op, 0, :++, {:string, 0, '. Last event: '}, {:var, 0, :P}}
                                 ]}
                              ]},
                             {:cons, 0, {:string, 0, '. State: '},
                              {:cons, 0, {:var, 0, :P},
                               {:cons, 0,
                                {:case, 0, {:var, 0, :Log},
                                 [
                                   {:clause, 0, [nil: 0], [], [{:string, 0, []}]},
                                   {:clause, 0, [{:var, 0, :_}], [],
                                    [{:op, 0, :++, {:string, 0, '. Log: '}, {:var, 0, :P}}]}
                                 ]}, {:cons, 0, {:string, 0, '.'}, {nil, 0}}}}}}}}}}}
                     ]}},
                   {:match, 0, {:var, 0, :Args0},
                    {:op, 0, :++,
                     {:cons, 0, {:var, 0, :Name}, {:cons, 0, {:var, 0, :FixedReason}, {nil, 0}}},
                     {:op, 0, :++,
                      {:case, 0, {:var, 0, :FixedStacktrace},
                       [
                         {:clause, 0, [nil: 0], [], [nil: 0]},
                         {:clause, 0, [{:var, 0, :_}], [],
                          [{:cons, 0, {:var, 0, :FixedStacktrace}, {nil, 0}}]}
                       ]},
                      {:op, 0, :++,
                       {:case, 0, {:var, 0, :Q},
                        [
                          {:clause, 0, [nil: 0], [], [nil: 0]},
                          {:clause, 0, [{:cons, 0, {:var, 0, :Event}, {:var, 0, :_}}], [],
                           [{:cons, 0, {:var, 0, :Event}, {nil, 0}}]}
                        ]},
                       {:op, 0, :++, {:cons, 0, {:var, 0, :FmtData}, {nil, 0}},
                        {:case, 0, {:var, 0, :Log},
                         [
                           {:clause, 0, [nil: 0], [], [nil: 0]},
                           {:clause, 0, [{:var, 0, :_}], [],
                            [{:cons, 0, {:var, 0, :Log}, {nil, 0}}]}
                         ]}}}}}},
                   {:match, 0, {:var, 0, :Args},
                    {:case, 0, {:var, 0, :Depth},
                     [
                       {:clause, 0, [{:atom, 0, :unlimited}], [], [{:var, 0, :Args0}]},
                       {:clause, 0, [{:var, 0, :_}], [],
                        [
                          {:call, 0, {:remote, 0, {:atom, 0, :lists}, {:atom, 0, :flatmap}},
                           [
                             {:fun, 0,
                              {:clauses,
                               [
                                 {:clause, 0, [{:var, 0, :A}], [],
                                  [
                                    {:cons, 0, {:var, 0, :A},
                                     {:cons, 0, {:var, 0, :Depth}, {nil, 0}}}
                                  ]}
                               ]}},
                             {:var, 0, :Args0}
                           ]}
                        ]}
                     ]}},
                   {:tuple, 0,
                    [
                      {:op, 0, :++, {:var, 0, :Format}, {:var, 0, :ClientFmt}},
                      {:op, 0, :++, {:var, 0, :Args}, {:var, 0, :ClientArgs}}
                    ]}
                 ]},
                {:clause, 0, [{:var, 0, :Report}, {:var, 0, :FormatOpts}], [],
                 [
                   {:call, 0, {:atom, 0, :format_log_multi},
                    [{:var, 0, :Report}, {:var, 0, :FormatOpts}]}
                 ]}
              ]},
             {:function, 0, :format_log_multi, 2,
              [
                {:clause, 0,
                 [
                   {:map, 0,
                    [
                      {:map_field_exact, 0, {:atom, 0, :label},
                       {:tuple, 0, [{:atom, 0, :gen_statem}, {:atom, 0, :terminate}]}},
                      {:map_field_exact, 0, {:atom, 0, :name}, {:var, 0, :Name}},
                      {:map_field_exact, 0, {:atom, 0, :queue}, {:var, 0, :Q}},
                      {:map_field_exact, 0, {:atom, 0, :postponed}, {:var, 0, :Postponed}},
                      {:map_field_exact, 0, {:atom, 0, :modules}, {:var, 0, :Modules}},
                      {:map_field_exact, 0, {:atom, 0, :callback_mode}, {:var, 0, :CallbackMode}},
                      {:map_field_exact, 0, {:atom, 0, :state_enter}, {:var, 0, :StateEnter}},
                      {:map_field_exact, 0, {:atom, 0, :state}, {:var, 0, :FmtData}},
                      {:map_field_exact, 0, {:atom, 0, :timeouts}, {:var, 0, :Timeouts}},
                      {:map_field_exact, 0, {:atom, 0, :log}, {:var, 0, :Log}},
                      {:map_field_exact, 0, {:atom, 0, :reason},
                       {:tuple, 0,
                        [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}]}},
                      {:map_field_exact, 0, {:atom, 0, :client_info}, {:var, 0, :ClientInfo}}
                    ]},
                   {:match, 0,
                    {:map, 0, [{:map_field_exact, 0, {:atom, 0, :depth}, {:var, 0, :Depth}}]},
                    {:var, 0, :FormatOpts}}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :P},
                    {:call, 0, {:atom, 0, :p}, [{:var, 0, :FormatOpts}]}},
                   {:match, 0,
                    {:tuple, 0, [{:var, 0, :FixedReason}, {:var, 0, :FixedStacktrace}]},
                    {:call, 0, {:atom, 0, :fix_reason},
                     [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}]}},
                   {:match, 0, {:tuple, 0, [{:var, 0, :ClientFmt}, {:var, 0, :ClientArgs}]},
                    {:call, 0, {:atom, 0, :format_client_log},
                     [{:var, 0, :ClientInfo}, {:var, 0, :P}, {:var, 0, :Depth}]}},
                   {:match, 0, {:var, 0, :CBMode},
                    {:case, 0, {:var, 0, :StateEnter},
                     [
                       {:clause, 0, [{:atom, 0, true}], [],
                        [
                          {:cons, 0, {:var, 0, :CallbackMode},
                           {:cons, 0, {:atom, 0, :state_enter}, {nil, 0}}}
                        ]},
                       {:clause, 0, [{:atom, 0, false}], [], [{:var, 0, :CallbackMode}]}
                     ]}},
                   {:match, 0, {:var, 0, :Format},
                    {:call, 0, {:remote, 0, {:atom, 0, :lists}, {:atom, 0, :append}},
                     [
                       {:cons, 0, {:string, 0, '** State machine '},
                        {:cons, 0, {:var, 0, :P},
                         {:cons, 0, {:string, 0, ' terminating~n'},
                          {:cons, 0,
                           {:case, 0, {:var, 0, :Q},
                            [
                              {:clause, 0, [nil: 0], [], [{:string, 0, []}]},
                              {:clause, 0, [{:var, 0, :_}], [],
                               [
                                 {:op, 0, :++, {:string, 0, '** Last event = '},
                                  {:op, 0, :++, {:var, 0, :P}, {:string, 0, '~n'}}}
                               ]}
                            ]},
                           {:cons, 0, {:string, 0, '** When server state  = '},
                            {:cons, 0, {:var, 0, :P},
                             {:cons, 0, {:string, 0, '~n'},
                              {:cons, 0, {:string, 0, '** Reason for termination = '},
                               {:cons, 0, {:var, 0, :P},
                                {:cons, 0, {:string, 0, ':'},
                                 {:cons, 0, {:var, 0, :P},
                                  {:cons, 0, {:string, 0, '~n'},
                                   {:cons, 0, {:string, 0, '** Callback modules = '},
                                    {:cons, 0, {:var, 0, :P},
                                     {:cons, 0, {:string, 0, '~n'},
                                      {:cons, 0, {:string, 0, '** Callback mode = '},
                                       {:cons, 0, {:var, 0, :P},
                                        {:cons, 0, {:string, 0, '~n'},
                                         {:cons, 0,
                                          {:case, 0, {:var, 0, :Q},
                                           [
                                             {:clause, 0,
                                              [
                                                {:cons, 0, {:var, 0, :_},
                                                 {:cons, 0, {:var, 0, :_}, {:var, 0, :_}}}
                                              ], [],
                                              [
                                                {:op, 0, :++, {:string, 0, '** Queued = '},
                                                 {:op, 0, :++, {:var, 0, :P}, {:string, 0, '~n'}}}
                                              ]},
                                             {:clause, 0, [{:var, 0, :_}], [], [{:string, 0, []}]}
                                           ]},
                                          {:cons, 0,
                                           {:case, 0, {:var, 0, :Postponed},
                                            [
                                              {:clause, 0, [nil: 0], [], [{:string, 0, []}]},
                                              {:clause, 0, [{:var, 0, :_}], [],
                                               [
                                                 {:op, 0, :++, {:string, 0, '** Postponed = '},
                                                  {:op, 0, :++, {:var, 0, :P}, {:string, 0, '~n'}}}
                                               ]}
                                            ]},
                                           {:cons, 0,
                                            {:case, 0, {:var, 0, :FixedStacktrace},
                                             [
                                               {:clause, 0, [nil: 0], [], [{:string, 0, []}]},
                                               {:clause, 0, [{:var, 0, :_}], [],
                                                [
                                                  {:op, 0, :++,
                                                   {:string, 0, '** Stacktrace =~n**  '},
                                                   {:op, 0, :++, {:var, 0, :P},
                                                    {:string, 0, '~n'}}}
                                                ]}
                                             ]},
                                            {:cons, 0,
                                             {:case, 0, {:var, 0, :Timeouts},
                                              [
                                                {:clause, 0,
                                                 [
                                                   {:tuple, 0, [{:integer, 0, 0}, {:var, 0, :_}]}
                                                 ], [], [{:string, 0, []}]},
                                                {:clause, 0, [{:var, 0, :_}], [],
                                                 [
                                                   {:op, 0, :++, {:string, 0, '** Time-outs: '},
                                                    {:op, 0, :++, {:var, 0, :P},
                                                     {:string, 0, '~n'}}}
                                                 ]}
                                              ]},
                                             {:cons, 0,
                                              {:case, 0, {:var, 0, :Log},
                                               [
                                                 {:clause, 0, [nil: 0], [], [{:string, 0, []}]},
                                                 {:clause, 0, [{:var, 0, :_}], [],
                                                  [
                                                    {:op, 0, :++, {:string, 0, '** Log =~n**  '},
                                                     {:op, 0, :++, {:var, 0, :P},
                                                      {:string, 0, '~n'}}}
                                                  ]}
                                               ]}, {nil, 0}}}}}}}}}}}}}}}}}}}}}}}}
                     ]}},
                   {:match, 0, {:var, 0, :Args0},
                    {:op, 0, :++,
                     {:cons, 0, {:var, 0, :Name},
                      {:case, 0, {:var, 0, :Q},
                       [
                         {:clause, 0, [nil: 0], [], [nil: 0]},
                         {:clause, 0, [{:cons, 0, {:var, 0, :Event}, {:var, 0, :_}}], [],
                          [{:cons, 0, {:var, 0, :Event}, {nil, 0}}]}
                       ]}},
                     {:op, 0, :++,
                      {:cons, 0, {:var, 0, :FmtData},
                       {:cons, 0, {:var, 0, :Class},
                        {:cons, 0, {:var, 0, :FixedReason},
                         {:cons, 0, {:var, 0, :Modules}, {:cons, 0, {:var, 0, :CBMode}, {nil, 0}}}}}},
                      {:op, 0, :++,
                       {:case, 0, {:var, 0, :Q},
                        [
                          {:clause, 0,
                           [
                             {:cons, 0, {:var, 0, :_},
                              {:match, 0, {:cons, 0, {:var, 0, :_}, {:var, 0, :_}},
                               {:var, 0, :Events}}}
                           ], [], [{:cons, 0, {:var, 0, :Events}, {nil, 0}}]},
                          {:clause, 0, [{:var, 0, :_}], [], [nil: 0]}
                        ]},
                       {:op, 0, :++,
                        {:case, 0, {:var, 0, :Postponed},
                         [
                           {:clause, 0, [nil: 0], [], [nil: 0]},
                           {:clause, 0, [{:var, 0, :_}], [],
                            [{:cons, 0, {:var, 0, :Postponed}, {nil, 0}}]}
                         ]},
                        {:op, 0, :++,
                         {:case, 0, {:var, 0, :FixedStacktrace},
                          [
                            {:clause, 0, [nil: 0], [], [nil: 0]},
                            {:clause, 0, [{:var, 0, :_}], [],
                             [{:cons, 0, {:var, 0, :FixedStacktrace}, {nil, 0}}]}
                          ]},
                         {:op, 0, :++,
                          {:case, 0, {:var, 0, :Timeouts},
                           [
                             {:clause, 0, [{:tuple, 0, [{:integer, 0, 0}, {:var, 0, :_}]}], [],
                              [nil: 0]},
                             {:clause, 0, [{:var, 0, :_}], [],
                              [{:cons, 0, {:var, 0, :Timeouts}, {nil, 0}}]}
                           ]},
                          {:case, 0, {:var, 0, :Log},
                           [
                             {:clause, 0, [nil: 0], [], [nil: 0]},
                             {:clause, 0, [{:var, 0, :_}], [],
                              [{:cons, 0, {:var, 0, :Log}, {nil, 0}}]}
                           ]}}}}}}}},
                   {:match, 0, {:var, 0, :Args},
                    {:case, 0, {:var, 0, :Depth},
                     [
                       {:clause, 0, [{:atom, 0, :unlimited}], [], [{:var, 0, :Args0}]},
                       {:clause, 0, [{:var, 0, :_}], [],
                        [
                          {:call, 0, {:remote, 0, {:atom, 0, :lists}, {:atom, 0, :flatmap}},
                           [
                             {:fun, 0,
                              {:clauses,
                               [
                                 {:clause, 0, [{:var, 0, :A}], [],
                                  [
                                    {:cons, 0, {:var, 0, :A},
                                     {:cons, 0, {:var, 0, :Depth}, {nil, 0}}}
                                  ]}
                               ]}},
                             {:var, 0, :Args0}
                           ]}
                        ]}
                     ]}},
                   {:tuple, 0,
                    [
                      {:op, 0, :++, {:var, 0, :Format}, {:var, 0, :ClientFmt}},
                      {:op, 0, :++, {:var, 0, :Args}, {:var, 0, :ClientArgs}}
                    ]}
                 ]}
              ]},
             {:function, 0, :fix_reason, 3,
              [
                {:clause, 0, [{:var, 0, :Class}, {:var, 0, :Reason}, {:var, 0, :Stacktrace}], [],
                 [
                   {:case, 0, {:var, 0, :Stacktrace},
                    [
                      {:clause, 0,
                       [
                         {:cons, 0,
                          {:tuple, 0,
                           [{:var, 0, :M}, {:var, 0, :F}, {:var, 0, :Args}, {:var, 0, :_}]},
                          {:var, 0, :ST}}
                       ],
                       [
                         [
                           {:op, 0, :"=:=", {:var, 0, :Class}, {:atom, 0, :error}},
                           {:op, 0, :"=:=", {:var, 0, :Reason}, {:atom, 0, :undef}}
                         ]
                       ],
                       [
                         {:case, 0,
                          {:call, 0, {:remote, 0, {:atom, 0, :code}, {:atom, 0, :is_loaded}},
                           [{:var, 0, :M}]},
                          [
                            {:clause, 0, [{:atom, 0, false}], [],
                             [
                               {:tuple, 0,
                                [
                                  {:tuple, 0,
                                   [
                                     {:atom, 0, :"module could not be loaded"},
                                     {:var, 0, :M}
                                   ]},
                                  {:var, 0, :ST}
                                ]}
                             ]},
                            {:clause, 0, [{:var, 0, :_}], [],
                             [
                               {:match, 0, {:var, 0, :Arity},
                                {:if, 0,
                                 [
                                   {:clause, 0, [],
                                    [
                                      [
                                        {:call, 0, {:atom, 0, :is_list}, [{:var, 0, :Args}]}
                                      ]
                                    ], [{:call, 0, {:atom, 0, :length}, [{:var, 0, :Args}]}]},
                                   {:clause, 0, [],
                                    [
                                      [
                                        {:call, 0, {:atom, 0, :is_integer}, [{:var, 0, :Args}]}
                                      ]
                                    ], [{:var, 0, :Args}]}
                                 ]}},
                               {:case, 0,
                                {:call, 0,
                                 {:remote, 0, {:atom, 0, :erlang},
                                  {:atom, 0, :function_exported}},
                                 [{:var, 0, :M}, {:var, 0, :F}, {:var, 0, :Arity}]},
                                [
                                  {:clause, 0, [{:atom, 0, true}], [],
                                   [
                                     {:tuple, 0, [{:var, 0, :Reason}, {:var, 0, :Stacktrace}]}
                                   ]},
                                  {:clause, 0, [{:atom, 0, false}], [],
                                   [
                                     {:tuple, 0,
                                      [
                                        {:tuple, 0,
                                         [
                                           {:atom, 0, :"function not exported"},
                                           {:tuple, 0,
                                            [
                                              {:var, 0, :M},
                                              {:var, 0, :F},
                                              {:var, 0, :Arity}
                                            ]}
                                         ]},
                                        {:var, 0, :ST}
                                      ]}
                                   ]}
                                ]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [{:tuple, 0, [{:var, 0, :Reason}, {:var, 0, :Stacktrace}]}]}
                    ]}
                 ]}
              ]},
             {:function, 0, :format_client_log_single, 3,
              [
                {:clause, 0, [{:atom, 0, :undefined}, {:var, 0, :_}, {:var, 0, :_}], [],
                 [{:tuple, 0, [{:string, 0, []}, {nil, 0}]}]},
                {:clause, 0,
                 [
                   {:tuple, 0, [{:var, 0, :Pid}, {:atom, 0, :dead}]},
                   {:var, 0, :_},
                   {:var, 0, :_}
                 ], [],
                 [
                   {:tuple, 0,
                    [
                      {:string, 0, ' Client ~0p is dead.'},
                      {:cons, 0, {:var, 0, :Pid}, {nil, 0}}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:tuple, 0, [{:var, 0, :Pid}, {:atom, 0, :remote}]},
                   {:var, 0, :_},
                   {:var, 0, :_}
                 ], [],
                 [
                   {:tuple, 0,
                    [
                      {:string, 0, ' Client ~0p is remote on node ~0p.'},
                      {:cons, 0, {:var, 0, :Pid},
                       {:cons, 0, {:call, 0, {:atom, 0, :node}, [{:var, 0, :Pid}]}, {nil, 0}}}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:tuple, 0,
                    [
                      {:var, 0, :_Pid},
                      {:tuple, 0, [{:var, 0, :Name}, {:var, 0, :Stacktrace0}]}
                    ]},
                   {:var, 0, :P},
                   {:var, 0, :Depth}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :Stacktrace},
                    {:call, 0, {:remote, 0, {:atom, 0, :lists}, {:atom, 0, :sublist}},
                     [{:var, 0, :Stacktrace0}, {:integer, 0, 4}]}},
                   {:match, 0, {:var, 0, :Format},
                    {:call, 0, {:remote, 0, {:atom, 0, :lists}, {:atom, 0, :append}},
                     [
                       {:cons, 0, {:string, 0, ' Client '},
                        {:cons, 0, {:var, 0, :P},
                         {:cons, 0, {:string, 0, ' stacktrace: '},
                          {:cons, 0, {:var, 0, :P}, {:cons, 0, {:string, 0, '.'}, {nil, 0}}}}}}
                     ]}},
                   {:match, 0, {:var, 0, :Args},
                    {:case, 0, {:var, 0, :Depth},
                     [
                       {:clause, 0, [{:atom, 0, :unlimited}], [],
                        [
                          {:cons, 0, {:var, 0, :Name},
                           {:cons, 0, {:var, 0, :Stacktrace}, {nil, 0}}}
                        ]},
                       {:clause, 0, [{:var, 0, :_}], [],
                        [
                          {:cons, 0, {:var, 0, :Name},
                           {:cons, 0, {:var, 0, :Depth},
                            {:cons, 0, {:var, 0, :Stacktrace},
                             {:cons, 0, {:var, 0, :Depth}, {nil, 0}}}}}
                        ]}
                     ]}},
                   {:tuple, 0, [{:var, 0, :Format}, {:var, 0, :Args}]}
                 ]}
              ]},
             {:function, 0, :format_client_log, 3,
              [
                {:clause, 0, [{:atom, 0, :undefined}, {:var, 0, :_}, {:var, 0, :_}], [],
                 [{:tuple, 0, [{:string, 0, []}, {nil, 0}]}]},
                {:clause, 0,
                 [
                   {:tuple, 0, [{:var, 0, :Pid}, {:atom, 0, :dead}]},
                   {:var, 0, :_},
                   {:var, 0, :_}
                 ], [],
                 [
                   {:tuple, 0,
                    [
                      {:string, 0, '** Client ~p is dead~n'},
                      {:cons, 0, {:var, 0, :Pid}, {nil, 0}}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:tuple, 0, [{:var, 0, :Pid}, {:atom, 0, :remote}]},
                   {:var, 0, :_},
                   {:var, 0, :_}
                 ], [],
                 [
                   {:tuple, 0,
                    [
                      {:string, 0, '** Client ~p is remote on node ~p~n'},
                      {:cons, 0, {:var, 0, :Pid},
                       {:cons, 0, {:call, 0, {:atom, 0, :node}, [{:var, 0, :Pid}]}, {nil, 0}}}
                    ]}
                 ]},
                {:clause, 0,
                 [
                   {:tuple, 0,
                    [
                      {:var, 0, :_Pid},
                      {:tuple, 0, [{:var, 0, :Name}, {:var, 0, :Stacktrace}]}
                    ]},
                   {:var, 0, :P},
                   {:var, 0, :Depth}
                 ], [],
                 [
                   {:match, 0, {:var, 0, :Format},
                    {:call, 0, {:remote, 0, {:atom, 0, :lists}, {:atom, 0, :append}},
                     [
                       {:cons, 0, {:string, 0, '** Client '},
                        {:cons, 0, {:var, 0, :P},
                         {:cons, 0, {:string, 0, ' stacktrace~n** '},
                          {:cons, 0, {:var, 0, :P}, {:cons, 0, {:string, 0, '~n'}, {nil, 0}}}}}}
                     ]}},
                   {:match, 0, {:var, 0, :Args},
                    {:case, 0, {:var, 0, :Depth},
                     [
                       {:clause, 0, [{:atom, 0, :unlimited}], [],
                        [
                          {:cons, 0, {:var, 0, :Name},
                           {:cons, 0, {:var, 0, :Stacktrace}, {nil, 0}}}
                        ]},
                       {:clause, 0, [{:var, 0, :_}], [],
                        [
                          {:cons, 0, {:var, 0, :Name},
                           {:cons, 0, {:var, 0, :Depth},
                            {:cons, 0, {:var, 0, :Stacktrace},
                             {:cons, 0, {:var, 0, :Depth}, {nil, 0}}}}}
                        ]}
                     ]}},
                   {:tuple, 0, [{:var, 0, :Format}, {:var, 0, :Args}]}
                 ]}
              ]},
             {:function, 0, :p, 1,
              [
                {:clause, 0,
                 [
                   {:map, 0,
                    [
                      {:map_field_exact, 0, {:atom, 0, :single_line}, {:var, 0, :Single}},
                      {:map_field_exact, 0, {:atom, 0, :depth}, {:var, 0, :Depth}},
                      {:map_field_exact, 0, {:atom, 0, :encoding}, {:var, 0, :Enc}}
                    ]}
                 ], [],
                 [
                   {:op, 0, :++, {:string, 0, '~'},
                    {:op, 0, :++, {:call, 0, {:atom, 0, :single}, [{:var, 0, :Single}]},
                     {:op, 0, :++, {:call, 0, {:atom, 0, :mod}, [{:var, 0, :Enc}]},
                      {:call, 0, {:atom, 0, :p}, [{:var, 0, :Depth}]}}}}
                 ]},
                {:clause, 0, [{:atom, 0, :unlimited}], [], [{:string, 0, 'p'}]},
                {:clause, 0, [{:var, 0, :_Depth}], [], [{:string, 0, 'P'}]}
              ]},
             {:function, 0, :single, 1,
              [
                {:clause, 0, [{:atom, 0, true}], [], [{:string, 0, '0'}]},
                {:clause, 0, [{:atom, 0, false}], [], [{:string, 0, []}]}
              ]},
             {:function, 0, :mod, 1,
              [
                {:clause, 0, [{:atom, 0, :latin1}], [], [{:string, 0, []}]},
                {:clause, 0, [{:var, 0, :_}], [], [{:string, 0, 't'}]}
              ]},
             {:attribute, 0, :compile, {:inline, [listify: 1]}},
             {:function, 0, :listify, 1,
              [
                {:clause, 0, [{:var, 0, :Item}],
                 [[{:call, 0, {:atom, 0, :is_list}, [{:var, 0, :Item}]}]], [{:var, 0, :Item}]},
                {:clause, 0, [{:var, 0, :Item}], [], [{:cons, 0, {:var, 0, :Item}, {nil, 0}}]}
              ]},
             {:attribute, 0, :compile, {:inline, [cancel_timer: 1]}},
             {:function, 0, :cancel_timer, 1,
              [
                {:clause, 0, [{:var, 0, :TimerRef}], [],
                 [
                   {:case, 0,
                    {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :cancel_timer}},
                     [{:var, 0, :TimerRef}]},
                    [
                      {:clause, 0, [{:atom, 0, false}], [],
                       [
                         {:receive, 0,
                          [
                            {:clause, 0,
                             [
                               {:tuple, 0,
                                [{:atom, 0, :timeout}, {:var, 0, :TimerRef}, {:var, 0, :_}]}
                             ], [], [{:atom, 0, :ok}]}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :ok}]}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :compile, {:inline, [cancel_timer: 3]}},
             {:function, 0, :cancel_timer, 3,
              [
                {:clause, 0, [{:var, 0, :TimeoutType}, {:var, 0, :TimerRef}, {:var, 0, :Timers}],
                 [],
                 [
                   {:case, 0, {:var, 0, :TimerRef},
                    [
                      {:clause, 0, [{:integer, 0, 0}], [],
                       [
                         {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :remove}},
                          [
                            {:block, 0, [{:var, 0, :TimeoutType}]},
                            {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :update}},
                             [
                               {:atom, 0, :t0q},
                               {:call, 0, {:remote, 0, {:atom, 0, :lists}, {:atom, 0, :delete}},
                                [
                                  {:block, 0, [{:var, 0, :TimeoutType}]},
                                  {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :get}},
                                   [{:atom, 0, :t0q}, {:block, 0, [{:var, 0, :Timers}]}]}
                                ]},
                               {:block, 0, [{:var, 0, :Timers}]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [{:var, 0, :_}], [],
                       [
                         {:case, 0,
                          {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :cancel_timer}},
                           [{:var, 0, :TimerRef}]},
                          [
                            {:clause, 0, [{:atom, 0, false}], [],
                             [
                               {:receive, 0,
                                [
                                  {:clause, 0,
                                   [
                                     {:tuple, 0,
                                      [
                                        {:atom, 0, :timeout},
                                        {:var, 0, :TimerRef},
                                        {:var, 0, :_}
                                      ]}
                                   ], [], [{:atom, 0, :ok}]}
                                ]}
                             ]},
                            {:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :ok}]}
                          ]},
                         {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :remove}},
                          [
                            {:block, 0, [{:var, 0, :TimeoutType}]},
                            {:block, 0, [{:var, 0, :Timers}]}
                          ]}
                       ]}
                    ]}
                 ]}
              ]},
             {:attribute, 0, :compile, {:inline, [cancel_timer: 2]}},
             {:function, 0, :cancel_timer, 2,
              [
                {:clause, 0, [{:var, 0, :TimeoutType}, {:var, 0, :Timers}], [],
                 [
                   {:case, 0, {:var, 0, :Timers},
                    [
                      {:clause, 0,
                       [
                         {:map, 0,
                          [
                            {:map_field_exact, 0, {:var, 0, :TimeoutType},
                             {:tuple, 0, [{:var, 0, :TimerRef}, {:var, 0, :_TimeoutMsg}]}}
                          ]}
                       ], [],
                       [
                         {:case, 0, {:var, 0, :TimerRef},
                          [
                            {:clause, 0, [{:integer, 0, 0}], [],
                             [
                               {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :remove}},
                                [
                                  {:block, 0, [{:var, 0, :TimeoutType}]},
                                  {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :update}},
                                   [
                                     {:atom, 0, :t0q},
                                     {:call, 0,
                                      {:remote, 0, {:atom, 0, :lists}, {:atom, 0, :delete}},
                                      [
                                        {:block, 0, [{:var, 0, :TimeoutType}]},
                                        {:call, 0,
                                         {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :get}},
                                         [
                                           {:atom, 0, :t0q},
                                           {:block, 0, [{:var, 0, :Timers}]}
                                         ]}
                                      ]},
                                     {:block, 0, [{:var, 0, :Timers}]}
                                   ]}
                                ]}
                             ]},
                            {:clause, 0, [{:var, 0, :_}], [],
                             [
                               {:case, 0,
                                {:call, 0,
                                 {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :cancel_timer}},
                                 [{:var, 0, :TimerRef}]},
                                [
                                  {:clause, 0, [{:atom, 0, false}], [],
                                   [
                                     {:receive, 0,
                                      [
                                        {:clause, 0,
                                         [
                                           {:tuple, 0,
                                            [
                                              {:atom, 0, :timeout},
                                              {:var, 0, :TimerRef},
                                              {:var, 0, :_}
                                            ]}
                                         ], [], [{:atom, 0, :ok}]}
                                      ]}
                                   ]},
                                  {:clause, 0, [{:var, 0, :_}], [], [{:atom, 0, :ok}]}
                                ]},
                               {:call, 0, {:remote, 0, {:atom, 0, :maps}, {:atom, 0, :remove}},
                                [
                                  {:block, 0, [{:var, 0, :TimeoutType}]},
                                  {:block, 0, [{:var, 0, :Timers}]}
                                ]}
                             ]}
                          ]}
                       ]},
                      {:clause, 0, [{:map, 0, []}], [], [{:var, 0, :Timers}]}
                    ]}
                 ]}
              ]},
             {:error, {0, :erl_parse, ['syntax error before: ', '\':=\'']}},
             {:eof, 0}
           ] = clean_tree
  end
end