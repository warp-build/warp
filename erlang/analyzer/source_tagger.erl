-module(source_tagger).

-export([tag/1]).

-export_type([t/0]).

-type t() :: #{ sources => [path:t()],
                headers => [path:t()],
                others  => [path:t()]
              }.

%% @doc given an number of source files, lets tag them and group them by
%% whether they are Erlang source files, Erlang header files, or other files.
%%
-spec tag(Files :: [path:t()]) -> t().
tag(Files) -> tag(Files,  [], [], []).

tag([], Srcs, Hdrs, Others) ->
  #{ sources => lists:reverse(Srcs),
     headers => lists:reverse(Hdrs),
     others => lists:reverse(Others)
   };

tag([<<"">>|Files], Srcs, Hdrs, Others) -> tag(Files, Srcs, Hdrs, Others);
tag([<<".">>|Files], Srcs, Hdrs, Others) -> tag(Files, Srcs, Hdrs, Others);
tag([File|Files], Srcs, Hdrs, Others) ->
  case (catch filename:extension(File)) of
    <<".erl">> -> tag(Files, [File|Srcs], Hdrs, Others);
    <<".hrl">> -> tag(Files, Srcs, [File|Hdrs], Others);
    _ -> tag(Files, Srcs, Hdrs, [File|Others])
  end.
