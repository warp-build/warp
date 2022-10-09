-module(wl_erlmk).

-export([from_file/1]).

from_file(Path) ->
  {ok, Data} = file:read_file(Path),
  Lines = string:split(Data, <<"\n">>, all),
  Deps = case find_var("DEPS", Lines) of
           {ok, D} -> get_deps(D, Lines);
           _ -> []
         end,
  {ok, [ {deps, Deps} ]}.

get_deps(Deps, Lines) ->
  Keys = [ {Dep, "dep_" ++ Dep} || Dep <- Deps ],
  lists:map(fun ({Name, Key}) ->
                {ok, DepStr} = find_var(Key, Lines),
                parse_dep(Name, DepStr)
            end, Keys).

find_var(VarName, Lines) -> find_var(VarName, Lines, none).
find_var(VarName, [], none) -> {error, {var_not_found, VarName}};
find_var(VarName, [Line|Rest], none) ->
  case string:prefix(Line, VarName) of
    nomatch -> find_var(VarName, Rest, none);
    Value -> {ok, string:split(string:prefix(Value, " = "), <<" ">>, all)}
  end.

parse_dep(Name, [_, Repo, Version]) ->
  {
   erlang:binary_to_atom(Name, utf8),
   {git,
    binary:bin_to_list(Repo),
    {tag, binary:bin_to_list(Version)}
   }
  }.
