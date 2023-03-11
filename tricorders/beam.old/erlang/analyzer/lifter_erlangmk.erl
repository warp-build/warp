-module(lifter_erlangmk).

-export([from_file/1]).

from_file(Path) ->
    {ok, Data} = file:read_file(Path),
    Lines = string:split(Data, <<"\n">>, all),
    Deps = parse(Lines, []),
    {ok, [{deps, Deps}]}.

parse([], Acc) ->
    Acc;
parse([Line | Lines], Acc) ->
    Tokens = [binary:list_to_bin(T) || T <- string:tokens(binary:bin_to_list(Line), " ")],
    Acc2 = parse_line(Tokens, Acc),
    parse(Lines, Acc2).

% NOTE(@ostera): since we are relying on rebar3 to flatten dependencies, we can't build ci.erlang.mk
parse_line([<<"dep_ci.erlang.mk">> | _], Acc) ->
    Acc;
parse_line([<<"dep_", Name/binary>>, <<"=">>, <<"git">>, Repo, Version], Acc) ->
    Dep = {erlang:binary_to_atom(Name, utf8), {git, Repo, {tag, Version}}},
    [Dep | Acc];
parse_line(_, Acc) ->
    Acc.
