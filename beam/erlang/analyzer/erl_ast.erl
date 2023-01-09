-module(erl_ast).

-export([parse_file/2]).
-export([get_includes/1]).
-export([has_epp_include_errors/1]).
-export([get_missing_includes/1]).
-export([has_parse_transforms/1]).
-export([get_parse_transforms/1]).

%% @doc Parse a String into an Erlang AST running the Preprocessor first.
parse_file(File, IncludePaths) when is_binary(File) ->
    epp:parse_file(binary:bin_to_list(File), [{includes, IncludePaths}]).

has_epp_include_errors(Forms) ->
    lists:any(
        fun
            ({error, {_Loc1, epp, {include, file, _File}}}) ->
                true;
            ({error, {_Loc1, epp, {include, lib, _File}}}) ->
                true;
            (_) ->
                false
        end,
        Forms
    ).

get_missing_includes(Forms) ->
    lists:filtermap(
        fun
            ({error, {_Loc1, epp, {include, file, File}}}) ->
                {true, binary:list_to_bin(File)};
            ({error, {_Loc1, epp, {include, lib, File}}}) ->
                {true, binary:list_to_bin(File)};
            (_) ->
                false
        end,
        Forms
    ).

has_parse_transforms(Forms) ->
    lists:any(
        fun
            ({attribute, _, compile, {parse_transform, Mod}}) -> true;
            (_) -> false
        end,
        Forms
    ).

get_parse_transforms(Forms) ->
    lists:filtermap(
        fun
            ({attribute, _, compile, {parse_transform, Mod}}) -> {true, Mod};
            (_) -> false
        end,
        Forms
    ).

get_includes(File) when is_binary(File) ->
    {ok, Forms} = epp:parse_file(binary:bin_to_list(File), []),
    lists:filtermap(
        fun
            ({error, {_Loc1, epp, {include, file, Path}}}) ->
                {true, binary:list_to_bin(Path)};
            ({error, {_Loc1, epp, {include, lib, Path}}}) ->
                {true, binary:list_to_bin(Path)};
            ({attribute, _Loc1, file, {File, _Loc2}}) ->
                case filename:extension(File) of
                    ".hrl" -> {true, binary:list_to_bin(File)};
                    _ -> false
                end;
            ({attribute, _Loc1, include, File}) ->
                {true, binary:list_to_bin(File)};
            ({attribute, _Loc1, include_lib, File}) ->
                {true, binary:list_to_bin(File)};
            (_Form) ->
                false
        end,
        Forms
    ).
