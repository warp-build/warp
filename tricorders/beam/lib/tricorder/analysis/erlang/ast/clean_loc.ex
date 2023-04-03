defmodule Tricorder.Analysis.Erlang.Ast.CleanLoc do
  require Logger

  def clean(ast) when is_list(ast), do: Enum.map(ast, &clean/1)

  def clean({:function, _loc, name, arity, body}) do
    {:function, 0, name, arity, clean(body)}
  end

  def clean({:function, mod, fun, arity}) do
    {:function, clean(mod), clean(fun), clean(arity)}
  end

  def clean({:attribute, _loc, :file, {path, number}}) do
    {:attribute, 0, :file, {clean_path(path), number}}
  end

  def clean({:attribute, _loc, name, body}) do
    {:attribute, 0, name, body}
  end

  def clean({:case, _loc, expr, branches}) do
    {:case, 0, clean(expr), clean(branches)}
  end

  def clean({:call, _loc, fun, args}) do
    {:call, 0, clean(fun), clean(args)}
  end

  def clean({:remote, _loc, mod, fun}) do
    {:remote, 0, clean(mod), clean(fun)}
  end

  def clean({:match, _loc, pat, expr}) do
    {:match, 0, clean(pat), clean(expr)}
  end

  def clean({:cons, _loc, head, tail}) do
    {:cons, 0, clean(head), clean(tail)}
  end

  def clean({:try, _loc, expr, ok, branches, rest}) do
    {:try, 0, clean(expr), clean(ok), clean(branches), clean(rest)}
  end

  def clean({:clause, _loc, pat, guards, body}) do
    {:clause, 0, clean(pat), clean(guards), clean(body)}
  end

  def clean({:tuple, _loc, parts}) do
    {:tuple, 0, clean(parts)}
  end

  def clean({:if, _loc, branches}) do
    {:if, 0, clean(branches)}
  end

  def clean({:op, _loc, name, left, right}) do
    {:op, 0, name, clean(left), clean(right)}
  end

  def clean({:op, _loc, name, left}) do
    {:op, 0, name, clean(left)}
  end

  def clean({:fun, _loc, {:clauses, clauses}}) do
    {:fun, 0, {:clauses, clean(clauses)}}
  end

  def clean({:fun, _loc, expr}) do
    {:fun, 0, clean(expr)}
  end

  def clean({:receive, _loc, clauses}) do
    {:receive, 0, clean(clauses)}
  end

  def clean({:receive, _loc, clauses, timeout, branches}) do
    {:receive, 0, clean(clauses), clean(timeout), clean(branches)}
  end

  def clean({:record, _loc, name, fields}) do
    {:record, 0, name, clean(fields)}
  end

  def clean({:record, _loc, expr, name, fields}) do
    {:record, 0, clean(expr), name, clean(fields)}
  end

  def clean({:record_field, _loc, name, value}) do
    {:record_field, 0, clean(name), clean(value)}
  end

  def clean({:record_field, _loc, expr, k, v}) do
    {:record_field, 0, clean(expr), k, clean(v)}
  end

  def clean({:block, _loc, exprs}) do
    {:block, 0, clean(exprs)}
  end

  def clean({:map, _loc, fields}) do
    {:map, 0, clean(fields)}
  end

  def clean({:map, _loc, expr, fields}) do
    {:map, 0, clean(expr), clean(fields)}
  end

  def clean({:map_field_assoc, _loc, k, v}) do
    {:map_field_assoc, 0, clean(k), clean(v)}
  end

  def clean({:map_field_exact, _loc, k, v}) do
    {:map_field_exact, 0, clean(k), clean(v)}
  end

  def clean({:lc, _loc, expr, gens}) do
    {:lc, 0, clean(expr), clean(gens)}
  end

  def clean({:generate, _loc, left, right}) do
    {:generate, 0, clean(left), clean(right)}
  end

  def clean({:error, {_loc, err, args}}) do
    {:error, {0, err, args}}
  end

  def clean({:bin, _loc, parts}) do
    {:bin, 0, clean(parts)}
  end

  def clean({:bin_element, _loc, expr, mode, opts}) do
    {:bin_element, _loc, clean(expr), mode, opts}
  end

  def clean({:catch, _loc, expr}) do
    {:catch, 0, clean(expr)}
  end

  def clean({:eof, _}), do: {:eof, 0}

  def clean({:integer, _loc, int}), do: {:integer, 0, int}
  def clean({:var, _loc, name}), do: {:var, 0, name}
  def clean({:char, _loc, char}), do: {:char, 0, char}
  def clean({:atom, _loc, name}), do: {:atom, 0, name}
  def clean({nil, _loc}), do: {nil, 0}
  def clean({:string, _loc, str}), do: {:string, 0, str}
  def clean({:function, name, arity}), do: {:function, name, arity}

  def clean_path(path) do
    bin_path = :binary.list_to_bin(path)

    if Tricorder.Deps.is_standard_header?(bin_path) do
      Path.split(bin_path)
      |> Enum.reverse()
      |> Enum.take(6)
      |> Enum.reverse()
      |> Path.join()
      |> :binary.bin_to_list()
    else
      path
    end
  end
end
