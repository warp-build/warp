defmodule Tricorder.Analysis.TestMatcher do
  def all, do: :all
  def match(parts) when is_list(parts), do: {:match, parts}
  def match(part), do: {:match, [part]}

  def from_parts(%{raw: []}), do: all()
  def from_parts(%{raw: raw}), do: {:match, raw}
  def from_parts(_), do: all()

  def matches?(:all, _), do: true

  def matches?({:match, raw}, v) when is_binary(v) or is_list(v) do
    String.starts_with?(v, raw) || String.ends_with?(v, raw)
  end

  def matches?(t, v) when is_atom(v), do: matches?(t, Atom.to_string(v))
  def matches?(t, {name, _}), do: matches?(t, name)
  def matches?(t, _), do: false
end
