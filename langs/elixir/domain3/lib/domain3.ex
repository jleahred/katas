defmodule Domain3 do
  @moduledoc """
  Domain3 provides macros to define lightweight structs with typed fields.

  - Injects `definition/1` and `field/2` into the module that uses it.
  - Generates the struct, default values, and helper functions at compile time.
  """

  @doc """
  Use Domain3 to define lightweight domain structs with typed fields.

  This injects `definition/1` and `field/2` into the calling module and sets up
  a before_compile hook that generates a struct, defaults and helper functions.

  Example

      iex> defmodule Example.User do
      ...>   use Domain3
      ...>   definition do
      ...>     field :name, type: :string
      ...>   end
      ...> end
      iex> Example.User.__meta_data__()
      [%{field: :name, type: :string, optional: false}]
  """
  defmacro __using__(_opts) do
    quote do
      import unquote(__MODULE__), only: [definition: 1, field: 2]
      Module.register_attribute(__MODULE__, :__meta_data__, accumulate: true)
      Module.register_attribute(__MODULE__, :__meta_fields, accumulate: true)
      @before_compile unquote(__MODULE__)
    end
  end

  @doc """
  Start a Domain3 definition block.

  Place `field/2` calls inside the `definition do ... end` block to declare the
  fields for the struct. The block is evaluated at compile time to collect
  metadata used to generate the struct and helpers.

  Example

      iex> defmodule Example.Item do
      ...>   use Domain3
      ...>   definition do
      ...>     field :id, type: :integer
      ...>     field :label, type: :string, optional: true
      ...>   end
      ...> end
      iex> Example.Item.__meta_data__() |> Enum.map(& &1.field)
      [:id, :label]
  """
  defmacro definition(do: block) do
    quote do
      # inicializa acumuladores antes de procesar campos
      @__meta_data__ []
      @__meta_fields []
      unquote(block)
    end
  end

  @doc """
  Declare a field inside a `definition` block.

  Options:
    * `:type` - one of `:string`, `:integer`, `:float`, `:date`, or `{:struct, Module}`
    * `:optional` - when true the field is optional (default is required)

  Examples

      iex> defmodule Example.Post do
      ...>   use Domain3
      ...>   definition do
      ...>     field :title, type: :string
      ...>     field :published_at, type: :date, optional: true
      ...>   end
      ...> end
      iex> Example.Post.__meta_data__() |> Enum.map(& &1.field)
      [:title, :published_at]
  """
  defmacro field(name, opts \\ []) do
    caller = __CALLER__

    # opts is usually a keyword list AST/literal; get the :type entry if present
    type = Keyword.get(opts, :type, :string)

    # If the type was specified as {:struct, Address} where Address is an alias AST,
    # expand the alias to the actual module atom at compilation time.
    type =
      case type do
        {:struct, {:__aliases__, _, _} = alias_ast} ->
          {:struct, Macro.expand(alias_ast, caller)}

        other ->
          other
      end

    supported_base_types = [:integer, :float, :string, :date]

    valid_type? =
      cond do
        type in supported_base_types -> true
        match?({:struct, mod} when is_atom(mod), type) -> true
        true -> false
      end

    unless valid_type? do
      raise ArgumentError, "unknown type #{inspect(type)} for field #{inspect(name)}"
    end

    quote bind_quoted: [name: name, opts: opts, type: type] do
      optional = !!opts[:optional]

      default =
        cond do
          optional -> nil
          type == :integer -> 0
          type == :float -> 0.0
          type == :string -> ""
          type == :date -> nil
          match?({:struct, _}, type) -> nil
          true -> nil
        end

      @__meta_data__ %{field: name, type: type, optional: optional}
      @__meta_fields {name, default}
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    # compute metadata from the target module's attributes at expansion time
    definition = Module.get_attribute(env.module, :__meta_data__) || []
    meta_maps = definition |> Enum.filter(&is_map/1) |> Enum.reverse()

    required_fields =
      meta_maps
      |> Enum.filter(fn %{optional: opt} -> not opt end)
      |> Enum.map(& &1.field)
      |> Enum.filter(&is_atom/1)

    optional_fields_with_defaults =
      meta_maps
      |> Enum.filter(fn %{optional: opt} -> opt end)
      |> Enum.map(fn %{field: f} -> {f, nil} end)

    defaults_map =
      optional_fields_with_defaults
      |> Enum.into(%{})
      |> Map.merge(Enum.into(required_fields, %{}, fn k -> {k, nil} end))

    quote do
      # inject computed lists/maps from expansion-time
      @enforce_keys unquote(Macro.escape(required_fields))
      defstruct unquote(Macro.escape(required_fields)) ++
                  unquote(Macro.escape(optional_fields_with_defaults))

      def __struct__(kv \\ %{}) do
        provided =
          cond do
            is_map(kv) -> kv
            is_list(kv) -> Enum.into(kv, %{})
            true -> kv
          end

        missing =
          Enum.filter(unquote(Macro.escape(required_fields)), fn key ->
            not Map.has_key?(provided, key)
          end)

        if missing != [] do
          raise ArgumentError,
                "the following keys must also be given when building struct #{inspect(__MODULE__)}: #{inspect(missing)}"
        end

        merged = Map.merge(unquote(Macro.escape(defaults_map)), provided)
        Map.put(merged, :__struct__, __MODULE__)
      end

      def __meta_data__ do
        @__meta_data__ |> Enum.filter(&is_map/1) |> Enum.reverse()
      end
    end
  end
end
