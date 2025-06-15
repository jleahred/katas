defmodule Wui4Web.Query do
  defmacro defqueryparams(name, do: block) do
    quote do
      defmodule unquote(name) do
        import Wui4Web.Query.FieldBuilder
        Module.register_attribute(__MODULE__, :fields, accumulate: true)
        unquote(block)
        defstruct Enum.map(@fields, fn {k, _v} -> k end)

        def from_query_params(params) do
          Enum.reduce(@fields, %__MODULE__{}, fn {key, opts}, acc ->
            val =
              case Map.get(params, to_string(key)) do
                nil -> Keyword.get(opts, :default)
                v -> cast(v, Keyword.get(opts, :type))
              end

            Map.put(acc, key, val)
          end)
        end

        def to_query(struct) do
          struct
          |> Map.from_struct()
          |> Enum.filter(fn {_k, v} -> not is_nil(v) end)

          # |> Enum.map(fn {k, v} -> "#{k}=#{URI.encode_www_form(to_string(v))}" end)
          # |> Enum.join("&")
          # |> Enum.into(%{})
        end

        def to_query_string(struct) do
          struct
          |> Map.from_struct()
          |> Enum.filter(fn {_k, v} -> not is_nil(v) end)
          |> Enum.map(fn {k, v} -> "#{k}=#{URI.encode_www_form(to_string(v))}" end)
          |> Enum.join("&")

          # |> Enum.into(%{})
        end

        defp cast(value, :integer), do: String.to_integer(value)
        defp cast(value, :boolean), do: value in ["true", "1"]
        defp cast(value, :string), do: value
        defp cast(value, _), do: value

        # defimpl Phoenix.Param, for: __MODULE__ do
        #   def to_param(struct) do
        #     struct
        #     |> Map.from_struct()
        #     |> Enum.filter(fn {_k, v} -> not is_nil(v) end)
        #     |> Enum.map(fn {k, v} -> "#{k}=#{URI.encode_www_form(to_string(v))}" end)
        #     |> Enum.join("&")
        #   end
        # end
      end
    end
  end
end

defmodule Wui4Web.Query.FieldBuilder do
  defmacro field(name, type, opts \\ []) do
    quote do
      @fields {unquote(name), Keyword.put(unquote(opts), :type, unquote(type))}
    end
  end
end

# defmodule Wui4Web.Query do
#   defmacro defquery(name, do: block) do
#     quote do
#       defmodule unquote(name) do
#         import Wui4Web.Query.FieldBuilder
#         Module.register_attribute(__MODULE__, :fields, accumulate: true)
#         unquote(block)
#         defstruct Enum.map(@fields, fn {k, _v} -> k end)

#         def from_query(params) do
#           Enum.reduce(@fields, %__MODULE__{}, fn {key, opts}, acc ->
#             val =
#               case Map.get(params, to_string(key)) do
#                 nil -> Keyword.get(opts, :default)
#                 v -> cast(v, Keyword.get(opts, :type))
#               end

#             Map.put(acc, key, val)
#           end)
#         end

#         def to_query(struct) do
#           struct
#           |> Map.from_struct()
#           |> Enum.filter(fn {_k, v} -> not is_nil(v) end)
#           |> Enum.map(fn {k, v} -> "#{k}=#{URI.encode_www_form(to_string(v))}" end)
#           |> Enum.join("&")
#         end

#         defp cast(value, :integer), do: String.to_integer(value)
#         defp cast(value, :boolean), do: value in ["true", "1"]
#         defp cast(value, :string), do: value
#         defp cast(value, _), do: value
#       end
#     end
#   end
# end

# defmodule Wui4Web.Query.FieldBuilder do
#   defmacro field(name, type, opts \\ []) do
#     quote do
#       @fields {unquote(name), Keyword.put(unquote(opts), :type, unquote(type))}
#     end
#   end
# end
