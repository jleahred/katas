defmodule Wui2Web.AdminUsersParams do
  defmodule Params do
    defstruct [
      # string
      :email,
      # int or nil
      :role_id
    ]
  end

  def param2int(s) do
    case Integer.parse(s) do
      {v, ""} -> v
      {_, _} -> nil
      :error -> nil
    end
  end

  def normalize_params(params) do
    params =
      params
      |> Enum.map(fn {k, v} ->
        case k do
          "email" ->
            {:email, v}

          "role_id" ->
            {:role_id, param2int(v)}

          _ ->
            nil
        end
      end)
      |> Enum.filter(&(&1 != nil))
      |> Enum.reduce(%{}, fn {k, v}, acc -> acc |> Map.put(k, v) end)

    struct(Params, params)
  end

  def merge_params(old, new) do
    s =
      new
      |> Map.from_struct()
      |> Enum.reduce(old |> Map.from_struct(), fn {k, v}, acc ->
        acc |> Map.put(k, v)
      end)

    struct(Params, s)
  end

  def params_to_url(params) do
    "?" <>
      (params
       |> Map.from_struct()
       |> URI.encode_query())
  end
end
