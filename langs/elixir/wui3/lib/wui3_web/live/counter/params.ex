defmodule Wui3Web.Counter.Params do
  use Ecto.Schema
  import Ecto.Changeset

  embedded_schema do
    field :count1, :integer, default: 0
    field :count2, :integer, default: 2
    field :increment, Ecto.Enum, values: [:single, :double, :triple], default: :single
  end

  def changeset(raw_params) do
    %__MODULE__{}
    |> cast(raw_params, [:count1, :count2, :increment])
    |> validate_number(:count1, greater_than_or_equal_to: 0)
    |> validate_number(:count2, greater_than_or_equal_to: 2)
  end

  def to_query_params(params = %__MODULE__{}) do
    params
    |> Map.from_struct()
    |> Map.filter(fn {key, _} -> key != :id end)
    |> Enum.into(%{}, fn {key, value} -> {Atom.to_string(key), to_string(value)} end)
    |> URI.encode_query()
  end

  def get_increment_as_num(params = %__MODULE__{}) do
    case params.increment do
      :single -> 1
      :double -> 2
      :triple -> 3
    end
  end

  def update_increment(params = %__MODULE__{}, increment) do
    params
    |> Map.from_struct()
    |> Map.update!(:increment, fn _ -> String.to_existing_atom(increment) end)
    |> changeset()
  end

  def update_counter(params = %__MODULE__{}, counter, direction) do
    increment = params |> get_increment_as_num()
    current_count = params |> Map.get(counter, 0)
    new_count = current_count + increment * direction

    params
    |> Map.from_struct()
    |> Map.put(counter, new_count)
    |> changeset()
  end

  def generate_random_params do
    %__MODULE__{
      count1: :rand.uniform(100),
      count2: :rand.uniform(100),
      increment: Enum.random([:single, :double, :triple])
    }
  end

  def generate_valid_random_params(attempts \\ 1000) do
    generate_valid_random_params(attempts, 0)
  end

  def generate_valid_random_params!(attempts \\ 1000) do
    {:ok, params} = generate_valid_random_params(attempts, 0)
    params
  end

  defp generate_valid_random_params(0, _attempts) do
    {:error, "Failed to generate valid random params"}
  end

  defp generate_valid_random_params(attempts, current_attempt) do
    changeset = generate_random_params() |> Map.from_struct() |> changeset()

    case changeset do
      %Ecto.Changeset{valid?: true} -> {:ok, changeset}
      _ -> generate_valid_random_params(attempts - 1, current_attempt + 1)
    end
  end
end
