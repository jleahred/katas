defmodule Wui4Web.QueryTest do
  use ExUnit.Case

  # Definimos un módulo de prueba usando el macro defqueryparams
  defmodule TestQuery do
    require Wui4Web.Query

    Wui4Web.Query.defqueryparams SearchParams do
      field :page, :integer, default: 1
      field :query, :string
      field :active, :boolean, default: false
      field :limit, :integer, default: 10
    end
  end

  alias TestQuery.SearchParams

  describe "defqueryparams macro" do
    test "creates a struct with the specified fields" do
      params = %SearchParams{}
      assert Map.keys(Map.from_struct(params)) == [:active, :limit, :query, :page]
      assert params.page == nil
      assert params.query == nil
      assert params.active == nil
      assert params.limit == nil
    end
  end

  describe "from_query/1" do
    test "converts query string params to struct" do
      query_params = %{
        "page" => "5",
        "query" => "search term",
        "active" => "true",
        "limit" => "20"
      }

      params = SearchParams.from_query(query_params)

      assert params.page == 5
      assert params.query == "search term"
      assert params.active == true
      assert params.limit == 20
    end

    test "uses default values when params are missing" do
      params = SearchParams.from_query(%{})

      assert params.page == 1
      assert params.query == nil
      assert params.active == false
      assert params.limit == 10
    end

    test "handles partial params correctly" do
      params = SearchParams.from_query(%{"query" => "test"})

      # default
      assert params.page == 1
      assert params.query == "test"
      # default
      assert params.active == false
      # default
      assert params.limit == 10
    end

    test "handles boolean conversions" do
      assert SearchParams.from_query(%{"active" => "true"}).active == true
      assert SearchParams.from_query(%{"active" => "1"}).active == true
      assert SearchParams.from_query(%{"active" => "false"}).active == false
      assert SearchParams.from_query(%{"active" => "0"}).active == false
    end
  end

  describe "to_query/1" do
    test "converts struct to query string" do
      params = %SearchParams{
        page: 5,
        query: "test search",
        active: true,
        limit: 20
      }

      query_string = SearchParams.to_query(params)

      assert query_string =~ "page=5"
      assert query_string =~ "query=test+search"
      assert query_string =~ "active=true"
      assert query_string =~ "limit=20"
    end

    test "skips nil values" do
      params = %SearchParams{
        page: 5,
        query: nil,
        active: true
      }

      query_string = SearchParams.to_query(params)

      assert query_string =~ "page=5"
      assert query_string =~ "active=true"
      refute query_string =~ "query="
    end

    test "properly URL encodes special characters" do
      params = %SearchParams{query: "hello world & special chars ?"}

      query_string = SearchParams.to_query(params)

      assert query_string =~ "query=hello+world+%26+special+chars+%3F"
    end
  end

  test "round-trip conversion works correctly" do
    original_params = %{
      "page" => "5",
      "query" => "test search",
      "active" => "true",
      "limit" => "20"
    }

    struct = SearchParams.from_query(original_params)
    query_string = SearchParams.to_query(struct)

    # Decode the query string back to a map for comparison
    decoded_params = URI.decode_query(query_string)

    assert decoded_params["page"] == "5"
    assert decoded_params["query"] == "test search"
    assert decoded_params["active"] == "true"
    assert decoded_params["limit"] == "20"
  end
end
