defmodule Wui6Web.HomeLive do
  use Wui6Web, :live_view

  # no metadata here
  # @metadata %{title: "Route Explorer"}
  # def metadata, do: @metadata

  @impl true
  def mount(params, _session, socket) do
    routes = build_routes()
    query = params["q"] || ""
    filtered = filter_routes(routes, query)

    {:ok,
     assign(socket,
       routes_catalog: routes,
       filtered_routes: filtered,
       query: query,
       total_routes: length(routes),
       match_count: length(filtered)
     )}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    query = params["q"] || ""
    filtered = filter_routes(socket.assigns.routes_catalog, query)

    {:noreply,
     assign(socket,
       filtered_routes: filtered,
       query: query,
       match_count: length(filtered)
     )}
  end

  @impl true
  def handle_event("search", %{"q" => query}, socket) do
    destination =
      if query |> to_string() |> String.trim() == "" do
        ~p"/"
      else
        ~p"/?#{[q: query]}"
      end

    {:noreply, push_patch(socket, to: destination, replace: true)}
  end

  def handle_event("clear", _params, socket) do
    {:noreply, push_patch(socket, to: ~p"/", replace: true)}
  end

  defp build_routes do
    Wui6Web.Router
    |> Phoenix.Router.routes()
    |> Enum.map(&route_to_entry/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq_by(&{&1.verb, &1.path})
    |> Enum.sort_by(& &1.path)
  end

  defp route_to_entry(route) do
    verb = route.verb |> to_string() |> String.upcase()
    path = route.path
    helper_label = route.helper && route.helper <> "_path"
    action_label = format_action(route.plug_opts)
    {metadata_module, metadata} = extract_metadata(route)

    if metadata == %{} do
      nil
    else
      title = metadata[:title]
      keywords = metadata[:keywords]
      {description_html, searchable_description} = format_description(metadata[:description])
      overridden_path = metadata[:route]

      path = overridden_path || path
      module_for_display = metadata_module || route.plug
      plug_label = format_module(module_for_display)

      search_text =
        [
          path,
          verb,
          helper_label,
          plug_label,
          action_label,
          title,
          keywords,
          searchable_description
        ]
        |> Enum.reject(&is_nil_or_empty?/1)
        |> Enum.map(&String.downcase(to_string(&1)))
        |> Enum.join(" ")

      %{
        path: path,
        verb: verb,
        helper_label: helper_label,
        plug_label: plug_label,
        action_label: action_label,
        title: title,
        keywords: keywords,
        description: description_html,
        search_text: search_text
      }
    end
  end

  defp filter_routes(routes, query) do
    tokens =
      query
      |> to_string()
      |> String.downcase()
      |> String.split(~r/\s+/, trim: true)
      |> Enum.uniq()

    cond do
      tokens == [] ->
        Enum.map(routes, &Map.put(&1, :match_score, 0))

      true ->
        routes
        |> Enum.map(fn route ->
          match_count = Enum.count(tokens, &String.contains?(route.search_text, &1))
          {match_count, route}
        end)
        |> Enum.filter(fn {count, _route} -> count > 0 end)
        |> Enum.sort_by(fn {count, route} -> {-count, route.path} end)
        |> Enum.map(fn {count, route} -> Map.put(route, :match_score, count) end)
    end
  end

  defp format_module(nil), do: ""

  defp format_module(module) when is_atom(module) do
    module
    |> Atom.to_string()
    |> String.trim_leading("Elixir.")
    |> String.trim_leading("Wui6Web.")
  end

  defp format_module(other), do: to_string(other)

  defp format_action(nil), do: ""
  defp format_action(action) when is_atom(action), do: Atom.to_string(action)
  defp format_action(action) when is_binary(action), do: action
  defp format_action(other), do: inspect(other)

  defp extract_metadata(route) do
    metadata_map = route.metadata || %{}

    live_module =
      case metadata_map[:phoenix_live_view] do
        {module, _action, _opts, _info} when is_atom(module) -> module
        _ -> nil
      end

    candidates =
      [metadata_map[:log_module], route.plug, live_module]
      |> Enum.filter(&is_atom/1)

    Enum.reduce(candidates, {nil, %{}}, fn module, {found_module, found_metadata} ->
      cond do
        found_metadata != %{} ->
          {found_module, found_metadata}

        true ->
          case ensure_loaded_metadata(module) do
            %{} = metadata when metadata != %{} -> {module, metadata}
            _ -> {found_module, found_metadata}
          end
      end
    end)
  end

  defp ensure_loaded_metadata(module) do
    with {:module, _} <- Code.ensure_loaded(module),
         true <- function_exported?(module, :metadata, 0),
         %{} = metadata <- safe_metadata(module) do
      metadata
    else
      _ -> %{}
    end
  end

  defp safe_metadata(module) do
    case module.metadata() do
      %{} = metadata -> metadata
      _ -> %{}
    end
  rescue
    _ -> %{}
  end

  defp format_description(nil), do: {nil, nil}

  defp format_description(description) when is_binary(description) do
    lines =
      description
      |> String.split("\n")
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))

    html = Enum.join(lines, "<br/>")
    plain = Enum.join(lines, " ")

    {html, plain}
  end

  defp is_nil_or_empty?(value)
  defp is_nil_or_empty?(value) when value in [nil, ""], do: true
  defp is_nil_or_empty?(_value), do: false
end
