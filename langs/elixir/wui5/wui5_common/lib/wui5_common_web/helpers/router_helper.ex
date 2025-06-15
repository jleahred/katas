defmodule Wui5CommonWeb.RouterMacros do
  defmacro scope2(path, options \\ [], do: block) do
    quote do
      @current_scope_path unquote(path)

      scope unquote(path), unquote(options) do
        unquote(block)
      end

      @current_scope_path nil
    end
  end

  @doc """
  IMPORTANTE: Debes usar el nombre completo del módulo para evitar problemas de expansión.

  Ejemplos:
    live2 Wui5CommonWeb.CounterLive
    live2 Wui5CommonWeb.UserProfileLive
  """
  defmacro live2(module_name) do
    quote do
      path = unquote(module_name).__meta__().url
      description = unquote(module_name).__meta__().description

      live(path, unquote(module_name))

      # IO.puts(
      #   "Registering route: #{path} for module #{unquote(module_name)}________________________"
      # )

      current_scope_path = if @current_scope_path == "/", do: "", else: @current_scope_path

      File.write!(
        "routes.jsonl",
        Jason.encode!(%{
          module: unquote(module_name) |> to_string(),
          path: "#{current_scope_path}#{path}",
          description: description
        }) <> "\n",
        [:append]
      )
    end
  end

  defmacro registering_routes_start() do
    IO.puts("Start registering routes...")
    File.rm("routes.jsonl")
    File.write!("routes.jsonl", "")
  end
end

defmodule Wui5CommonWeb.RouterRegistry do
  def get_all_routes do
    routes =
      if File.exists?("routes.jsonl") do
        File.stream!("routes.jsonl")
        |> Stream.map(&Jason.decode!/1)
        |> Enum.map(fn %{"module" => module_str, "path" => path, "description" => description} ->
          %{
            module: Module.concat(Elixir, String.to_atom(module_str)),
            path: path,
            description: description,
            action: :index,
            opts: [],
            registered_at: DateTime.utc_now()
          }
        end)
      else
        []
      end

    routes
  end

  def list_routes do
    get_all_routes()
    |> Enum.map(fn route ->
      %{
        path: route.path,
        description: route.description,
        module: route.module,
        action: route.action
      }
    end)
    |> Enum.sort_by(& &1.path)
  end

  @doc """
  Busca rutas por descripción o path
  """
  def search_routes(query) do
    query_words =
      query
      |> String.downcase()
      |> String.split(~r/\s+/, trim: true)
      |> Enum.uniq()

    get_all_routes()
    |> Enum.filter(fn route ->
      text = String.downcase("#{route.description} #{route.path}")
      Enum.any?(query_words, &String.contains?(text, &1))
    end)
    |> Enum.map(fn route ->
      text = String.downcase("#{route.description} #{route.path}")

      match_count =
        Enum.count(query_words, &String.contains?(text, &1))

      %{
        path: route.path,
        description: route.description,
        module: route.module,
        action: route.action,
        match_count: match_count
      }
    end)
    |> Enum.sort_by(fn r -> {-r.match_count, r.path} end)
    |> Enum.map(fn r ->
      Map.drop(r, [:match_count])
    end)
  end
end

# LiveView para mostrar todas las rutas registradas
defmodule Wui5CommonWeb.RoutesLive do
  use Wui5CommonWeb, :live_view

  def __meta__ do
    %{
      url: "/",
      description: "List all routes registered in the application",
      keywords: "list routes"
    }
  end

  def mount(_params, _session, socket) do
    routes = Wui5CommonWeb.RouterRegistry.list_routes()

    socket =
      socket
      |> assign(:routes, routes)
      |> assign(:search_query, "")
      |> assign(:filtered_routes, routes)

    {:ok, socket}
  end

  def handle_event("search", %{"key" => _, "value" => query}, socket) do
    filtered_routes =
      if query == "" do
        socket.assigns.routes
      else
        Wui5CommonWeb.RouterRegistry.search_routes(query)
      end

    socket =
      socket
      |> assign(:search_query, query)
      |> assign(:filtered_routes, filtered_routes)

    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <div class="p-6">
      <!-- Buscador -->
      <div class="mb-6">
        <input
          type="text"
          value={@search_query}
          placeholder="Search routes by description, path or keywords..."
          phx-keyup="search"
          phx-debounce="300"
          class="input input-bordered w-full"
        />
      </div>
      
    <!-- Contador -->
      <p class="mb-4 text-sm text-base-content">
        Mostrando {length(@filtered_routes)} de {length(@routes)} rutas
      </p>
      
    <!-- Lista de rutas -->
      <div class="grid gap-4">
        <div
          :for={route <- @filtered_routes}
          class="card border border-base-300 bg-base-100 shadow-sm"
        >
          <div class="card-body p-4">
            <div class="flex justify-between items-start mb-2">
              <h3 class="card-title text-primary text-lg">
                <a href={route.path} class="link link-hover">{route.path}</a>
              </h3>
              <div class="badge badge-outline badge-sm">{route.action}</div>
            </div>

            <p class="text-base-content/80 mb-2">{route.description}</p>

            <div class="text-sm text-base-content/60">
              <code class="bg-base-200 px-2 py-1 rounded">
                {inspect(route.module)}
              </code>
            </div>
          </div>
        </div>
      </div>
      
    <!-- Sin resultados -->
      <div :if={length(@filtered_routes) == 0} class="text-center py-8 text-base-content/60">
        No se encontraron rutas que coincidan con tu búsqueda.
      </div>
    </div>
    """
  end
end
