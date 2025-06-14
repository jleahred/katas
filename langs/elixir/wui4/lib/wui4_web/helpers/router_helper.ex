defmodule Wui4Web.RouterMacros do
  @doc """
  Macro simplificada que requiere que el LiveView implemente route_path/0

  IMPORTANTE: Debes usar el nombre completo del módulo para evitar problemas de expansión.

  Ejemplos:
    live2 Wui4Web.CounterLive
    live2 Wui4Web.UserProfileLive
  """
  defmacro live2(module_name) do
    quote do
      path = unquote(module_name).route_path()
      description = unquote(module_name).route_description()
      live(path, unquote(module_name))
      # Wui4Web.RouteRegistry.register_route(unquote(module_name), path, description)
      File.write!(
        "routes.jsonl",
        Jason.encode!(%{
          module: unquote(module_name) |> to_string(),
          path: path,
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

  # defmacro live2(module_name, action) when is_atom(action) do
  #   quote do
  #     path = unquote(module_name).route_path()
  #     live(path, unquote(module_name), unquote(action))
  #   end
  # end

  # defmacro live2(module_name, action, opts) do
  #   quote do
  #     path = unquote(module_name).route_path()
  #     live(path, unquote(module_name), unquote(action), unquote(opts))
  #   end
  # end
end

# defmodule Wui4Web.RouterMacros do
#   @doc """
#   Macro simplificada que requiere que el LiveView implemente route_path/0 y opcionalmente route_description/0

#   IMPORTANTE: Debes usar el nombre completo del módulo para evitar problemas de expansión.

#   Ejemplos:
#     live2 Wui4Web.CounterLive
#     live2 Wui4Web.UserProfileLive
#   """
#   defmacro live2(module_name) do
#     quote do
#       path = unquote(module_name).route_path()
#       description = get_route_description(unquote(module_name))

#       # Registrar la ruta en el registro global
#       Wui4Web.RouteRegistry.register_route(unquote(module_name), path, description)

#       live(path, unquote(module_name))
#     end
#   end

#   defmacro live2(module_name, action) when is_atom(action) do
#     quote do
#       path = unquote(module_name).route_path()
#       description = get_route_description(unquote(module_name))

#       # Registrar la ruta con acción
#       Wui4Web.RouteRegistry.register_route(
#         unquote(module_name),
#         path,
#         description,
#         unquote(action)
#       )

#       live(path, unquote(module_name), unquote(action))
#     end
#   end

#   defmacro live2(module_name, action, opts) do
#     quote do
#       path = unquote(module_name).route_path()
#       description = get_route_description(unquote(module_name))

#       # Registrar la ruta con acción y opciones
#       Wui4Web.RouteRegistry.register_route(
#         unquote(module_name),
#         path,
#         description,
#         unquote(action),
#         unquote(opts)
#       )

#       live(path, unquote(module_name), unquote(action), unquote(opts))
#     end
#   end

#   # Función auxiliar para obtener la descripción de la ruta
#   defp get_route_description(module) do
#     if function_exported?(module, :route_description, 0) do
#       module.route_description()
#     else
#       # Generar descripción automática basada en el nombre del módulo
#       module
#       |> Atom.to_string()
#       |> String.split(".")
#       |> List.last()
#       |> String.replace(~r/(Live|LiveView)$/, "")
#       |> String.replace(~r/([A-Z])/, " \\1")
#       |> String.trim()
#       |> String.downcase()
#       |> String.capitalize()
#     end
#   end
# end

# Módulo para registrar y consultar todas las rutas
defmodule Wui4Web.RouteRegistry do
  use GenServer

  @doc """
  Inicia el registro de rutas
  """
  def start_link(_opts) do
    IO.puts("Iniciando el registro de rutas...")
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @doc """
  Registra una nueva ruta
  """
  def register_route(module, path, description, action \\ :index, opts \\ []) do
    IO.puts("registrando ruta: #{path} para el módulo #{module}")

    route_info = %{
      module: module,
      path: path,
      description: description,
      action: action,
      opts: opts,
      registered_at: DateTime.utc_now()
    }

    GenServer.cast(__MODULE__, {:register_route, route_info})
  end

  @doc """
  Obtiene todas las rutas registradas
  """
  def get_all_routes do
    routes =
      GenServer.call(__MODULE__, :get_all_routes)
      |> case do
        [] ->
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

        routes ->
          routes
      end

    routes
    # GenServer.call(__MODULE__, :get_all_routes)
  end

  @doc """
  Obtiene las rutas en formato de lista para mostrar
  """
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
    # |> Enum.map(fn route ->
    #   IO.puts("Ruta encontrada: #{inspect(route)}")
    #   route
    # end)
    |> Enum.sort_by(fn r -> {-r.match_count, r.path} end)
    |> Enum.map(fn r ->
      Map.drop(r, [:match_count])
    end)
  end

  # Callbacks del GenServer
  @impl true
  def init(state) do
    {:ok, state}
  end

  @impl true
  def handle_cast({:register_route, route_info}, state) do
    # Usar el módulo como clave para evitar duplicados
    new_state = Map.put(state, route_info.module, route_info)
    {:noreply, new_state}
  end

  @impl true
  def handle_call(:get_all_routes, _from, state) do
    routes = Map.values(state)
    {:reply, routes, state}
  end

  # def load_routes_from_file() do
  #   IO.puts("Loading routes from file...")
  #   # Limpiar el registro de rutas antes de volver a registrar
  #   :ok = GenServer.cast(Wui4Web.RouteRegistry, {:clear_routes})

  #   File.stream!("routes.jsonl")
  #   |> Stream.map(&Jason.decode!/1)
  #   |> Enum.each(fn %{"module" => module_str, "path" => path, "description" => description} ->
  #     IO.puts(
  #       "Registrando ruta desde routes.jsonl: #{inspect(%{module: module_str, path: path, description: description})}"
  #     )

  #     module = Module.concat(Elixir, String.to_atom(module_str))
  #     Wui4Web.RouteRegistry.register_route(module, path, description)
  #   end)
  # end
end

# # Behaviour para LiveViews con rutas documentadas
# defmodule Wui4Web.RoutableLive do
#   @doc """
#   Behaviour para LiveViews que definen su propia ruta y descripción.
#   """
#   @callback route_path() :: String.t()
#   @callback route_description() :: String.t()

#   @optional_callbacks route_description: 0

#   @doc """
#   Macro helper para usar en tus LiveViews.
#   Define la ruta y descripción usando opciones.
#   """
#   defmacro __using__(opts) do
#     route = Keyword.get(opts, :route)
#     description = Keyword.get(opts, :description)

#     quote do
#       @behaviour Wui4Web.RoutableLive

#       unquote(
#         if route do
#           quote do
#             def route_path, do: unquote(route)
#           end
#         end
#       )

#       unquote(
#         if description do
#           quote do
#             def route_description, do: unquote(description)
#           end
#         end
#       )
#     end
#   end
# end

# LiveView para mostrar todas las rutas registradas
defmodule Wui4Web.RoutesLive do
  use Wui4Web, :live_view

  def route_path, do: "/dev/routes"
  def route_description, do: "Lista de todas las rutas registradas en la aplicación"

  def mount(_params, _session, socket) do
    routes = Wui4Web.RouteRegistry.list_routes()

    socket =
      socket
      |> assign(:routes, routes)
      |> assign(:search_query, "")
      |> assign(:filtered_routes, routes)

    {:ok, socket}
  end

  # def handle_event("search", %{"key" => _, "value" => query}, socket) do
  #   filtered_routes =
  #     if query == "" do
  #       socket.assigns.routes
  #     else
  #       Wui4Web.RouteRegistry.search_routes(query)
  #     end

  #   socket =
  #     socket
  #     |> assign(:search_query, query)
  #     |> assign(:filtered_routes, filtered_routes)

  #   {:noreply, socket}
  # end

  def handle_event("search", %{"key" => _, "value" => query}, socket) do
    filtered_routes =
      if query == "" do
        socket.assigns.routes
      else
        Wui4Web.RouteRegistry.search_routes(query)
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
      <!-- <h1 class="text-3xl font-bold mb-6">Rutas registradas</h1> -->

    <!-- Buscador -->
      <div class="mb-6">
        <input
          type="text"
          value={@search_query}
          placeholder="Buscar rutas por path o descripción..."
          phx-keyup="search"
          phx-debounce="300"
          class="w-full p-3 border rounded-lg"
        />
      </div>
      
    <!-- Contador -->
      <p class="mb-4 text-gray-600">
        Mostrando {length(@filtered_routes)} de {length(@routes)} rutas
      </p>
      
    <!-- Lista de rutas -->
      <div class="grid gap-4">
        <div :for={route <- @filtered_routes} class="border rounded-lg p-4 bg-white shadow-sm">
          <div class="flex justify-between items-start mb-2">
            <h3 class="text-lg font-semibold text-blue-600">
              <a href={route.path} class="hover:underline">{route.path}</a>
            </h3>
            <span class="text-sm text-gray-500 bg-gray-100 px-2 py-1 rounded">
              {route.action}
            </span>
          </div>

          <p class="text-gray-700 mb-2">{route.description}</p>

          <div class="text-sm text-gray-500">
            <span class="font-mono bg-gray-100 px-2 py-1 rounded">
              {inspect(route.module)}
            </span>
          </div>
        </div>
      </div>

      <div :if={length(@filtered_routes) == 0} class="text-center py-8 text-gray-500">
        No se encontraron rutas que coincidan con tu búsqueda.
      </div>
    </div>
    """
  end
end

# Ejemplos de uso en LiveViews:

# defmodule Wui4Web.CounterLive do
#   use Wui4Web, :live_view

#   def route_path, do: "/counter"
#   def route_description, do: "Contador interactivo con incremento y decremento"

#   def mount(_params, _session, socket) do
#     {:ok, assign(socket, count: 0)}
#   end

#   def render(assigns) do
#     ~H"""
#     <div class="p-6">
#       <h1 class="text-3xl font-bold mb-4">Counter: {@count}</h1>
#       <div class="space-x-4">
#         <button phx-click="increment" class="bg-blue-500 text-white px-4 py-2 rounded">+</button>
#         <button phx-click="decrement" class="bg-red-500 text-white px-4 py-2 rounded">-</button>
#       </div>
#     </div>
#     """
#   end

#   def handle_event("increment", _, socket) do
#     {:noreply, assign(socket, count: socket.assigns.count + 1)}
#   end

#   def handle_event("decrement", _, socket) do
#     {:noreply, assign(socket, count: socket.assigns.count - 1)}
#   end
# end

# defmodule Wui4Web.UserProfileLive do
#   use Wui4Web, :live_view

#   use Wui4Web.RoutableLive,
#     route: "/profile",
#     description: "Perfil de usuario con información personal"

#   def mount(_params, _session, socket) do
#     {:ok, socket}
#   end

#   def render(assigns) do
#     ~H"""
#     <div class="p-6">
#       <h1 class="text-3xl font-bold">User Profile</h1>
#       <p class="mt-4">Información del perfil de usuario</p>
#     </div>
#     """
#   end
# end

# En tu aplicación principal, añade el RouteRegistry al supervision tree:

# En application.ex
# children = [
#   # ... otros children
#   Wui4Web.RouteRegistry,
#   # ... resto de children
# ]

# En tu router:
# defmodule Wui4Web.Router do
#   use Wui4Web, :router
#   import Wui4Web.RouterMacros
#
#   scope "/", Wui4Web do
#     pipe_through :browser
#
#     live2 Wui4Web.CounterLive
#     live2 Wui4Web.UserProfileLive
#     live2 Wui4Web.RoutesLive  # Para ver todas las rutas
#   end
# end
