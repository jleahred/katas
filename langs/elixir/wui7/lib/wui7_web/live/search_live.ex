defmodule Wui7Web.SearchLive do
  use Wui7Web, :live_view

  @impl true
  def mount(_params, _session, socket) do
    pages = load_pages()
    query = ""

    {:ok,
     socket
     |> assign(:pages, pages)
     |> assign(:query, query)
     |> assign(:results, rank_pages(pages, query))
     |> assign(:search_form, to_form(%{"query" => query}, as: :search))}
  end

  @impl true
  def handle_event("search", %{"search" => %{"query" => query}}, socket) do
    {:noreply,
     socket
     |> assign(:query, query)
     |> assign(:results, rank_pages(socket.assigns.pages, query))
     |> assign(:search_form, to_form(%{"query" => query}, as: :search))}
  end

  defp load_pages do
    live_routes()
    |> Enum.flat_map(fn %{module: module, path: path} ->
      with {:module, _} <- Code.ensure_loaded(module),
           true <- function_exported?(module, :meta_info, 0) do
        info = module.meta_info()

        [
          %{
            title: info.title,
            description: info.description,
            keywords: List.wrap(info.keywords),
            path: path,
            search_fields: build_search_fields(info)
          }
        ]
      else
        _ -> []
      end
    end)
  end

  defp live_routes do
    Phoenix.Router.routes(Wui7Web.Router)
    |> Enum.flat_map(fn
      %{
        plug: Phoenix.LiveView.Plug,
        path: path,
        metadata: %{phoenix_live_view: {module, _, _, _}}
      } ->
        [%{module: module, path: path}]

      _ ->
        []
    end)
  end

  defp build_search_fields(info) do
    [info.title, info.description]
    |> Enum.concat(List.wrap(info.keywords))
    |> Enum.reject(&is_nil/1)
    |> Enum.map(&String.downcase/1)
  end

  defp rank_pages(pages, query) do
    tokens = tokenize(query)

    pages
    |> Enum.map(fn page ->
      matches = match_count(page, tokens)
      Map.put(page, :matches, matches)
    end)
    |> Enum.filter(fn page -> tokens == [] || page.matches > 0 end)
    |> Enum.sort_by(fn page -> {-page.matches, page.title} end)
  end

  defp tokenize(query) do
    query
    |> String.downcase()
    |> String.split(~r/\s+/, trim: true)
  end

  defp match_count(_page, []), do: 0

  defp match_count(page, tokens) do
    Enum.reduce(tokens, 0, fn token, acc ->
      if Enum.any?(page.search_fields, &String.contains?(&1, token)) do
        acc + 1
      else
        acc
      end
    end)
  end

  defp result_dom_id(path) do
    path
    |> to_string()
    |> String.trim_leading("/")
    |> case do
      "" -> "result-home"
      other -> "result-" <> String.replace(other, "/", "-")
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <section class="space-y-8 animate-fade-in">
        <header class="space-y-4 text-center">
          <p class="badge badge-info badge-outline px-4 py-3 text-xs uppercase tracking-[0.3em]">
            Buscador
          </p>
        </header>

        <.form
          for={@search_form}
          id="search-form"
          class="max-w-2xl mx-auto"
          phx-change="search"
          phx-submit="search"
        >
          <.input
            field={@search_form[:query]}
            type="search"
            placeholder="Escribe para buscar..."
            phx-debounce="300"
            class="input input-bordered input-lg w-full"
          />
        </.form>

        <div class="text-sm text-base-content/70 text-center">
          Mostrando {length(@results)} {if length(@results) == 1, do: "resultado", else: "resultados"}
        </div>

        <div id="results-list" class="space-y-4">
          <div :if={@results == []} class="alert alert-warning">
            <.icon name="hero-magnifying-glass" class="w-6 h-6" />
            <div>
              <h3 class="font-semibold">Sin coincidencias</h3>
              <p class="text-sm">
                Intenta con otras palabras clave o borra la búsqueda para ver todas las páginas.
              </p>
            </div>
          </div>

          <.link
            :for={result <- @results}
            id={result_dom_id(result.path)}
            navigate={result.path}
            class="block rounded-2xl focus:outline-none focus-visible:ring-2 focus-visible:ring-primary group"
          >
            <div class="card border border-base-300/80 shadow-sm transition cursor-pointer group-hover:border-primary group-hover:bg-primary/10 group-hover:shadow-xl">
              <div class="card-body space-y-3">
                <div class="flex flex-col gap-3 sm:flex-row sm:items-center sm:justify-between">
                  <div>
                    <h2 class="text-2xl font-semibold transition group-hover:text-primary">
                      {result.title}
                    </h2>
                    <p class="text-base-content/70">{result.description}</p>
                  </div>
                </div>
                <div class="flex flex-wrap gap-2">
                  <span :for={keyword <- result.keywords} class="badge badge-ghost">{keyword}</span>
                </div>
              </div>
            </div>
          </.link>
        </div>
      </section>
    </Layouts.app>
    """
  end
end
