defmodule Wui6Web.Layouts do
  @moduledoc """
  This module holds different layouts used by your application.

  See the `layouts` directory for all templates available.
  The "root" layout is a skeleton rendered as part of the
  application router. The "app" layout is rendered as component
  in regular views and live views.
  """
  use Wui6Web, :html

  embed_templates "layouts/*"

  @doc """
  Renders the app layout

  ## Examples

      <Layouts.app flash={@flash}>
        <h1>Content</h1>
      </Layout.app>

  """
  attr :flash, :map, required: true, doc: "the map of flash messages"

  attr :page_title, :string,
    default: nil,
    doc: "optional page title rendered beside the logo when present"

  attr :current_scope, :map,
    default: nil,
    doc: "the current [scope](https://hexdocs.pm/phoenix/scopes.html)"

  attr :container_class, :string,
    default: "mx-auto max-w-2xl space-y-4",
    doc: "CSS classes applied to the main content container"

  attr :return_to, :string,
    default: nil,
    doc:
      "optional navigation target used to render a contextual Return button next to the page title"

  attr :current_user_email, :string,
    default: nil,
    doc: "email of the current session token holder to show in the navigation bar"

  attr :rest, :global,
    include: ~w(phx-window-keydown phx-key phx-keydown phx-click phx-target phx-hook),
    doc: "additional HTML attributes applied to the main container"

  slot :inner_block, required: true

  def app(assigns) do
    assigns = assign(assigns, :app_version, Application.spec(:wui6, :vsn) |> to_string())

    ~H"""
    <header class="navbar px-4 sm:px-6 lg:px-8">
      <div class="grid w-full grid-cols-[auto_1fr_auto] items-center gap-3">
        <a href="/" class="flex items-center gap-2">
          <img src={~p"/images/logo.svg"} width="36" />
          <span class="text-sm font-semibold">WUI6 v{@app_version}</span>
        </a>
        <div class="flex items-center justify-center gap-3">
          <span
            :if={@page_title}
            class="truncate text-center text-sm font-semibold text-base-content/80 sm:text-lg"
          >
            {@page_title}
          </span>

          <.link
            :if={@return_to}
            navigate={@return_to}
            class="btn btn-xs sm:btn-sm btn-ghost text-base-content/70 hover:text-base-content flex items-center gap-1"
          >
            <.icon name="hero-arrow-left" class="size-4" />
            <span>Go back</span>
          </.link>
        </div>
        <div class="justify-self-end px-1 flex items-center gap-3">
          <.link
            :if={is_nil(@current_user_email)}
            navigate={~p"/admin/user/register"}
            class="btn btn-ghost btn-sm"
          >
            Register
          </.link>

          <div :if={@current_user_email} class="flex items-center gap-2">
            <.link navigate={~p"/user"} class="btn btn-ghost btn-sm font-mono">
              {@current_user_email}
            </.link>

            <.link href={~p"/logout"} class="btn btn-outline btn-sm">
              Logout
            </.link>
          </div>

          <.theme_toggle />
        </div>
      </div>
    </header>

    <main class="px-4 py-4 sm:px-6 lg:px-8" {@rest}>
      <div class={@container_class}>
        {render_slot(@inner_block)}
      </div>
    </main>

    <.flash_group flash={@flash} />
    """
  end

  @doc """
  Shows the flash group with standard titles and content.

  ## Examples

      <.flash_group flash={@flash} />
  """
  attr :flash, :map, required: true, doc: "the map of flash messages"
  attr :id, :string, default: "flash-group", doc: "the optional id of flash container"

  def flash_group(assigns) do
    ~H"""
    <div id={@id} aria-live="polite">
      <.flash kind={:info} flash={@flash} />
      <.flash kind={:error} flash={@flash} />

      <.flash
        id="client-error"
        kind={:error}
        title={gettext("We can't find the internet")}
        phx-disconnected={show(".phx-client-error #client-error") |> JS.remove_attribute("hidden")}
        phx-connected={hide("#client-error") |> JS.set_attribute({"hidden", ""})}
        hidden
      >
        {gettext("Attempting to reconnect")}
        <.icon name="hero-arrow-path" class="ml-1 size-3 motion-safe:animate-spin" />
      </.flash>

      <.flash
        id="server-error"
        kind={:error}
        title={gettext("Something went wrong!")}
        phx-disconnected={show(".phx-server-error #server-error") |> JS.remove_attribute("hidden")}
        phx-connected={hide("#server-error") |> JS.set_attribute({"hidden", ""})}
        hidden
      >
        {gettext("Attempting to reconnect")}
        <.icon name="hero-arrow-path" class="ml-1 size-3 motion-safe:animate-spin" />
      </.flash>
    </div>
    """
  end

  @doc """
  Provides dark vs light theme toggle based on themes defined in app.css.

  See <head> in root.html.heex which applies the theme before page load.
  """
  def theme_toggle(assigns) do
    ~H"""
    <div class="card relative flex flex-row items-center border-2 border-base-300 bg-base-300 rounded-full">
      <div class="absolute w-1/3 h-full rounded-full border-1 border-base-200 bg-base-100 brightness-200 left-0 [[data-theme=light]_&]:left-1/3 [[data-theme=dark]_&]:left-2/3 transition-[left]" />

      <button
        phx-click={JS.dispatch("phx:set-theme", detail: %{theme: "system"})}
        class="flex p-2 cursor-pointer w-1/3"
      >
        <.icon name="hero-computer-desktop-micro" class="size-4 opacity-75 hover:opacity-100" />
      </button>

      <button
        phx-click={JS.dispatch("phx:set-theme", detail: %{theme: "light"})}
        class="flex p-2 cursor-pointer w-1/3"
      >
        <.icon name="hero-sun-micro" class="size-4 opacity-75 hover:opacity-100" />
      </button>

      <button
        phx-click={JS.dispatch("phx:set-theme", detail: %{theme: "dark"})}
        class="flex p-2 cursor-pointer w-1/3"
      >
        <.icon name="hero-moon-micro" class="size-4 opacity-75 hover:opacity-100" />
      </button>
    </div>
    """
  end
end
