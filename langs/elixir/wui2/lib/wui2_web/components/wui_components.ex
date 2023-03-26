defmodule Wui2Web.WUIComponents do
  @moduledoc """
  Provides WUI UI components.

  The components in this module use Tailwind CSS, a utility-first CSS framework.
  See the [Tailwind CSS documentation](https://tailwindcss.com) to learn how to
  customize the generated components in this module.

  Icons are provided by [heroicons](https://heroicons.com), using the
  [heroicons_elixir](https://github.com/mveytsman/heroicons_elixir) project.
  """
  use Phoenix.Component

  # alias Phoenix.LiveView.JS
  # import Wui2Web.Gettext

  @doc """
  Renders a big button.

  ## Examples

      <.wbig_button>Send!</.wbig_button>
      <.wbig_button phx-click="go" class="ml-2">Send!</.button>
  """
  attr :admin, :boolean, default: false
  attr :href, :string, default: "#"
  # attr :icon, :string, default: nil
  attr :icon2, :any, default: nil
  attr :type, :string, default: nil
  attr :class, :string, default: nil
  attr :rest, :global, include: ~w(disabled form name value)

  slot :icon, required: false
  slot :inner_block, required: true

  def wbig_button(assigns) do
    admin_class = """
    " w-full
    px-10 rounded-3xl hover:no-underline
    border border-red-600
    hover:bg-red-600 hover:text-white
    bg-transparent text-red-600
    py-5
    text-4xl font-medium"
    """

    normal_class = """
    " w-full
    px-10 rounded-3xl hover:no-underline
    border border-indigo-600
    hover:bg-indigo-600 hover:text-white
    bg-transparent text-indigo-600
    py-5
    text-4xl font-medium"
    """

    assigns =
      assigns
      |> Map.put(:admin_class, admin_class)
      |> Map.put(:normal_class, normal_class)

    ~H"""
    <div class="flex py-2">
      <a type={@type} class={if @admin, do: @admin_class, else: @normal_class} href={@href}>
        <span {@rest}>
          <div class="flex">
            <span class="px-5 w-1/6">
              <%= if @icon != [],
                do: render_slot(@icon),
                else: "" %>
            </span>
            <div class="px-10">
              <%= render_slot(@inner_block) %>
            </div>
          </div>
        </span>
      </a>
    </div>
    """
  end

  @doc ~S"""
  Renders a table with generic styling.

  ## Examples

      <.table id="users" rows={@users}>
        <:col :let={user} label="id"><%= user.id %></:col>
        <:col :let={user} label="username"><%= user.username %></:col>
      </.table>
  """
  attr :id, :string, required: true
  attr :row_click, :any, default: nil
  attr :rows, :list, required: true

  slot :col, required: true do
    attr :label, :string
  end

  slot :action, doc: "the slot for showing user actions in the last table column"

  def wtable(assigns) do
    ~H"""
    <div id={@id} class="overflow-y-auto px-4 sm:overflow-visible sm:px-0">
      <table class="mt-11 w-[40rem] sm:w-full">
        <thead class="text-left text-[0.8125rem] leading-6 text-zinc-500">
          <tr>
            <th :for={col <- @col} class="p-0 pb-4 pr-6 "><%= col[:label] %></th>
            <th class="relative p-0 pb-4"><span class="sr-only"><%= "Actions" %></span></th>
          </tr>
        </thead>
        <tbody class="relative divide-y divide-zinc-100 border-t border-zinc-200 text-sm leading-6 text-zinc-700">
          <tr
            :for={{row, rindex} <- Enum.with_index(@rows)}
            id={"#{@id}-#{rindex}"}
            class="relative group hover:bg-zinc-50"
          >
            <td
              :for={{col, i} <- Enum.with_index(@col)}
              phx-click={@row_click && @row_click.(row)}
              class={["p-0", @row_click && "hover:cursor-pointer"]}
            >
              <div :if={i == 0}>
                <span class="absolute h-full w-4 top-0 -left-4 group-hover:bg-zinc-50 sm:rounded-l-xl" />
                <span class="absolute h-full w-4 top-0 -right-4 group-hover:bg-zinc-50 sm:rounded-r-xl" />
              </div>
              <div class="block py-4 pr-6">
                <span class={["relative", i == 0 && "font-semibold text-zinc-900"]}>
                  <%= render_slot(col, row) %>
                </span>
              </div>
            </td>
            <td :if={@action != []} class="p-0 w-14">
              <div class="relative whitespace-nowrap py-4 text-right text-sm font-medium">
                <span
                  :for={action <- @action}
                  class="relative ml-0 font-semibold leading-6 text-zinc-900 hover:text-zinc-700"
                >
                  <%= render_slot(action, row) %>
                </span>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
    </div>
    """
  end

  @doc """
  Renders a header with title.
  """
  attr(:class, :string, default: nil)

  slot(:inner_block, required: true)
  slot(:subtitle)
  slot(:actions)

  def wheader(assigns) do
    ~H"""
    <header class={[@actions != [] && "flex items-center justify-between gap-6", @class]}>
      <div>
        <h1 class="text-2xl font-semibold leading-8 text-zinc-800">
          <%= render_slot(@inner_block) %>
        </h1>
        <p :if={@subtitle != []} class="mt-2 text-sm leading-6 text-zinc-600">
          <%= render_slot(@subtitle) %>
        </p>
      </div>
      <div class="flex-none"><%= render_slot(@actions) %></div>
    </header>
    """
  end
end
