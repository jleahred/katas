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

      <.big_button>Send!</.big_button>
      <.big_button phx-click="go" class="ml-2">Send!</.button>
  """
  attr :admin, :boolean, default: false
  attr :href, :string, default: "#"
  attr :icon, :string, default: nil
  attr :type, :string, default: nil
  attr :class, :string, default: nil
  attr :rest, :global, include: ~w(disabled form name value)

  slot :inner_block, required: true

  def big_button(assigns) do
    admin_class = """
    " w-full
    text-center rounded-full hover:no-underline
    border border-red-600
    hover:bg-red-600 hover:text-white
    bg-transparent text-red-600
    py-5
    text-4xl font-medium"
    """

    normal_class = """
    " w-full text-center rounded-full hover:no-underline
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
          <i class={@icon}></i>
          <%= render_slot(@inner_block) %>
        </span>
      </a>
    </div>
    """
  end
end
