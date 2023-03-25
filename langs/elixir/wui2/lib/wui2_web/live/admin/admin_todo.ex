defmodule Wui2Web.AdminTodo do
  use Wui2Web, :live_view

  def mount(_params, session, socket) do
    # if connected?(socket), do: Process.send_after(self(), :tick, 1000)

    # an = socket.private.assign_new |> elem(0)
    # current_user = an.current_user.email

    {
      :ok,
      socket
      |> assign(debug: session)
    }
  end

  def render(assigns) do
    ~H"""
    <.header>
      TODO:
    </.header>
    <pre>
      * remove daisy
      * components page
      * check permisions
      * email link on develop
    </pre>

    <.header>
      Done
    </.header>
    <pre>
      * remove UI-kit
    </pre>
    """
  end
end
