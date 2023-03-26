defmodule Wui2Web.TestWuiComponents do
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
    <div class="pt-10">
      <div class="rounded-xl bg-transparent _ring mx-auto max-w-2xl py-6">
        <a class="btn" style="text-transform: none">
          Buttonaaa
        </a>
        <button class="btn btn-primary" style="text-transform: none">Button</button>
        <button class="btn btn-secondary">Button</button>
        <button class="btn btn-accent">Button</button>
        <button class="btn btn-ghost">Button</button>
        <button class="btn btn-link">Button</button>

        <.wbig_button admin={true} href={~p"/admin"} icon="fa-solid fa-user-doctor">
          Admin
        </.wbig_button>

        <.wbig_button href={~p"/test/wui_components"}>
          Test WUI components
        </.wbig_button>

        <.wbig_button href="/download">
          <Heroicons.academic_cap class="w-10 h-10 inline" /> Download
        </.wbig_button>

        <.wbig_button href="/download">
          Download
        </.wbig_button>

        <.wbig_button href="/download">
          Download
        </.wbig_button>

        <.wbig_button href="/download">
          Download
        </.wbig_button>

        <.wbig_button href="/download">
          Download
        </.wbig_button>

        <.wbig_button href="/download">
          Download
        </.wbig_button>
      </div>
    </div>
    """
  end
end
