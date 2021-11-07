defmodule JleWeb.LiveHelpers do
  import Phoenix.LiveView.Helpers

  @doc """
  Renders a component inside the `JleWeb.ModalComponent` component.

  The rendered modal receives a `:return_to` option to properly update
  the URL when the modal is closed.

  ## Examples

      <%= live_modal JleWeb.User2Live.FormComponent,
        id: @user2.id || :new,
        action: @live_action,
        user2: @user2,
        return_to: Routes.user2_index_path(@socket, :index) %>
  """
  def live_modal(component, opts) do
    path = Keyword.fetch!(opts, :return_to)
    modal_opts = [id: :modal, return_to: path, component: component, opts: opts]
    live_component(JleWeb.ModalComponent, modal_opts)
  end
end
