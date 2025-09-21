defmodule Wui6Web.AdminUserRegisterLive do
  use Wui6Web, :live_view

  alias Wui6.Accounts

  @metadata %{
    title: "Admin · registrar sesión",
    description: "Envía un enlace para crear una sesión registrada mediante email.",
    keywords: ["admin", "register", "sessions"]
  }

  def metadata, do: @metadata

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:form, to_form(%{"email" => ""}))
     |> assign(:status, :idle)
     |> assign(:last_email, nil)
     |> assign(:page_title, "Registrar sesión")
     |> assign(:current_path, ~p"/admin/user/register")}
  end

  @impl true
  def handle_event("send_link", %{"email" => email_param}, socket) do
    email = email_param |> to_string() |> String.trim()

    builder = fn request -> url(~p"/admin/register_session/#{request.token}") end

    case Accounts.send_session_registration_link(email, url_builder: builder) do
      {:ok, _request} ->
        {:noreply,
         socket
         |> assign(:form, to_form(%{"email" => ""}))
         |> assign(:status, :sent)
         |> assign(:last_email, email)
         |> put_flash(:info, "Se ha enviado un enlace de registro a #{email}.")}

      {:error, :not_found} ->
        {:noreply,
         socket
         |> assign(:form, to_form(%{"email" => email}))
         |> assign(:status, :error)
         |> put_flash(:error, "No existe un usuario con ese correo electrónico.")}

      {:error, :invalid_email} ->
        {:noreply,
         socket
         |> assign(:form, to_form(%{"email" => email}))
         |> assign(:status, :error)
         |> put_flash(:error, "Correo electrónico no válido.")}

      {:error, reason} ->
        {:noreply,
         socket
         |> assign(:form, to_form(%{"email" => email}))
         |> assign(:status, :error)
         |> put_flash(:error, "No se pudo enviar el enlace (#{inspect(reason)}).")}
    end
  end

  def handle_event("send_link", _params, socket) do
    {:noreply, socket}
  end
end
