defmodule Wui6Web.RegisterSessionController do
  use Wui6Web, :controller

  alias Wui6.Accounts
  alias Wui6Web.RequestContext

  def show(conn, %{"token" => token}) do
    snapshot = RequestContext.snapshot(conn)

    case Accounts.finalize_session_registration(token, snapshot) do
      {:ok, session} ->
        conn
        |> put_resp_cookie("wui6_token", session.token, cookie_opts(conn))
        |> put_resp_header("cross-origin-resource-policy", "same-origin")
        |> put_resp_header("cross-origin-opener-policy", "same-origin")
        |> put_flash(:info, "Sesión registrada para #{session.user.email}.")
        |> redirect(to: ~p"/admin/sessions")

      {:error, :not_found} ->
        conn
        |> put_flash(:error, "El token no es válido o ya fue utilizado.")
        |> redirect(to: ~p"/admin/user/register")

      {:error, {:error, changeset}} ->
        conn
        |> put_flash(:error, "No se pudo crear la sesión (#{changeset_error(changeset)}).")
        |> redirect(to: ~p"/admin/user/register")

      {:error, reason} ->
        conn
        |> put_flash(:error, "No se pudo crear la sesión (#{inspect(reason)}).")
        |> redirect(to: ~p"/admin/user/register")
    end
  end

  defp cookie_opts(conn) do
    [
      sign: true,
      http_only: true,
      same_site: "Strict",
      secure: conn.scheme == :https,
      max_age: 60 * 60 * 24 * 30
    ]
  end

  defp changeset_error(changeset) do
    changeset
    |> Ecto.Changeset.traverse_errors(fn {msg, _opts} -> msg end)
    |> Enum.map(fn {field, messages} -> "#{field}: #{Enum.join(List.wrap(messages), ", ")}" end)
    |> Enum.join("; ")
  end
end
