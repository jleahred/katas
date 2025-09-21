defmodule Wui6.Accounts.SessionNotifier do
  @moduledoc """
  Sends emails related to session registration tokens.
  """

  import Swoosh.Email

  alias Wui6.Accounts.SessionRequest
  alias Wui6.Mailer

  @from_address {"WUI6", "no-reply@wui6.local"}

  @spec deliver_register_link(SessionRequest.t(), String.t()) :: :ok | {:error, term()}
  def deliver_register_link(%SessionRequest{} = request, url) when is_binary(url) do
    email =
      new()
      |> to({request.email, request.email})
      |> from(@from_address)
      |> subject("Tu enlace de registro a WUI6")
      |> text_body(register_body(request, url))

    case Mailer.deliver(email) do
      {:ok, _metadata} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  def deliver_register_link(_, _), do: {:error, :invalid_email_payload}

  defp register_body(_request, url) do
    """
    Hola,

    Usa el siguiente enlace para completar el registro de sesión:
    #{url}

    Si no solicitaste este acceso, puedes ignorar este mensaje.
    """
  end
end
