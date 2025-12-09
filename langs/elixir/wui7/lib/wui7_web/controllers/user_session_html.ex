defmodule Wui7Web.UserSessionHTML do
  use Wui7Web, :html

  embed_templates "user_session_html/*"

  defp local_mail_adapter? do
    Application.get_env(:wui7, Wui7.Mailer)[:adapter] == Swoosh.Adapters.Local
  end
end
