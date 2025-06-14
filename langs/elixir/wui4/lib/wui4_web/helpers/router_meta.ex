defmodule Wui4Web.Helpers.RouterMeta do
  @enforce_keys [:url, :description]
  defstruct [:url, :description, :keywords]
end
