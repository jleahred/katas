defmodule Wui5CommonWeb.Helpers.RouterMeta do
  @enforce_keys [:url, :description, :keywords, :grants]
  defstruct [:url, :description, :keywords, :grants]
end
