defmodule Wui4Web.Helpers.RouterMeta do
  @moduledoc """
  Structure to store route metadata.
  """

  defstruct [
    # String
    :url,
    # String
    :description,
    # String
    :keywords
  ]

  @type t :: %__MODULE__{
          url: String.t(),
          description: String.t(),
          keywords: String.t()
        }
end
