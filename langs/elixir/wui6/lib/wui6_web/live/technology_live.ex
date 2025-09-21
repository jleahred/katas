defmodule Wui6Web.TechnologyLive do
  use Wui6Web, :live_view

  @metadata %{
    title: "Tecnología",
    description: "Tecnologías que impulsan esta aplicación",
    route: "/technology",
    keywords: ["elixir", "phoenix", "liveview", "daisyui", "tailwind"]
  }

  def metadata, do: @metadata

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:page_title, "Tecnología")
     |> assign(:technologies, technologies())}
  end

  defp technologies do
    [
      %{
        name: "Phoenix LiveView",
        link: "https://hexdocs.pm/phoenix_live_view/Phoenix.LiveView.html",
        logo: ~p"/images/logo-phoenix.png",
        description: "Interfaces reactivas sin escribir JS personalizado."
      },
      %{
        name: "Elixir",
        link: "https://elixir-lang.org/",
        logo: ~p"/images/logo-elixir.png",
        description: "Fantástico lenguaje funcional y concurrente sobre la Erlang/BEAM."
      },
      %{
        name: "Tailwind CSS",
        link: "https://tailwindcss.com/",
        logo: "https://upload.wikimedia.org/wikipedia/commons/d/d5/Tailwind_CSS_Logo.svg",
        description: "Sistema de utilidades CSS altamente personalizable."
      },
      %{
        name: "daisyUI",
        link: "https://daisyui.com/",
        logo: ~p"/images/logo-daisyui.svg",
        description: "Componentes UI sobre Tailwind listos para usar."
      },
      %{
        name: "Erlang",
        link: "https://www.erlang.org/",
        logo: ~p"/images/logo-erlang.png",
        description:
          "Lenguaje y máquina virtual creado por Ericsson que dio origen a la máquina virtual BEAM."
      },
      %{
        name: "Linux",
        link: "https://www.linuxfoundation.org/",
        logo: ~p"/images/logo-linux.png",
        description: "Kernel libre que alimenta servidores, dispositivos y contenedores."
      },
      %{
        name: "Phoenix",
        link: "https://www.phoenixframework.org/",
        logo: ~p"/images/logo-phoenix.png",
        description: "Framework web de alto rendimiento para Elixir."
      }
    ]
  end
end
