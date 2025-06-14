defmodule Wui4Web.Macros.AutoRoutes do
  @moduledoc """
  Macro para generar rutas automáticamente desde módulos con metadata
  """

  defmacro auto_live_routes(directory_path, opts \\ []) do
    # Configuración por defecto
    scope_prefix = Keyword.get(opts, :scope, "")
    module_suffix = Keyword.get(opts, :suffix, "Live")

    quote bind_quoted: [
            directory_path: directory_path,
            scope_prefix: scope_prefix,
            module_suffix: module_suffix
          ] do
      # Obtener módulos en tiempo de compilación
      modules =
        Wui4Web.Macros.AutoRoutes.find_modules_with_meta(
          directory_path,
          module_suffix
        )

      # Generar rutas para cada módulo
      for {module, meta} <- modules do
        # Escribir la información meta en routes.jsonl
        File.write!(
          "routes.jsonl",
          Jason.encode!(%{
        module: Atom.to_string(module),
        meta: meta
          }) <> "\n",
          [:append]
        )

        live(
          Path.join(scope_prefix, meta.url),
          module,
          :index,
          as: Wui4Web.Macros.AutoRoutes.route_name_from_module(module)
        )
      end
      end
    end
  end

  @doc """
  Encuentra módulos con función meta/0 en el directorio especificado
  """
  def find_modules_with_meta(directory_path, suffix \\ "Live") do
    base_path = Path.join(["lib", "wui4_web", directory_path])

    case File.exists?(base_path) do
      false ->
        IO.warn("Directorio no encontrado: #{base_path}")
        []

      true ->
        base_path
        |> Path.join("**/*.ex")
        |> Path.wildcard()
        |> Enum.map(&extract_module_info(&1, suffix))
        |> Enum.filter(& &1)
    end
  end

  defp extract_module_info(file_path, suffix) do
    try do
      # Leer el archivo y extraer el nombre del módulo
      content = File.read!(file_path)

      case extract_module_name(content, suffix) do
        nil ->
          nil

        module_name ->
          # Intentar cargar el módulo y obtener su metadata
          module = String.to_existing_atom("Elixir.#{module_name}")

          case Code.ensure_loaded(module) do
            {:module, ^module} ->
              if function_exported?(module, :meta, 0) do
                meta = apply(module, :meta, [])
                {module, meta}
              else
                nil
              end

            {:error, _} ->
              nil
          end
      end
    rescue
      _ -> nil
    end
  end

  defp extract_module_name(content, suffix) do
    # Regex para encontrar el nombre del módulo
    case Regex.run(~r/defmodule\s+([A-Za-z0-9_.]+#{suffix})\s+do/, content) do
      [_, module_name] -> module_name
      _ -> nil
    end
  end

  @doc """
  Genera un nombre de ruta basado en el nombre del módulo
  """
  def route_name_from_module(module) do
    module
    |> Module.split()
    |> List.last()
    |> Macro.underscore()
    |> String.replace("_live", "")
    |> String.to_atom()
  end
end
