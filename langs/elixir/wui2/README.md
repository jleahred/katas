# Wui

This is a template of a base Web User Interface application

It's modular


### Recomendable

* Extensible
* Fácil despliegue
* Creación fácil de entornos de desarrollo
* UI interactiva

## Framework

* Phoenix
* Es un potente framework web
* Facilita desarrollo rápido, incluso para webs interactivas

## Despliegue

* El despliegue para desarrollo de la aplicación, consistirá en bajar el repositorio y ejecutar dos comandos (tanto en linux, windows o macos)
* El despliegue en producción se entregará con un zip autocontenido
  * Podrá ejecutarse como servicio (daemon)
  * Aunque se puede desplegar para windows y macos, se realizará para linux

## Bases de datos

* En producción se utilizará mysql o posgresql
* Para desarrollo, por defecto se configurará sqlite
  * Esto permitirá un arranque de un entorno de desarrollo rápido y sencillo (mínimas dependencias y configuraciones)
  * Cada uno podrá poner la bbdd que desee
* Soporte de migraciones con rollback
* Se podrá hacer un dump de producción para pasar a simulación (independientemente de que los motores de base de datos sean el mismo o no)

## Usuarios

* Los usuarios podrán regitrarse ellos mismos
  * Esto será fácil de desconectar si no se desea
* El registro se hará introduciendo un correo electrónico y contraseña
* Las contraseñas estarán con un cifrado no reversible utilizando una clave privada (generada en el despliegue)
* Tras registrar el usuario, se enviará un correo electrónico al usuario con un enlace para su activación
* Además de la activación, un administrador deberá desbloquear al usuario
* En un futuro se podría introducir contraseñas de un solo uso con configuración en google-authenticator o microsoft-authenticar con código QR
* En la pantalla de login, se dará la opción de recuperar contraseña
  * Se enviará un correo electrónico con un token para introducir la nueva contraseña

## Permisos

* A cada usuario se le podrá asignar uno o varios roles
* Cada página o incluso partes de las páginas, podrán configurar los roles requeridos para cargar y mostrar la información


## Administración usuarios y roles

* El primer usuario creado tendrá capacidad de administración de usuarios
  * role ADMIN_USERS (ver en siguiente sección roles)


## Roles

* ADMIN_USERS
  * Ver los usuarios
  * Bloquearlos o desbloquearlos
  * Crear o borrar roles
  * Añadir o quitar roles a los usuarios
  * Administrar entidades de los usuarios
* SHOW_OPERATIONS
  * Consultar operaciones (para las entidades que tenga configuradas el usuario)
* ADD_OPERATIONS
  * Alta operaciones (para las entidades que tenga configuradas el usuario)



## Run on develop

To start your Phoenix server:

  * Install dependencies with `mix deps.get`
  * Create and migrate your database with `mix ecto.setup`
  * Start Phoenix endpoint with `mix phx.server` or inside IEx with `iex -S mix phx.server`

Now you can visit [`localhost:4002`](http://localhost:4002) from your browser.

Ready to run in production? Please [check our deployment guides](https://hexdocs.pm/phoenix/deployment.html).

## Learn more

  * Official website: https://www.phoenixframework.org/
  * Guides: https://hexdocs.pm/phoenix/overview.html
  * Docs: https://hexdocs.pm/phoenix
  * Forum: https://elixirforum.com/c/phoenix-forum
  * Source: https://github.com/phoenixframework/phoenix


----

# Wui2

To start your Phoenix server:

  * Install dependencies with `mix deps.get`
  * Create and migrate your database with `mix ecto.setup`
  * Start Phoenix endpoint with `mix phx.server` or inside IEx with `iex -S mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.

Ready to run in production? Please [check our deployment guides](https://hexdocs.pm/phoenix/deployment.html).

## Learn more

  * Official website: https://www.phoenixframework.org/
  * Guides: https://hexdocs.pm/phoenix/overview.html
  * Docs: https://hexdocs.pm/phoenix
  * Forum: https://elixirforum.com/c/phoenix-forum
  * Source: https://github.com/phoenixframework/phoenix
