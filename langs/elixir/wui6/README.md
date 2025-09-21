# Wui6

To start your Phoenix server:

* Run `mix setup` to install and setup dependencies
* Start Phoenix endpoint with `mix phx.server` or inside IEx with `iex -S mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.

Ready to run in production? Please [check our deployment guides](https://hexdocs.pm/phoenix/deployment.html).

## Learn more

* Official website: https://www.phoenixframework.org/
* Guides: https://hexdocs.pm/phoenix/overview.html
* Docs: https://hexdocs.pm/phoenix
* Forum: https://elixirforum.com/c/phoenix-forum
* Source: https://github.com/phoenixframework/phoenix


## THINKING

* Si recibo una sesión con un token que no existe banear al usuario
* al crear una sesión con el enlace, ir a return_to o si no hay, al home
* cuadno se acceda a un recurso con una sesión, actualizar last_access
* session request que se borren pasado 3 minutos
* comprimir las migraciones, no hemos pasado a producción y podemos empezar con una bbdd nueva
* en sessions expired at
* ¿se podría borrar la sesión al cerrar todas las pestañas de la sesión? (sería interesante)
* poder ver un admin con todas las conexiones activas
* añadir permisos por url o módulos (grants)
* borrar sesiones que hayan expirado hace más de 1 semana?
* si una petición con sesión cambia la ip o agente desde donde se conecta, banear al usuario
* si el usuario no está activo, redirigir a "usuario no activo, contacte con admin"
* admin para borrar sesiones
* el filtro de usuarios no funciona, rehacer
* hay que actualizar última actividad en user