# KVR

KVR es una clase de almacenamiento key-value replicado basada en SQLite. Cada bucket es una tabla con
`id`, `created_at`, `deleted_at`, `key` y `value`. El sistema incluye un mecanismo de sincronización
por polling sobre TCP para replicar registros entre nodos.

## Características

- Buckets dinámicos: se crean bajo demanda.
- `set` crea una nueva versión de la clave y marca la anterior como borrada.
- `get` lee solo el registro activo (`deleted_at = 0`).
- Sincronización por polling: el cliente pide y el servidor responde con registros.

## Ejemplo sencillo

```cpp
#include <chrono>
#include <iostream>
#include <thread>

#include "kvr.h"

int main() {
    const std::string bucket_name = "users";
    kvr::Kvr server_store("kvr_server.db");
    kvr::Kvr client_store("kvr_client.db");

    server_store.add_poll_server(bucket_name, kvr::TcpSide::server, "127.0.0.1", 9000);
    client_store.add_poll_client(bucket_name, kvr::TcpSide::client, "127.0.0.1", 9000);

    if (!server_store.start() || !client_store.start()) {
        std::cerr << "Start failed.\n";
        return 1;
    }

    server_store.set(bucket_name, "alice", "hello");
    std::this_thread::sleep_for(std::chrono::seconds(3));

    const auto value = client_store.get(bucket_name, "alice");
    if (value.has_value()) {
        std::cout << "Client value: " << value.value() << "\n";
    }

    client_store.stop();
    server_store.stop();
    return 0;
}
```

## Nota sobre deleted_at

`deleted_at` **puede divergir** entre nodos. En el protocolo `POLL` no se sincroniza la marca de
borrado del primer registro, así que este valor no está garantizado que coincida entre bases de datos.

## Replicación

La replicación es opcional. Puedes usar KVR como almacenamiento local sin iniciar TCP.

Hay dos planos separados:

- TCP server/client: define si el socket escucha (`server`) o conecta (`client`).
- Poll server/client: define si responde peticiones `POLL` (server) o si las hace (client).

Un nodo puede mezclar roles según su topología. Por ejemplo, un TCP client puede actuar como
poll client (pide datos) o poll server (responde) según cómo se configure el endpoint.

## API REST

La API REST es opcional y está implementada con `cpp-httplib` (header-only). Permite hacer `set`, `del`,
consultar `get` y leer la configuración.

### Endpoints

- `POST /buckets/{bucket}/set`
  - Body: `{"key":"...", "value":"..."}`
  - Respuesta: `{"ok":true}` o `{"error":"..."}`
- `POST /buckets/{bucket}/del`
  - Body: `{"key":"...", "value":"..."}`
  - Respuesta: `{"ok":true}` o `{"error":"..."}`
- `GET /buckets/{bucket}/get?key=...`
  - Respuesta: `{"value":"..."}` o `{"error":"Not found"}`
- `GET /buckets/{bucket}/keys?key=prefix&start_key=...&next_id=...&skip=0&limit=50`
  - Devuelve `keys` de registros activos (`deleted_at = 0`) cuyo `key` empieza por `prefix`
  - `limit` máximo permitido: 100 (si se pide más, error)
  - `start_key` indica la clave mínima a considerar, `next_id` pagina dentro de una misma clave
  - `skip` pagina y `limit` limita resultados
  - Respuesta incluye `next_id` y `next_key` para paginación sin perder elementos
- `GET /config`
  - Respuesta: `{"buckets":[...], "poll":[...]}`
  - `poll_server_buckets` incluye objetos con `name` y `poll_max_reply_count`
  - `poll_client_buckets` incluye objetos con `name` y `poll_interval_ms`
- `GET /status`
  - Respuesta: `{"status":[...]}`
  - Cada entrada incluye `role`, `bucket`, `host`, `port` y `last_id`

### Uso básico

```cpp
#include "kvr.h"
#include "rest_server.h"

int main() {
    kvr::Kvr store("kvr_rest.db");
    kvr::RestServer server(store, "127.0.0.1", 8080);
    if (!server.start()) {
        return 1;
    }
    // Espera o loop de la aplicación...
    server.stop();
    return 0;
}
```

### Pruebas rápidas en navegador

- `http://127.0.0.1:8080/config`
- `http://127.0.0.1:8080/buckets/users/get?key=alice`
- `http://127.0.0.1:8080/buckets/users/keys?key=al&start_key=alice&next_id=0&skip=0&limit=50`

### Compilación

Se usa la cabecera `third_party/httplib/httplib.h`. En CMake basta con añadir el include:

```cmake
target_include_directories(kvr PRIVATE ../third_party/httplib)
```

No hace falta enlazar librerías adicionales.

## Hacking

- Cada bucket es una tabla con índice único `(key, deleted_at)` para mantener un solo registro activo.
- `set` abre una transacción: marca como borrado el registro activo y crea uno nuevo.
- El polling usa un framing binario simple: longitud (uint32) + tipo + payload.
- `POLL` envía el `bucket_name`, el primer registro local (id/key/value) y el `last_id`.
- `REPLY` envía `id`, `created_at`, `deleted_at`, `key` y `value`, y se aplica con `INSERT ... ON CONFLICT(id)`.
- Si se detecta que el primer registro no coincide, se fuerza una sincronización completa con `RESET`.
