# KVR

Es una BBDD clave valor con soporte de replicación/sincronización y trabajo con subclaves.

La replicación/sincronización se realiza por polling.

El polling debe ser lento y contenido, no es para mover grandes flujos de información en poco tiempo. Todo lo contrario, es para tener una BBDD clave valor replicable sin consumir mucha CPU ni mucha red.



## Características

- Buckets dinámicos: se crean bajo demanda.
- `set` crea una nueva versión de la clave y marca la anterior como borrada.
- `get` lee solo el registro activo (`deleted_at = 0`).
- Sincronización por polling: el cliente pide y el servidor responde con registros.

## API

Funciones públicas de `kvr::Kvr`:

- `Kvr(const std::string& db_path)`: abre (o crea) la base SQLite.
- `bool ensure_bucket(const std::string& bucket_name)`: crea el bucket si no existe.
- `bool reset_bucket(const std::string& bucket_name)`: borra y recrea el bucket.
- `bool set(const std::string& bucket_name, const std::string& key, const std::string& value)`: inserta nueva versión de la clave.
- `bool del(const std::string& bucket_name, const std::string& key, const std::string& value)`: marca como borrada la clave; además inserta un registro de borrado con el `value` indicado para garantizar la sincronización.
- `Optional<std::string> get(const std::string& bucket_name, const std::string& key)`: lee el valor activo.
- `std::vector<std::string> list_buckets()`: lista los buckets existentes.
- `KeyListResult list_active_keys(...)`: lista claves activas con paginación por `key` y `id`.
- `SubKeyListResult get_sub_keys(...)`: lista sub-keys directas bajo un prefijo (paginable con `last_full_key` y `limit`).
- `std::vector<HistoryRecord> list_history(...)`: historial de un `key` ordenado por `id` descendente.
- `DbSize db_size() const`: tamaño de la base y métricas de páginas (`page_count`, `page_size`, `freelist_count`).
- `bool incremental_vacuum(int64_t pages = 0)`: ejecuta `PRAGMA incremental_vacuum` (0 = todo).
- `bool compact_full()`: ejecuta `VACUUM` completo (más agresivo; puede bloquear durante su ejecución).
- `bool optimize()`: ejecuta `PRAGMA optimize` para actualizar estadísticas.
- `bool wal_checkpoint()`: ejecuta `PRAGMA wal_checkpoint(TRUNCATE)` para reducir el WAL.
- `bool begin_transaction()`: inicia una transacción (`BEGIN IMMEDIATE`).
- `bool commit_transaction()`: confirma la transacción.
- `bool rollback_transaction()`: revierte la transacción.
- `bool set_meta_info(const std::string& bucket_name, const MetaInfo& info)`: fija metadatos del bucket.
- `MetaInfo get_meta_info(const std::string& bucket_name)`: lee metadatos (default: infinito si no existe).
- `int64_t last_id_for_bucket(const std::string& bucket_name)`: id máximo del bucket.
- `std::vector<PollConfig> list_poll_configs() const`: configuración de endpoints poll.
- `std::vector<PollClientStatus> list_poll_client_status() const`: estado de polling por bucket.
- `size_t poll_connection_count() const`: cantidad de conexiones TCP activas para polling.
- `void add_poll_server(...)`: configura un poll server por bucket.
- `void add_poll_client(...)`: configura un poll client por bucket (incluye `max_records_per_poll`).
- `bool start_polling()`: inicia TCP/polling.
- `void stop_polling()`: detiene TCP/polling.
- `const std::string& last_error() const`: último error interno.

Estructuras devueltas:

- `Optional<T>`: mini-optional propio compatible con C++14 (`has_value()`, `value()`).
- `KeyListResult`: `keys`, `next_id`, `next_key`.
- `SubKeyListResult`: `sub_keys`, `last_full_key`.
- `HistoryRecord`: `id`, `created_at`, `deleted_at`, `value`.
- `DbSize`: `db_bytes`, `wal_bytes`, `shm_bytes`, `total_bytes`, `page_count`, `page_size`,
  `freelist_count`, `used_bytes`, `free_bytes`, `fragmentation_pct`.
- `MetaInfo`: `max_deleted_per_key` (`-1` = infinito, `1` = mantener 1 borrado por clave, etc).
- `PollClientStatus`: `bucket`, `connected`, `last_poll_ms_ago`, `last_reply_ms_ago`.

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

    if (!server_store.start_polling() || !client_store.start_polling()) {
        std::cerr << "Start failed.\n";
        return 1;
    }

    server_store.set(bucket_name, "alice", "hello");
    std::this_thread::sleep_for(std::chrono::seconds(3));

    const auto value = client_store.get(bucket_name, "alice");
    if (value.has_value()) {
        std::cout << "Client value: " << value.value() << "\n";
    }

    client_store.stop_polling();
    server_store.stop_polling();
    return 0;
}
```

## Nota sobre deleted_at

`deleted_at` **puede divergir** entre nodos. En el protocolo `POLL` no se sincroniza la marca de
borrado del primer registro, así que este valor no está garantizado que coincida entre bases de datos.
Para estabilizar la verificación de `POLL`, KVR nunca elimina físicamente el registro con menor `id`
del bucket, incluso durante la poda de borrados.


## Replicación

La replicación es opcional. Puedes usar KVR como almacenamiento local sin iniciar TCP.

Hay dos planos separados:

- TCP server/client: define si el socket escucha (`server`) o conecta (`client`).
- Poll server/client: define si responde peticiones `POLL` (server) o si las hace (client).

Un nodo puede mezclar roles según su topología. Por ejemplo, un TCP client puede actuar como
poll client (pide datos) o poll server (responde) según cómo se configure el endpoint.

Puede haber bucles en replicación, pero sólo debería haber un nodo escritor.

> Es replicación, no es sharding ni distribución. No hay gestión de conflictos de concurrencia.
>
> Se espera que haya un escritor. Escrituras simultáneas en varios nodos no garantizan la consistencia de los datos.

En caso de haber bucles en replicación, los reset pueden verse comprometidos



## Subkeys por prefijo

Puedes listar sub-keys directas bajo un prefijo usando `get_sub_keys`. La función devuelve los
segmentos hijos inmediatos (sin cargar todas las claves) y un `last_full_key` para continuar.

```cpp
// sub_key puede terminar o no en "/"
auto result = store.get_sub_keys("users", "/home/test1");
// result.sub_keys -> {"hello", "buy"}
// result.last_full_key -> ultima clave completa encontrada

// Para continuar si hay mas:
auto more = store.get_sub_keys("users", "/home/test1", result.last_full_key, 50);
```

## Metadatos por bucket (`__meta_info__`)

Cada bucket puede definir metadatos en la clave reservada `__meta_info__`. La réplica los recibe
por polling, garantizando la misma estrategia en todos los nodos.

Por ahora se usa:

- `max_deleted_per_key`: cantidad de registros borrados a mantener por clave (`-1` = infinito).

Si no existe `__meta_info__`, el valor por defecto es `1` (mantener un borrado por clave). Si se reduce el valor, el recorte
se aplica solo en futuras operaciones (set/del/replicación).


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
- `GET /buckets/{bucket}/sub_keys?key=prefix&last_full_key=...&limit=50`
  - Devuelve sub-keys directas (segmento hijo inmediato) bajo `prefix`
  - `last_full_key` permite continuar la paginación
  - `limit` máximo permitido: 100 (si se pide más, error)
  - Respuesta incluye `sub_keys` y `last_full_key`
- `GET /buckets/{bucket}/history?key=...&start=0&limit=50`
  - Devuelve historial del `key` (todos los registros) ordenado por `id` descendente
  - `start` es offset y `limit` máximo permitido: 100 (si se pide más, error)
  - Respuesta incluye `records` con `id`, `created_at`, `deleted_at`, `value`
- `GET /db_size`
  - Devuelve tamaño de la base y del WAL/SHM, además de `page_count`, `page_size`,
    `freelist_count` y `fragmentation_pct`
- `POST /db/incremental_vacuum?pages=0`
  - Ejecuta `PRAGMA incremental_vacuum` (0 = todo)
- `POST /db/compact_full`
  - Ejecuta `VACUUM` completo (puede bloquear temporalmente la base)
- `POST /db/optimize`
  - Ejecuta `PRAGMA optimize`
- `POST /db/wal_checkpoint`
  - Ejecuta `PRAGMA wal_checkpoint(TRUNCATE)`
- `GET /poll_status`
  - Devuelve conexiones activas y estado de polling por bucket
- `GET /config`
  - Respuesta: `{"buckets":[...], "poll":[...]}`
  - `poll_server_buckets` incluye objetos con `name` y `poll_max_reply_count`
- `poll_client_buckets` incluye objetos con `name`, `poll_interval_ms`, `poll_interval_active_ms`
  y `max_records_per_poll`
- `GET /status`
  - Respuesta: `{"status":[...]}`
  - Cada entrada incluye `role`, `bucket`, `host`, `port`, `last_id`
    y, para `poll_client`, `poll_interval_ms`, `poll_interval_active_ms` y `max_records_per_poll`

### Uso básico

```cpp
#include "kvr.h"
#include "rest_server.h"

int main() {
    kvr::Kvr store("kvr_rest.db");
    kvr::RestServer server(store, "127.0.0.1", 8080);
    if (!server.start_polling()) {
        return 1;
    }
    // Espera o loop de la aplicación...
    server.stop_polling();
    return 0;
}
```

### Pruebas rápidas en navegador

- `http://127.0.0.1:8080/config`
- `http://127.0.0.1:8080/buckets/users/get?key=alice`
- `http://127.0.0.1:8080/buckets/users/keys?key=al&start_key=alice&next_id=0&skip=0&limit=50`
- `http://127.0.0.1:8080/buckets/users/sub_keys?key=/home/test1&limit=50`
- `http://127.0.0.1:8080/buckets/users/sub_keys?key=/home/test1&last_full_key=/home/test1/buy/&limit=50`
- `http://127.0.0.1:8080/buckets/users/history?key=alice&start=0&limit=50`
- `http://127.0.0.1:8080/db_size`
- `http://127.0.0.1:8080/db/incremental_vacuum?pages=0`
- `http://127.0.0.1:8080/db/compact_full`
- `http://127.0.0.1:8080/db/optimize`
- `http://127.0.0.1:8080/db/wal_checkpoint`
- `http://127.0.0.1:8080/poll_status`



### Compilación

Se usa la cabecera `third_party/httplib/httplib.h`. En CMake basta con añadir el include:

```cmake
target_include_directories(kvr PRIVATE ../third_party/httplib)
```

No hace falta enlazar librerías adicionales.

## Hacking

KVR es una clase de almacenamiento key-value replicado basada en SQLite. Cada bucket es una tabla con
`id`, `created_at`, `deleted_at`, `key` y `value`.

- Cada bucket es una tabla con índice único `(key, deleted_at)` para mantener un solo registro activo.
- `set` abre una transacción: marca como borrado el registro activo y crea uno nuevo.
- El polling usa un framing binario simple: longitud (uint32) + tipo + payload.
- `POLL` envía el `bucket_name`, el primer registro local (id/key/value) y el `last_id`.
- `REPLY` envía `id`, `created_at`, `deleted_at`, `key` y `value`, y se aplica con `INSERT ... ON CONFLICT(id)`.
- Si se detecta que el primer registro no coincide, se fuerza una sincronización completa con `RESET`.
