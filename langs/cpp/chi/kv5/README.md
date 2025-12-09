# Key Value store

K/V ligero sobre SQLite3 con claves únicas y API de patrones para extraer paths o valores (opcionalmente con selectores JSON). Incluye utilidades de replicación apoyadas en las colas `chi::QueueWriter/QueueReader` (directorio `chi/ipcnet`).

## API rápida

- `KvStore(KvStoreConfig)`: abre/crea la DB; `db_path`, `scan_window`, `table`, `import_table`, `queue_root` son configurables.
- `bool set_value(key, value)`: upsert de valor string.
- `set_value(key, value)`: upsert de valor string.
- `std::optional<std::string> get_value(key)`: obtiene por índice único (omite `is_deleted`).
- `ScanResult get_paths(key_pattern, start_on)`: devuelve claves que cumplen regex por segmentos (`{.*}` etc.) recorriendo por `id` con ventana `scan_window`; `continue_from` indica el último id visitado.
- `ScanResult get_values(key_pattern, start_on)`: igual que `get_paths` pero devuelve valores; puede incluir selector JSON en la última sección `{.prop[1] && .other}`.
- Nota: se normaliza el patrón quitando una `/` final (salvo el caso `/`), así `/root/demo/` se trata como `/root/demo`.
- `mark_deleted(key)`: marca soft-delete (`is_deleted=1`); ningún getter/scan devuelve registros borrados, pero se replican como tombstones.
- `PruneResult prune_deleted(start_on)`: borra físicamente filas marcadas con `is_deleted` en lotes del tamaño `scan_window`, devolviendo `continue_from` y `pruned`.
- `replicate_to(queue) / send2replica(queue, limit)`: prepara y envía filas en JSON a una cola (una o varias colas destino) vía netipc (`replica_host`, `replica_writer_port`). Usa cursor incremental por cola (`last_send_<queue>`). `replicate_to` resetea el cursor de envío para reemitir desde el principio. Se persiste la configuración en SQLite para que al reiniciar pueda continuar por donde iba. Si llamas a `send2replica` con una cola no configurada, devuelve `false` y no crea el writer automáticamente.
- `delete_replica_to(queue)`: borra writer netipc y entrada de configuración.
- `replicate_from(queue) / get_from_replica(limit)`: se conecta vía netipc (`replica_host`, `replica_reader_port`) y aplica mensajes en la tabla de importación; tras `replica2master()` se promueven a la tabla principal (y posteriores lecturas ya escriben en master).
- `bool replica2master()`: copia import → master y borra la tabla de importación.
- `replica_outputs()`: devuelve las colas de salida configuradas (persistidas en SQLite) a las que se envían registros con `replicate_to`.
- `replica_input()`: devuelve, si existe, la cola de entrada configurada (persistida) usada por `replicate_from`/`get_from_replica`.
- `replica_outputs_stats()` / `replica_input_stats()`: estadísticas para saber progreso de réplica; ver abajo.
- Tabla de paths: cada clave inserta sus prefijos en `kv_paths` (`/a`, `/a/b`, …). `get_paths`/`get_values` consultan ahí (filtro `LIKE` por prefijo literal y regex final) y usan `start_on` basado en `kv_paths.id`.

### Patrones de clave

- Literal: `/root/item`
- Regex por segmento entre paréntesis: `/root/(.*ha^)/tail`
- Selector JSON en la última sección (llaves): `/root/(.*)/{.payload.id}` ó `/root/(.*)/{.arr[2..5] .other}`. Llaves solo para seleccionar JSON en values; puedes combinar selectores separados por espacio para componer un objeto con cada último token como clave (`.payload.id .payload.range` -> `{"id":...,"range":...}`).

## Cómo saber si la réplica se completó

- **Envío (fuente → cola)**: usa `replica_outputs_stats()`. Para cada cola obtienes `last_sent` (último id enviado) y `max_id` (máximo id en la tabla). Si `last_sent >= max_id` en ese instante, la cola ya tiene todo lo visible. Vuelve a consultarlo después de nuevos writes si necesitas asegurar que sigue al día.
- **Recepción (cola → destino)**: llama a `get_from_replica()` hasta que devuelva `false` (no aplicó nada) y consulta `replica_input_stats()`: `last_batch_empty == true` indica que la última petición no trajo registros; `last_imported` es el último id aplicado. Si conoces el `last_sent` en la fuente, compáralo con `last_imported` para confirmar que no falta ningún id. 

## Hacking

- **Esquema principal**: tabla `kv_items` (configurable) con `id` autoincremental, `key` única, `value` texto, `updated` y `is_deleted`. Cada write/delete calcula un `id` nuevo incluso en conflicto de clave (para versiones monotónicas).
- **Paths index**: tabla `kv_paths` con `id` autoincremental y `path` único. Cada clave añade todos sus prefijos; al borrar, se eliminan prefijos de mayor a menor solo si no quedan filas activas con ese prefijo. `get_paths`/`get_values` iteran sobre `kv_paths` para filtrar por `LIKE` + regex y respetan `start_on` sobre `kv_paths.id`.
- **Soft-delete y poda**: `mark_deleted` marca `is_deleted=1` (tombstone replicable). `prune_deleted(start_on)` borra físicamente lotes de `is_deleted=1` y limpia `kv_paths` si el prefijo queda sin uso.
- **Réplica**: configuración persistida en `kv_replication_config` (dirección `in`/`out`). Colas se resetean en `replicate_to` y cursors de envío se guardan en `kv_meta` (`last_send_<queue>`). Importación usa tabla `import_table`; tras `replica2master`, se reconstruyen paths.
- **Sin dependencias externas**: SQLite vendorizado (`third_party/sqlite/sqlite-amalgamation-3450200`) y ASIO vendorizado en `chi/ipcnet/third_party/asio`.
- **Promoción de réplica** (`replica2master`): abre transacción inmediata, vacía la tabla principal, inserta todo lo importado desde `import_table`, borra `import_table`, rehace `kv_paths` y desactiva el modo importación (`use_import_replica_ = false`). Devuelve `true` solo si toda la secuencia se completa (con commit); si falla alguna parte, no hace commit y retorna `false`.

## Ejemplo mínimo

Consulta `cmake/main.cpp` para un ejemplo de dos BB.DD. replicando a través de una cola en disco.
