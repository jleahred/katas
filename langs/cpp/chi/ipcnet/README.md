# Network Inter Process Communication

Pequeño experimento de colas en disco y una capa de red TCP cruda para leer y escribir registros entre procesos y máquinas.

## Estado actual
- `chi::QueueWriter` / `chi::QueueReader`: funcionales, escriben y leen colas en disco con cursor y firma.
- `chi::ClientWriter`, `chi::ClientReader`, `chi::ServerWriter`, `chi::ServerReader`: hablan TCP crudo (ASIO standalone) con framing propio, chunking y callbacks; cada rol usa puerto distinto (writer y reader separados para simplificar).

## Build y ejecución rápida
ASIO ya viene vendorizado en `third_party/asio` (standalone, sin Boost). Compila así:
```
cd cmake
cmake -S . -B build
cmake --build build
./build/ipc
```

## Ejemplo mínimo (`cmake/main.cpp`)
El ejemplo levanta un “server” writer en `127.0.0.1:6001` y reader en `127.0.0.1:6002`, arranca un cliente, encola tres mensajes en la cola lógica `orders` y solicita hasta 10 registros. La resolución de colas se hace a ficheros bajo `./queues`.
```cpp
const auto queue_root = std::filesystem::path("queues");
std::filesystem::create_directories(queue_root);
const chi::QueuePathResolver resolver = [queue_root](const std::string& queue_name) {
    return queue_root / (queue_name + ".queue");
};

chi::ServerWriter mut_server_writer("127.0.0.1", 6001, resolver);
chi::ServerReader mut_server_reader("127.0.0.1", 6002, resolver);
mut_server_writer.start();
mut_server_reader.start();

chi::ClientWriter mut_client_writer("127.0.0.1", 6001);
chi::ClientReader mut_client_reader("127.0.0.1", 6002);
mut_client_writer.start();
mut_client_reader.start();

mut_client_writer.enqueue("orders", std::string_view("alpha"));
mut_client_writer.enqueue("orders", std::string_view("beta"));
mut_client_writer.enqueue("orders", std::string_view("gamma"));

auto mut_future = mut_client_reader.request("orders", 10);
const auto batch = mut_future.get();  // devuelve registros si existen en la cola

mut_client_writer.stop();
mut_client_reader.stop();
mut_server_writer.stop();
mut_server_reader.stop();
```

## Espera no bloqueante del futuro
Puedes consultar periódicamente si el `std::future` ya tiene datos sin bloquear el hilo principal:
```cpp
auto mut_future = mut_client_reader.request("orders", 10);

while (mut_future.wait_for(std::chrono::milliseconds(0)) != std::future_status::ready) {
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    // haz otras tareas aquí si quieres
}

const auto batch = mut_future.get();
for (const auto& message : batch.messages) {
    std::cout << "[client] got: " << message << "\n";
}
```

## API breve
- `chi::QueueWriter(path)`: escribe registros binarios o de texto, maneja firma y truncado seguro. `reset()` reinicia la cola con nueva firma.
- `chi::QueueReader(path)`: lee hasta `max_records`, devuelve `ReadResult` con `messages`, `signature_changed`, `inconsistent`.
- `chi::NetThrottleConfig`: controla `max_chunk_size` y `inter_chunk_delay` para frenar envíos grandes.
- `chi::ClientWriter::enqueue(queue_name, data)`: encola envíos TCP; `start()/stop()` gestionan el hilo interno y reconexión.
- `chi::ClientReader::request(queue_name, max_records)`: devuelve un `std::future<ReadBatch>`; `ReadBatch` trae `messages`, `disconnected`, `error`, `signature_changed`.
- `chi::ServerWriter` / `chi::ServerReader`: escuchan en `bind_address:port` y resuelven cada `queue_name` a un fichero vía `QueuePathResolver`; exponen `drop_queue(queue_name)` para borrar cola y cursor en disco.
- Callbacks marcados `on_th_*` se ejecutan en hilos internos; incluyen conect/disconnect y estadísticas (`NetStats`: mensajes/bytes/chunks acumulados).

## Consideraciones de diseño
- Framing TCP simple: `u8 type` (write/read-request/read-response) + `u32 queue_len` + nombre de cola + campos específicos; write lleva `u32 payload_len` + payload; read-response lleva flags de error/firma y lista de registros (cada uno `u32 len` + bytes).
- Guard-rails: `queue_name` limitado a 4096 bytes, payload a 64MB, chunking configurable (`max_chunk_size`, `inter_chunk_delay`).
- Chunking: envíos grandes se trocean respetando la configuración; mismo framing en reader y writer.
- Cursors aislados por cliente en `ServerReader` (un `QueueReader` por cola y cliente) para evitar carreras; se pueden borrar cola+cursor con `drop_queue`.
- `signature_changed` ahora viaja en `ReadBatch.signature_changed` para que el cliente reinicie cursores si el servidor hizo `reset()`.
- Código busca portabilidad Linux/Windows (usa `std::jthread`, `std::filesystem`, guardas `_WIN32` en `ipc.cpp`, y ASIO standalone sin SSL).
