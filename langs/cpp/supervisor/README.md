# Supervisor

Pequeña utilidad en C++20 para lanzar y supervisar procesos hijos, leer su salida estándar/errores por líneas y detenerlos bajo demanda.

## Compilar

```bash
cmake -S . -B build
cmake --build build
./build/supervisor
```

## Ejemplo sencillo (comando externo)

```cpp
Supervisor mut_supervisor;

const auto pid = mut_supervisor.run({"/bin/sh", "-c", "echo hola; sleep 1; echo adios"});
if (pid == 0) {
    return 1;
}

while (mut_supervisor.is_running()) {
    const auto batch = mut_supervisor.poll_output();
    for (const auto& line : batch.stdout_lines) {
        std::cout << line << std::endl;
    }
    for (const auto& line : batch.stderr_lines) {
        std::cerr << line << std::endl;
    }
}

const auto final_batch = mut_supervisor.poll_output();
for (const auto& line : final_batch.stdout_lines) {
    std::cout << line << std::endl;
}
for (const auto& line : final_batch.stderr_lines) {
    std::cerr << line << std::endl;
}
```

## Ejemplo: supervisar al mismo binario

El propio `main` hace esto: si recibe `--self <nombre>`, saluda y termina; de lo contrario, lanza dos procesos hijos ejecutando el mismo binario con `--self one` y `--self second`.

```cpp
int main(int argc, char* argv[]) {
    if (argc > 1 && std::string_view(argv[1]) == "--self") {
        const auto name = argc > 2 ? std::string_view(argv[2]) : std::string_view{"anonimo"};
        run_self(name); // imprime y duerme un poco
        return 0;
    }

    const auto self_path = std::filesystem::absolute(argv[0]).string();
    Supervisor mut_one;
    Supervisor mut_second;

    mut_one.run({self_path, "--self", "one"});
    mut_second.run({self_path, "--self", "second"});

    while (mut_one.is_running() || mut_second.is_running()) {
        const auto batch_one = mut_one.poll_output();
        for (const auto& line : batch_one.stdout_lines) {
            std::cout << "[one stdout] " << line << std::endl;
        }

        const auto batch_second = mut_second.poll_output();
        for (const auto& line : batch_second.stdout_lines) {
            std::cout << "[second stdout] " << line << std::endl;
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }

    // drenar cualquier línea final
    const auto final_one = mut_one.poll_output();
    const auto final_second = mut_second.poll_output();
    return 0;
}
```

Puedes probar manualmente:

```bash
./build/supervisor --self prueba
./build/supervisor          # lanza dos hijos y muestra su salida por consola
```

## API principal

- `long run(const std::vector<std::string>& command)`: lanza un proceso externo; devuelve el pid (o 0 si falla).
- `long run(const std::function<int()>& action)`: POSIX únicamente; ejecuta una función en un hijo y redirige sus stdout/stderr.
- `bool is_running()`: indica si el hijo sigue vivo; al detectar que termina, lee la salida pendiente.
- `bool request_stop()`: envía SIGTERM (POSIX) o `TerminateProcess` (Windows); intenta drenar la salida antes de cerrar recursos.
- `OutputBatch poll_output()`: devuelve solo las líneas no leídas de stdout/stderr; si no hay nuevas, los vectores vienen vacíos.
- `bool write(std::string_view data)`: escribe en la entrada estándar del hijo; devuelve `false` si no hay proceso activo o si la escritura no pudo completarse.
- `long pid() const`: obtiene el pid del hijo actual (0 si no hay).

## Detalles de comportamiento

- En Linux/Unix se usan pipes no bloqueantes y `waitpid` con `WNOHANG`; en Windows se usa `CreateProcess` y `PeekNamedPipe`.
- `poll_output` conserva fragmentos parciales hasta que llega un salto de línea; al finalizar el hijo, se entrega también la última línea sin `\n`.
- No se soporta relanzar un proceso mientras uno esté vivo (`run` devuelve 0 si ya hay un hijo activo).
- Es importante drenar la salida periódicamente: si no se lee stdout/stderr y el hijo escribe lo suficiente, los buffers del pipe se llenan (~64 KB) y el hijo quedará bloqueado al escribir. Usa `poll_output` de forma regular o redirige la salida para evitarlo.
