# Multi-target wrapper con `--only` y soporte *critical*

Esta librería soporta **dos modos**:

1) **Wrapper mode** (sin `--<target>`): lanza uno o varios *targets* como subprocesos del propio binario y multiplexa su salida.
2) **Subcommand mode** (`--<target> <config>`): ejecuta directamente el `main()` interno del target.

Además, soporta `--only` para elegir qué targets lanzar en *wrapper mode*, y una política `critical`:
- si un target marcado como **critical** se detiene, el wrapper intenta detener el resto (primero `request_stop()`, luego `kill()` tras un timeout).

---

## Targets disponibles (ejemplo)

En el código (tabla `commands[]`):

- `fix` (flag `--fix`) **[critical]**
- `db`  (flag `--db`)

> El nombre del target es el flag sin `--` (por ejemplo `--fix` → `fix`).

---

## Uso

### Ayuda

```bash
./mybin --help
```

### 1) Wrapper mode (por defecto)

Lanza todos los targets marcados como `launch_by_default=true`:

```bash
./mybin
```

Si no pasas config, usa la config por defecto:

- `${MTK_SERV_FOLDER}/etc/__dev_config.cfg`

Con una config explícita:

```bash
./mybin /ruta/a/config.cfg
```

### 2) Wrapper mode con `--only`

Lanza **solo** los targets listados (separados por comas):

```bash
./mybin --only fix
./mybin --only db
./mybin --only fix,db
```

También puedes combinar con config (en cualquier orden):

```bash
./mybin /ruta/a/config.cfg --only fix
./mybin --only fix,db /ruta/a/config.cfg
```

Si `--only` incluye un target desconocido, el programa falla con error y muestra los targets válidos.

### 3) Subcommand mode (ejecución directa)

Ejecuta **solo un target** y le pasa la config como único argumento al `main()` del submódulo:

```bash
./mybin --fix /ruta/a/config.cfg
./mybin --db  /ruta/a/config.cfg
```

### 4) Versión

Llama a todos los `main()` con `--version` (igual que tu comportamiento original):

```bash
./mybin --version
```

---

## Semántica `critical`

En *wrapper mode*, si un target marcado como `critical` pasa de `is_running()==true` a `is_running()==false`, el supervisor:

1. Imprime un mensaje:
   - `[supervisor] critical target 'X' stopped; requesting stop for the rest...`
2. Llama a `request_stop()` para el resto.
3. Espera hasta **2 segundos** drenando output.
4. Si alguno sigue vivo, hace `kill()`.

> Si quieres que “critical” signifique “solo si exit code != 0”, necesitas que `Process` exponga el código de salida; con la API actual, la detección se basa en “se paró”.

---

## Cómo añadir más targets

Solo añade filas en `commands[]`:

```cpp
{ "--risk", "Risk service", &risk::main, true,  true  },
{ "--gui",  "GUI service",  &gui::main,  false, false }, // no arranca por defecto
```

- `launch_by_default=false`: no se lanza salvo que el usuario lo pida con `--only gui` (o lo incluyas en la lista).
- `critical=true`: si cae, intenta detener el resto.

---

## Nota sobre includes

En `main.cpp` hay un include:

```cpp
#include "chi_proc.hpp"
```

Ajústalo a la ruta real donde tengas tus headers de:

- `chi::proc::Argv`
- `chi::proc::Process`
- `chi::proc::OutputBatch`

---

## Ejemplos rápidos

- Arrancar defaults con config dev por defecto:
  ```bash
  ./mybin
  ```

- Arrancar solo FIX:
  ```bash
  ./mybin --only fix /etc/config.cfg
  ```

- Ejecutar el FIX directamente (sin supervisor):
  ```bash
  ./mybin --fix /etc/config.cfg
  ```


## Ejemplo simple

```cpp
int main(int argc, char** argv) {
    // Nota: marca critical los que realmente quieras que "tiren" del conjunto
    constexpr Subcommand commands[] = {
        { "--fix", "FIX service", &fix::main, true,  true  },  // critical
        { "--db",  "DB service",  &db::main,  true,  false },  // non-critical
        // { "--foo", "Foo worker", &foo::main, false, false }, // ejemplo: no default
    };

    (void)dispatch_or_wrapper(argc, argv, commands);
    return 0;
}
```
