use std::fs;
use std::io::{self, Read};

pub fn get_process_env_var(pid: u32, var_name: &str) -> Result<Option<String>, io::Error> {
    // Construir la ruta al archivo environ
    let environ_path = format!("/proc/{}/environ", pid);

    // Leer el contenido del archivo
    let content = fs::read(environ_path)?;

    // Las variables de entorno están separadas por caracteres nulos
    for var in content.split(|&b| b == 0) {
        // Convertir a String, ignorando bytes inválidos UTF-8
        let Ok(env_var) = String::from_utf8_lossy(var).into_owned().parse::<String>();
        // Buscar el formato NOMBRE=VALOR
        if let Some(pos) = env_var.find('=') {
            let (name, value) = env_var.split_at(pos);
            if name == var_name {
                // Quitar el caracter '=' al inicio del valor
                return Ok(Some(value[1..].to_string()));
            }
        }
    }

    // No se encontró la variable
    Ok(None)
}

// // Ejemplo de uso
// fn main() {
//     let pid = 1234; // Reemplaza con el PID que quieras verificar
//     let var_name = "MI_COOKIE";

//     match get_process_env_var(pid, var_name) {
//         Ok(Some(value)) => println!("La variable {} tiene el valor: {}", var_name, value),
//         Ok(None) => println!(
//             "La variable {} no está definida para el proceso {}",
//             var_name, pid
//         ),
//         Err(e) => eprintln!("Error al leer las variables de entorno: {}", e),
//     }
// }
