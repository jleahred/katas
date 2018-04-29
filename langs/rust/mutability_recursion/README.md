# Rust mutability and recursion

Como ya he comentado en alguna ocasión, la mutabilidad no es vírica y en Rust se puede diseñar un
sistema que evite la copia de forma explícita combinándola con el _ownership_.

Este es un ejemplo de una función que recibe la propiedad de _status_ y lo
declara como mutable.

Luego utilzamos un bucle _for_

```Rust
pub(crate) fn parse_literal<'a>(mut status: Status<'a>, literal: &'a Literal<'a>) -> Result<'a> {
    for byte in literal.0.bytes() {
        status = parse_byte(status, byte)
            .map_err(|st| Error::from_status(&st, &format!("parsing literal {}", literal.0)))?;
    }
    Ok((status, literal.0.to_string()))
}
```

No es una mala solución.

Quizá debería haber quitado los _lifetimes_ para dejar el ejemplo más sencillo. No es relevante para lo que estoy planteando aquí. Tampoco molesta ni es malo familiarizarse con ello.

La mutabilidad es muy limitada (muy pocas líneas), no es vírica, el código es conciso,
gestión de errores simplificado...

No obstante, para un amante de la programación funcional, es inevitable tensar la cuerda
tratando de quitar la mutabilidad.

El primer intento lógico es, utilizando la recursión:

```Rust
pub(crate) fn parse_literal<'a>(status: Status<'a>, literal: &'a Literal<'a>) -> Result<'a> {
    fn parse_au8<'a>(status: Status<'a>, au8: &[u8], literal: &'a Literal<'a>) -> Result<'a> {
        if au8.len() == 0 {
            Ok((status, literal.0.to_string()))
        } else {
            parse_au8(
                parse_byte(status, au8[0]).map_err(|st| {
                    Error::from_status(&st, &format!("parsing literal {}", &literal.0))
                })?,
                &au8[1..],
                literal,
            )
        }
    }

    parse_au8(status, literal.0.as_bytes(), literal)
}
```

Hemos quitado la mutabilidad. La recursión es de cola, pero no está garantizada su elminación por el compilador ni LLVM :-(

No obstante, la sobrecarga de la recursión, incluso sin optimización de cola, no es significativa. Gracias al _ownership_ status no se copia, y los otros dos parámetros son referencias de estructuras sencillas. Uno de ellos es siempre la misma estructura, y el otro, sí se crea en la pila en cada contexto de llamada.

Ni Rust ni LLVM garantizan la recursión de cola, no obstante, es posible escribir un poco de código (20 líneas), para tener algo que parece un recursión de cola y hace cuack

Esta es la solución que "simula" la sintáxis de recursión de cola y utiliza un bucle (por debajo, nada feo que ver)

0% mutabilidad declarada

```Rust
pub(crate) fn parse_literal<'a>(status: Status<'a>, literal: &'a Literal<'a>) -> Result<'a> {
    let init_tc = (literal.0[0..].as_bytes(), Ok(status));

    let result_status =
        tail_call(init_tc, |(pend_lit, acc)| {
            if pend_lit.len() == 0 {
                Ok(TailCall::Return(acc?))
            } else {
                Ok(TailCall::Call((
                    &pend_lit[1..],
                    parse_byte(acc?, pend_lit[0]),
                )))
            }
        }).map_err(|st| Error::from_status(&st, &format!("parsing literal {}", literal.0)))?;

    Ok((result_status, literal.0.to_string()))
}
```

## Show me the data

Y como siempre, las palabas están bien, pero muestráme las cifras...

Los datos siguientes son comparativos entre las diferentes soluciones, no entre las diferentes configuraciones (no sólo cambia al tamaño, también cambia el número de iteraciones)

En las gráficas se indica el timepo invertido, lógicamente menos es mejor.

### Literal a procesar pequeño (5 caracteres)

![graph](rust_for_recurs_short.png)

Todos los ejemplos utilizan sólo memoria de pila, no hay diferencia en uso de memoria del montículo.

Múltiples ejecuciones. Error inferior al 1%

La ejecución con recursión es un 60% más lenta.

La ejecución con _recursión de cola artificial_ es sólo un 4% más lenta.

### Literal a procesar medio (1_000 caracteres)

![graph](rust_for_recurs_med.png)

Error inferior al 1.5%

La recursión directa está entre 3 y 4 veces más lenta.

La recursión con optimización de cola artifial, 1.17 veces más lenta.

### Literal a procesar grande (1_000_000 caracteres)

La solución con recursión desborda la pila :-(

![graph](rust_for_recurs_big.png)

Error inferior al 0.2%

La recursión con optimización de cola artifial, 1.15 veces más lenta.

## Conclusión

No hay optimización de recursión de cola garantizada. IMPORTANTE

Se puede utilizar una recusión de cola artificial. Esta evitará la sobrecarga de la pila sin incurrir en un coste significativo de cpu.

Código completo:
