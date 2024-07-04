formulageneral <- function(a, b, c) {
  discriminante <- b^2 - 4 * a * c

  if (discriminante > 0) {
    # Dos soluciones reales distintas
    x1 <- (-b + sqrt(discriminante)) / (2 * a)
    x2 <- (-b - sqrt(discriminante)) / (2 * a)
    return(list(x1 = x1, x2 = x2))
  } else if (discriminante == 0) {
    # Una solución real doble
    x <- -b / (2 * a)
    return(x)
  } else {
    # Soluciones complejas
    parte_real <- -b / (2 * a)
    parte_imaginaria <- sqrt(-discriminante) / (2 * a)
    return(list(x1 = paste(parte_real, "+", parte_imaginaria, "i"),
                x2 = paste(parte_real, "-", parte_imaginaria, "i")))
  }
}

