formulageneral <- function(a, b, c) {
  discriminante <- b^2 - 4 * a * c
  
  if (discriminante > 0) {
    # Dos soluciones reales distintas
    x1 <- (-b + sqrt(discriminante)) / (2 * a)
    x2 <- (-b - sqrt(discriminante)) / (2 * a)
    mensaje <- paste("La factorización es:", x1, "y", x2)
    cat(mensaje, "\n")
  } else if (discriminante == 0) {
    # Una solución real doble
    x <- -b / (2 * a)
    mensaje <- paste("La factorización es:", x)
    cat(mensaje, "\n")
  } else {
    # No se imprimen soluciones complejas
    cat("No hay soluciones reales para los coeficientes dados.\n")
  }
}
