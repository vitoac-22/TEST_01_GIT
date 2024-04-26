# Gráfica de la Distribución Binomial
# Ejercicio: Dado los valores
n <- 5
p <- 10/15
q <- 1-p
prob <- choose(n, 2) * (p^2) * (q^3) # Equivalente a Combinatoria choose()

cat("La probabilidad, dado los datos es:", prob)

# ---------------------------------------------------
# FUNCIÓN DENSIDAD
# ---------------------------------------------------
# Tipo 1 de Gráfica
x <- seq(0, n) # Generando secuencia de 0 hasta n-veces, que se repite el experimento
fx <- choose(n, x) * (p^x) * (q^(n-x)) 
# Plot
x11()
plot(x, fx, main = "Distribución Binomial",
     xlab = "Valores de X", ylab = "Valores de f(x)", 
     type = "b", col = "red" ) # Type -> Tipo de línea

abline(v = n * p, lty = 3, col = "blue") # Genera una línea vertical, donde se especifique

# Tipo 2 de Gráfica [Con su función determinada en R-Studio]
f_x <- dbinom(x, n, p)
# Plot
x11()
plot(x, f_x, main = "Distribución Binomial",
     xlab = "Valores de X", ylab = "Valores de f(x)", 
     type = "b", col = "red" ) # Type -> Tipo de línea

abline(v = n * p, lty = 3, col = "blue") # Genera una línea vertical, donde se especifique
# ---------------------------------------------------

# ---------------------------------------------------
# FUNCIÓN DISTRIBUCIÓN
# ---------------------------------------------------

# Función de Distribución
x <- seq(0, n)
Fx <- pbinom(x, n, p)

x11()
plot(x, Fx, main = "Distribución Binomial",
     xlab = "Valores de X", ylab = "Valores de F(x)", 
     type = "b", col = "red" ) # Type -> Tipo de línea

abline(h = 1, lty = 3, col = "blue")
abline(v = n * p, lty = 3, col = "blue") # Conocer ubicación Esperanza

