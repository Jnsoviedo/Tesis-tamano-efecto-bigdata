

                                     # Delta de Cliff

set.seed(123)

# Tamaño por cohorte (dos muestras independientes)
n <- 1250

# Cohorte ANTES (media ~ 6, sd ~ 1.5)
antes <- round(rnorm(n, mean = 6, sd = 1.5))

# Cohorte DESPUÉS (media ~ 4, sd ~ 1.2)
despues <- round(rnorm(n, mean = 4, sd = 1.2))

# Se limitan las puntuaciones a 0-10
antes <- pmin(pmax(antes, 0), 10)
despues <- pmin(pmax(despues, 0), 10)

# Data frame en formato largo (un registro por empleado)
datos <- data.frame(
  ID_Empleado = 1:(2 * n),
  Grupo = rep(c("Antes", "Despues"), each = n),
  Puntuacion = c(antes, despues)
)

head(datos)

# Se calcula Delta de Cliff (dos grupos independientes)
# install.packages("effsize")  # correr si no se tiene
library(effsize)

delta <- cliff.delta(antes, despues)
print(delta)
