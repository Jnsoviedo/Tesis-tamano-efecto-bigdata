

                                   # Delta de Glass

set.seed(123)

# Número total de observaciones por grupo
n_total <- 8880

# Control (Método A)
grupo_control <- round(rnorm(n_total, mean = 75, sd = 10))
grupo_control <- pmin(pmax(grupo_control, 0), 100)

# Experimental (Método B)
grupo_experimental <- round(rnorm(n_total, mean = 80, sd = 20))
grupo_experimental <- pmin(pmax(grupo_experimental, 0), 100)

# Data frame (ancho)
datos <- data.frame(
  control_A = grupo_control,
  experimental_B = grupo_experimental
)

head(datos)

# Se calcula Delta de Glass
# install.packages("effectsize")  # correr si no se tiene
library(effectsize)

delta_glass <- glass_delta(grupo_experimental, grupo_control)
print(delta_glass)

