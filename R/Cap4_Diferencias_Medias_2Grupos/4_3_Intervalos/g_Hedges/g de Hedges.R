

                              # g de Hedges (B − A)

set.seed(123)

# Parámetros poblacionales
nA   <- 600
nB   <- 600
mu_A <- 140
sd_A <- 12
mu_B <- 130
sd_B <- 10

# Datos simulados (independientes)
A <- rnorm(nA, mean = mu_A, sd = sd_A)
B <- rnorm(nB, mean = mu_B, sd = sd_B)

# Data frame en formato largo (para descriptivos)
datos <- data.frame(
  grupo   = factor(rep(c("A", "B"), times = c(nA, nB)), levels = c("A", "B")),
  presion = c(A, B)
)

head(datos)

# Se calcula g de Hedges con dirección (B − A)
# install.packages("effectsize")  # correr si no se tiene
library(effectsize)

g_hedges <- hedges_g(B, A, ci = 0.95)  # orden fijo: B - A
print(g_hedges)
