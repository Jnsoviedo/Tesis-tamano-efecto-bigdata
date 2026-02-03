

                                     # A de Vargha-Delaney

set.seed(123)

# Número de pacientes por grupo
n <- 10000

# Grupos
grupo <- rep(c("Grupo A", "Grupo B"), each = n)

# Puntuación inicial (HAM-D) por grupo
puntuacion_inicial_A <- rnorm(n, mean = 20, sd = 6)
puntuacion_inicial_B <- rnorm(n, mean = 20, sd = 5)
puntuacion_inicial <- c(puntuacion_inicial_A, puntuacion_inicial_B)

# Se limita a 0-52 y se redondea
puntuacion_inicial <- round(pmin(pmax(puntuacion_inicial, 0), 52))

# Puntuación final (después del tratamiento)
puntuacion_final_A <- puntuacion_inicial_A - rnorm(n, mean = 10, sd = 6)
puntuacion_final_B <- puntuacion_inicial_B - rnorm(n, mean = 14, sd = 4)
puntuacion_final <- c(puntuacion_final_A, puntuacion_final_B)

# Se limita a 0-52 y se redondea
puntuacion_final <- round(pmin(pmax(puntuacion_final, 0), 52))

# Variables adicionales
sexo <- sample(c("Masculino", "Femenino"), 2 * n, replace = TRUE)
edad <- sample(18:65, 2 * n, replace = TRUE)

# Data frame final
data <- data.frame(
  ID_Paciente = 1:(2 * n),
  Sexo = sexo,
  Edad = edad,
  Grupo = grupo,
  Puntuacion_Inicial = puntuacion_inicial,
  Puntuacion_Final = puntuacion_final
)

head(data)

# Descriptivos
summary(data$Puntuacion_Inicial)
summary(data$Puntuacion_Final)

# Puntuaciones finales por grupo
grupo_A <- data$Puntuacion_Final[data$Grupo == "Grupo A"]
grupo_B <- data$Puntuacion_Final[data$Grupo == "Grupo B"]

# Se calcula A de Vargha-Delaney
# install.packages("effectsize")  # correr si no se tiene
library(effectsize)

vd_a(grupo_A, grupo_B)
