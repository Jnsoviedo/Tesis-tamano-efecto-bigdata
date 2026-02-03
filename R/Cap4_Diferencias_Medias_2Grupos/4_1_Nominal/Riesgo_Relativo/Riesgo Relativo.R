
                              # Riesgo Relativo

# install.packages("epitools")  # correr si no se tiene
library(epitools)

set.seed(123)  # reproducibilidad

n <- 100000  # número total de pacientes (Big Data)

# Asignación aleatoria a grupos (1 = Expuesto, 0 = No expuesto)
expuesto <- rbinom(n, 1, 0.5)

# Probabilidades de infección por grupo
prob_infeccion_expuesto    <- 0.05
prob_infeccion_no_expuesto <- 0.10

# Vector de infección (1 = infección, 0 = no infección)
infeccion <- numeric(n)

# Se asigna infección según grupo
infeccion[expuesto == 1] <- rbinom(sum(expuesto == 1), 1, prob_infeccion_expuesto)
infeccion[expuesto == 0] <- rbinom(sum(expuesto == 0), 1, prob_infeccion_no_expuesto)

# Data frame final
datos <- data.frame(
  Expuesto  = ifelse(expuesto == 1, "Si", "No"),
  Infeccion = ifelse(infeccion == 1, "Si", "No")
)

# Tabla 2x2
tabla <- table(datos$Expuesto, datos$Infeccion)
print(tabla)

# Se calcula el Riesgo Relativo (Wald)
RR <- riskratio.wald(tabla)
print(RR)
