
                          # Odds Ratio

# install.packages("epitools")  # correr si no se tiene
library(epitools)

# Número de registros
n <- 10000  # 10,000 datos

# Simulación
set.seed(42)  # reproducibilidad

vacuna <- sample(c("Yes", "No"), n, replace = TRUE)
hospitalizado <- sample(c("Yes", "No"), n, replace = TRUE)

data <- data.frame(vacuna, hospitalizado)

# Vista rápida
head(data)

# Tabla de contingencia
tabla_h <- table(data$vacuna, data$hospitalizado)
print(tabla_h)

# Odds Ratio (Wald)
odds_ratio_hospitalizacion <- oddsratio.wald(tabla_h)
print(odds_ratio_hospitalizacion)
