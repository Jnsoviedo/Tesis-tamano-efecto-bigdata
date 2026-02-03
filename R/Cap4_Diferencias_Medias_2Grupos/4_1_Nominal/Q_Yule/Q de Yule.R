
                                  
                                     # Q de Yule

# install.packages("DescTools")  # correr si no se tiene
library(DescTools)

# Tabla de contingencia 2x2
tabla_contingencia <- matrix(c(1609, 10647,
                               1478, 12210),
                             nrow = 2, byrow = TRUE,
                             dimnames = list(
                               "Sexo" = c("Hombre", "Mujer"),
                               "Posición ideológica" = c("Izquierda", "Otra")
                             ))
print(tabla_contingencia)

# Se calcula Q de Yule
Q_yule <- YuleQ(tabla_contingencia)
print(Q_yule)

# Función para convertir OR e IC a Q de Yule e IC
yuleQ_CI <- function(or, or_lower, or_upper) {
  transform_or_to_q <- function(x) (x - 1) / (x + 1)
  
  Q       <- transform_or_to_q(or)
  Q_lower <- transform_or_to_q(or_lower)
  Q_upper <- transform_or_to_q(or_upper)
  
  list(Q = Q,
       CI = c(Q_lower, Q_upper))
}

# Se calcula OR y su intervalo, para transformar a Q con IC
# install.packages("epitools")  # correr si no se tiene
library(epitools)

res <- oddsratio(tabla_contingencia)
res

OR_value <- res$measure["Mujer", "estimate"]
OR_lower <- res$measure["Mujer", "lower"]
OR_upper <- res$measure["Mujer", "upper"]

OR_value
OR_lower
OR_upper

Intervalo_Conf <- yuleQ_CI(OR_value, OR_lower, OR_upper)
Intervalo_Conf

