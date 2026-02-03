# Gamma de Goodman y Kruskal

# install.packages("DescTools")  # correr si no se tiene
library(DescTools)

set.seed(123)

# Datos simulados (escala 1-5)
X <- sample(1:5, 19000, replace = TRUE)
Y <- sample(1:5, 19000, replace = TRUE)

# Tabla de contingencia
tabla_contingencia <- table(X, Y)
print(tabla_contingencia)

# Se calcula Gamma de Goodman y Kruskal
gamma_KG <- GoodmanKruskalGamma(tabla_contingencia, conf.level = TRUE)
print(gamma_KG)

