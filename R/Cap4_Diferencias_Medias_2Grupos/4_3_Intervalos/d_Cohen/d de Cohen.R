

                                  # d de Cohen

set.seed(123)

grupo_control      <- round(rnorm(5500, mean = 300, sd = 50), 2)
grupo_experimental <- round(rnorm(5500, mean = 270, sd = 45), 2)

datos <- data.frame(
  grupo = factor(rep(c("control", "experimental"), each = 5500)),
  tiempo_reaccion = c(grupo_control, grupo_experimental)
)

# install.packages("effsize")  # correr si no se tiene
library(effsize)

# Se calcula d de Cohen 
resultado <- effsize::cohen.d(tiempo_reaccion ~ grupo, data = datos)
print(resultado)
