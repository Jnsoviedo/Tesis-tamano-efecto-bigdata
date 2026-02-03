
                              # Coeficiente Phi

# install.packages("psych")  # correr si no lo tienes
library(psych)

set.seed(33)

# Tamaño de muestra
n_total <- 11000
n_masculino <- 5516
n_femenino <- 5484  # n_masculino + n_femenino = n_total

# Variable Sexo
Sexo <- factor(c(rep("Masculino", n_masculino),
                 rep("Femenino",  n_femenino)))

# Variable Dejar_Fumar (asociación positiva: mayor % en mujeres)
prob_dejar_masculino <- 0.3
prob_dejar_femenino  <- 0.5

Dejar_Fumar <- factor(c(
  sample(c("Si", "No"), size = n_masculino, replace = TRUE,
         prob = c(prob_dejar_masculino, 1 - prob_dejar_masculino)),
  sample(c("Si", "No"), size = n_femenino, replace = TRUE,
         prob = c(prob_dejar_femenino, 1 - prob_dejar_femenino))
), levels = c("Si", "No"))  # dejo "Si" como primer nivel

datos <- data.frame(Sexo, Dejar_Fumar)

# Tabla
tabla <- table(datos$Sexo, datos$Dejar_Fumar)
print(tabla)

# Coeficiente Phi
phi(tabla, digits = 2)

