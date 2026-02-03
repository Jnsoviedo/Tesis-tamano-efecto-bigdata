
                                        # w de Cohen

# Crear tabla 2x2
Tabla <- matrix(c(1289, 108,
                  909,  428), nrow = 2)

colnames(Tabla) <- c("Femenino", "Masculino")
rownames(Tabla) <- c("No", "Si")
Tabla

# La w de Cohen se puede calcular con varios paquetes.
# Aquí se hace con effectsize.
# install.packages("effectsize")  # correr si no se tiene
library(effectsize)

# Se calcula la w de Cohen 
cohens_w(Tabla, alternative = "two.sided")


