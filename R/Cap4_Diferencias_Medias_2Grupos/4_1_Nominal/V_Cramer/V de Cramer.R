
                                       # V de Cramér

# Tabla 3x3
Tabla <- matrix(c(1024, 108, 404,
                  511,  864, 615,
                  523,  287, 723), nrow = 3)

colnames(Tabla) <- c("Vigoroso", "Moderado", "Leve")
rownames(Tabla) <- c("Mestizo", "Afro", "Indígena")
Tabla

# Aquí se hace el calculo de la V de Cramer con el paquete ufs.
# install.packages("ufs")  # correr si no se tiene
library(ufs)

# Se calcula V de Cramér
cramersV(Tabla)

# Intervalo de confianza para V
confIntV(Tabla)

