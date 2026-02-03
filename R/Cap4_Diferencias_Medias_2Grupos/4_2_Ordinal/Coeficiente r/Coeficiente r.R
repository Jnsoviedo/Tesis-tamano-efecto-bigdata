# Wilcoxon (Mann–Whitney) y tamaño del efecto r

# install.packages(c("dplyr", "rstatix"))  # correr si no se tiene
library(dplyr)
library(rstatix)

set.seed(1234)

N  <- 15000
n1 <- N / 2  # control
n2 <- N / 2  # intervención

# Simulación (días/semana)
control      <- round(rnorm(n1, mean = 3, sd = 1))
intervencion <- round(rnorm(n2, mean = 5, sd = 1))

# Se acota a 0-7
control      <- pmin(pmax(control, 0), 7)
intervencion <- pmin(pmax(intervencion, 0), 7)

datos <- data.frame(
  id        = 1:N,
  grupo     = factor(c(rep("control", n1), rep("intervencion", n2)),
                     levels = c("control", "intervencion")),
  ejercicio = c(control, intervencion)
)

head(datos)

# Prueba Wilcoxon rank-sum / Mann–Whitney (independientes)
wilcox_res <- wilcox.test(ejercicio ~ grupo, data = datos, exact = FALSE)
wilcox_res

# Tamaño del efecto r (Wilcoxon) con IC (bootstrap)
eff_r <- datos %>%
  wilcox_effsize(ejercicio ~ grupo, paired = FALSE,
                 ci = TRUE, conf.level = 0.95, nboot = 200)

eff_r
