# Omega cuadrado (ANOVA one-way)

set.seed(123)
n_por_grupo <- 20000
k_grupos <- 3
n <- n_por_grupo * k_grupos

# Factor (3 estrategias)
estrategia <- factor(rep(c("A", "B", "C"), each = n_por_grupo))

# Medias por estrategia (días activos)
mediaA <- 90
mediaB <- 92
mediaC <- 93

# Desviación estándar
sd_err <- 15

# Datos simulados
dias_A <- round(rnorm(n_por_grupo, mean = mediaA, sd = sd_err))
dias_B <- round(rnorm(n_por_grupo, mean = mediaB, sd = sd_err))
dias_C <- round(rnorm(n_por_grupo, mean = mediaC, sd = sd_err))

# Se limita a 0–180
dias_A <- pmin(pmax(dias_A, 0), 180)
dias_B <- pmin(pmax(dias_B, 0), 180)
dias_C <- pmin(pmax(dias_C, 0), 180)

dias_activos <- c(dias_A, dias_B, dias_C)

datos <- data.frame(estrategia, dias_activos)
summary(datos$dias_activos)

# ANOVA one-way
anova_model <- aov(dias_activos ~ estrategia, data = datos)
summary(anova_model)

# Se calcula omega^2
# install.packages("effectsize")  # correr si no se tiene
library(effectsize)

omega_out <- omega_squared(anova_model, ci = 0.95)
omega_out
as.data.frame(omega_out)

# Supuestos (solo lo esencial)
res <- residuals(anova_model)
fit <- fitted(anova_model)

# Guardado opcional de gráficos
guardar_figuras <- FALSE  # cambiar a TRUE si se desea guardar en output/

if (guardar_figuras) {
  dir.create("output", showWarnings = FALSE)
  dir.create("output/omega2_anova", showWarnings = FALSE)
  png("output/omega2_anova/diagnosticos_residuos.png", width = 1200, height = 500)
}

par(mfrow = c(1, 2))
plot(fit, res,
     main = "Residuos vs Ajustados",
     xlab = "Valores ajustados",
     ylab = "Residuos")
abline(h = 0)
qqnorm(res, main = "QQ-plot de residuos"); qqline(res)

if (guardar_figuras) dev.off()

# Homogeneidad de varianzas (Levene)
# install.packages("car")  # correr si no se tiene
library(car)

leveneTest(dias_activos ~ estrategia, data = datos, center = median)

# Boxplot por grupo (inspección práctica)
if (guardar_figuras) {
  png("output/omega2_anova/boxplot_grupos.png", width = 700, height = 600)
}

boxplot(dias_activos ~ estrategia, data = datos,
        main = "Días activos por estrategia",
        xlab = "Estrategia", ylab = "Días activos")

if (guardar_figuras) dev.off()

