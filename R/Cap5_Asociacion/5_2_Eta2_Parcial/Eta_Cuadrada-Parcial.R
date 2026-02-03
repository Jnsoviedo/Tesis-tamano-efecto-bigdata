

                          # Eta cuadrado parcial (ANOVA dos vías)

set.seed(123)

# Factores
Edad_grupo <- rep(c("Joven", "Mayor"), each = 30000)
Formacion <- rep(rep(c("Basica", "Intermedia", "Avanzada"), each = 10000), 2)

# Medias por combinación
media <- c(70, 70.5, 71, 69.3, 69.8, 70)
sd <- 10

# Variable respuesta
rendimiento <- c(
  rnorm(10000, mean = media[1], sd = sd),
  rnorm(10000, mean = media[2], sd = sd),
  rnorm(10000, mean = media[3], sd = sd),
  rnorm(10000, mean = media[4], sd = sd),
  rnorm(10000, mean = media[5], sd = sd),
  rnorm(10000, mean = media[6], sd = sd)
)

datos <- data.frame(Edad_grupo, Formacion, rendimiento)
head(datos)

# ANOVA de dos vías con interacción
anova <- aov(rendimiento ~ Edad_grupo * Formacion, data = datos)
summary(anova)

# Se calcula eta^2 parcial
# install.packages("effectsize")  # correr si no se tiene
library(effectsize)

eta_squared(anova, partial = TRUE)

# Supuestos
res <- residuals(anova)
fit <- fitted(anova)

# Guardado opcional de gráficos
guardar_figuras <- FALSE  # cambiar a TRUE si se desea guardar en output/

if (guardar_figuras) {
  dir.create("output", showWarnings = FALSE)
  dir.create("output/eta2_parcial_anova_2vias", showWarnings = FALSE)
  png("output/eta2_parcial_anova_2vias/diagnosticos_residuos.png", width = 1200, height = 500)
}

par(mfrow = c(1, 2))
hist(res, breaks = 40, main = "Histograma de residuos", xlab = "Residuos")
qqnorm(res, main = "QQ-plot de residuos"); qqline(res)

if (guardar_figuras) dev.off()

# Homogeneidad de varianzas (Levene)
# install.packages("car")  # correr si no se tiene
library(car)

lev <- leveneTest(rendimiento ~ Edad_grupo * Formacion, data = datos)
print(lev)

# Residuos vs ajustados
if (guardar_figuras) {
  png("output/eta2_parcial_anova_2vias/residuos_vs_ajustados.png", width = 800, height = 600)
}

par(mfrow = c(1, 1))
plot(fit, res,
     main = "Residuos vs Ajustados",
     xlab = "Valores ajustados",
     ylab = "Residuos")
abline(h = 0)

if (guardar_figuras) dev.off()
