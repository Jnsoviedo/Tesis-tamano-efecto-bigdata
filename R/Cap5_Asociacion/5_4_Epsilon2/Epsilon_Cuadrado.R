

                         # Epsilon cuadrado (ANOVA ona via)

set.seed(123)

# Planes de suscripción (tres grupos)
plan <- rep(c("Basico", "Estandar", "Premium"), each = 25000)

# Medias por plan (min/mes)
MediaBasico   <- 2000
MediaEstandar <- 2200
MediaPremium  <- 2300
sd <- 600

min_basico   <- round(rnorm(25000, mean = MediaBasico,   sd = sd))
min_estandar <- round(rnorm(25000, mean = MediaEstandar, sd = sd))
min_premium  <- round(rnorm(25000, mean = MediaPremium,  sd = sd))

minutos_visualizados <- c(min_basico, min_estandar, min_premium)

datos <- data.frame(plan, minutos_visualizados)
head(datos)

# ANOVA one-way
anova <- aov(minutos_visualizados ~ plan, data = datos)
summary(anova)

# Se calcula epsilon^2
# install.packages("effectsize")  # correr si no se tiene
library(effectsize)

epsilon_squared(anova)
as.data.frame(epsilon_squared(anova))

# Supuestos (solo lo esencial)
res <- residuals(anova)
fit <- fitted(anova)

# Guardado opcional de gráficos
guardar_figuras <- FALSE  # cambiar a TRUE si se desea guardar en output/

if (guardar_figuras) {
  dir.create("output", showWarnings = FALSE)
  dir.create("output/epsilon2_anova", showWarnings = FALSE)
  png("output/epsilon2_anova/diagnosticos_residuos.png", width = 1200, height = 500)
}

par(mfrow = c(1, 2))
hist(res, breaks = 40, main = "Histograma de residuos", xlab = "Residuos")
qqnorm(res, main = "QQ-plot de residuos"); qqline(res)

if (guardar_figuras) dev.off()

# Homogeneidad de varianzas (Levene)
# install.packages("car")  # correr si no se tiene
library(car)

lev <- leveneTest(minutos_visualizados ~ plan, data = datos)
print(lev)

# Boxplot por grupo
if (guardar_figuras) {
  png("output/epsilon2_anova/boxplot_grupos.png", width = 700, height = 600)
}

boxplot(minutos_visualizados ~ plan, data = datos,
        main = "Minutos visualizados por plan",
        xlab = "Plan", ylab = "Minutos/mes")

if (guardar_figuras) dev.off()

# Alternativa robusta (Welch)
welch <- oneway.test(minutos_visualizados ~ plan, data = datos, var.equal = FALSE)
print(welch)
