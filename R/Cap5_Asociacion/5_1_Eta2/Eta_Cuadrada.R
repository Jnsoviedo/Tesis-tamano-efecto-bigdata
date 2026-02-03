# Eta cuadrado (ANOVA one-way)

set.seed(123)
n_g <- 10000

# Simulación (3 diseños)
A <- rnorm(n_g, mean = 20.0, sd = 5)
B <- rnorm(n_g, mean = 20.3, sd = 5)
C <- rnorm(n_g, mean = 20.1, sd = 5)

tiempo <- c(A, B, C)
diseno <- factor(rep(c("A", "B", "C"), each = n_g))
datos <- data.frame(tiempo, diseno)

head(datos)

# ANOVA one-way
modelo <- aov(tiempo ~ diseno, data = datos)
summary(modelo)

# Se calcula eta^2 (IC bilateral)
# install.packages("effectsize")  # correr si no se tiene
library(effectsize)

as.data.frame(eta_squared(modelo, alternative = "two.sided"))

# Supuestos 
res <- residuals(modelo)

# Guardado opcional de gráficos
guardar_figuras <- FALSE  # cambiar a TRUE si se desea guardar en output/

if (guardar_figuras) {
  dir.create("output", showWarnings = FALSE)
  dir.create("output/eta2_anova", showWarnings = FALSE)
  png("output/eta2_anova/diagnosticos_residuos.png", width = 1200, height = 500)
}

par(mfrow = c(1, 2))
hist(res, breaks = 50, main = "Histograma de residuos", xlab = "Residuos")
qqnorm(res, main = "QQ-plot de residuos"); qqline(res)

if (guardar_figuras) dev.off()

# Homogeneidad de varianzas (Levene)
# install.packages("car")  # correr si no se tiene
library(car)

lev <- leveneTest(tiempo ~ diseno, data = datos)
print(lev)

# Boxplot por grupo
if (guardar_figuras) {
  png("output/eta2_anova/boxplot_grupos.png", width = 700, height = 600)
}

boxplot(tiempo ~ diseno, data = datos,
        main = "Tiempo por diseño", xlab = "Diseño", ylab = "Tiempo")

if (guardar_figuras) dev.off()

# Alternativa robusta (Welch)
welch <- oneway.test(tiempo ~ diseno, data = datos, var.equal = FALSE)
print(welch)
