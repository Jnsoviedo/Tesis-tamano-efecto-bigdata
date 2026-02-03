# f de Cohen (ANOVA one-way)

set.seed(123)

# Número de participantes por dieta
n <- 8000

# Medias por dieta (pérdida de peso en kg)
media <- c(-5, -4, -6)
sd <- 2

# Datos simulados
Dieta_A <- round(rnorm(n, mean = media[1], sd = sd), digits = 2)
Dieta_B <- round(rnorm(n, mean = media[2], sd = sd), digits = 2)
Dieta_C <- round(rnorm(n, mean = media[3], sd = sd), digits = 2)

PerdidaPeso <- c(Dieta_A, Dieta_B, Dieta_C)
Dieta <- factor(rep(c("A", "B", "C"), each = n))
datos <- data.frame(PerdidaPeso, Dieta)

head(datos)

# ANOVA one-way
anova <- aov(PerdidaPeso ~ Dieta, data = datos)
summary(anova)

# Se calcula f de Cohen
# install.packages("effectsize")  # correr si no se tiene
library(effectsize)

cohens_f(anova)
as.data.frame(cohens_f(anova))

# Supuestos (solo lo esencial)
res <- residuals(anova)
fit <- fitted(anova)

# Guardado opcional de gráficos
guardar_figuras <- FALSE  # cambiar a TRUE si se desea guardar en output/

if (guardar_figuras) {
  dir.create("output", showWarnings = FALSE)
  dir.create("output/f_cohen_anova", showWarnings = FALSE)
}

# Boxplot por grupo
if (guardar_figuras) {
  png("output/f_cohen_anova/boxplot_grupos.png", width = 700, height = 600)
}

boxplot(PerdidaPeso ~ Dieta, data = datos,
        main = "Pérdida de peso por dieta", xlab = "Dieta", ylab = "Kg")

if (guardar_figuras) dev.off()

# Residuos vs ajustados + QQ-plot + histograma de residuos
if (guardar_figuras) {
  png("output/f_cohen_anova/diagnosticos_residuos.png", width = 1200, height = 500)
}

par(mfrow = c(1, 2))
plot(fit, res,
     main = "Residuos vs Ajustados",
     xlab = "Valores ajustados",
     ylab = "Residuos")
abline(h = 0)
qqnorm(res, main = "QQ-plot de residuos"); qqline(res)

if (guardar_figuras) dev.off()

if (guardar_figuras) {
  png("output/f_cohen_anova/histograma_residuos.png", width = 700, height = 600)
}

hist(res, breaks = 40, main = "Histograma de residuos", xlab = "Residuos")

if (guardar_figuras) dev.off()

# Homogeneidad de varianzas (Levene)
# install.packages("car")  # correr si no se tiene
library(car)

lev_out <- leveneTest(PerdidaPeso ~ Dieta, data = datos)
print(lev_out)

# Alternativa robusta (Welch)
welch_out <- oneway.test(PerdidaPeso ~ Dieta, data = datos, var.equal = FALSE)
print(welch_out)
