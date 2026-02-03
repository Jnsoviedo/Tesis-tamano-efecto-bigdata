

        # Modelo lineal múltiple: betas estandarizados, R^2, R^2-ajustado

set.seed(1234)
n <- 100000

# Predictoras
superficie <- rnorm(n, mean = 100, sd = 30)
superficie[superficie < 0] <- abs(superficie[superficie < 0])

edad <- round(rnorm(n, mean = 20, sd = 10))
edad[edad < 0] <- abs(edad[edad < 0])

banos <- round(rpois(n, lambda = 2))
banos[banos == 0] <- 1

# Coeficientes para simular precio (miles de USD)
B0 <- 50
B1 <- 0.9
B2 <- -0.5
B3 <- 10

precio_sin_error <- B0 + B1 * superficie + B2 * edad + B3 * banos
precio_final <- precio_sin_error + rnorm(n, mean = 0, sd = 20)

datos <- data.frame(
  superficie = superficie,
  edad = edad,
  banos = banos,
  precio_k = precio_final
)

head(datos)

# Modelo lineal
modelo <- lm(precio_k ~ superficie + edad + banos, data = datos)
summary(modelo)

# Betas estandarizados (tamaño del efecto de coeficientes)
# install.packages("effectsize")  # correr si no se tiene
library(effectsize)

modelo_estandarizado <- standardize_parameters(modelo)
modelo_estandarizado

# R2 y R2 ajustado
resumen_modelo <- summary(modelo)
r_squared <- resumen_modelo$r.squared
adj_r_squared <- resumen_modelo$adj.r.squared

print(paste("R-squared:", round(r_squared, 4)))
print(paste("Adjusted R-squared:", round(adj_r_squared, 4)))

# Supuestos (solo lo esencial)
res <- residuals(modelo)
fit <- fitted(modelo)

# Guardado opcional de gráficos
guardar_figuras <- FALSE  # cambiar a TRUE si se desea guardar en output/

if (guardar_figuras) {
  dir.create("output", showWarnings = FALSE)
  dir.create("output/regresion_lineal", showWarnings = FALSE)
  png("output/regresion_lineal/diagnosticos_residuos.png", width = 1200, height = 500)
}

par(mfrow = c(1, 2))
hist(res, breaks = 30, main = "Histograma de residuos", xlab = "Residuos")
qqnorm(res, main = "QQ-plot de residuos"); qqline(res)

if (guardar_figuras) dev.off()

# Residuos vs ajustados
if (guardar_figuras) {
  png("output/regresion_lineal/residuos_vs_ajustados.png", width = 800, height = 600)
}

plot(fit, res,
     main = "Residuos vs Ajustados",
     xlab = "Valores ajustados",
     ylab = "Residuos")
abline(h = 0)

if (guardar_figuras) dev.off()

# Heterocedasticidad (Breusch-Pagan)
# install.packages("lmtest")  # correr si no se tiene
library(lmtest)

bptest(modelo)

# Multicolinealidad (VIF)
# install.packages("car")  # correr si no se tiene
library(car)

vif(modelo)

