

                            # Correlación de Pearson (r)

set.seed(123)

n <- 100000

# Ingesta de azúcar (g/día), truncada a valores no negativos
azucar <- round(rnorm(n, mean = 70, sd = 20), digits = 2)
azucar[azucar < 0] <- 0

# Variable latente para inducir correlación con azúcar
azucar_std <- scale(azucar)
alpha <- 0.25
z <- alpha * as.vector(azucar_std) + rnorm(n, mean = 0, sd = sqrt(1 - alpha^2))

# Glucosa en ayunas (mg/dL)
glucosa <- round((100 + 15 * z), digits = 2)

datos <- data.frame(azucar, glucosa)
head(datos)

# Correlación de Pearson y prueba
corr_test <- cor.test(datos$azucar, datos$glucosa, method = "pearson")
corr_test

# Supuestos / diagnóstico 
# install.packages("ggplot2")  # correr si no se tiene
library(ggplot2)
# Se usa una muestra para los gráficos por tamaño muestral
muestra <- datos[sample(1:n, 1000), ]

# Guardado opcional de gráficos
guardar_figuras <- FALSE  # cambiar a TRUE si se desea guardar en output/

if (guardar_figuras) {
  dir.create("output", showWarnings = FALSE)
  dir.create("output/pearson_r", showWarnings = FALSE)
}

# Linealidad: dispersión + línea de regresión
p_disp <- ggplot(muestra, aes(x = azucar, y = glucosa)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Dispersión: azúcar vs glucosa",
    x = "Ingesta diaria de azúcar (g)",
    y = "Glucosa en ayunas (mg/dL)"
  ) +
  theme_minimal()

p_disp

if (guardar_figuras) {
  ggsave("output/pearson_r/dispersion.png", plot = p_disp, width = 7, height = 5, dpi = 150)
}

# Homocedasticidad (orientativo): residuos vs ajustados del modelo lineal
modelo <- lm(glucosa ~ azucar, data = datos)
residuos <- resid(modelo)

p_res <- ggplot(data = datos, aes(x = fitted(modelo), y = residuos)) +
  geom_point(alpha = 0.05) +
  geom_hline(yintercept = 0) +
  labs(
    title = "Residuos vs ajustados",
    x = "Valores ajustados",
    y = "Residuos"
  ) +
  theme_minimal()

p_res

if (guardar_figuras) {
  ggsave("output/pearson_r/residuos_vs_ajustados.png", plot = p_res, width = 7, height = 5, dpi = 150)
}

# Normalidad aproximada (orientativo): QQ-plots en submuestra
idx <- sample.int(n, size = 5000)
azucar_sub <- datos$azucar[idx]
glucosa_sub <- datos$glucosa[idx]

if (guardar_figuras) {
  png("output/pearson_r/qqplots.png", width = 1200, height = 500)
}

par(mfrow = c(1, 2))
qqnorm(azucar_sub, main = "QQ-plot: azúcar"); qqline(azucar_sub)
qqnorm(glucosa_sub, main = "QQ-plot: glucosa"); qqline(glucosa_sub)
par(mfrow = c(1, 1))

if (guardar_figuras) dev.off()

# Outliers (orientativo): boxplots en muestra
p_box_az <- ggplot(muestra, aes(y = azucar)) +
  geom_boxplot() +
  labs(title = "Boxplot: azúcar", y = "Azúcar (g/día)") +
  theme_minimal()

p_box_gl <- ggplot(muestra, aes(y = glucosa)) +
  geom_boxplot() +
  labs(title = "Boxplot: glucosa", y = "Glucosa (mg/dL)") +
  theme_minimal()

p_box_az
p_box_gl

if (guardar_figuras) {
  ggsave("output/pearson_r/boxplot_azucar.png", plot = p_box_az, width = 5, height = 5, dpi = 150)
  ggsave("output/pearson_r/boxplot_glucosa.png", plot = p_box_gl, width = 5, height = 5, dpi = 150)
}
