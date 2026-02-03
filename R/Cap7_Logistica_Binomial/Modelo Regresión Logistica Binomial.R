# Regresión logística binomial: tamaños del efecto estandarizados (LT y OE)

set.seed(12345)

# Tamaño de muestra
n <- 200000

# Predictoras (escala original)
edad <- rnorm(n, mean = 40, sd = 12)
uso_diario <- rgamma(n, shape = 2, scale = 1.5)      # horas/día (positiva)
puntos_fidelidad <- rpois(n, lambda = 20)            # conteo
descuento_recibido <- rbinom(n, size = 1, prob = 0.3)

# Centrado (para intercepto interpretable)
edad_c <- edad - 40
uso_c <- uso_diario - 3
puntos_c <- puntos_fidelidad - 20

# Ecuación generadora (logit) para simular abandono
eta <- -2.5 +
  0.015 * edad_c +
  -0.40 * uso_c +
  -0.03 * puntos_c +
  -0.70 * descuento_recibido

# Probabilidad y variable dependiente binaria
pi_prob <- 1 / (1 + exp(-eta))
abandono <- rbinom(n, size = 1, prob = pi_prob)

# Data frame final
datos <- data.frame(
  abandono,
  edad_c,
  uso_c,
  puntos_c,
  descuento_recibido
)

head(datos)

# Modelo logístico (link logit)
modelo_logit <- glm(
  abandono ~ edad_c + uso_c + puntos_c + descuento_recibido,
  data = datos,
  family = binomial(link = "logit")
)

summary(modelo_logit)

# ==============================
# Coeficientes estandarizados (LT y OE)
# ==============================

# Coeficientes y errores estándar (sin intercepto)
b <- coef(modelo_logit)[-1]
se <- summary(modelo_logit)$coefficients[-1, 2]

# Desv. estándar de predictores
sdX <- sapply(datos[, names(b)], sd)

# Predicción en escala link
eta_hat <- predict(modelo_logit, type = "link")
var_eta <- var(eta_hat)

# ---- LT (Latente-Teórico): Var(eps) = pi^2/3 para logit ----
sd_y_star <- sqrt(var_eta + (pi^2) / 3)

b_std_LT <- b * (sdX / sd_y_star)
se_std_LT <- se * (sdX / sd_y_star)

IC_LT <- cbind(
  Est = b_std_LT,
  LI = b_std_LT - 1.96 * se_std_LT,
  LS = b_std_LT + 1.96 * se_std_LT
)

# ---- OE (Empírico-Observado): basado en R2 observacional ----
R2_OE <- cor(datos$abandono, p_hat)^2
sd_y_OE <- sqrt(var_eta / R2_OE)

b_std_OE <- b * (sdX / sd_y_OE)
se_std_OE <- se * (sdX / sd_y_OE)

IC_OE <- cbind(
  Est = b_std_OE,
  LI = b_std_OE - 1.96 * se_std_OE,
  LS = b_std_OE + 1.96 * se_std_OE
)

# Tabla resumen (LT vs OE)
tabla_std <- data.frame(
  Parametro = names(b),
  Std_LT = b_std_LT,
  IC95_LT = paste0("[", round(IC_LT[, "LI"], 3), ", ", round(IC_LT[, "LS"], 3), "]"),
  Std_OE = b_std_OE,
  IC95_OE = paste0("[", round(IC_OE[, "LI"], 3), ", ", round(IC_OE[, "LS"], 3), "]")
)

tabla_std

# Diagnóstico básico 
modelo_logit$converged

p_hat <- predict(modelo_logit, type = "response")
range(p_hat)

# Verificación de probabilidades extremas (cerca de 0 o 1)
eps <- 1e-6
casi_cero <- sum(p_hat < eps)
casi_uno  <- sum(p_hat > (1 - eps))
c(casi_cero = casi_cero, casi_uno = casi_uno)

# AUC y curva ROC
# install.packages("pROC")  # correr si no se tiene
library(pROC)

roc_obj <- roc(response = datos$abandono, predictor = p_hat, quiet = TRUE)
auc_val <- auc(roc_obj)
auc_val

# Curva ROC (opcional guardarla)
guardar_figuras <- FALSE  # cambiar a TRUE si se desea guardar en output/

if (guardar_figuras) {
  dir.create("output", showWarnings = FALSE)
  dir.create("output/logit_auc_roc", showWarnings = FALSE)
  png("output/logit_auc_roc/curva_roc.png", width = 800, height = 600)
}

plot(roc_obj, main = "Curva ROC")

if (guardar_figuras) dev.off()

