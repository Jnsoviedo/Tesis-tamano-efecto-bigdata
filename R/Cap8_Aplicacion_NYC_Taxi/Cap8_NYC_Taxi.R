# =============================================================================
# ANÁLISIS ESTADÍSTICO COMPLETO CON TAMAÑOS DEL EFECTO EN BIG DATA
# Dataset : NYC Yellow Taxi Trip Records 2024 — Datos Abiertos NYC
# Licencia: Dominio público (NYC Open Data Law). Sin restricciones éticas.
# Propósito: Demostrar que en Big Data el p-valor es insuficiente y debe
#            acompañarse del tamaño del efecto y su intervalo de confianza.
#
# POLÍTICA DE USO DE DATOS EN ESTE SCRIPT:
#   Se trabaja con LA POBLACIÓN COMPLETA en todos los análisis posibles.
#   Sólo se recurre a una muestra cuando el algoritmo tiene complejidad
#   computacional O(n²) o superior, lo que hace inviable su ejecución con
#   millones de registros. Cada excepción está documentada con su justificación.
# =============================================================================


# =============================================================================
# SECCIÓN 0: INSTALACIÓN Y CARGA DE PAQUETES
# =============================================================================
paquetes_necesarios <- c(
  "arrow",       # Leer Parquet sin cargar todo en RAM (modo lazy)
  "dplyr",       # Manipulación de datos
  "lubridate",   # Funciones de fecha/hora (hour(), etc.)
  "effectsize",  # Tamaños del efecto: d, g, eta2, omega2, phi, V, OR...
  "effsize",     # cliff.delta(), VD.A() — solo para muestras (O(n²))
  "rstatix",     # rank_biserial(), kruskal_effsize()
  "DescTools",   # OddsRatio(), GoodmanKruskalGamma()
  "ggplot2",     # Visualizaciones
  "patchwork",   # Combinar gráficos
  "scales",       # Formato de números en ejes
  "psych",      # phi()
  "ufs",        # cramersV(), confIntV()
  "epitools"    # oddsratio.wald(), riskratio.wald()
)

nuevos <- paquetes_necesarios[
  !paquetes_necesarios %in% installed.packages()[, "Package"]
]
if (length(nuevos) > 0) {
  message("Instalando paquetes faltantes: ", paste(nuevos, collapse = ", "))
  install.packages(nuevos, dependencies = TRUE)
}
invisible(lapply(paquetes_necesarios, library, character.only = TRUE))

options(
  OutDec = ".",   # decimal estándar
  scipen = 999    # evita notación científica
)
# =============================================================================
# SECCIÓN 1: CONTEXTO Y FORMULACIÓN DEL ESTUDIO
# =============================================================================
cat("
================================================================================
CONTEXTO DEL ESTUDIO
================================================================================
Título:
  ¿El tipo de pago influye en el comportamiento del viaje en taxi en Nueva York?
  Un análisis estadístico en Big Data con tamaños del efecto.

Descripción del problema:
  La Comisión de Taxis y Limusinas de Nueva York (NYC-TLC) registra cada viaje.
  Con más de 40 millones de registros anuales, este dataset es un caso
  paradigmático de Big Data.

  Se investiga si el TIPO DE PAGO (tarjeta vs. efectivo) está asociado con
  diferencias en tarifa, propina, distancia del viaje y número de pasajeros.

Fuente:
  NYC TLC Yellow Taxi Trip Records 2024
  URL: https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page
  Formato: Parquet | Licencia: Dominio público (NYC Open Data Law)
  Filas: ~40.296.805 registros anuales | Variables: 19 columnas

Variables seleccionadas:
  payment_cat    -> Tipo de pago         (nominal dicotómica: tarjeta/efectivo)
  tipped_cat     -> Recibió propina      (nominal dicotómica: sí/no)
  airport_cat    -> Viaje a aeropuerto   (nominal dicotómica: sí/no)
  passenger_count-> Número de pasajeros  (ordinal: 1 a 6)
  turno          -> Turno del día        (ordinal: madrugada/mañana/tarde/noche)
  fare_amount    -> Tarifa base en USD   (razón, continua)
  tip_amount     -> Propina en USD       (razón, continua)
  trip_distance  -> Distancia en millas  (razón, continua)
  total_amount   -> Monto total en USD   (razón, continua)
================================================================================
")


# =============================================================================
# SECCIÓN 2: DESCARGA Y CARGA DE DATOS
# =============================================================================

# --- 2.1 Descarga de archivos Parquet (3 meses ≈ 10 millones de viajes) -----
urls_taxi <- list(
  enero   = "https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2024-01.parquet",
  febrero = "https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2024-02.parquet",
  marzo   = "https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_2024-03.parquet"
)

dir.create("datos_taxi", showWarnings = FALSE)

for (mes in names(urls_taxi)) {
  ruta <- file.path("datos_taxi", paste0("yellow_2024_", mes, ".parquet"))

  # Si el archivo existe pero pesa menos de 5 MB, está corrupto: se elimina
  if (file.exists(ruta) && file.info(ruta)$size < 5242880) {
    message("Archivo de ", mes, " corrupto. Eliminando para redescargar...")
    unlink(ruta)
  }

  if (!file.exists(ruta)) {
    message("Descargando ", mes, "... (puede tardar varios minutos)")
    tryCatch(
      download.file(urls_taxi[[mes]], ruta, mode = "wb",
                    quiet = FALSE, method = "libcurl"),
      error = function(e) {
        message("Error en ", mes, ": ", e$message)
        if (file.exists(ruta)) unlink(ruta)
      }
    )
  } else {
    message("Archivo de ", mes, " ya existe y es válido.")
  }
}

# --- 2.2 Carga eficiente con Arrow (modo lazy: no carga todo en RAM) --------
message("\nAbriendo dataset con Arrow...")
taxi_ds <- open_dataset("datos_taxi/", format = "parquet")

# --- 2.3 Filtrado en Arrow + transformaciones en R --------------------------
# ESTRATEGIA: Arrow ejecuta el filtrado y la selección en disco (muy rápido).
# Las funciones de lubridate, case_when con factor y ordered sólo funcionan
# en R, por eso se aplican DESPUÉS de collect().
message("Filtrando (en Arrow) y transformando (en R)...")

taxi <- taxi_ds |>
  filter(
    fare_amount    >  0,  fare_amount    < 200,
    trip_distance  >  0,  trip_distance  <  50,
    tip_amount     >= 0,  tip_amount     < 100,
    passenger_count >= 1, passenger_count <= 6,
    payment_type %in% c(1L, 2L)   # 1 = tarjeta, 2 = efectivo
  ) |>
  select(
    fare_amount, tip_amount, trip_distance, total_amount,
    passenger_count, payment_type, RatecodeID, tpep_pickup_datetime
  ) |>
  collect() |>    # <-- Aquí se carga a RAM sólo lo filtrado/seleccionado
  mutate(
    # Variables nominales
    payment_cat  = factor(
      ifelse(payment_type == 1L, "tarjeta", "efectivo"),
      levels = c("efectivo", "tarjeta")
    ),
    tipped_cat   = factor(
      ifelse(tip_amount > 0, "con_propina", "sin_propina"),
      levels = c("sin_propina", "con_propina")
    ),
    airport_cat  = factor(
      ifelse(RatecodeID == 2L, "aeropuerto", "ciudad"),
      levels = c("ciudad", "aeropuerto")
    ),
    is_tipped    = as.integer(tip_amount > 0),
    airport_trip = as.integer(RatecodeID == 2L),

    # Variables ordinales
    pasajeros_f  = factor(passenger_count, levels = 1:6, ordered = TRUE),
    trip_hour    = lubridate::hour(tpep_pickup_datetime),
    turno        = factor(
      dplyr::case_when(
        lubridate::hour(tpep_pickup_datetime) >= 6  &
          lubridate::hour(tpep_pickup_datetime) < 12 ~ "mañana",
        lubridate::hour(tpep_pickup_datetime) >= 12 &
          lubridate::hour(tpep_pickup_datetime) < 18 ~ "tarde",
        lubridate::hour(tpep_pickup_datetime) >= 18 &
          lubridate::hour(tpep_pickup_datetime) < 24 ~ "noche",
        TRUE ~ "madrugada"
      ),
      levels  = c("madrugada", "mañana", "tarde", "noche"),
      ordered = TRUE
    )
  )

N_TOTAL <- nrow(taxi)
cat(sprintf("\nDatos listos: %s filas cargadas en memoria.\n",
            format(N_TOTAL, big.mark = ",", decimal.mark = ".")))

# Resumen de las variables clave
cat("\n--- Distribución: tipo de pago ---\n")
print(table(taxi$payment_cat))
cat("\n--- Distribución: propina ---\n")
print(table(taxi$tipped_cat))
cat("\n--- Distribución: pasajeros ---\n")
print(table(taxi$pasajeros_f))

str(taxi)
head(taxi, n = 6)


# =============================================================================
# SECCIÓN 3: DEMOSTRACIÓN CENTRAL — EL PROBLEMA DEL p-VALOR EN BIG DATA
# =============================================================================
# JUSTIFICACIÓN DEL MUESTREO EN ESTA SECCIÓN:
#   El objetivo aquí es EXPERIMENTAL: mostrar cómo cambia el p-valor y el
#   d de Cohen al variar el tamaño de n. Por definición, esto requiere extraer
#   submuestras de distintos tamaños. No es una limitación computacional, sino
#   el diseño del experimento demostrativo. La población completa se usa como
#   punto de anclaje final (último valor de tamanos_n). Se utilizan las hipotesis
#   H0: La tarifa media es igual entre viajes de 1 pasajero y 2 o más pasajeros.
#   mu(1 pasajero) = mu(2+ pasajeros)
#   H1: La tarifa media difiere entre viajes de 1 pasajero y 2 o más pasajeros.
#   mu(1 pasajero) ≠ mu(2+ pasajeros). Se usa una Prueba: t de Student bilateral 
#   para dos muestras independientes.

taxi_bin <- taxi |>
  mutate(pasajeros_bin = factor(
    ifelse(passenger_count == 1, "1 pasajero", "2+ pasajeros"),
    levels = c("1 pasajero", "2+ pasajeros")
  )) |>
  arrange(tpep_pickup_datetime, fare_amount, trip_distance)

tamanos_n2 <- c(30, 100, 500, 1000, 5000, 10000, 50000,
                100000, 500000, 1000000, N_TOTAL)

resultados_demo2 <- data.frame(
  n        = tamanos_n2,
  p_valor  = NA_real_,
  d_cohen  = NA_real_,
  magnitud = NA_character_,
  stringsAsFactors = FALSE
)

set.seed(2024)
for (i in seq_along(tamanos_n2)) {
  n_i     <- tamanos_n2[i]
  datos_i <- if (n_i >= N_TOTAL) taxi_bin else slice_sample(taxi_bin, n = n_i)
  tt_i    <- t.test(fare_amount ~ pasajeros_bin, data = datos_i)
  cd_i    <- effectsize::cohens_d(fare_amount ~ pasajeros_bin, data = datos_i)
  resultados_demo2$p_valor[i]  <- tt_i$p.value
  resultados_demo2$d_cohen[i]  <- abs(cd_i$Cohens_d)
  resultados_demo2$magnitud[i] <- as.character(
    effectsize::interpret_cohens_d(abs(cd_i$Cohens_d))
  )
}
cat(sprintf("%-20s | %-18s | %-12s | %-15s\n",
            "n", "p-valor", "|d de Cohen|", "Magnitud"))
cat(paste(rep("-", 72), collapse = ""), "\n")

for (i in seq_len(nrow(resultados_demo2))) {
  p_fmt <- ifelse(resultados_demo2$p_valor[i] < 1e-300, "< 1e-300",
                  ifelse(resultados_demo2$p_valor[i] < 0.001,
                         formatC(resultados_demo2$p_valor[i], format = "e", digits = 2),
                         round(resultados_demo2$p_valor[i], 4)))
  etiq_n <- ifelse(resultados_demo2$n[i] == N_TOTAL,
                   paste0(format(N_TOTAL, big.mark = ",", decimal.mark = "."),
                          " (TOTAL)"),
                   format(resultados_demo2$n[i], big.mark = ",",
                          decimal.mark = "."))
  cat(sprintf("%-20s | %-18s | %-12.4f | %-15s\n",
              etiq_n, p_fmt,
              resultados_demo2$d_cohen[i],
              resultados_demo2$magnitud[i]))
}
# --- Figura 1: demostración gráfica — tarifa ~ pasajeros (1 vs 2+) ---

# AJUSTE NECESARIO: con p < 1e-300, -log10(p) = Inf → hay que truncar
# para que la figura sea representable visualmente.
resultados_demo2$log_p <- pmin(-log10(resultados_demo2$p_valor), 300)

g1 <- ggplot(resultados_demo2, aes(x = n, y = log_p)) +
  geom_hline(yintercept = -log10(0.05), color = "red",
             linetype = "dashed", linewidth = 0.8) +
  geom_line(color = "#533ab7", linewidth = 1.2) +
  geom_point(color = "#533ab7", size = 2.5) +
  annotate("text", x = 5000, y = -log10(0.05) + 15,
           label = "p = 0.05", color = "red",
           size = 3.5, fontface = "italic") +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_continuous(
    breaks = c(0, 1.30, 50, 100, 200, 300),
    labels = c("0", "1.30\n(p=0.05)", "50", "100", "200", "≥300 (p≈0)")
  ) +
  labs(
    title = "A. El p-valor colapsa a cero con N grande",
    x     = "Tamaño de muestra (escala log)",
    y     = expression(-log[10](p))
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 10))

g2 <- ggplot(resultados_demo2, aes(x = n, y = d_cohen)) +
  geom_hline(yintercept = 0.2, color = "#e8903c", linetype = "dashed") +
  geom_hline(yintercept = 0.5, color = "red",     linetype = "dashed") +
  geom_hline(yintercept = 0.8, color = "darkred", linetype = "dashed") +
  geom_line(color = "#1d9e75", linewidth = 1.2) +
  geom_point(color = "#1d9e75", size = 2.5) +
  annotate("text", x = 200, y = 0.22, label = "pequeño (0.2)",
           color = "#e8903c", size = 3.2, fontface = "italic") +
  annotate("text", x = 200, y = 0.52, label = "mediano (0.5)",
           color = "red",     size = 3.2, fontface = "italic") +
  annotate("text", x = 200, y = 0.82, label = "grande (0.8)",
           color = "darkred", size = 3.2, fontface = "italic") +
  scale_x_log10(labels = scales::comma_format()) +
  coord_cartesian(ylim = c(0, 0.9)) +
  labs(
    title = "B. |d de Cohen|: converge a 0.146 (muy pequeño)",
    x     = "Tamaño de muestra (escala log)",
    y     = "|d de Cohen|"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 10))

fig1 <- (g1 / g2) 

fig1
#ggsave(
#  filename = "fig1_demo_bigdata.pdf",
#  plot     = fig1,
#  width    = 18,
#  height   = 14,
#  units    = "cm"
#)

# =============================================================================
# CAPÍTULO 4.1 — TAMAÑOS DEL EFECTO EN ESCALA NOMINAL
# =============================================================================


cat(sprintf("
================================================================================
CAPÍTULO 4.1 — ESCALA NOMINAL (N POBLACIONAL = %s)
================================================================================
VARIABLES:
  payment_cat (efectivo / tarjeta) × tipped_cat (sin_propina / con_propina)
================================================================================
", format(N_TOTAL, big.mark = ",", decimal.mark = ".")))

# --- Tabla de contingencia 2×2 (población completa) ------------------------
tabla_2x2 <- table(pago = taxi$payment_cat, propina = taxi$tipped_cat)

cat("\n--- Frecuencias observadas ---\n")
print(tabla_2x2, big.mark = ",", decimal.mark = ".")
cat("\n--- Porcentajes por fila (prob. condicional) ---\n")
print(round(prop.table(tabla_2x2, margin = 1) * 100, 2))

# Chi-cuadrado base (usado en todos los análisis nominales 2×2)
chi2_2x2 <- chisq.test(tabla_2x2, correct = FALSE)
cat(sprintf("\nChi-cuadrado(1) = %s, p < 2.2e-16\n",
            format(round(chi2_2x2$statistic, 2), big.mark = ",", decimal.mark = ".")))

# ─────────────────────────────────────────────────────────────────────────────
# 4.1.1 COEFICIENTE PHI (φ) — paquete: psych y paquete effectsize
# ─────────────────────────────────────────────────────────────────────────────
#library(psych)
# Tabla
phi_coef <- psych::phi(tabla_2x2, digits = 2)
print(phi_coef)

#Calculo Alternativo con el paquete effectsize
phi_ci     <- effectsize::phi(tabla_2x2, adjust = FALSE, ci = 0.95)
print(phi_ci)
# ─────────────────────────────────────────────────────────────────────────────
# 4.1.2 w DE COHEN — paquete: effectsize
# ─────────────────────────────────────────────────────────────────────────────

# w para tabla 2×2 (pago × propina)
w_2x2 <- effectsize::cohens_w(tabla_2x2,alternative = "two.sided", ci = 0.95)
print(w_2x2)

# Tabla 2×4 (pago × turno del día) — muestra utilidad de w en tablas r×c
tabla_pago_turno <- table(pago = taxi$payment_cat, turno = taxi$turno)
print(tabla_pago_turno, big.mark = ",", decimal.mark = ".")
chi2_pt          <- chisq.test(tabla_pago_turno, correct = FALSE)
print(chi2_pt)
w_2x4            <- effectsize::cohens_w(tabla_pago_turno, ci = 0.95)
print(w_2x4)

# ─────────────────────────────────────────────────────────────────────────────
# 4.1.3 V DE CRAMER — paquete: ufs
# ─────────────────────────────────────────────────────────────────────────────
#calculamos el valor de la V de Cramer, redondeamos a 2 digitos
v_cramer <- ufs::cramersV(tabla_pago_turno)
print(v_cramer)
# Intervalos de confianza 95% 
v_ci     <- ufs::confIntV(tabla_pago_turno)
print(v_ci)

# ─────────────────────────────────────────────────────────────────────────────
# 4.1.4 ODDS RATIO (OR) — cálculo manual
# ─────────────────────────────────────────────────────────────────────────────
cat("
--- 4.1.4 ODDS RATIO (OR) ---
HIPÓTESIS:
  H0: La probabilidad de dar propina es igual para tarjeta y efectivo (OR = 1)
  H1: La probabilidad difiere entre grupos (OR ≠ 1)
NOTA TÉCNICA: epitools::oddsratio.wald() produce overflow de enteros con
            N > 8 millones. Se usa cálculo manual con as.numeric() para
            convertir a doble precisión antes de multiplicar. El resultado
            es algebraicamente idéntico al método Wald de epitools.")

a_2x2 <- as.numeric(tabla_2x2[1, 1])  # efectivo × sin_propina  (a₁₁)
b_2x2 <- as.numeric(tabla_2x2[1, 2])  # efectivo × con_propina  (a₁₂)
c_2x2 <- as.numeric(tabla_2x2[2, 1])  # tarjeta  × sin_propina  (a₂₁)
d_2x2 <- as.numeric(tabla_2x2[2, 2])  # tarjeta  × con_propina  (a₂₂)

# fórmula, ecuación OR
or_val <- (a_2x2 * d_2x2) / (b_2x2 * c_2x2)
print(or_val)
# # Intervalos de confianza 95% método delta log-normal (Wald)
log_or    <- log(or_val)
se_log_or <- sqrt(1/a_2x2 + 1/b_2x2 + 1/c_2x2 + 1/d_2x2)
or_ic_inf <- exp(log_or - 1.96 * se_log_or)
or_ic_sup <- exp(log_or + 1.96 * se_log_or)
print(or_ic_inf)
print(or_ic_sup)

# ─────────────────────────────────────────────────────────────────────────────
# 4.1.5 Riesgo Relativo (RR) — cálculo manual
# ─────────────────────────────────────────────────────────────────────────────

cat("
--- 4.1.5 RIESGO RELATIVO (RR) ---
HIPÓTESIS:
  H0: La probabilidad de propina es igual en ambos grupos de pago (RR = 1)
  H1: La probabilidad difiere entre grupos (RR ≠ 1)")

# Probabilidades condicionales por fila
prop_filas      <- prop.table(tabla_2x2, margin = 1)
p_prop_tarjeta  <- prop_filas["tarjeta",  "con_propina"]
p_prop_efectivo <- prop_filas["efectivo", "con_propina"]

n_tarjeta  <- a_2x2 + b_2x2
n_efectivo <- c_2x2 + d_2x2

# RR = p_T / p_C — fórmula
rr_val <- p_prop_tarjeta / p_prop_efectivo
print(rr_val)
# Intervalos de confianza
se_rr     <- sqrt(p_prop_tarjeta  / n_tarjeta +
                    p_prop_efectivo / n_efectivo)
rr_ic_inf <- rr_val - 1.96 * se_rr
rr_ic_sup <- rr_val + 1.96 * se_rr

print(rr_ic_inf)
print(rr_ic_sup)

# ─────────────────────────────────────────────────────────────────────────────
# 4.1.6 Q DE YULE
# ─────────────────────────────────────────────────────────────────────────────
cat("
--- 4.1.6 Q DE YULE ---
HIPÓTESIS:
  H0: No existe asociación entre tipo de pago y propina (Q = 0)
  H1: Existe asociación entre las variables (Q ≠ 0)")

# Q de Yule — fórmula 
q_yule <- (a_2x2 * d_2x2 - b_2x2 * c_2x2) /
  (a_2x2 * d_2x2 + b_2x2 * c_2x2)
print(q_yule)

#Intervalos de confianza
se_q <- sqrt(0.25 * (1 - q_yule^2)^2 *
               (1/a_2x2 + 1/b_2x2 + 1/c_2x2 + 1/d_2x2))

q_ic_inf <- q_yule - 1.96 * se_q
q_ic_sup <- q_yule + 1.96 * se_q
print(q_ic_inf)
print(q_ic_sup)

# =============================================================================
# CAPÍTULO 4.2 — TAMAÑOS DEL EFECTO EN ESCALA ORDINAL
# =============================================================================
# =============================================================================
cat("
================================================================================
CAPÍTULO 4.2 — ESCALA ORDINAL
================================================================================
VARIABLES:
  - delta de Cliff (4.2.1), A de Vargha-Delaney (4.2.2) y r rank-biserial
    (4.2.4): passenger_count (1 vs. 2+ pasajeros) × fare_amount (tarifa,
    tratada como variable de respuesta continua con distribución asimétrica,
    sin asumir normalidad).
  - Gamma de Goodman-Kruskal (4.2.3): turno del día (ordinal: madrugada,
    mañana, tarde, noche) × fare_amount (tarifa). Mide asociación ordinal,
    no comparación de grupos.

HIPÓTESIS:
  - delta de Cliff, A de Vargha-Delaney, r rank-biserial:
    H0: No existe desplazamiento de localización entre las distribuciones
        de tarifa de viajes de 1 pasajero y 2 o más pasajeros.
    H1: Existe un desplazamiento de localización distinto de cero.
  - Gamma de Goodman-Kruskal:
    H0: gamma = 0 (no existe asociación ordinal entre turno y tarifa)
    H1: gamma != 0 (existe asociación ordinal entre turno y tarifa)
================================================================================
")

# Grupos para comparación 1 pasajero vs. 2+ pasajeros
g_1pas <- taxi$fare_amount[taxi$passenger_count == 1]
g_2mas <- taxi$fare_amount[taxi$passenger_count >= 2]

cat(sprintf("Grupo 1 pasajero   : N = %s, Mediana = $%.2f, Media = $%.2f\n",
            format(length(g_1pas), big.mark = ",", decimal.mark = "."), median(g_1pas), mean(g_1pas)))
cat(sprintf("Grupo 2+ pasajeros : N = %s, Mediana = $%.2f, Media = $%.2f\n",
            format(length(g_2mas), big.mark = ",", decimal.mark = "."), median(g_2mas), mean(g_2mas)))

# Prueba Mann-Whitney sobre la POBLACIÓN COMPLETA (O(n log n): factible)
wil_pob <- wilcox.test(g_1pas, g_2mas, exact = FALSE, correct = TRUE)
cat(sprintf("Mann-Whitney: W = %.0f, p = %s\n",
            wil_pob$statistic,
            ifelse(wil_pob$p.value < 2.2e-16, "< 2.2e-16", wil_pob$p.value)))

# ─────────────────────────────────────────────────────────────────────────────
# 4.2.1 DELTA DE CLIFF (δ) — REQUIERE MUESTRA
# ─────────────────────────────────────────────────────────────────────────────

N_CLIFF <- 40000   # ← corregido: evita overflow int32 en cliff.delta()
set.seed(101)
g1_m <- sample(g_1pas, N_CLIFF)
g2_m <- sample(g_2mas, N_CLIFF)
cliff_delta <- effsize::cliff.delta(g1_m, g2_m, conf.level = 0.95)
print(cliff_delta)
# ─────────────────────────────────────────────────────────────────────────────
# 4.2.2 A DE VARGHA-DELANEY — REQUIERE MUESTRA
# ─────────────────────────────────────────────────────────────────────────────
A_de_V<- effsize::VD.A(g1_m, g2_m)
print(A_de_V)
# Intervalos de confianza
A_ic_inf <- (cliff_delta$conf.int[1] + 1) / 2
A_ic_sup <- (cliff_delta$conf.int[2] + 1) / 2
print(A_ic_inf)
print(A_ic_sup)

# ─────────────────────────────────────────────────────────────────────────────
# 4.2.3 GAMMA DE GOODMAN-KRUSKAL (γ) — REQUIERE MUESTRA
# NOTA DE REPRODUCIBILIDAD:
#   El valor de gamma depende de la muestra aleatoria extraída. Aunque se
#   fija la semilla con set.seed(102), el resultado puede variar ligeramente
#   entre ejecuciones si el orden de las filas del dataset difiere (lo que
#   puede ocurrir al cargar datos desde Parquet en distintas sesiones).
#   El valor reportado en el trabajo (γ = 0.0028, IC [-0.0124, 0.0180],
#   z = 0.363, p = 0.716) fue obtenido con esta configuración.
#   En cualquier caso, la conclusión sustantiva no cambia: el IC incluye
#   el cero y no existe evidencia estadística suficiente para rechazar H0,
#   independientemente de la muestra específica extraída.

N_GAMMA <- 15000
set.seed(102)
muestra_gamma <- slice_sample(taxi, n = N_GAMMA)

gamma <- DescTools::GoodmanKruskalGamma(
  x          = as.integer(muestra_gamma$turno),
  y          = muestra_gamma$fare_amount,
  conf.level = 0.95
)
print(gamma)

# z y p derivados del IC (normal aproximado)
se_gamma <- (gamma["upr.ci"] - gamma["lwr.ci"]) / (2 * 1.96)
z_gamma  <- gamma["gamma"] / se_gamma
p_gamma  <- 2 * (1 - pnorm(abs(z_gamma)))
cat(sprintf("z = %.3f, p = %.3f\n", z_gamma, p_gamma))
# ─────────────────────────────────────────────────────────────────────────────
# 4.2.4 r RANK-BISERIAL (Wilcoxon) — POBLACIÓN COMPLETA
# ─────────────────────────────────────────────────────────────────────────────
cat("Calculando rank-biserial sobre la población completa...\n")

rank_bis <- effectsize::rank_biserial(
  fare_amount ~ payment_cat,
  data        = taxi,
  ci          = 0.95,
  alternative = "two.sided"
)
print(rank_bis)

# =============================================================================
# CAPÍTULO 4.3 — ESCALA DE INTERVALOS: d, g, Δ — POBLACIÓN COMPLETA
# =============================================================================
cat(sprintf("
================================================================================
CAPÍTULO 4.3 — ESCALA DE INTERVALOS (N POBLACIONAL = %s)
================================================================================
VARIABLES:
  turno (mañana / tarde) × fare_amount (tarifa USD).
HIPÓTESIS GENERAL:
  H0: mu(mañana) = mu(tarde)  →  diferencia estandarizada = 0
  H1: mu(mañana) ≠ mu(tarde)  →  diferencia estandarizada ≠ 0
TODAS LAS MEDIDAS SE CALCULAN SOBRE LA POBLACIÓN COMPLETA.
================================================================================
", format(N_TOTAL, big.mark = ",", decimal.mark = ".")))

# Filtrar solo mañana y tarde
taxi_2turnos <- taxi |>
  filter(turno %in% c("mañana", "tarde")) |>
  mutate(turno = droplevels(turno))  # elimina niveles vacíos

# Estadísticos descriptivos
desc_43 <- taxi_2turnos |>
  dplyr::group_by(turno) |>
  dplyr::summarise(
    n   = dplyr::n(),
    M   = mean(fare_amount),
    DE  = sd(fare_amount),
    Med = median(fare_amount),
    .groups = "drop"
  )
cat("\n--- Estadísticos descriptivos: tarifa por turno ---\n\n")
cat(sprintf("%-12s | %12s | %8s | %8s | %8s\n",
            "Grupo", "n", "Media $", "DE $", "Mediana $"))
cat(paste(rep("-", 58), collapse = ""), "\n")
for (i in seq_len(nrow(desc_43))) {
  cat(sprintf("%-12s | %12s | %8.2f | %8.2f | %8.2f\n",
              as.character(desc_43$turno[i]),
              format(desc_43$n[i], big.mark = ",", decimal.mark = "."),
              desc_43$M[i], desc_43$DE[i], desc_43$Med[i]))
}

# t-test de Welch — población completa
t_pob <- t.test(fare_amount ~ turno, data = taxi_2turnos)
t_pob

# d de Cohen
d_turno <- effectsize::cohens_d(fare_amount ~ turno,
                                data = taxi_2turnos)
print(d_turno)

# g de Hedges
g_turno <- effectsize::hedges_g(fare_amount ~ turno,
                                data = taxi_2turnos)
print(g_turno)

# delta de Glass
glass_turno <- effectsize::glass_delta(fare_amount ~ turno,
                                       data = taxi_2turnos)
print(glass_turno)

# =============================================================================
# CAPÍTULO 5 — MEDIDAS DE ASOCIACIÓN ANOVA — POBLACIÓN COMPLETA
# =============================================================================

cat(sprintf("
================================================================================
CAPÍTULO 5 — MEDIDAS DE ASOCIACIÓN ANOVA (N POBLACIONAL = %s)
================================================================================
VARIABLES:
  pasajeros_f (1 a 6 pasajeros, 6 grupos) × fare_amount (tarifa USD).
HIPÓTESIS:
  H0: ningún grupo difiere
  H1: Al menos un par de medias difiere.
TODAS LAS MEDIDAS SE CALCULAN SOBRE LA POBLACIÓN COMPLETA.
================================================================================
", format(N_TOTAL, big.mark = ",")))

# Estadísticos descriptivos por grupo
desc_5 <- taxi |>
  dplyr::group_by(pasajeros_f) |>
  dplyr::summarise(n = dplyr::n(), M = mean(fare_amount), DE = sd(fare_amount),
                   .groups = "drop")

# ANOVA sobre la población completa
aov_pob <- aov(fare_amount ~ pasajeros_f, data = taxi)
summary(aov_pob)

# =============================================================================
# 5.1.1 ETA CUADRADO
# =============================================================================
cat("
--- 5.1.1 ETA CUADRADO
HIPÓTESIS:
  H0: El factor pasajeros_f no explica varianza en la tarifa
  H1: El factor explica una proporción de la varianza total")

eta2_pob <- effectsize::eta_squared(aov_pob, partial = FALSE, ci = 0.95)
print(eta2_pob)

# =============================================================================
# 5.1.2 ETA CUADRADO PARCIAL
# =============================================================================
cat("
--- 5.1.2 ETA CUADRADO PARCIAL
HIPÓTESIS:
  H0: El factor no explica varianza neta del error
  H1: El factor explica una proporción de la varianza residual")

# En ANOVA de una vía partial = TRUE devuelve columna Eta2, no Eta2_partial
eta2p_pob <- effectsize::eta_squared(aov_pob, partial = TRUE, ci = 0.95)
print(eta2p_pob)

# =============================================================================
# 5.1.3 OMEGA CUADRADO 
# =============================================================================
cat("
--- 5.1.3 OMEGA CUADRADO
HIPÓTESIS:
  H0: El factor no explica varianza poblacional
  H1: El factor explica una proporción de la varianza poblacional")

omega2_pob <- effectsize::omega_squared(aov_pob, ci = 0.95)
print(omega2_pob)

# =============================================================================
# 5.1.4 ÉPSILON CUADRADO 
# =============================================================================
cat("
    --- 5.1.4 ÉPSILON CUADRADO
HIPÓTESIS:
  H0: El factor no explica varianza poblacional
  H1: El factor explica una proporción de la varianza poblacional")

eps2_pob <- effectsize::epsilon_squared(aov_pob, ci = 0.95)
print(eps2_pob)

# =============================================================================
# 5.1.5 f DE COHEN
# =============================================================================
cat("
--- 5.1.5 f DE COHEN ---
HIPÓTESIS:
  H0: La dispersión de medias grupales es cero relativa a la DE dentro
      de los grupos (f = 0)
  H1: Existe dispersión sistemática entre medias grupales (f > 0)")

f_pob <- effectsize::cohens_f(aov_pob, ci = 0.95)
print(f_pob)

# =============================================================================
# CAPÍTULO 6 — MODELOS LINEALES MÚLTIPLES — POBLACIÓN COMPLETA
# =============================================================================
# lm() con N millones es O(n × p^2): con 4 predictores es perfectamente viable.
# Puede tardar 1-3 minutos con N > 5 millones.

cat(sprintf("
================================================================================
CAPÍTULO 6 — MODELOS LINEALES MÚLTIPLES (N POBLACIONAL = %s)
================================================================================
VARIABLES RESPUESTA   : fare_amount (tarifa USD)
PREDICTORES           :
  trip_distance    — distancia del viaje (millas)
  passenger_count  — número de pasajeros
  trip_hour        — hora del viaje (0-23)
  payment_type     — tipo de pago (1=tarjeta, 2=efectivo)
HIPÓTESIS:
  H0: ningún predictor explica la tarifa
  H1: Al menos un predictor es diferente de cero
================================================================================
", format(N_TOTAL, big.mark = ",")))


# =============================================================================
# 6.1 CORRELACIÓN DE PEARSON (r) — POBLACIÓN COMPLETA
# =============================================================================
cat("
--- 6.1 CORRELACIÓN DE PEARSON (r) ---
HIPÓTESIS:
  H0: No existe correlación lineal entre distancia y tarifa (ρ = 0)
  H1: Existe correlación lineal (ρ ≠ 0)")

# Correlacion de Pearson: distancia x tarifa
corr_test <- cor.test(taxi$trip_distance,
                      taxi$fare_amount,
                      method = "pearson")
print(corr_test)

# Modelo lineal multiple
modelo_lm <- lm(
  fare_amount ~ trip_distance + passenger_count +
    trip_hour + payment_type,
  data = taxi)
sum_lm <- summary(modelo_lm)
print(sum_lm)

# R^2, R^2 ajustado y f^2 de Cohen
R2     <- sum_lm$r.squared
R2_adj <- sum_lm$adj.r.squared

# Efecto global
f2     <- R2 / (1 - R2)
cat(sprintf("R2 = %.4f | R2_adj = %.4f | f2 = %.4f\n",
            R2, R2_adj, f2))

# Coeficientes estandarizados
modelo_est <- effectsize::standardize_parameters(modelo_lm)
print(modelo_est, digits = 6)


# =============================================================================
# CAPÍTULO 7 — REGRESIÓN LOGÍSTICA BINOMIAL — POBLACIÓN COMPLETA
# =============================================================================
cat(sprintf("
================================================================================
CAPÍTULO 7 — REGRESIÓN LOGÍSTICA BINOMIAL (N POBLACIONAL = %s)
================================================================================
VARIABLE RESPUESTA: is_tipped (0 = sin propina, 1 = con propina)
PREDICTORES:
  trip_distance  — distancia del viaje (millas)
  fare_amount    — tarifa base (USD)
  trip_hour      — hora del viaje (0-23)
  airport_trip   — viaje al aeropuerto (0/1)
  payment_type   — tipo de pago (1=tarjeta, 2=efectivo)
HIPÓTESIS:
  H0: Ningún predictor está asociado con la probabilidad de propina (todos OR=1)
  H1: Al menos un predictor influye en la probabilidad de propina (OR ≠ 1)
================================================================================
", format(N_TOTAL, big.mark = ",")))


# Modelo logístico — población completa
modelo_logit <- glm(
  is_tipped ~ trip_distance + fare_amount +
    trip_hour + airport_trip + payment_type,
  data   = taxi,
  family = binomial(link = "logit"))
sum_logit <- summary(modelo_logit)
print(sum_logit)

# OR e Intervalos de confianza
or_cr <- exp(coef(modelo_logit))
or_ci     <- exp(confint.default(modelo_logit))
print(round(cbind(OR = or_cr, or_ci), 4))

# Coeficientes estandarizados LT y OE
b_log  <- coef(modelo_logit)[-1]
se_log <- sum_logit$coefficients[-1, 2]
sdX    <- sapply(taxi[, names(b_log)], sd)

eta_hat <- predict(modelo_logit, type = "link")
p_hat   <- predict(modelo_logit, type = "response")
var_eta <- var(eta_hat)

# Enfoque LT
sd_y_star <- sqrt(var_eta + (pi^2) / 3)
b_std_LT  <- b_log * (sdX / sd_y_star)
se_std_LT <- se_log * (sdX / sd_y_star)

# Enfoque OE
R2_OE    <- cor(taxi$is_tipped, p_hat)^2
sd_y_OE  <- sqrt(var_eta / R2_OE)
b_std_OE  <- b_log * (sdX / sd_y_OE)
se_std_OE <- se_log * (sdX / sd_y_OE)

# Tabla comparativa LT vs OE
resultados_std <- data.frame(
  b_LT  = round(b_std_LT, 4),
  li_LT = round(b_std_LT - 1.96 * se_std_LT, 4),
  ls_LT = round(b_std_LT + 1.96 * se_std_LT, 4),
  b_OE  = round(b_std_OE, 4),
  li_OE = round(b_std_OE - 1.96 * se_std_OE, 4),
  ls_OE = round(b_std_OE + 1.96 * se_std_OE, 4))
print(resultados_std)

# =============================================================================
# SECCIÓN FINAL — VERIFICACIÓN DE SUPUESTOS (SOLO LO ESENCIAL)
# =============================================================================
# NOTA METODOLÓGICA:
#   Con N > 8 millones, las pruebas formales de hipótesis (Breusch-Pagan)
#   rechazarán H0 para cualquier desviación mínima e irrelevante, reproduciendo
#   exactamente el problema del p-valor documentado en este trabajo. Se presentan
#   únicamente los diagnósticos esenciales: visuales de residuos y VIF.
#   Los gráficos se generan sobre submuestras de n = 5,000 para agilizar
#   la ejecución sin perder representatividad visual.
# =============================================================================

library(lmtest)   # bptest()
library(car)      # vif()
library(pROC)     # roc(), auc()

guardar_figuras <- FALSE  # cambiar a TRUE para guardar en output/

# ── CORRELACIÓN DE PEARSON: trip_distance × fare_amount ──────────────────────
cat("
================================================================================
SUPUESTOS — CORRELACIÓN DE PEARSON
NOTA: Gráfico sobre submuestra n = 5,000 por razones de visualización.
================================================================================
")

set.seed(200)
muestra_pearson <- taxi[sample(1:nrow(taxi), 5000), ]

if (guardar_figuras) {
  dir.create("output/supuestos_pearson", showWarnings = FALSE, recursive = TRUE)
  png("output/supuestos_pearson/dispersion.png", width = 800, height = 600)
}
plot(muestra_pearson$trip_distance, muestra_pearson$fare_amount,
     pch = ".", col = rgb(0, 0, 1, 0.3),
     main = "Dispersión: distancia vs tarifa (n = 5,000)",
     xlab = "Distancia (millas)", ylab = "Tarifa (USD)")
abline(lm(fare_amount ~ trip_distance, data = muestra_pearson),
       col = "red")
if (guardar_figuras) dev.off()

# ── MODELO LINEAL MÚLTIPLE ────────────────────────────────────────────────────
cat("
================================================================================
SUPUESTOS — MODELO LINEAL MÚLTIPLE
NOTA: Diagnósticos visuales sobre submuestra n = 5,000.
================================================================================
")

res_lm <- residuals(modelo_lm)

# Submuestra para gráficos
set.seed(300)
idx_res    <- sample(1:length(res_lm), 5000)
res_lm_sub <- res_lm[idx_res]

if (guardar_figuras) {
  dir.create("output/supuestos_lm", showWarnings = FALSE, recursive = TRUE)
  png("output/supuestos_lm/diagnosticos.png", width = 1200, height = 500)
}
par(mfrow = c(1, 2))
hist(res_lm_sub, breaks = 50,
     main = "Histograma de residuos (n = 5,000)",
     xlab = "Residuos", col = "steelblue", border = "white")
qqnorm(res_lm_sub, main = "QQ-plot de residuos (n = 5,000)")
qqline(res_lm_sub, col = "red")
par(mfrow = c(1, 1))
if (guardar_figuras) dev.off()

# Breusch-Pagan (sobre modelo completo — es rápido)
cat("
--- Breusch-Pagan (homocedasticidad) ---
ADVERTENCIA: Con N > 8 millones este test rechazará H0 para cualquier
desviación mínima. Interpretar con cautela junto al diagnóstico visual.
")
print(bptest(modelo_lm))

# VIF (sobre modelo completo — es rápido)
cat("
--- VIF (multicolinealidad) — modelo lineal ---
Criterio: < 5 sin problema | 5-10 moderado | > 10 grave
")
print(vif(modelo_lm))

# ── MODELO LOGÍSTICO BINOMIAL ─────────────────────────────────────────────────
cat("
================================================================================
SUPUESTOS — MODELO LOGÍSTICO BINOMIAL
================================================================================
")

# Convergencia
cat(sprintf("Modelo convergido: %s\n", modelo_logit$converged))

# VIF
cat("
--- VIF (multicolinealidad) — modelo logístico ---
Criterio: < 5 sin problema | 5-10 moderado | > 10 grave
NOTA: Los valores elevados de trip_distance (14.85) y fare_amount (13.69)
reflejan la alta correlación entre ambas variables (r = 0.9494, documentada
en el análisis de correlación de Pearson). Esta multicolinealidad afecta
la estabilidad individual de los coeficientes pero no invalida las
predicciones del modelo ni el AUC = 0.9065. En el contexto de este trabajo,
el objetivo del modelo logístico es ilustrar los coeficientes estandarizados
beta* como medida de tamaño del efecto, no la inferencia individual de
cada predictor.
")
print(vif(modelo_logit))

# AUC y curva ROC
cat("
--- AUC y curva ROC ---
NOTA: El AUC es una medida de discriminación del modelo. No es una prueba
de hipótesis y no se ve afectada por el tamaño muestral.
")
roc_obj <- roc(response  = taxi$is_tipped,
               predictor = p_hat,
               quiet     = TRUE)
cat(sprintf("\nAUC = %.4f\n", auc(roc_obj)))

if (guardar_figuras) {
  dir.create("output/supuestos_logit", showWarnings = FALSE, recursive = TRUE)
  png("output/supuestos_logit/curva_roc.png", width = 800, height = 600)
}
plot(roc_obj,
     main = "Curva ROC — modelo logístico",
     col  = "#533ab7", lwd = 2)
legend("bottomright",
       legend = sprintf("AUC = %.4f", auc(roc_obj)),
       col = "#533ab7", lwd = 2, bty = "n")
if (guardar_figuras) dev.off()

