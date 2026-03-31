rm(list = ls())
library(readr)
library(dplyr)
library(tidyr)

# ── IMPORTAR ──────────────────────────────────────────────────────────────────
fuerzat_mayo  <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Mayo 2025/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
fuerzat_junio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Junio 2025/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
fuerzat_julio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Julio 2025/CSV/Fuerza de trabajo.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

no_ocup_mayo  <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Mayo 2025/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocup_junio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Junio 2025/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
no_ocup_julio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Julio 2025/CSV/No ocupados.CSV",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

ocupados_mayo  <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Mayo 2025/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_junio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Junio 2025/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_julio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Julio 2025/CSV/Ocupados.CSV",
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

caract_gral_mayo  <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Mayo 2025/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
caract_gral_junio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Junio 2025/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
caract_gral_julio <- read_delim("C:/Users/USUARIO/OneDrive/Documentos/7mo/Estadistica II/Analisis-de-Datos/datos/Julio 2025/CSV/Características generales, seguridad social en salud y educación.CSV",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

fuerzat <- bind_rows(fuerzat_mayo, fuerzat_junio, fuerzat_julio)
no_ocup <- bind_rows(no_ocup_mayo, no_ocup_junio, no_ocup_julio)

ocupados_mayo$INGLABO  <- as.numeric(ocupados_mayo$INGLABO)
ocupados_junio$INGLABO <- as.numeric(ocupados_junio$INGLABO)
ocupados_julio$INGLABO <- as.numeric(ocupados_julio$INGLABO)
ocupados <- bind_rows(ocupados_mayo, ocupados_junio, ocupados_julio)

caract_grales <- bind_rows(caract_gral_mayo, caract_gral_junio, caract_gral_julio)

# ── VARIABLE CUALITATIVA  ─────────────────────────────────────────────────────────────────────────────────
caract_grales <- caract_grales %>%
  mutate(nivel_educacion = case_when(
    P3042 == 1                 ~ "Sin educación",
    P3042 %in% c(2, 3)        ~ "Primaria",
    P3042 %in% c(4, 5, 6, 7)  ~ "Secundaria",
    P3042 == 8                 ~ "Técnico",
    P3042 == 9                 ~ "Tecnólogo",
    P3042 %in% c(10,11,12,13) ~ "Profesional",
    P3042 == 99                ~ NA_character_
  )) %>%
  mutate(nivel_educacion = factor(nivel_educacion,
                                  levels = c("Sin educación", "Primaria",
                                             "Secundaria", "Técnico",
                                             "Tecnólogo", "Profesional"),
                                  ordered = TRUE))

edu_directorio <- caract_grales %>%
  group_by(DIRECTORIO) %>%
  summarise(
    nivel_educacion = names(which.max(table(nivel_educacion, useNA = "no"))),
    .groups = 'drop'
  ) %>%
  mutate(nivel_educacion = factor(nivel_educacion,
                                  levels = c("Sin educación", "Primaria",
                                             "Secundaria", "Técnico",
                                             "Tecnólogo", "Profesional"),
                                  ordered = TRUE))

# ── BASE AGREGADA POR DIRECTORIO ──────────────────────────────────────────────
base <- plyr::join_all(
  list(
    fuerzat %>%
      group_by(DIRECTORIO) %>%
      summarise(
        en_edad           = sum(PET == 1, na.rm = TRUE),
        en_fuerza_trabajo = sum(FT  == 1, na.rm = TRUE),
        fuera_ft          = sum(FFT == 1, na.rm = TRUE),
        .groups = 'drop'
      ),
    ocupados %>%
      group_by(DIRECTORIO) %>%
      summarise(
        n_ocupados   = n(),
        ingreso_prom = mean(INGLABO, na.rm = TRUE),
        .groups = 'drop'
      ),
    no_ocup %>%
      group_by(DIRECTORIO) %>%
      summarise(
        n_desocupados = sum(DSI  == 1, na.rm = TRUE),
        n_disponibles = sum(P744 == 1, na.rm = TRUE),
        .groups = 'drop'
      ),
    edu_directorio
  ),
  by = 'DIRECTORIO', type = "left", match = 'all'
)

# ── LIMPIEZA Y TRANSFORMACIONES──────────────────────────────────────────────────────────────────
base <- base %>%
  filter(
    en_edad > 0,
    !is.na(ingreso_prom),
    ingreso_prom > 0
  ) %>%
  mutate(
    n_desocupados = replace_na(n_desocupados, 0),
    n_disponibles = replace_na(n_disponibles, 0),

                         #if_else(condición, valor_si_TRUE, valor_si_FALSE)
    tasa_desempleo      = if_else(en_fuerza_trabajo == 0, 0, (n_desocupados / en_fuerza_trabajo) * 100),
    tasa_disponibilidad = if_else(fuera_ft == 0, 0, (n_disponibles / fuera_ft) * 100),
    log_ingreso_prom    = log(ingreso_prom)
  )

# === ANÁLISIS NUMÉRICO VARIABLES CUANTITATIVAS =====================================================================
analisis_numerico <- function(x) {
  x <- x[!is.na(x)]
  data.frame(
    Media    = mean(x),
    Mediana  = median(x),
    Moda     = as.numeric(names(sort(table(x), decreasing = TRUE)[1])),
    STD       = sd(x),
    Varianza = var(x),
    CV       = (sd(x) / mean(x)) * 100,
    Min      = min(x),
    Q1       = quantile(x, 0.25),
    Q3       = quantile(x, 0.75),
    Max      = max(x),
    IQR      = IQR(x),
    Asimetria = (mean(x) - median(x)) / sd(x)  # índice de Pearson
  )
}

resumen_desempleo      <- analisis_numerico(base$tasa_desempleo)
resumen_disponibilidad <- analisis_numerico(base$tasa_disponibilidad)
resumen_ingreso        <- analisis_numerico(base$log_ingreso_prom)

tabla_resumen <- bind_rows(
  resumen_desempleo,
  resumen_disponibilidad,
  resumen_ingreso
)

# Agregar nombres de variables
tabla_resumen$Variable <- c("Tasa desempleo (%)",
                            "Tasa disponibilidad (%)",
                            "Log ingreso promedio")

tabla_resumen <- tabla_resumen %>%
  select(Variable, everything())

print(tabla_resumen)

# === ANÁLISIS NUMÉRICO VARIABLE CUALITATIVA ======================================================
tabla_edu <- base %>%   
  filter(!is.na(nivel_educacion)) %>%
  count(nivel_educacion) %>%
  mutate(
    Porcentaje    = round((n / sum(n)) * 100, 2),
    Acumulado     = cumsum(Porcentaje)
  ) %>%
  rename(
    Nivel         = nivel_educacion,
    Frecuencia    = n
  )

print(tabla_edu)

# === ANÁLISIS GRÁFICO ============================================================================
library(ggplot2)
library(grid)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# HISTOGRAMAS ──────────────────────────────────────────────────────────────────
multiplot(
  ggplot(data = base, aes(x = tasa_desempleo)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50,
                   fill = 'white', col = 'black') +
    theme_test() +
    ggtitle('Tasa de Desempleo del Hogar') +
    xlab('Tasa (%)') + ylab('Densidad'),
  
  ggplot(data = base, aes(x = tasa_disponibilidad)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50,
                   fill = 'white', col = 'black') +
    theme_test() +
    ggtitle('Tasa de Disponibilidad del Hogar') +
    xlab('Tasa (%)') + ylab('Densidad'),
  
  ggplot(data = base, aes(x = log_ingreso_prom)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50,
                   fill = 'white', col = 'black') +
    theme_test() +
    ggtitle('Log del Ingreso Promedio del Hogar') +
    xlab('Log(ingreso)') + ylab('Densidad'),
  
  cols = 3
)

# BOXPLOTS ─────────────────────────────────────────────────────────────────────
multiplot(
  ggplot(data = base, aes(y = tasa_desempleo)) +
    geom_boxplot(fill = 'white', col = 'black') +
    theme_test() +
    ggtitle('Tasa de Desempleo') +
    ylab('Tasa (%)'),
  
  ggplot(data = base, aes(y = tasa_disponibilidad)) +
    geom_boxplot(fill = 'white', col = 'black') +
    theme_test() +
    ggtitle('Tasa de Disponibilidad') +
    ylab('Tasa (%)'),
  
  ggplot(data = base, aes(y = log_ingreso_prom)) +
    geom_boxplot(fill = 'white', col = 'black') +
    theme_test() +
    ggtitle('Log Ingreso Promedio') +
    ylab('Log(ingreso)'),
  
  cols = 3
)

# GRÁFICO DE BARRAS — nivel_educacion ──────────────────────────────────────────
ggplot(data = base %>% filter(!is.na(nivel_educacion)),
       aes(x = nivel_educacion)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)) * 100),
           fill = 'white', col = 'black') +
  theme_test() +
  ggtitle('Distribución por Nivel Educativo del Hogar') +
  xlab('Nivel educativo') +
  ylab('Porcentaje (%)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# === ESTIMACIÓN POR ANALOGÍA =====================================================================================================
set.seed(123)

n_muestra <- 500  # tamaño de muestra

muestra_ale_base <- base[sample(nrow(base), n_muestra), ]  # muestra aleatoria de la base

muestra_v1 <- muestra_ale_base$tasa_desempleo
muestra_v2 <- muestra_ale_base$tasa_disponibilidad
muestra_v3 <- muestra_ale_base$log_ingreso_prom

# ── Variables Cuantitativas ──────────────────────────────────────────────────
analogia <- data.frame(
  Variable  = c("Tasa desempleo (%)", "Tasa disponibilidad (%)", "Log ingreso promedio"),
  Estimador = c("Media muestral", "Media muestral", "Media muestral"),
  Valor     = c(
    round(mean(muestra_v1, na.rm = TRUE), 4),
    round(mean(muestra_v2, na.rm = TRUE), 4),
    round(mean(muestra_v3, na.rm = TRUE), 4)
  ),
  Valor_Poblacional = c(
    round(mean(base$tasa_desempleo,      na.rm = TRUE), 4),
    round(mean(base$tasa_disponibilidad, na.rm = TRUE), 4),
    round(mean(base$log_ingreso_prom,    na.rm = TRUE), 4)
  )
)
print(analogia)

# ── Variable Cualitativa ──────────────────────────────────────────────────
analogia_edu <- muestra_ale_base %>%
  filter(!is.na(nivel_educacion)) %>%
  count(nivel_educacion) %>%     # Genera automáticamente una columna llamada n con esos conteos.
  mutate(
    p_muestral   = round(n / sum(n), 4),
    p_poblacional = round(
      table(base$nivel_educacion)[as.character(nivel_educacion)] / 
        sum(!is.na(base$nivel_educacion)), 4)
  )
print(analogia_edu)

# === ESTIMACIÓN POR MÁXIMA VEROSIMILITUD =====================================

# ── FUNCIÓN DE LOG-VEROSIMILITUD (normal) ───────────────────────────────────
log_verosimilitud_normal <- function(params, datos) {
  mu    <- params[1]  # media
  sigma <- params[2]  # desviación estándar
  
  if (sigma <= 0) return(Inf)  # sigma debe ser positivo
  -sum(dnorm(datos, mean = mu, sd = sigma, log = TRUE))  # negativo porque optim() minimiza
}

# ── OPTIMIZACIÓN ──────────────────────────────────────────────────────────────
optimizar_mle <- function(datos) {
  datos <- datos[!is.na(datos) & is.finite(datos)]
  optim(
    par    = c(mu = mean(datos), sigma = sd(datos)),  # valores iniciales
    fn     = log_verosimilitud_normal,
    datos  = datos,
    #method = "L-BFGS-B",                              # método eficiente O(n)
    lower  = c(-Inf, 1e-6)                            # sigma > 0
  )
}

mle_v1 <- optimizar_mle(muestra_v1)
mle_v2 <- optimizar_mle(muestra_v2)
mle_v3 <- optimizar_mle(muestra_v3)

# ── TABLA RESUMEN ─────────────────────────────────────────────────────────────
mle_tabla <- data.frame(
  Variable = c("Tasa desempleo (%)", "Tasa disponibilidad (%)", "Log ingreso promedio"),
  MLE_media = c(
    round(mle_v1$par["mu"], 4),
    round(mle_v2$par["mu"], 4),
    round(mle_v3$par["mu"], 4)
  ),
  MLE_sigma = c(
    round(mle_v1$par["sigma"], 4),
    round(mle_v2$par["sigma"], 4),
    round(mle_v3$par["sigma"], 4)
  ),
  Valor_Poblacional = c(
    round(mean(base$tasa_desempleo,      na.rm = TRUE), 4),
    round(mean(base$tasa_disponibilidad, na.rm = TRUE), 4),
    round(mean(base$log_ingreso_prom,    na.rm = TRUE), 4)
  )
)
print(mle_tabla)

# ── VARIABLE CUALITATIVA: MLE para proporción ─────────────────────────────────
log_verosimilitud_bernoulli <- function(p, x, n) {
  if (p <= 0 | p >= 1) return(Inf)
  -(x * log(p) + (n - x) * log(1 - p))  # negativo para minimizar
}

n_total_muestra <- sum(!is.na(muestra_ale_base$nivel_educacion))

mle_edu <- muestra_ale_base %>%
  filter(!is.na(nivel_educacion)) %>%
  count(nivel_educacion) %>%
  mutate(
    MLE_proporcion = round(n / n_total_muestra, 4),
    p_poblacional  = round(
      table(base$nivel_educacion)[as.character(nivel_educacion)] /
        sum(!is.na(base$nivel_educacion)), 4),
    
    # Verificar con optimización numérica
    MLE_verificado = round(sapply(n, function(x) {
      optimize(log_verosimilitud_bernoulli,
               interval = c(1e-6, 1 - 1e-6),
               x = x, n = n_total_muestra)$minimum 
    }), 4)
  )

print(mle_edu)
