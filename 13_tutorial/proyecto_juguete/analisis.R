# =============================================================================
# Proyecto de juguete — análisis mínimo reproducible
# -----------------------------------------------------------------------------
# Este script es deliberadamente chiquito. Su única razón de ser es servir de
# ejemplo de un proyecto con entorno reproducible (renv): tres paquetes, un CSV
# inventado y un gráfico. Si lo abrís desde `proyecto_juguete.Rproj`, el
# .Rprofile activa renv automáticamente y vas a estar usando las versiones
# exactas que están fijadas en renv.lock.
#
# Para reproducirlo desde cero:
#   1. Abrí proyecto_juguete.Rproj en RStudio
#   2. En la consola: renv::restore()   (instala las versiones del lockfile)
#   3. source("analisis.R")
# =============================================================================

library(readr)    # leer el CSV
library(dplyr)    # transformar los datos
library(ggplot2)  # graficar

# Semilla: acá no hay aleatoriedad, pero la dejamos como recordatorio de que,
# si la hubiera, set.seed() es parte de hacer el análisis reproducible.
set.seed(2025)

# 1. Leemos los datos --------------------------------------------------------
# Usamos una ruta relativa a la raíz del proyecto. Como renv y los Proyectos de
# RStudio fijan el working directory en la raíz, "data/encuesta.csv" siempre
# resuelve bien, corra quien corra el script.
encuesta <- read_csv("data/encuesta.csv", show_col_types = FALSE)

glimpse(encuesta)

# 2. Transformamos -----------------------------------------------------------
# Ingreso promedio por región y cantidad de encuestados.
resumen <- encuesta |>
  group_by(region) |>
  summarise(
    n              = n(),
    ingreso_medio  = mean(ingreso),
    edad_media     = mean(edad),
    .groups = "drop"
  ) |>
  arrange(desc(ingreso_medio))

print(resumen)

# 3. Graficamos --------------------------------------------------------------
grafico <- resumen |>
  ggplot(aes(x = reorder(region, ingreso_medio), y = ingreso_medio)) +
  geom_col(fill = "#2C3E50") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_number(prefix = "$", big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    x     = NULL,
    y     = "Ingreso medio declarado",
    title = "Ingreso medio por región",
    subtitle = "Datos ficticios — proyecto de juguete reproducible"
  ) +
  theme_minimal(base_size = 13)

# 4. Guardamos el resultado --------------------------------------------------
# Creamos output/ si no existe y guardamos el gráfico ahí.
if (!dir.exists("output")) dir.create("output")
ggsave("output/ingreso_por_region.png", grafico, width = 7, height = 4, dpi = 150)

message("Listo: gráfico guardado en output/ingreso_por_region.png")
