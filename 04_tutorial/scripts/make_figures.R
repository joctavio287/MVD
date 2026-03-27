# ==============================================================================
# SCRIPT 03: Análisis y Visualización
# Este script se encarga de realizar un análisis exploratorio de los datos limpi
# os, y generar visualizaciones comparativas entre las provincias de Buenos Aire
# s y Córdoba en relación al turismo receptivo.
# ==============================================================================
library(tidyverse)
library(ggplot2)
library(arrow)
library(here)

# Leemos la data en .rds para preservar la estructura de los factores
output_dir = here("04_tutorial", "output")
figures_dir = here("04_tutorial", "figures")
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir)
} 
message("Directorio de figuras creado: ", figures_dir)


turismo_clean = readRDS(file.path(output_dir, "turismo_limpio.rds"))

# Notemos que si leemos el parquet, los factores también se preserva porque es b
# inario, pero no es el caso de los csv o Excel. 
# En esos casos, al leerlos, los factores se convierten en caracteres.
turismo_clean_parquet = read_parquet(file.path(output_dir, "turismo_limpio.parquet"))

# Vemos el tipo de variable de pais_factor en ambos casos
mensaje_estructura = capture.output(str(turismo_clean$pais_factor))
mensaje_estructura_parq =capture.output(str(turismo_clean_parquet$pais_factor)) 

# Imprimimos el mensaje combinando ambos textos
message(
  "Estructura de pais_factor en turismo_clean (rds):\n", 
  paste(mensaje_estructura, collapse = "\n"),
  "\nEstructura de pais_factor en turismo_clean_parquet (parquet):\n", 
  paste(mensaje_estructura_parq, collapse = "\n")
)

# Resumen por trimestre y país
turismo_trimestral_summary = turismo_clean |> 
  group_by(pais_factor, trimestre) |> 
  summarise(
    media_turistas_ba = mean(turistas_ba, na.rm = TRUE),
    media_turistas_co = mean(turistas_co, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(trimestre, desc(media_turistas_ba))

# Transformamos a formato largo para facilitar la comparación entre provincias
# entonces tenemos una fila por trimestre, país y media de c/ provincia
turismo_comparativo = turismo_trimestral_summary |> 
  pivot_longer(
    cols = c(media_turistas_ba, media_turistas_co),
    names_to = "provincia",
    values_to = "media_turistas"
  ) |> 
  
  mutate(provincia = case_when(
    provincia == "media_turistas_ba" ~ "Buenos Aires",
    provincia == "media_turistas_co" ~ "Córdoba"
  ))

plot_turism = turismo_comparativo |> 
  ggplot(aes(x = factor(trimestre), y = media_turistas, fill = provincia)) +
  geom_col(show.legend = FALSE) + # Quitamos la leyenda porque el título de la fila ya avisa
  # Creamos la grilla: filas por provincia, columnas por país
  # scales = "free_y" permite que cada FILA tenga su propio rango
  facet_grid(provincia ~ pais_factor, scales = "free_y") + 
  labs(
    title = "Contraste de Turismo Receptivo por Punto de Ingreso",
    x = "Trimestre",
    y = "Media de Turistas No Residentes"
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0, face = "bold"), # Rota el texto lateral
    panel.spacing = unit(1, "lines") # Da aire entre gráficos
  )

# Save plot using date in turismo_clean attribute for versioning
turismo_clean_date = attr(turismo_clean, "last_updated")
file_name = paste0("contraste_turismo_receptivo_", turismo_clean_date, ".png")
ggsave(
  filename = file.path(figures_dir, file_name),
  plot = plot_turism,
  width = 12, height = 8, dpi = 300
)
message("Figura guardada en: ", file.path(figures_dir, file_name))