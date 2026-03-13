# Ejecuta el análisis final: carga, limpieza, cruces, export CSV y guardar figura.

# Paquetes
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(readr, dplyr, stringr, tidyr, lubridate, ggplot2, scales)

# Rutas
ruta_nacion <- "02_tutorial/data/indicadores-provinciales.csv"
ruta_catalogo_nacion <- "02_tutorial/data/indicadores-provinciales-catalog.csv"
ruta_pba <- "02_tutorial/data/pbg-base-2004-serie-31122004-31122023.csv"
out_dir <- "02_tutorial/output"
out_csv <- file.path(out_dir, "comparativa_proporciones.csv")
out_plot <- file.path(out_dir, "comparativa_proporciones.png")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Carga
nacion <- read_csv(ruta_nacion, show_col_types = FALSE)
catalogo_nacion <- read_csv(ruta_catalogo_nacion, show_col_types = FALSE)
pba_raw <- read_csv(ruta_pba, show_col_types = FALSE)

# Limpieza PBA: eliminar columnas vacías y renombrar
pba <- pba_raw %>%
  select(where(~ any(!is.na(.)))) %>%
  rename(
    actividad = actividad_detalle,
    sector_letra = actividad_sector_letra,
    sector_detalle = actividad_sector_detalle,
    year = anio,
    valor_corrientes = valor_precios_corrientes,
    valor_constantes = valor_precios_constantes
  ) %>%
  mutate(year = as.integer(year))

# Serie PBG PBA desde archivo nacional (filtro por fuente que incluya 'Buenos Aires')
pbg_pba <- nacion %>%
  filter(
    sector_nombre == "Indicadores Provinciales",
    indicador == "PBG - Base 2004",
    str_detect(fuente, regex("Buenos Aires", ignore_case = TRUE))
  ) %>%
  mutate(year = year(indice_tiempo), valor = valor / 1000) %>%
  select(year, unidad_de_medida, valor)

# Serie nacional (sumatoria de provincias reportadas por año)
pbg_nacion_17 <- nacion %>%
  filter(
    sector_nombre == "Indicadores Provinciales",
    indicador == "PBG - Base 2004"
  ) %>%
  mutate(year = year(indice_tiempo), valor = valor / 1000) %>%
  group_by(year) %>%
  summarise(
    valor_nacion_17 = sum(valor, na.rm = TRUE),
    provincias_contabilizadas = n(),
    .groups = "drop"
  )

# Agregación PBA (archivo PBA desagregado por actividad -> sumar por año, valor_constantes)
pba_agg <- pba %>%
  group_by(year) %>%
  summarize(pbg_pba_const = sum(as.numeric(valor_constantes), na.rm = TRUE), .groups = "drop") %>%
  arrange(year)

# Proporciones: desde PBA file / desde suma nacional
proporcion_pgb_ba <- pba_agg %>%
  left_join(pbg_nacion_17, by = "year") %>%
  mutate(proporcion_ba = 100 * (pbg_pba_const / valor_nacion_17)) %>%
  select(year, proporcion_ba)

proporcion_pgb_ba_por_nacion <- pbg_pba %>%
  left_join(pbg_nacion_17, by = "year") %>%
  mutate(proporcion_ba = 100 * (valor / valor_nacion_17)) %>%
  select(year, proporcion_ba)

# Unificar y pivotar para comparar
comparativa_proporciones <- proporcion_pgb_ba %>%
  rename(prop_desde_pba_file = proporcion_ba) %>%
  left_join(proporcion_pgb_ba_por_nacion %>% rename(prop_desde_nacion_file = proporcion_ba),
            by = "year") %>%
  pivot_longer(cols = starts_with("prop"),
               names_to = "fuente_dato",
               values_to = "porcentaje") %>%
  arrange(year, fuente_dato)

# Export CSV
write_csv(comparativa_proporciones, out_csv)

# Grafico
p <- ggplot(comparativa_proporciones, aes(x = year, y = porcentaje, color = fuente_dato)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Peso de PBA en el Total de 17 Provincias",
    subtitle = "Comparativa según fuente de datos (Nación vs Provincia)",
    x = "Año",
    y = "Proporción (%)",
    color = "Fuente de Información"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("prop_desde_pba_file" = "firebrick", "prop_desde_nacion_file" = "steelblue"),
                     labels = c("prop_desde_pba_file" = "Suma PBA (archivo PBA)",
                                "prop_desde_nacion_file" = "Datos Nación (PBG)"))

# Guardar figura (png)
ggsave(filename = out_plot, plot = p, width = 10, height = 6, dpi = 300)

message("Exportados: ", out_csv, " y ", out_plot)