library(ggplot2)
library(tidyverse)
library(lubridate)

# Rutas
ruta_nacion <- "02_tutorial/data/indicadores-provinciales.csv"
ruta_pba <- "02_tutorial/data/pbg-base-2004-serie-31122004-31122023.csv"

# Carga
nacion <- read_csv(ruta_nacion, show_col_types = FALSE)
pba_raw <- read_csv(ruta_pba, show_col_types = FALSE)

# Limpieza PBA
pba <- pba_raw %>% select(where(~ any(!is.na(.))))
pba <- pba %>% rename(
  actividad = actividad_detalle,
  sector_letra = actividad_sector_letra,
  sector_detalle = actividad_sector_detalle,
  year = anio,
  valor_corrientes = valor_precios_corrientes,
  valor_constantes = valor_precios_constantes
) %>% mutate(year = as.integer(year))

# Serie PBA agregada (valores constantes)
pba_agg_PBA <- pba %>%
  group_by(year) %>%
  summarize(pbg_pba_const = sum(as.numeric(valor_constantes), na.rm = TRUE), .groups = "drop") %>%
  arrange(year)

# Serie Nación: filtros y agregado por año
pbg_pba_NACION <- nacion %>%
  filter(
    sector_nombre == "Indicadores Provinciales",
    indicador == "PBG - Base 2004",
    str_detect(fuente, "Buenos Aires"),
    unidad_de_medida == "miles de pesos a precios de 2004"
  ) %>%
  mutate(
    unidad_de_medida = "millones de pesos a precios de 2004",
    year = year(indice_tiempo),
    valor = valor / 1000
  ) %>%
  select(year, unidad_de_medida, valor)

pbi_NACION <- nacion %>%
  filter(
    sector_nombre == "Indicadores Provinciales",
    indicador == "PBG - Base 2004",
    unidad_de_medida == "miles de pesos a precios de 2004"
  ) %>%
  mutate(unidad_de_medida = "millones de pesos a precios de 2004", year = year(indice_tiempo), valor = valor / 1000) %>%
  group_by(year) %>%
  summarise(
    pbi = sum(valor, na.rm = TRUE),
    provincias_contabilizadas = n(),
    .groups = "drop"
  ) %>%
  arrange(year)

# Proporciones
proporcion_pbi_PBA <- pba_agg_PBA %>%
  left_join(pbi_NACION, by = "year") %>%
  mutate(proporcion_pba = 100 * (pbg_pba_const / pbi)) %>%
  select(year, proporcion_pba)

proporcion_pbi_NACION <- pbg_pba_NACION %>%
  left_join(pbi_NACION, by = "year") %>%
  mutate(proporcion_pba = 100 * (valor / pbi)) %>%
  select(year, proporcion_pba)

# Unificar y guardar CSV (formato ancho)
comparativa_wide <- proporcion_pbi_PBA %>%
  rename(prop_desde_pba_file = proporcion_pba) %>%
  left_join(proporcion_pbi_NACION %>% rename(prop_desde_nacion_file = proporcion_pba), by = "year")

write_csv(comparativa_wide, "02_tutorial/output/comparativa_proporciones_wide.csv")
write_csv(pba_agg_PBA, "02_tutorial/output/pba_agg_PBA.csv")
write_csv(pbi_NACION, "02_tutorial/output/pbi_NACION.csv")

# Gráfico y guardar PNG
comparativa_long <- comparativa_wide %>%
  pivot_longer(cols = starts_with("prop"), names_to = "fuente_dato", values_to = "porcentaje")

p <- ggplot(comparativa_long, aes(x = year, y = porcentaje, color = fuente_dato)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Peso de PBA en el Total de 17 Provincias",
    subtitle = "Comparativa según fuente de datos (Nación vs. Provincia)",
    x = "Año",
    y = "Proporción del PBI(%)",
    color = "Fuente de Información"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("firebrick", "steelblue"),
                     labels = c("Datos PBA (archivo provincia)", "Datos Nación (PBG)"))

ggsave("02_tutorial/output/peso_PBA_vs_nacion.png", plot = p, width = 10, height = 6, dpi = 300)