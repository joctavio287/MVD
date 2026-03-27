# ==============================================================================
# SCRIPT 02: Limpieza, Base de Datos y Wrangling Complejo
# Este script realiza una limpieza avanzada de los datos, utilizando una base de
# datos en memoria para operaciones SQL, y luego exporta el resultado en formato
# s eficientes para análisis futuros.
# ==============================================================================

library(tidyverse)
library(janitor)
library(stringi)
library(arrow)
library(DBI)
library(duckdb)
library(dbplyr)
library(here)

# ==============================================================================
# Definimos rutas de entrada y salida, y verificamos su existencia

input_dir = here("04_tutorial/data")
output_dir = here("04_tutorial/output")

# Verificamos si existe; si no, la creamos incluyendo carpetas padres si faltan
if (!dir.exists(output_dir)) {
  message("Creando el directorio: ", output_dir)
  dir.create(output_dir, recursive = TRUE) 
}
if (!dir.exists(input_dir)) {
  stop("Error: El directorio de entrada no existe: ", input_dir)
}

# Carga y normalización inicial 
pba_raw = read_csv(
  file.path(input_dir, "turismo_ba.csv")
) |> clean_names()
cor_raw = read_csv(
  file.path(input_dir, "turismo_co.csv")
) |> clean_names()

# ==============================================================================
# Wrangling con DB y dplyr: limpieza de strings, manejo de NAs, operaciones 
# temporales, colapsado de categorías y reordenamiento

# Armamos una conexión con la Base de Datos (DB) en memoria para velocidad
db_conection = dbConnect(
  duckdb::duckdb(), # especificamos que queremos usar duckdb como backend para la DB
  dbdir = ":memory:" # indicamos que la DB se cree en memoria (no se guarda en disco, es temporal y rápida)
)

# dvdb es un paquete que permite usar una base de datos SQL como backend para 
# manipular data frames con dplyr. Es parte de la librería de dbplyr.

# En este caso, usamos duckdb, que es una base de datos en memoria muy rápida y
# eficiente para análisis de datos.

# La ventaja de usar una DB es que podemos manejar datasets más grandes que la 
# memoria de R, y aprovechar la velocidad de procesamiento de SQL para 
# operaciones complejas.

# Además, al usar dbplyr, podemos escribir código en R que se traduce 
# automáticamente a SQL, lo que facilita el wrangling sin tener que escribir 
# consultas SQL manualmente.

# En este caso, como los datasets no son muy grandes, el beneficio principal es
# la experiencia de trabajar con una DB y la eficiencia en operaciones 
# complejas, pero también es una buena práctica para manejar datasets más 
# grandes en el futuro.

# Subimos las tablas a la DB para procesamiento SQL (no apareceran como variables en R)
dbWriteTable(db_conection, "turismo_ba", pba_raw)
dbWriteTable(db_conection, "turismo_co", cor_raw)

# Wrangling con dbplyr (traducción automática a SQL :))
# seleccionar, filtrar y renombrar
pba_db = tbl(db_conection, "turismo_ba") |> 
  select(
    indice_tiempo,
    pais = pais_de_residencia,
    turistas = turistas_no_residentes
  ) |>
  filter(!is.na(turistas))
cor_db = tbl(db_conection, "turismo_co") |>
  select(
    indice_tiempo,
    pais = pais_de_residencia,
    turistas = turistas_no_residentes
  )

# Traemos a R local con collect() para limpieza de strings y cambio de formato a
# trimestral: operaciones que son más fáciles de hacer en R que en SQL. 
# Luego, agrupamos por país y trimestre para sumar el total de turistas por período y país.
pba_clean = pba_db |> collect() |> 
  mutate(
    pais = str_to_lower(pais) |> 
      stri_trans_general("Latin-ASCII") |> # quita tildes y acentos
      str_replace_all(" ", "_") |>     # espacios por guiones
      str_remove_all("[[:punct:]]") |> # quita puntos y signos restantes
      str_trim(), # quita espacios al inicio y final
    pais = case_when(
      str_starts(pais, "eeuu") ~ "america_norte",
      str_starts(pais, "resto") ~ "otros_paises",
      str_starts(pais, "eur") ~ "europa", 
      TRUE ~ pais
    ),
    trimestre = quarter(indice_tiempo) # extraemos el trimestre 
  ) |> 
  group_by(pais, indice_tiempo, trimestre) |> # agrupamos
  summarise( # tomamos valores medios /sumas según el indicador
    turistas = sum(turistas, na.rm = TRUE),
    .groups = "drop" # Para que el DF deje de estar agrupado tras el cálculo
  )

cor_clean = cor_db |> collect() |> 
  mutate(
    pais = str_to_lower(pais) |> 
      stri_trans_general("Latin-ASCII") |> # quita tildes y acentos
      str_replace_all(" ", "_") |>     # espacios por guiones
      str_remove_all("[[:punct:]]") |> # quita puntos y signos restantes
      str_trim(), # quita espacios al inicio y final
    pais = case_when(
      str_starts(pais, "eeuu") ~ "america_norte",
      str_starts(pais, "resto") ~ "otros_paises",
      str_starts(pais, "eur") ~ "europa", 
      TRUE ~ pais
    ),
    trimestre = quarter(indice_tiempo) # extraemos el trimestre 
  ) |> 
  group_by(pais, indice_tiempo, trimestre) |> # agrupamos
  summarise( # tomamos valores medios /sumas según el indicador
    turistas = sum(turistas, na.rm = TRUE),
    .groups = "drop" # Para que el DF deje de estar agrupado tras el cálculo
  )

# Usamos coalesce para asegurar que no haya NAs en turistas 
# y fill para series temporales
turismo_total = pba_clean |> 
  inner_join( # solo combinamos filas con mismo indice_tiempo y pais
    cor_clean, 
    by = c("indice_tiempo", "pais", "trimestre"), 
    suffix = c("_ba", "_co")
  ) |> 
  mutate(
    turistas_ba = coalesce(turistas_ba, 0), # reemplaza NAs por 0 en turistas_ba
    turistas_co = coalesce(turistas_co, 0) # idem para turistas_co
  ) |> 
  arrange(indice_tiempo) |> 
  group_by(pais) |> 
  fill(turistas_ba, turistas_co, .direction = "down") |>  # rellena hacia abajo los valores faltantes en la serie temporal por país
  ungroup()

# ===================================================================
# Factores y categorías (forcats). Colapsamos categorías con fct_lump 
# y reordenamos para análisis
turismo_clean = turismo_total |> 
  mutate(
    pais_factor = fct_lump( # colapsa las categorías menos frecuentes
      f=pais, n = 3, w = turistas_co, other_level = "pocos_turistas"
    ),
    pais_factor = fct_reorder( # reordena según el total de turistas en BA
      .f=pais_factor, .x=turistas_ba, .fun = sum, .desc = TRUE)
  )

# fct_lump es una función de forcats que colapsa las categorías menos frecuentes
# en una categoría "Otros".

# --> Elejimos las n=3 categorías (países) más frecuentes según el número de 
# turistas en CO y asignamos el resto a la categoría "pocos_turistas".

# --> notar que se asigna un peso (w) a cada categoría para determinar cuáles 
# son las n más frecuentes. En este caso, usamos el número de turistas en CO 
# como peso para identificar los países más relevantes.

# fct_reorder reordena las categorías de pais_factor (la variable que creamos 
# recién) según la suma total de turistas en BA,
# de mayor a menor (.desc = TRUE)

# --> .f es la variable de factores que queremos reordenar, 
# .x es la variable numérica que se utiliza para determinar el orden,
# .fun es la función que se aplica a .x para obtener un valor resumen por 
# categoría. En este caso sum, y .desc = TRUE indica que queremos el orden de 
# mayor a menor. 

# Cerramos la conexión a la DB
dbDisconnect(db_conection, shutdown = TRUE)
message("Wrangling completo. Datos listos para exportar.")

# ==============================================================================
# Exportamos el resultado en formatos eficientes para análisis futuros: .rds y .parquet

# Sumamos atributos con metadatos para facilitar el seguimiento de la información en el futuro
attr(turismo_clean, "source") = "SINTA - Turismo Internacional"
attr(turismo_clean, "description") = "Datos de turistas no residentes por país de residencia, combinando datos de Buenos Aires y Córdoba, con limpieza y wrangling avanzado."
attr(turismo_clean, "last_updated") = Sys.Date()

# Guardamos en .rds (formato nativo de R que preserva factores y metadatos)
resultado = try(saveRDS(turismo_clean, file.path(output_dir, "turismo_limpio.rds")))

# Utilizamos try() para capturar cualquier error que pueda ocurrir durante la escritura 
# del archivo .rds, como problemas de permisos o espacio en disco. Si el resultado es un 
# error, no se mostrará el mensaje de éxito.
if (!inherits(resultado, "try-error")) {
  message("Archivo .rds guardado exitosamente en ", output_dir)
} else {
  message("Error al guardar el archivo .rds: ", resultado)
}

# Guardamos en .parquet usando la librería arrow
resultado = try(write_parquet(turismo_clean, file.path(output_dir, "turismo_limpio.parquet")))
if (!inherits(resultado, "try-error")) {
  message("Archivo .parquet guardado exitosamente en ", output_dir)
} else {
  message("Error al guardar el archivo .parquet: ", resultado)
} 

# En este caso, el formato es binario, columnar y mucho más eficiente 
# que un .csv. porque preserva tipos de datos y es más rápido de leer/escribir.
