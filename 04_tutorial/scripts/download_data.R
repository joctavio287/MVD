# ==============================================================================
# SCRIPT 01: Descarga de datos robusta
# Este script se encarga de descargar los datos desde las fuentes oficiales, ase
# gurando la creación de directorios necesarios, y registrando metadatos de la d
# escarga.
# ==============================================================================
library(here)

data_dir = here("04_tutorial", "data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

# Definimos las rutas de descarga (SINTA - Turismo Internacional)
url_ba = "https://datos.yvera.gob.ar/dataset/78b880c1-50d5-4a0c-9c87-7350e70548c2/resource/32cd65c4-7558-48cf-a8ac-bbb07147b5a1/download/turistas_pernoctes_estadia_media_turistas_no_residentes_por_residencia_ezeiza_aeroparque_mensual.csv"
url_co = "https://datos.yvera.gob.ar/dataset/78b880c1-50d5-4a0c-9c87-7350e70548c2/resource/dae8b7f8-f3dc-431b-887e-3de4b54f09a9/download/turistas_pernoctes_estadia_media_turistas_no_residentes_por_residencia_aeropuerto_cordoba_trimes.csv"

# message("Descargando bases de turismo...")

# download.file(url_ba, destfile = file.path(data_dir, "turismo_ba.csv"), mode = "wb")
# download.file(url_co, destfile = file.path(data_dir, "turismo_co.csv"), mode = "wb")
# message("Proceso de descarga finalizado.")
message("Descargando bases de turismo...")

# Intentamos la descarga normal; si falla, forzamos sin verificar SSL
tryCatch({
  download.file(url_ba, destfile = file.path(data_dir, "turismo_ba.csv"), mode = "wb")
  download.file(url_co, destfile = file.path(data_dir, "turismo_co.csv"), mode = "wb")
}, error = function(e) {
  message("Error de SSL detectado. Reintentando con descarga forzada...")
  download.file(url_ba, destfile = file.path(data_dir, "turismo_ba.csv"), method = "curl", extra = "-k")
  download.file(url_co, destfile = file.path(data_dir, "turismo_co.csv"), method = "curl", extra = "-k")
})

message("Proceso de descarga finalizado.")

# Registrar/actualizar un CSV persistente de metadatos
# columnas: datetime, source, organism, url_ba, url_co
meta_file = file.path(data_dir, "metadata.csv")
entries = data.frame(
  datetime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  source_website_co = "https://datos.yvera.gob.ar/dataset/78b880c1-50d5-4a0c-9c87-7350e70548c2/resource/dae8b7f8-f3dc-431b-887e-3de4b54f09a9",
  source_website_ba = "https://datos.yvera.gob.ar/dataset/78b880c1-50d5-4a0c-9c87-7350e70548c2/resource/32cd65c4-7558-48cf-a8ac-bbb07147b5a1",
  url_ba_download = url_ba,
  url_co_download = url_co,
  organism = 'SINTA',
  stringsAsFactors = FALSE
)

if (!file.exists(meta_file)) {
  write.csv(entries, meta_file, row.names = FALSE)
  message("Se creó el archivo de metadata: ", meta_file)
} else {
  write.table(entries, meta_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
  message("Se actualizó el archivo de metadata: ", meta_file)
}
