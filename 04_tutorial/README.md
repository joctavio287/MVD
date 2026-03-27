---
editor_options: 
  markdown: 
    wrap: sentence
---

# 04_tutorial

Breve guía para ejecutar el pipeline de ejemplo contenido en esta carpeta.

**Contenido relevante**

-   `scripts/` : scripts que implementan las etapas del pipeline (descarga, limpieza, figuras).

-   `data/` : datos crudos descargados.

-   `output/` : resultados procesados (.rds, .parquet).

-   `figures/` : gráficos exportados.

**Requisitos**

-   Tener `R` y `Rscript` en el `PATH`.

-   Paquetes R utilizados: `tidyverse`, `here`, `janitor`, `stringi`, `arrow`, `DBI`, `duckdb`, `dbplyr`, `forcats`.

**Ejecución (desde la raíz del repositorio)**

Windows (PowerShell):

```         
powershell -ExecutionPolicy Bypass -File 04_tutorial/run_pipeline.ps1
```

macOS / Linux (bash):

```         
bash 04_tutorial/run_pipeline.sh
```

**Notas**

-   Los scripts usan rutas relativas a `04_tutorial` (se ejecutan correctamente desde la propia carpeta `04_tutorial` o desde la raíz del repo si se invocan con los caminos mostrados arriba).

-   Hay un archivo `04_tutorial/data/metadata.csv` que se actualiza en cada descarga con registros simples.
