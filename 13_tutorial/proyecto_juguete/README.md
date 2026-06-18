---

editor_options: 
  markdown: 
    wrap: 72
---

# Proyecto de juguete — entorno reproducible

Este es un proyecto mínimo que sirve de ejemplo para la clase del tutorial 13 (entornos reproducibles). No tiene ningún valor analítico: lee un CSV inventado, calcula el ingreso medio por región y guarda un gráfico.

Lo único interesante es que viene con un **entorno `renv`** que fija las versiones exactas de los paquetes.

## Cómo reproducirlo

1.  **Abrí** `proyecto_juguete.Rproj` en RStudio. Al abrirlo, el archivo `.Rprofile` ejecuta `renv/activate.R` y activa el entorno del proyecto automáticamente (vas a ver un mensaje de renv en la consola).

2.  **Instalá las versiones del lockfile** corriendo en la consola:

    ``` r
    renv::restore()
    ```

    Esto lee `renv.lock` e instala exactamente esas versiones en la librería privada del proyecto (`renv/library/`), sin tocar tu librería global.

3.  **Corré el análisis:**

    ``` r
    source("analisis.R")
    ```

    Si todo salió bien, vas a tener el gráfico en `output/ingreso_por_region.png`.

## Qué es cada archivo

| Archivo / carpeta | Para qué sirve |
|------------------|------------------------------------------------------|
| `renv.lock` | El lockfile: versión de R y de cada paquete. **Se commitea a git.** |
| `.Rprofile` | Activa renv al abrir el proyecto (`source("renv/activate.R")`). |
| `renv/activate.R` | El "arranque" de renv. **Se commitea.** |
| `renv/library/` | La librería privada con los paquetes instalados. **NO se commitea** (la regenera `renv::restore()`). |
| `data/encuesta.csv` | Datos ficticios de entrada. |
| `analisis.R` | El script de ejemplo. |
| `output/` | Resultados generados (gráfico). |

## Versión de R

Este entorno se armó con **R 4.6.0**. `renv` guarda la versión de R en el lockfile, pero no la instala por vos: si usás una versión muy distinta puede haber diferencias. Lo ideal es usar la misma (o muy cercana).
