# ── app_brasil.R ──────────────────────────────────────────────────────────────
#
# App Shiny: Explorador Electoral — Brasil 2020
#
# Funcionalidad:
#   - Dropdown de partidos: pinta cada municipio con el % que sacó ese partido
#   - Dropdown de estados: hace zoom en el estado elegido (NULL = todo Brasil)
#   - Click en municipio: muestra tabla con detalle de resultados y datos socioeconómicos
#
# Cómo correr:
#   shiny::runApp("app_brasil.R")           (desde la consola)
#   → o el botón "Run App" en RStudio
#
# Para compartir sin servidor (todos necesitan R instalado):
#   shiny::runGitHub("repo", "usuario", subdir = "carpeta-con-esta-app")
#
# Librerías necesarias:
#   install.packages(c("shiny", "bslib", "tidyverse", "sf", "leaflet", "DT", "here"))
#   install.packages("geobr")  # desde CRAN
# ─────────────────────────────────────────────────────────────────────────────

library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(leaflet)
library(DT)
library(here)

# ══════════════════════════════════════════════════════════════════════════════
# DATOS GLOBALES
# Todo lo que está acá fuera de ui y server se ejecuta UNA SOLA VEZ al iniciar
# la app, y queda disponible para todos los usuarios y todas las sesiones.
# Es el lugar correcto para cargar datos y hacer preparaciones costosas.
# ══════════════════════════════════════════════════════════════════════════════

message("Cargando datos... (esto puede tardar unos segundos la primera vez)")

df_elections      <- readRDS(here("09_tutorial/data/elections.rds"))
df_municipalities <- readRDS(here("09_tutorial/data/municipalities.rds"))
df_socioeconomic  <- readRDS(here("09_tutorial/data/socioeconomic.rds"))
df_finance        <- readRDS(here("09_tutorial/data/finance.rds"))
df_transfers      <- readRDS(here("09_tutorial/data/transfers_combined.rds"))

# Geometrías de municipios y estados
sf_municipios_raw <- geobr::read_municipality(year = 2020, showProgress = FALSE) |>
  mutate(id_municipio = as.character(code_muni))

sf_estados_raw <- geobr::read_state(year = 2020, showProgress = FALSE) |>
  mutate(sigla_uf = abbrev_state) |>
  st_transform(crs = 4326)  # WGS84 para leaflet

# Preparamos la tabla base de municipios con datos socioeconómicos y fiscales
# Usamos el año 2020 para coincidir con los datos electorales
df_socio_2020 <- df_socioeconomic |>
  filter(year == 2020)

df_finance_2020 <- df_finance |>
  filter(year == 2020)

df_transf_2020 <- df_transfers |>
  filter(year == 2020) |>
  group_by(id_municipio) |>
  summarise(
    transfer_total = sum(transfer_amount, na.rm = TRUE),
    transfer_n     = sum(transfer_n,      na.rm = TRUE),
    .groups        = "drop"
  )

df_municipios_base <- df_municipalities |>
  left_join(df_socio_2020,   by = "id_municipio") |>
  left_join(df_finance_2020, by = "id_municipio") |>
  left_join(df_transf_2020,  by = "id_municipio")

# Cruce espacial: el sf siempre a la izquierda del left_join
# para conservar la columna geometry en el resultado
sf_municipios_base <- sf_municipios_raw |>
  left_join(df_municipios_base, by = "id_municipio") |>
  st_transform(crs = 4326)  # WGS84 para leaflet

# Precalculamos los votos totales por municipio en 2020
# (denominador para calcular % de cualquier partido)
df_votos_totales_2020 <- df_elections |>
  filter(year == 2020) |>
  group_by(id_municipio) |>
  summarise(
    total_votos_mun = sum(total_votes, na.rm = TRUE),
    .groups         = "drop"
  )

# Opciones para los dropdowns de la UI
# Calculamos una sola vez para no repetir en cada sesión de usuario
partidos_disp <- df_elections |>
  filter(year == 2020) |>
  distinct(sigla_partido) |>
  arrange(sigla_partido) |>
  pull(sigla_partido)

estados_disp <- df_municipalities |>
  distinct(sigla_uf) |>
  arrange(sigla_uf) |>
  pull(sigla_uf)

message("Datos listos. Iniciando app...")


# ══════════════════════════════════════════════════════════════════════════════
# UI: INTERFAZ DE USUARIO
# Define qué ve el usuario. Se construye una sola vez al cargar la app.
# No hay lógica de R acá: solo layout y controles de input/output.
# ══════════════════════════════════════════════════════════════════════════════

ui <- page_sidebar(

  title = "Explorador Electoral — Brasil 2020",
  theme = bs_theme(bootswatch = "flatly"),  # tema visual de bslib

  # ── Panel lateral izquierdo ──────────────────────────────────────────────
  sidebar = sidebar(
    width = 240,

    h5("Filtros"),

    # Dropdown 1: partido político
    # inputId = "partido" → accesible en el server como input$partido
    selectInput(
      inputId  = "partido",
      label    = "Partido:",
      choices  = partidos_disp,
      selected = "PT"
    ),

    # Dropdown 2: estado (con opción "Todo Brasil")
    # Cuando el usuario elige "ALL", el mapa muestra todo el país
    selectInput(
      inputId  = "estado",
      label    = "Estado:",
      choices  = c("Todo Brasil" = "ALL", estados_disp),
      selected = "ALL"
    ),

    hr(),

    # Texto de ayuda para el usuario
    p(
      class = "text-muted small",
      "El mapa muestra el porcentaje de votos del partido seleccionado",
      "en cada municipio (elecciones municipales 2020)."
    ),
    p(
      class = "text-muted small",
      icon("hand-pointer"),
      "Hacé click en un municipio para ver su detalle en la tabla."
    )
  ),

  # ── Área principal ───────────────────────────────────────────────────────

  # Card del mapa (ocupa la parte superior)
  # full_screen = TRUE agrega un botón para expandir la card a pantalla completa
  card(
    full_screen = TRUE,
    card_header(
      textOutput("titulo_mapa", inline = TRUE)  # título dinámico con el partido elegido
    ),
    leafletOutput("mapa", height = "480px")
  ),

  # Card de la tabla (debajo del mapa)
  card(
    card_header("Detalle del municipio seleccionado"),
    DTOutput("tabla")
  )

)


# ══════════════════════════════════════════════════════════════════════════════
# SERVER: LÓGICA DE LA APLICACIÓN
# Se ejecuta una vez por sesión de usuario. Define el grafo de reactividad:
# qué se recalcula y cuándo.
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Expresiones reactivas ────────────────────────────────────────────────

  # datos_mapa(): se recalcula cuando cambia input$partido o input$estado.
  # Devuelve el sf filtrado y con la columna 'pct' del partido elegido.
  # Al ser reactive(), el resultado queda cacheado: si tanto el mapa como
  # la tabla lo necesitan en el mismo ciclo reactivo, R solo lo calcula una vez.
  datos_mapa <- reactive({

    # Calculamos el % de votos del partido elegido por municipio
    df_pct <- df_elections |>
      filter(year == 2020, sigla_partido == input$partido) |>
      left_join(df_votos_totales_2020, by = "id_municipio") |>
      mutate(
        pct = round(total_votes / total_votos_mun * 100, 1),
        pct = replace_na(pct, 0)
      ) |>
      select(id_municipio, votos_partido = total_votes, total_votos_mun, pct)

    # Cruzamos con el sf base
    sf_out <- sf_municipios_base |>
      left_join(df_pct, by = "id_municipio") |>
      mutate(pct = replace_na(pct, 0))

    # Filtramos por estado si el usuario no eligió "Todo Brasil"
    if (input$estado != "ALL") {
      sf_out <- sf_out |> filter(sigla_uf == input$estado)
    }

    sf_out
  })


  # ── Outputs ──────────────────────────────────────────────────────────────

  # Título dinámico del mapa (se actualiza con el partido elegido)
  output$titulo_mapa <- renderText({
    paste0("% votos ", input$partido, " por municipio")
  })

  # Mapa leaflet
  # Se re-renderiza cuando cambia input$partido (porque datos_mapa() cambia).
  # El zoom se maneja por separado con leafletProxy() para no re-renderizar
  # el mapa entero cuando solo cambia el estado.
  output$mapa <- renderLeaflet({

    sf_m   <- datos_mapa()
    paleta <- colorNumeric("YlOrRd", domain = sf_m$pct, na.color = "#CCCCCC")

    leaflet(sf_m) |>
      addProviderTiles(providers$CartoDB.Positron) |>

      addPolygons(
        # Estética
        fillColor   = ~paleta(pct),
        fillOpacity = 0.75,
        color       = "white",
        weight      = 0.5,
        opacity     = 1,

        # Resalte al pasar el mouse
        highlight = highlightOptions(
          weight      = 2.5,
          color       = "#333333",
          fillOpacity = 0.95,
          bringToFront = TRUE
        ),

        # Label: aparece al pasar el mouse (sin click)
        label = ~paste0(name_muni, " — ", pct, "% ", input$partido),
        labelOptions = labelOptions(
          style    = list("font-size" = "13px", "font-weight" = "bold"),
          textsize = "13px"
        ),

        # Popup: aparece al hacer click (HTML enriquecido)
        popup = ~paste0(
          "<div style='font-family: sans-serif; font-size: 13px; min-width: 180px;'>",
          "<b style='font-size: 15px;'>", name_muni, "</b><br>",
          "<span style='color: #666;'>", sigla_uf, " — ", region, "</span>",
          "<hr style='margin: 5px 0;'>",
          "<b>", input$partido, ":</b> ", pct, "%<br>",
          "<b>Población:</b> ",
            format(replace_na(population, 0), big.mark = ".", scientific = FALSE),
          "<br>",
          "<hr style='margin: 5px 0; border-style: dashed;'>",
          "<span style='color: #999; font-size: 11px;'>",
          "Hacé click para ver el detalle completo en la tabla.</span>",
          "</div>"
        ),

        # layerId: identificador único del polígono.
        # Shiny lo expone como input$mapa_shape_click$id cuando se hace click.
        layerId = ~id_municipio
      ) |>

      addLegend(
        position  = "bottomright",
        pal       = paleta,
        values    = ~pct,
        title     = paste0("% ", input$partido, "<br><small>(elecciones 2020)</small>"),
        labFormat = labelFormat(suffix = "%"),
        opacity   = 0.85
      )
  })


  # Zoom reactivo al cambio de estado
  # observeEvent() se dispara cuando input$estado cambia.
  # Usamos leafletProxy() para actualizar el zoom SIN re-renderizar el mapa entero.
  # Regenerar el mapa de 5000+ polígonos en cada cambio de estado sería muy lento.
  observeEvent(input$estado, {

    if (input$estado == "ALL") {
      # Bounding box aproximado de Brasil continental
      leafletProxy("mapa") |>
        fitBounds(lng1 = -74, lat1 = -33, lng2 = -35, lat2 = 5)

    } else {
      # Bounding box del estado elegido (calculado a partir del sf de estados)
      bbox <- sf_estados_raw |>
        filter(sigla_uf == input$estado) |>
        st_bbox()

      leafletProxy("mapa") |>
        fitBounds(
          lng1 = bbox["xmin"], lat1 = bbox["ymin"],
          lng2 = bbox["xmax"], lat2 = bbox["ymax"]
        )
    }
  })


  # Tabla reactiva al click en un municipio del mapa
  # input$mapa_shape_click se actualiza cada vez que el usuario hace click
  # en un polígono del mapa. Tiene los campos: id, lat, lng, group.
  output$tabla <- renderDT({

    click <- input$mapa_shape_click

    # Estado inicial: todavía no se hizo ningún click
    if (is.null(click)) {
      return(
        datatable(
          data.frame(
            Info = paste0(
              "Hacé click en cualquier municipio del mapa ",
              "para ver aquí su detalle."
            )
          ),
          rownames = FALSE,
          options  = list(dom = "t")
        )
      )
    }

    # Filtramos el municipio clickeado usando el layerId (= id_municipio)
    df_detalle <- datos_mapa() |>
      st_drop_geometry() |>
      filter(id_municipio == click$id) |>
      mutate(
        # Calculamos variables per cápita para el detalle
        gdp_pc        = round(gdp         / pmax(population, 1), 0),
        health_exp_pc = round(health_exp  / pmax(population, 1), 0),
        educ_exp_pc   = round(educ_exp    / pmax(population, 1), 0),
        urban_exp_pc  = round(urban_exp   / pmax(population, 1), 0),
        transfer_pc   = round(transfer_total / pmax(population, 1), 0)
      ) |>
      select(
        Municipio          = name_muni,
        Estado             = sigla_uf,
        Región             = region,
        `% Partido`        = pct,
        Población          = population,
        `PBI pc (R$)`      = gdp_pc,
        `Salud pc (R$)`    = health_exp_pc,
        `Educ. pc (R$)`    = educ_exp_pc,
        `Urbano pc (R$)`   = urban_exp_pc,
        `Transf. pc (R$)`  = transfer_pc
      )

    datatable(
      df_detalle,
      rownames = FALSE,
      options  = list(dom = "t", scrollX = TRUE)
    ) |>
      formatCurrency(
        columns  = c("PBI pc (R$)", "Salud pc (R$)", "Educ. pc (R$)",
                     "Urbano pc (R$)", "Transf. pc (R$)"),
        currency = "R$ ",
        digits   = 0,
        mark     = "."
      ) |>
      formatStyle(
        "% Partido",
        fontWeight = "bold",
        color      = "white",
        background = styleInterval(
          cuts   = c(10, 20, 30),
          values = c("#95a5a6", "#f39c12", "#e67e22", "#c0392b")
        )
      )
  })

}


# ══════════════════════════════════════════════════════════════════════════════
# ARRANQUE
# ══════════════════════════════════════════════════════════════════════════════

shinyApp(ui = ui, server = server)