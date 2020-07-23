library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(magrittr)
library(fresh)
library(echarts4r)
library(lubridate)
library(tidyverse)


ui <- dashboardPage(
  dashboardHeader(
    title = "Cetesb"
    ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Informações gerais", tabName = "info"),
      menuItem(text = "Concentração de poluentes por hora do dia", tabName = "horadia")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "info",
        h1("Informações gerais:"),
        infoBoxOutput(
          outputId = "num_estacoes"
        ),
        infoBoxOutput(
          outputId = "num_datas"
        ),
        infoBoxOutput(
          outputId = "num_poluentes"
        ) %>% withSpinner(type = 4)
      ),
      tabItem(
        tabName = "horadia",
        h1("Análise de poluentes"),
        br(),
        box(
          width = 12,
          title = "Seleção de variáveis",
          background = "light-blue",
          solidHeader = F,
          fluidRow(
            column(
              width = 6,
              uiOutput("ui_estacao")
            ),
            column(
              width = 6,
              uiOutput("ui_poluente")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            footer = "Fonte: Cetesb",
            echarts4rOutput(outputId = "graficodados") %>%
              withSpinner(type = 4)
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  cetesb <- readRDS("cetesb.rds") %>%
    mutate(horario = if_else(condition = str_length(hora) == 1,
                             true = str_pad(hora, width = 4, pad = 0, side = "both"),
                             false = str_pad(hora, width = 4, pad = 0, side = "right")),
           horario = hms::parse_hm(paste0(str_extract(horario, "^.{2}"), ":", str_extract(horario, ".{2}$"))),
           Hora = hour(horario),
           Hora = as.factor(ifelse(Hora == 24, 0, Hora))) %>%
    rename(Poluente = poluente)

  output$num_estacoes <- renderInfoBox({
    infoBox(
      title = "Número de estações de coleta",
      value = n_distinct(cetesb$estacao_cetesb),
      fill = TRUE,
      color = "green",
      icon = shiny::icon("hashtag")
    )
  })

  output$num_datas <- renderInfoBox({
    infoBox(
      title = "Número de datas amostradas",
      value = n_distinct(cetesb$data),
      fill = TRUE,
      color = "blue",
      icon = shiny::icon("hashtag")
    )
  })

  output$num_poluentes <- renderInfoBox({
    infoBox(
      title = "Número de poluentes avaliados",
      value = n_distinct(cetesb$Poluente),
      fill = TRUE,
      color = "red",
      icon = shiny::icon("hashtag")
    )
  })

  output$ui_estacao <- renderUI({
    est <- cetesb %>%
      distinct(estacao_cetesb) %>%
      pull() %>%
      sort()

    selectInput(
      inputId = "estacao",
      label = "Selecione uma estação de amostragem:",
      choices = est
    )
  })

  output$ui_poluente <- renderUI({
    pol <- cetesb %>%
      distinct(Poluente) %>%
      pull() %>%
      sort()

    pickerInput(
      inputId = "poluente",
      label = "Selecione os poluentes de interesse:",
      choices = unique(cetesb[which(cetesb$estacao_cetesb == input$estacao), 2]),
      selected = pol,
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        multipleSeparator = "; ",
        selectOnTab	= TRUE,
        noneSelectedText = "Aguardando seleção",
        selectAllText = "Selecionar tudo",
        deselectAllText = "Limpar tudo",
        size = 10)
    )
  })

  e_common(font_family = "Open Sans",
           theme = "macarons")

  output$graficodados <- renderEcharts4r({

    req(input$estacao)
    req(input$poluente)

    cetesb %>%
      filter(estacao_cetesb == input$estacao,
             Poluente %in% input$poluente) %>%
      group_by(Hora, Poluente) %>%
      summarise(concentracao_media = mean(concentracao, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(Poluente) %>%
      e_charts(x = Hora) %>%
      e_line(serie = concentracao_media,
             smooth = T,
             x_index = 0,
             #y_index = 0,
             #name = "Concentração do poluente",
             legend = T) %>%
      e_x_axis(nameTextStyle = list(fontWeight = "bold")) %>%
      e_y_axis(nameTextStyle = list(fontWeight = "bold")) %>%
      e_axis_labels(x = "Hora",
                    y = "Concentração do poluente") %>%
      #e_format_y_axis(prefix = "U$") %>%
      e_legend(top = .5) %>%
      e_tooltip(trigger = "axis",
                axisPointer = list(type = "line")) %>%
      e_datazoom(x_index = 0) %>%
      e_toolbox() %>%
      e_toolbox_feature(feature = "magicType",
                        type = list("line", "bar", "tiled"))
  })
}

shinyApp(ui = ui, server = server)
