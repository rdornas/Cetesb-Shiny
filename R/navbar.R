library(shiny)
library(shinydashboard)
library(shinythemes)
library(tippy)
library(sever)
library(DT)
library(fresh)
library(htmltools)
library(shinyWidgets)
library(leaflet)
library(echarts4r)
library(waiter)
library(magrittr)
library(lubridate)
library(tidyverse)

temazul <- create_theme(
  theme = "cerulean",
  bs_vars_wells(
    bg = "#CEECF5"
  )
)

sess_desconecta <- sever_default(
  title = "Ixi!",
  subtitle = "Sua sessão foi encerrada.",
  button = "Reconectar",
  button_class = "info"
)

ui <- navbarPage(
  title = "Cetesb",
  header = tagList(
    use_sever(),
    useShinydashboard(),
    use_theme(temazul)
  ),
  # Primeira aba ----
  tabPanel(
    title = "Apresentação",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h2("Sobre este app"),
        br(),
        p("Esta página foi elaborada como projeto de trabalho final do curso 'Dashboards', ofertado pela ",
          a(href = "https://www.curso-r.com/", "Curso-R."),
          "A proposta consistia na criação de um dashboard dinâmico, totalmente em R, utilizando o pacote",
          a(href = "https://shiny.rstudio.com/", "shiny"),
          "e seus derivados. Dentre as bases de dados disponíveis, foi escolhida aquela referente à concentração de alguns poluentes obtidos por meio de estações da Companhia Ambiental do Estado de São Paulo (Cetesb) localizadas na região metropolitana de São Paulo, entre 01/01/2017 e 31/05/2020. A fim de melhor esclarecer os parâmetros referentes a cada um dos poluentes, um breve texto é apresentado ao lado, retirado da",
          a(href = "https://cetesb.sp.gov.br/ar/poluentes/", "página da própria Cetesb.")
        ),
        br(),
        br(),
        br(),
        helpText("Para navegar na página, acesse as opções pelo menu superior."),
        br(),
        br(),
        hr(),
        h6("Autor do projeto: Rubem Dornas"),
        h6("Contato: ", a(href = "mailto:rapdornas@gmail.com", "rapdornas@gmail.com."))
      ),
      mainPanel(
        width = 8,
        h2("Poluentes avaliados"),
        br(),
        h4("Partículas Inaláveis (MP10)"),
        p("Podem ser definidas de maneira simplificada como aquelas cujo diâmetro aerodinâmico é menor ou igual a 10 µm. Dependendo da distribuição de tamanho na faixa de 0 a 10 µm, podem ficar retidas na parte superior do sistema respiratório ou penetrar mais profundamente, alcançando os alvéolos pulmonares."),
        br(),
        h4("Monóxido de Carbono (CO)"),
        p("É um gás incolor e inodoro que resulta da queima incompleta de combustíveis de origem orgânica (combustíveis fósseis, biomassa etc.). Em geral é encontrado em maiores concentrações nas cidades, emitido principalmente por veículos automotores. Altas concentrações de CO são encontradas em áreas de intensa circulação de veículos."
        ),
        br(),
        h4(HTML(paste0("Oxidantes Fotoquímicos, como o Ozônio (O",
                       tags$sub("3"),
                       ")"))),
        p("“Oxidantes fotoquímicos” é a denominação que se dá à mistura de poluentes secundários formados por reações entre os óxidos de nitrogênio e compostos orgânicos voláteis, na presença de luz solar, sendo estes últimos liberados na queima incompleta e evaporação de combustíveis e solventes. O principal produto dessa reação é o ozônio, por isso mesmo utilizado como parâmetro indicador da presença de oxidantes fotoquímicos na atmosfera. Tais poluentes formam a chamada névoa fotoquímica ou “smog fotoquímico”, que possui esse nome porque causa na atmosfera diminuição da visibilidade."),
        br(),
        h4(HTML(paste0("Óxidos de Nitrogênio (NO",
                       tags$sub("x"),
                       ")"))),
        p(HTML(paste0("São formados durante processos de combustão. Em grandes cidades, os veículos geralmente são os principais responsáveis pela emissão dos óxidos de nitrogênio. O NO, sob a ação de luz solar se transforma em NO",
                      tags$sub("2"),
                      " tem papel importante na formação de oxidantes fotoquímicos como o ozônio. Dependendo das concentrações, o NO",
                      tags$sub("2"),
                      " causa prejuízos à saúde.")))
      )
    ),
    hr(),
    box(
      width = 12,
      title = "Sumário de informações quantitativas:",
      background = NULL,
      solidHeader = T,
      use_waitress(),
      fluidRow(
        column(
          width = 4,
          valueBoxOutput(
            width = 12,
            outputId = "num_estacoes"
          )
        ),
        column(
          width = 4,
          valueBoxOutput(
            width = 12,
            outputId = "num_poluentes"
          )
        ),
        column(
          width = 4,
          valueBoxOutput(
            width = 12,
            outputId = "num_datas"
          )
        )
      )
    )
  ),
  # Segunda aba ----
  navbarMenu(
    title = "Análise descritiva",
    #Opção 1 ----
    tabPanel(
      title = "Análise por hora do dia",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h4("Selecione uma estação", icon("info-circle", class = "tip")),
          tippy_class(
            class = "tip",
            content = "<span style='font-size:16px;'>Ao selecionar uma estação no mapa, um gráfico com a concentração média de todos os poluentes disponíveis para essa estação será exibido no painel ao lado. Os respectivos poluentes constarão da barra de legenda acima do gráfico. Ao clicar na legenda de cada poluente é possível ligar/desligar essa informação no gráfico. Por fim, para selecionar um intervalo, basta deslizar os cantos da barra localizada na parte inferior do gráfico.<span>",
            allowHTML = TRUE),
          leafletOutput(
            outputId = "mapa"
          ),
        ),
        mainPanel(
          width = 8,
          echarts4rOutput(
            outputId = "graficohora"
          ),
          hr(),
          conditionalPanel(
            condition = "input.mapa_marker_click",
            materialSwitch(
              inputId = "switch1",
              label = "Exibir tabela",
              value = FALSE,
              status = "info"
            )
          ),
          conditionalPanel(
            condition = "input.switch1",
            dataTableOutput(
              "tabelahora"
            )
          )
        )
      )
    ),
    # Opção 2 ----
    tabPanel(
      title = "Análise por dia da semana",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h4("Selecione uma estação", icon("info-circle", class = "tip")),
          tippy_class(
            class = "tip",
            allowHTML = TRUE),
          leafletOutput(
            outputId = "mapadiasem"
          ),
        ),
        mainPanel(
          width = 8,
          echarts4rOutput(
            outputId = "graficodiasem"
          ),
          hr(),
          conditionalPanel(
            condition = "input.mapadiasem_marker_click",
            materialSwitch(
              inputId = "switch2",
              label = "Exibir tabela",
              value = FALSE,
              status = "info"
            )
          ),
          conditionalPanel(
            condition = "input.switch2",
            dataTableOutput(
              outputId = "tabeladiasem"
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  sever(html = sess_desconecta,
        bg_color = "white",
        color = "black")

  waitress <- Waitress$new(theme = "overlay-percent",
                           min = 0,
                           max = 10,
  )

  waitress$notify()

  for(i in 1:10){
    waitress$inc(1) # aumento (increase)
    Sys.sleep(.3)
  }

  cetesb <- readRDS("cetesb.rds") %>%
    mutate(
      # horario = if_else(condition = str_length(hora) == 1,
      #                   true = str_pad(hora, width = 4, pad = 0, side = "both"),
      #                   false = str_pad(hora, width = 4, pad = 0, side = "right")),
      # horario = hms::parse_hm(paste0(str_extract(horario, "^.{2}"), ":", str_extract(horario, ".{2}$"))),
      # Hora = hour(horario),
      # Hora = as.factor(ifelse(Hora == 24, 0, Hora))
      Hora = as.factor(ifelse(hora == 24, 0, hora)),
      Dia = wday(data, label = TRUE, abbr = TRUE),
      Mes = month(data)) %>%
    rename(Poluente = poluente)

  waitress$close() # fecha quando finalizado

  # Primeira aba ----
  output$num_estacoes <- renderValueBox({
    valueBox(
      subtitle = "Estações de coleta",
      value = n_distinct(cetesb$estacao_cetesb),
      color = "aqua",
      icon = icon("map-marker-alt")
    )
  })

  output$num_poluentes <- renderValueBox({
    valueBox(
      subtitle = "Poluentes avaliados",
      value = n_distinct(cetesb$Poluente),
      color = "aqua",
      icon = icon("smog")
    )
  })

  output$num_datas <- renderValueBox({
    valueBox(
      subtitle = "Datas amostradas",
      value = n_distinct(cetesb$data),
      color = "aqua",
      icon = icon("calendar-alt")
    )
  })

  # Segunda aba, Opção 1 ----
  output$mapa <- renderLeaflet({
    cetesb %>%
      distinct(estacao_cetesb, lat, long) %>%
      leaflet() %>%
      addProviderTiles('Esri.WorldImagery') %>% # Sugestões
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      addCircleMarkers(
        lng = ~long,
        lat = ~lat,
        color = "#00BFFF",
        radius = 5,
        label = ~estacao_cetesb,
        popup = ~estacao_cetesb,
        layerId = ~estacao_cetesb
      )
  })

  # output$ui_poluente <- renderUI({
  #   pol <- cetesb %>%
  #     distinct(Poluente) %>%
  #     pull() %>%
  #     sort()
  #
  #   pickerInput(
  #     inputId = "poluente",
  #     label = "Selecione os poluentes de interesse:",
  #     choices = unique(cetesb[which(cetesb$estacao_cetesb == input$mapa_marker_click$id), 2]),
  #     selected = pol,
  #     multiple = TRUE,
  #     options = pickerOptions(
  #       actionsBox = TRUE,
  #       multipleSeparator = "; ",
  #       selectOnTab	= TRUE,
  #       noneSelectedText = "Aguardando seleção",
  #       selectAllText = "Selecionar tudo",
  #       deselectAllText = "Limpar tudo",
  #       size = 10)
  #   )
  # })

  e_common(font_family = "Open Sans")

  output$graficohora <- renderEcharts4r({

    #req(input$poluente)
    req(input$mapa_marker_click$id)

    cetesb %>%
      filter(
        estacao_cetesb == input$mapa_marker_click$id) %>%
      #Poluente %in% input$poluente
      group_by(Hora, Poluente) %>%
      summarise(concentracao_media = round(mean(concentracao, na.rm = TRUE), 2)) %>%
      ungroup() %>%
      group_by(Poluente) %>%
      arrange(Hora) %>%
      mutate(Hora = as.character.factor(Hora)) %>%
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
                    y = "Concentração média") %>%
      e_grid(left = 90) %>%  # dá um espaço ao lado para evitar cortar o gráfico
      #e_format_y_axis(prefix = "U$") %>%
      e_legend(top = .5) %>%
      e_tooltip(trigger = "axis",
                axisPointer = list(type = "line")) %>%
      e_datazoom(x_index = 0) %>%
      e_toolbox() %>%
      e_theme("macarons") %>%
      e_toolbox_feature(feature = "magicType",
                        type = list("bar", "line", "tiled"),
                        title = list(line = "Trocar para gráfico de linha",
                                     bar = "Trocar para gráfico de barra"))
  })

  output$tabelahora <- renderDataTable({

    req(input$mapa_marker_click$id)

    cetesb %>%
      filter(
        estacao_cetesb == input$mapa_marker_click$id) %>%
      group_by(estacao_cetesb, Hora, Poluente) %>%
      summarise(concentracao_media = mean(concentracao, na.rm = TRUE)) %>%
      datatable(rownames = FALSE,
                class = "stripe hover order-column",
                filter = "top",
                colnames = c("Estação" = "estacao_cetesb",
                             "Concentração média" = "concentracao_media"),
                caption = "Se desejar fazer o download completo dessa tabela, selecione 'todos' na opção 'Exibir registros', localizada abaixo da tabela.",
                extensions = 'Buttons', # create buttons
                options = list(dom = 'Bfrtlip', # code for all attributes that are printed in the table,
                               searching = FALSE,
                               #searchHighlight = TRUE,
                               language = list(
                                 paginate =
                                   list(
                                     sPrevious = "Anterior",
                                     sNext = "Próxima"),
                                 searchPlaceholder = "todos",
                                 search = "Pesquisar:",
                                 thousands = ".",
                                 decimal = ",",
                                 loadingRecords = "Carregando...",
                                 lengthMenu = "Exibir _MENU_ registros",
                                 info = "Exibindo de _START_ a _END_ de _TOTAL_ registros.",
                                 infoEmpty = "Exibindo 0 de 0 registros de um total de 0.",
                                 sProcessing = "Processando...",
                                 sInfoFiltered = "(Filtrados de _MAX_ registros)",
                                 sZeroRecords = "Nenhum registro encontrado",
                                 sEmptyTable = "Nenhum registro encontrado"
                               ),
                               buttons = list(
                                 list(
                                   extend = "copy",
                                   text = "Copiar",
                                   exportOptions = list(
                                     modifier = list(
                                       selected = TRUE
                                     )
                                   )
                                 ),
                                 list(
                                   extend = "collection",
                                   buttons = c("csv", "excel"),
                                   text = "Download"
                                 )
                               ), # end of buttons customization
                               # customize the length menu
                               lengthMenu = list(
                                 c(25, 50, 100, -1),
                                 c(25, 50, 100, "todos")
                               ),
                               pageLength = 10,
                               autoWidth = T)) %>%
      formatRound(columns = c(4),
                  digits = 2,
                  dec.mark = ",") %>%
      formatStyle(columns = c(1:3),
                  'text-align' = 'center')
  })

  #Segunda aba, Opção 2 ----
  output$mapadiasem <- renderLeaflet({
    cetesb %>%
      distinct(estacao_cetesb, lat, long) %>%
      leaflet() %>%
      addProviderTiles('Esri.WorldImagery') %>% # Sugestões
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      addCircleMarkers(
        lng = ~long,
        lat = ~lat,
        radius = 5,
        label = ~estacao_cetesb,
        popup = ~estacao_cetesb,
        layerId = ~estacao_cetesb
      )
  })

  output$graficodiasem <- renderEcharts4r({

    #req(input$poluente)
    req(input$mapadiasem_marker_click$id)

    cetesb %>%
      filter(
        estacao_cetesb == input$mapadiasem_marker_click$id) %>%
      group_by(Dia, Poluente) %>%
      summarise(concentracao_media = round(mean(concentracao, na.rm = TRUE), 2)) %>%
      ungroup() %>%
      group_by(Poluente) %>%
      arrange(Dia) %>%
      mutate(Dia = as.character.factor(Dia)) %>%
      e_charts(x = Dia) %>%
      e_line(serie = concentracao_media,
             smooth = T,
             x_index = 0,
             #y_index = 0,
             #name = "Concentração do poluente",
             legend = T) %>%
      e_x_axis(nameTextStyle = list(fontWeight = "bold")) %>%
      e_y_axis(nameTextStyle = list(fontWeight = "bold")) %>%
      e_axis_labels(x = "Dia",
                    y = "Concentração média") %>%
      e_grid(left = 90) %>%  # dá um espaço ao lado para evitar cortar o gráfico
      #e_format_y_axis(prefix = "U$") %>%
      e_legend(top = .5) %>%
      e_tooltip(trigger = "axis",
                axisPointer = list(type = "line")) %>%
      e_datazoom(x_index = 0) %>%
      e_toolbox() %>%
      e_theme("macarons") %>%
      e_toolbox_feature(feature = "magicType",
                        type = list("bar", "line", "tiled"),
                        title = list(line = "Trocar para gráfico de linha",
                                     bar = "Trocar para gráfico de barra"))
  })

  output$tabeladiasem <- renderDataTable({

    req(input$mapadiasem_marker_click$id)

    cetesb %>%
      filter(
        estacao_cetesb == input$mapadiasem_marker_click$id) %>%
      group_by(estacao_cetesb, Dia, Poluente) %>%
      summarise(concentracao_media = mean(concentracao, na.rm = TRUE)) %>%
      datatable(rownames = FALSE,
                class = "stripe hover order-column",
                filter = "top",
                colnames = c("Estação" = "estacao_cetesb",
                             "Concentração média" = "concentracao_media"),
                caption = "Se desejar fazer o download completo dessa tabela, selecione 'todos' na opção 'Exibir registros', localizada abaixo da tabela.",
                extensions = 'Buttons', # create buttons
                options = list(dom = 'Bfrtlip', # code for all attributes that are printed in the table,
                               searching = FALSE,
                               #searchHighlight = TRUE,
                               language = list(
                                 paginate =
                                   list(
                                     sPrevious = "Anterior",
                                     sNext = "Próxima"),
                                 searchPlaceholder = "todos",
                                 search = "Pesquisar:",
                                 thousands = ".",
                                 decimal = ",",
                                 loadingRecords = "Carregando...",
                                 lengthMenu = "Exibir _MENU_ registros",
                                 info = "Exibindo de _START_ a _END_ de _TOTAL_ registros.",
                                 infoEmpty = "Exibindo 0 de 0 registros de um total de 0.",
                                 sProcessing = "Processando...",
                                 sInfoFiltered = "(Filtrados de _MAX_ registros)",
                                 sZeroRecords = "Nenhum registro encontrado",
                                 sEmptyTable = "Nenhum registro encontrado"
                               ),
                               buttons = list(
                                 list(
                                   extend = "copy",
                                   text = "Copiar",
                                   exportOptions = list(
                                     modifier = list(
                                       selected = TRUE
                                     )
                                   )
                                 ),
                                 list(
                                   extend = "collection",
                                   buttons = c("csv", "excel"),
                                   text = "Download"
                                 )
                               ), # end of buttons customization
                               # customize the length menu
                               lengthMenu = list(
                                 c(25, 50, 100, -1),
                                 c(25, 50, 100, "todos")
                               ),
                               pageLength = 10,
                               autoWidth = T)) %>%
      formatRound(columns = c(4),
                  digits = 2,
                  dec.mark = ",") %>%
      formatStyle(columns = c(1:3),
                  'text-align' = 'center')
  })

}

shinyApp(ui, server)
