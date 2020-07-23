library(shiny)
library(shinydashboard)
library(shinythemes)
library(tippy)
#library(sever)
library(DT)
library(fresh)
library(htmltools)
library(shinyWidgets)
library(leaflet)
library(echarts4r)
library(shinycssloaders)
library(waiter)
library(rpivotTable)
library(magrittr)
library(lubridate)
library(tidyverse)

options(rsconnect.locale = "pt_br.utf-8")

temazul <- create_theme(
  theme = "cerulean",
  bs_vars_navbar(
    default_bg = "#CEECF5",
    default_color = "#20c1ed",
    default_link_color = "#207faa", #cor do texto normal (sem seleção ou hover)
    default_link_active_color = "#00a6d1", #cor do texto quando a opção está selecionada
    default_link_hover_color = "#00a6d1" #cor do texto quando se passa o mouse sobre
  ),
  bs_vars_wells(
    bg = "#CEECF5"
  )
)

# sess_desconecta <- sever_default(
#   title = "Íxi!",
#   subtitle = "Sua sessão foi encerrada.",
#   button = "Reconectar",
#   button_class = "info"
# )

ui <- navbarPage(
  title = div(
    img(
      src = "cetesb_transp.png",
      style = "margin-top: -14px; padding-bottom:10px",
      width = "221.19px",
      height = "60px"
    )
  ),
  windowTitle = "Shiny App - Cetesb Poluentes",
  header = tagList(
    #use_sever(),
    useShinydashboard(),
    use_theme(temazul)
  ),
  # Primeira aba ----
  tabPanel(
    title = "Início",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h2("Sobre este app"),
        br(),
        p("Esta página foi elaborada como projeto de trabalho final do curso 'Dashboards', ofertado pela ",
          a(href = "https://www.curso-r.com/", "Curso-R."),
          "A proposta consistia na criação de um dashboard dinâmico, totalmente em R, utilizando o pacote",
          a(href = "https://shiny.rstudio.com/", "shiny"),
          "e seus derivados. Dentre as bases de dados disponíveis, foi escolhida aquela referente à concentração de alguns poluentes obtida por meio de estações da Companhia Ambiental do Estado de São Paulo (Cetesb) localizadas na região metropolitana de São Paulo, entre 01/01/2017 e 31/05/2020. A intenção primordial desse app é uma avaliação geral dos dados, focada em atributos temporais, incluindo fases pré e pós-decreto de quarentena no estado de São Paulo. A fim de melhor esclarecer os principais poluentes sob análise, um breve texto é apresentado ao lado, retirado da",
          a(href = "https://cetesb.sp.gov.br/ar/poluentes/", "página da própria Cetesb.")
        ),
        br(),
        br(),
        helpText("Para navegar na página, acesse as opções pelo menu superior."),
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
      use_waitress(
        color = "#20c1ed",
        percent_color = "#357faa"
      ),
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
    title = "Análise de dados",
    #Opção 1 ----
    tabPanel(
      title = "Apresentação por período",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          h5(
            icon("info-circle"),
            "Selecione uma estação",
            class = "tip_map",
            `data-tippy-content` = "<span style='font-size:16px;color:#20c1ed'>Ao selecionar uma estação no mapa, um gráfico com a concentração média de todos os poluentes disponíveis para essa estação será exibido no painel ao lado. Os respectivos poluentes constarão da barra de legenda acima do gráfico. Ao clicar na legenda de cada poluente é possível ligar/desligar essa informação, fazendo com que ela seja (ou não) apresentada. Por fim, para selecionar um intervalo específico, basta deslizar os cantos da barra localizada na parte inferior do gráfico. A temporalidade relacionada ao tempo pode ser alterada de acordo com as opções abaixo do mapa.<span>"
          ),
          tippy_class(
            class = "tip_map",
            theme = "light-border",
            allowHTML = TRUE
          ),
          leafletOutput(
            outputId = "mapa"
          ),
          br(),
          awesomeRadio(
            inputId = "temporal",
            label = h5("Selecione uma opção:"),
            choices = c("Hora", "Dia", "Mês"),
            selected = "Hora",
            status = "info",
            inline = TRUE
          )
        ),
        mainPanel(
          width = 8,
          echarts4rOutput(
            outputId = "grafico"
          ),
          hr(),
          conditionalPanel(
            condition = "input.mapa_marker_click",
            materialSwitch(
              inputId = "switch1",
              label = h5("Exibir tabela"),
              value = FALSE,
              status = "info"
            )
          ),
          conditionalPanel(
            condition = "input.switch1",
            dataTableOutput(
              outputId = "tabela"
            ) %>% withSpinner(
              type = 3,
              color.background = "#20c1ed",
              color = "#357faa",
            )
          )
        )
      )
    ),
    #Opção 2 ----
    tabPanel(
      title = "Análise relacionada à COVID-19",
      sidebarLayout(
        sidebarPanel(
          uiOutput(
            outputId = "ui_estacao"
          ),
          hr(),
          uiOutput(
            outputId = "ui_poluente"
          ),
          hr(),
          awesomeRadio(
            inputId = "temporal2",
            label = h5("Selecione uma opção:"),
            choices = c("Hora", "Dia"),
            selected = "Hora",
            status = "info",
            inline = TRUE
          ),
          hr(),
          br(),
          br(),
          h4("Análise:"),
          p("O período 'pré-quarentena' é aquele anteriormente ao decreto do estado de São Paulo, emitido pelo governo em 24/03/2020. Consequentemente, o período 'pós-quarentena' é compreendido do próprio dia 24/03/2020 até 31/05/2020. É possível perceber uma alteração dos valores dos poluentes entre as fases analisadas, com redução dos gases tóxicos após a regulamentação do fechamento de estabelecimentos e distanciamento social. Todavia, os resultados devem ser avaliados com cautela, uma vez que trata-se das médias obtidas para os períodos, sendo que a fase 'pós-quarentena' está representada por menos de três meses - que não são aqueles com maiores valores para esses poluentes (ver 'Calendário de concentração de poluentes' no menu 'Visualizações especiais dos dados). Dessa forma, a simples comparação entre as fases pode levar a conclusões equivocadas, haja vista que o conjunto de dados é menor e menos abrangente no período 'pós' em relação ao período 'pré'.")
        ),
        mainPanel(
          echarts4rOutput(outputId = "line_quarentena"),
          hr(),
          echarts4rOutput(outputId = "box_quarentena")
        )
      )
    )
  ),
  #Terceira aba ----
  navbarMenu(
    title = "Visualizações especiais dos dados",
    # Opção 1 ----
    tabPanel(
      title = "Calendário de concentração de poluentes",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          uiOutput(
            outputId = "ui_estacao2"
          ),
          hr(),
          uiOutput(
            outputId = "ui_poluente2"
          ),
          hr(),
        ),
        mainPanel(
          width = 9,
          echarts4rOutput(
            height = "700px",
            outputId = "calend_quarentena"
          ) %>% withSpinner(
            type = 3,
            color.background = "#20c1ed",
            color = "#357faa",
          )
        )
      )
    ),
    # Opção 2 ----
    tabPanel(
      title = "Tabela dinâmica",
      fluidPage(
        h3(
          icon("info-circle"),
          "Tabela dinâmica",
          class = "tip_pivot"
        ),
        tippy_class(
          theme = "light-border",
          class = "tip_pivot",
          allowHTML = TRUE,
          content = "<span style='font-size:16px;color:#20c1ed'>O banco de dados completo está disponível nessa tabela dinâmica, que oferece várias possibilidades de análise. A barra branca destacada como 'Tabela', provê diversas visualizações dos dados, tais como mapas de calor, mapas de árvore e até mesmo gráficos. Logo abaixo, uma gama de opções está disponível, como o cálculo pela média, contagem, soma, listagem de valores, entre outras. Para manipular a tabela dinâmica, basta arrastar os campos entre os espaços reservados para as linhas e colunas.<span>"
        ),
        rpivotTableOutput(
          outputId = "pivot"
        ) %>% withSpinner(
          type = 3,
          color.background = "#20c1ed",
          color = "#357faa",
        )
      )
    )
  )
)

server <- function(input, output, session) {
  #Sys.setlocale("LC_ALL", "pt_br.utf-8")

  # sever(html = sess_desconecta,
  #       bg_color = "#CEECF5",
  #       color = "black")

  waitress <- Waitress$new(
    "#num_datas",
    theme = "overlay-percent",
    min = 0,
    max = 10
  )

  waitress$notify()

  for(i in 1:10){
    waitress$inc(1) # aumento (increase)
    Sys.sleep(.3)
  }

  cetesb <- readRDS("data-raw/cetesb.rds") %>%
    mutate(
      Hora = as.factor(ifelse(hora == 24, 0, hora)),
      Dia = wday(data, label = TRUE, abbr = TRUE),
      Mês = month(data, label = TRUE, abbr = TRUE)) %>%
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


  e_common(font_family = "Open Sans")

  output$grafico <- renderEcharts4r({

    req(input$mapa_marker_click$id)

    cetesb %>%
      filter(estacao_cetesb == input$mapa_marker_click$id) %>%
      group_by_(input$temporal, quo(Poluente)) %>%
      summarise(concentracao_media = round(mean(concentracao, na.rm = TRUE), 2)) %>%
      ungroup() %>%
      group_by(Poluente) %>%
      mutate(across(where(is.factor), as.character.factor)) %>%
      arrange(input$temporal) %>%
      e_charts_(x = input$temporal) %>%
      e_line(serie = concentracao_media,
             smooth = T,
             x_index = 0,
             legend = T) %>%
      e_x_axis(nameTextStyle = list(fontWeight = "bold")) %>%
      e_y_axis(nameTextStyle = list(fontWeight = "bold")) %>%
      e_axis_labels(x = input$temporal,
                    y = "Concentração média") %>%
      e_grid(left = 90) %>%  # dá um espaço ao lado para evitar cortar o gráfico
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

  output$tabela <- renderDataTable({

    cetesb %>%
      filter(
        estacao_cetesb == input$mapa_marker_click$id) %>%
      group_by_(quo(estacao_cetesb), input$temporal, quo(Poluente)) %>%
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

  output$ui_estacao <- renderUI({
    est <- cetesb %>%
      distinct(estacao_cetesb) %>%
      pull() %>%
      sort()

    pickerInput(
      inputId = "estacao",
      label = h5("Selecione a(s) estação(ções) de interesse:"),
      choices = est,
      selected = est,
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        selectOnTab	= TRUE,
        multipleSeparator = "; ",
        noneSelectedText = "Aguardando seleção",
        selectAllText = "Selecionar tudo",
        deselectAllText = "Limpar tudo",
        selectedTextFormat = "count > 4",
        countSelectedText = "{0} estações selecionadas (de {1} totais)",
        style = "btn-primary",
        size = 10
      )
    )
  })

  output$ui_poluente <- renderUI({
    req(input$estacao)

    pol <- cetesb %>%
      distinct(Poluente) %>%
      pull() %>%
      sort()

    pickerInput(
      inputId = "poluente",
      label = h5("Selecione os poluentes de interesse:"),
      choices = unique(cetesb[which(cetesb$estacao_cetesb == input$estacao), 2]),
      selected = "CO",
      options = pickerOptions(
        actionsBox = TRUE,
        selectOnTab	= TRUE,
        noneSelectedText = "Aguardando seleção",
        selectAllText = "Selecionar tudo",
        deselectAllText = "Limpar tudo",
        style = "btn-primary",
        size = 10)
    )
  })

  output$line_quarentena <- renderEcharts4r({
    req(input$estacao)
    req(input$poluente)

    cetesb %>%
      filter(estacao_cetesb %in% input$estacao) %>%
      mutate(Fase = if_else(condition = data < "2020-03-24",
                            true = "Pré-quarentena",
                            false = "Pós-quarentena"),
             Fase = fct_relevel(as.factor(Fase), "Pré-quarentena", "Pós-quarentena")) %>%
      group_by_(input$temporal2, quo(Poluente), quo(Fase)) %>%
      summarise(concentracao_media = round(mean(concentracao, na.rm = TRUE), 2)) %>%
      ungroup() %>%
      filter(Poluente == input$poluente) %>%
      group_by(Fase) %>%
      mutate(across(where(is.factor), as.character.factor)) %>%
      arrange(input$temporal2) %>%
      e_charts_(x = input$temporal2) %>%
      e_line(serie = concentracao_media,
             smooth = T,
             x_index = 0,
             legend = T) %>%
      e_show_loading(text_color = "#357faa",
                     color = "#20c1ed",
                     text = "Carregando...") %>%
      e_x_axis(nameTextStyle = list(fontWeight = "bold")) %>%
      e_y_axis(nameTextStyle = list(fontWeight = "bold")) %>%
      e_axis_labels(x = input$temporal2,
                    y = paste0("Concentração média de ", input$poluente)) %>%
      e_grid(left = 90) %>%  # dá um espaço ao lado para evitar cortar o gráfico
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

  output$box_quarentena <- renderEcharts4r({
    req(input$estacao)
    req(input$poluente)

    cetesb %>%
      filter(estacao_cetesb %in% input$estacao,
             Poluente == input$poluente,
             !is.na(concentracao)) %>%
      mutate(Fase = if_else(condition = data < "2020-03-24",
                            true = "Pré-quarentena",
                            false = "Pós-quarentena"),
             Fase = fct_relevel(as.factor(Fase), "Pré-quarentena", "Pós-quarentena")) %>%
      reshape2::dcast(Hora ~ Fase, fun.aggregate = mean, value.var = "concentracao") %>%
      mutate(across(where(is.numeric), round, 2)) %>%
      e_charts() %>%
      e_boxplot(`Pré-quarentena`,
                name = NULL,
                outliers = TRUE,
                itemStyle = list(color = "rgba(20, 237, 172, 0)")) %>%
      e_boxplot(`Pós-quarentena`,
                name = NULL,
                outliers = TRUE,
                itemStyle = list(color = "rgba(20, 237, 172, 0)",
                                 borderColor = "#b4a2db")) %>%
      e_show_loading(text_color = "#357faa",
                     color = "#20c1ed",
                     text = "Carregando...") %>%
      e_theme("macarons") %>%
      e_x_axis(nameTextStyle = list(fontWeight = "bold")) %>%
      e_y_axis(nameTextStyle = list(fontWeight = "bold")) %>%
      e_axis_labels(x = "Fase",
                    y = paste0("Concentração de ", input$poluente)) %>%
      e_tooltip(trigger = "axis",
                axisPointer = list(type = "shadow")) %>%
      e_toolbox_feature(feature = "magicType")
  })

  #Terceira aba, Opção 1 ----

  output$ui_estacao2 <- renderUI({
    est <- cetesb %>%
      distinct(estacao_cetesb) %>%
      pull() %>%
      sort()

    pickerInput(
      inputId = "estacao2",
      label = h5("Selecione a estação de interesse:"),
      choices = est,
      selected = "Congonhas",
      multiple = FALSE,
      options = pickerOptions(
        actionsBox = TRUE,
        selectOnTab	= TRUE,
        style = "btn-primary",
        size = 10
      )
    )
  })

  output$ui_poluente2 <- renderUI({
    req(input$estacao2)

    pol <- cetesb %>%
      distinct(Poluente) %>%
      pull() %>%
      sort()

    pickerInput(
      inputId = "poluente2",
      label = h5("Selecione o poluente de interesse:"),
      choices = unique(cetesb[which(cetesb$estacao_cetesb == input$estacao2), 2]),
      selected = "CO",
      multiple = FALSE,
      options = pickerOptions(
        actionsBox = TRUE,
        selectOnTab	= TRUE,
        style = "btn-primary",
        size = 10)
    )
  })

  output$calend_quarentena <- renderEcharts4r({
    req(input$estacao2)
    req(input$poluente2)

    dayLabel <- c("D", "S", "T", "Q", "Q", "S", "S")
    monthLabel <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

    cetesb %>%
      filter(estacao_cetesb == input$estacao2,
             Poluente == input$poluente2) %>%
      group_by(Poluente, estacao_cetesb, data) %>%
      summarise(concentracao_media = round(mean(concentracao, na.rm = TRUE), 2)) %>%
      mutate(Ano = as.character(year(data))) %>%
      ungroup() %>%
      group_by(Ano) %>%
      e_charts(data) %>%
      e_calendar(range = "2017",
                 top = 30,
                 left = "10%",
                 right = "10%",
                 cellSize = 15,
                 dayLabel = list(nameMap = dayLabel),
                 monthLabel = list(nameMap = monthLabel)) %>%
      e_calendar(range = "2018",
                 top = 180,
                 left = "10%",
                 right = "10%",
                 cellSize = 15,
                 dayLabel = list(nameMap = dayLabel),
                 monthLabel = list(nameMap = monthLabel)) %>%
      e_calendar(range = "2019",
                 top = 330,
                 left = "10%",
                 right = "10%",
                 cellSize = 15,
                 dayLabel = list(nameMap = dayLabel),
                 monthLabel = list(nameMap = monthLabel)) %>%
      e_calendar(range = c("2020-01-01", "2020-05-31"),
                 top = 480,
                 left = "10%",
                 #right = "10%", # não colocar para deixar ajustado com os calendários acima
                 cellSize = 15,
                 dayLabel = list(nameMap = dayLabel),
                 monthLabel = list(nameMap = monthLabel)) %>%
      e_heatmap(concentracao_media,
                coord_system = "calendar") %>%
      e_visual_map(concentracao_media,
                   inRange = list(
                     color = c("#f7fbff",
                               "#deebf7",
                               "#c6dbef",
                               "#9ecae1",
                               "#6baed6",
                               "#4292c6",
                               "#2171b5",
                               "#08519c",
                               "#08306b")
                   ),
                   show = FALSE,
                   precision = 1,
                   borderColor = "#b4a2db",
                   orient = "horizontal",
                   left = "center",
                   top = 600) %>%
      e_show_loading(text_color = "#357faa",
                     color = "#20c1ed",
                     text = "Carregando...") %>%
      e_tooltip()
  })

  #Terceira aba, Opção 2 ----
  output$pivot <- renderRpivotTable({

    cetesb2 <- cetesb %>%
      mutate(Fase = if_else(condition = data < "2020-03-24",
                            true = "Pré-quarentena",
                            false = "Pós-quarentena")) %>%
      select(-hora) %>%
      janitor::clean_names("big_camel") %>%
      rename(Estação = EstacaoCetesb,
             Concentração = Concentracao,
             Mês = Mes)

    change_locale(
      rpivotTable(
        data = cetesb2,
        rows = "Estação",
        cols = "Poluente",
        vals = "Concentração",
        aggregatorName = "Média",
        rendererName = "Tabela",
        width = "100%",
        height = "100%",
        sorters = "function(attr) {
var sortAs = $.pivotUtilities.sortAs;
if (attr == \"Fase\") { return sortAs([\"Pré-quarentena\", \"Pós-quarentena\"]); }
if (attr == \"Dia\") { return sortAs([\"Dom\", \"Seg\", \"Ter\", \"Qua\", \"Qui\", \"Sex\", \"Sáb\"]); }
if (attr == \"Mês\") { return sortAs([\"Jan\", \"Fev\", \"Mar\", \"Abr\", \"Mai\", \"Jun\", \"Jul\", \"Ago\", \"Set\", \"Out\", \"Nov\", \"Dez\"]); }
}"),
      "pt")
  })
}

shinyApp(ui, server)
