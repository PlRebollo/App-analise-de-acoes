library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(fresh)
library(yfR)  #Pacote com ações
library(ggplot2)
library(bslib)
library(rugarch)
library(xts)
library(dygraphs)
library(fpp3)
library(tidyverse)
library(rvest)
library(DT)
library(forecast)

source("modelos.R")

my_theme = create_theme(
  adminlte_color(
    light_blue = "#1b274c"
  )
)

options(spinner.color="#1b274c", spinner.color.background="#ffffff", spinner.size=2)

scrapeYahooFinanceTable <- function() {
  url <- "https://finance.yahoo.com/trending-tickers/"
  page <- read_html(url)
  
  # Extract table data
  table_data <- page %>%
    html_nodes("td") %>%
    html_text()
  
  # Convert the data into a matrix
  num_cols <- 11  # Assuming 7 columns in the table
  table_matrix <- matrix(table_data, ncol = num_cols, byrow = TRUE)
  
  # Create a data frame
  table_df <- as.data.frame(table_matrix, stringsAsFactors = FALSE)
  table_df = table_df[,-c(9:11)]
  colnames(table_df) = c("Symbol", "Name", "Last Price", "Market Time", "Change", "% Change", "Volume", "Market Cap")
  
  return(table_df)
}

data_ini  <- "2010-01-01" # Data de inicio
data_fim  <- Sys.Date() # Data de fim
tickers_descricao <- read.csv("tickers_descricao.csv")

ui <- fluidPage(theme = my_theme, dashboardPage(
  dashboardHeader(title = "Análise Temporal de ações do Google"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Capa", tabName = "capa"),
      menuItem("Modelo", tabName = "modelo"),
      menuItem("Análises", tabName = "Análises")
    ),
    sidebar <- tags$head(tags$style(HTML(".sidebar {background-color: #1b274c;color: black;}")))
  ),
  dashboardBody(use_theme(my_theme),
                tags$head(
                  tags$style(
                    HTML(
                      '
                      body > nav{
                          margin-bottom:0 !important;}
                          body > div.container-fluid{
                          padding:0;}
                       #sidebar {
            background-color: #606682;
            color: white;
                       }
                       
        /* Custom CSS styles */
        .skin-blue .main-header .logo{
          color: white;
          font-weight: bold;
          font-size: 24px;
        }
        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #1b274c;
                              }
        
        /* Custom CSS styles */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
          color: white;
          background-color: #333a5e;
          border-left-color: #5b75f6;
        }
        
        /* Custom CSS styles */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
          color: white;
        }
        
        /* Custom CSS styles */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
          color: white;
          background-color: #333a5e;
          border-left-color: #5b75f6;
        }
        
        #verificacao{
          color: #1b274c;
          font-size: 40px;
          font-weight:900;
        }
        
        /* body */
        .content-wrapper, .right-side {
        background-color: white;
        color: black;
        }        
        '
                    )
                  )
                ),
                tabItems(
                  # Capa tab
                  tabItem(tabName = "capa",
                          fluidRow(
                            column(8, align="center",
                                   img(id = "homeimg", src = "Capa1.png", style = "width: 120%; padding: 0;")
                            )
                          ), column(8, align = "center", offset = 2,
                                    selectizeInput(
                                      inputId = "acao_select",
                                      label = "Escolha sua ação",
                                      choices = c("", tickers_descricao$names |> sort()),
                                      multiple = FALSE
                                    ),
                                    actionButton(
                                      inputId = "acao",
                                      label = "Escolher"
                                    ),
                                    withSpinner(textOutput('verificacao'), type = 2),
                                    DTOutput("yahooTable"))
                  ),
                  
                  # Modelo tab
                  tabItem(tabName = "modelo",
                          h2("Modelos",
                             style = "font-weight: bold; color: #1b274c;"),
                          tabsetPanel(
                            tabPanel(
                              title = "Serie",
                              dygraphOutput("plot")
                            ),
                            tabPanel(
                              title = "Retornos",
                              dygraphOutput("ret")
                            ),
                            tabPanel(
                              title = "ACF",
                              plotOutput("acf")
                            ),
                            tabPanel(
                              title = "PACF",
                              plotOutput("pacf")
                            ))
                  ),
                  
                  # Análise tab
                  tabItem(tabName = "Análises",
                          h2("Análises",
                             style = "font-weight: bold; color: #1b274c;"),
                          tabsetPanel(
                            tabPanel("ARIMA",
                                     sidebarLayout(
                                       sidebarPanel(id = "sidebar",
                                                    numericInput("AR", min = 0, max = 10, value = 1, label="AR"),
                                                    numericInput("I", min = 0, max = 10, value = 1, label="I"),
                                                    numericInput("MA", min = 0, max = 10, value = 1, label="MA"),
                                                    actionButton("rodar_ARIMA", label="Escolha o modelo")
                                       ),
                                       mainPanel(
                                         plotOutput("result_ARIMA")
                                       )
                                     )),
                            tabPanel("ARMA-GARCH", 
                                     sidebarLayout(
                                       sidebarPanel(id = "sidebar",
                                                    numericInput("AR", min = 0, max = 10, value = 1, label="AR"),
                                                    numericInput("MA", min = 0, max = 10, value = 1, label="MA"),
                                                    numericInput("GARCH_P", min = 0, max = 10, value = 1, label="GARCH - P"),
                                                    numericInput("GARCH_Q", min = 0, max = 10, value = 1, label="GARCH - Q"),
                                                    radioButtons("dist", h3("Distribuição"),
                                                                 choices = list("Normal" = "norm", "T-Student" = "std")),
                                                    actionButton("rodar_GARCH", label="Escolha o modelo")
                                       ),
                                       mainPanel(
                                         plotOutput("result")
                                       )
                                     )),
                            tabPanel("DRIFT", plotOutput("result_DRIFT")),
                            tabPanel("NAIVE", plotOutput("result_NAIVE")),
                            tabPanel("SNAIVE",
                                     sidebarLayout(
                                       sidebarPanel(id = "sidebar",
                                                    numericInput("sazonalidade", min = 0, max = 100, value = 1, label="Período Sazonal"),
                                                    actionButton("rodar_SNAIVE", label="Escolha o modelo")
                                       ),
                                       mainPanel(
                                         plotOutput("result_SNAIVE")
                                       )
                                     )),
                            tabPanel("Média", plotOutput("result_MEAN")),
                            tabPanel("Regressão", textOutput("regress"), plotOutput("result_regress")),
                            tabPanel("Suavização Exponencial", textOutput("SE"), plotOutput("result_SE")),
                            tabPanel("HWA", textOutput("HWA"), plotOutput("result_HWA"))
                          ))
                  )
                )
  )
)

server <- function(input, output) {
  #Carregando dados
  precos = reactiveVal(NULL)
  carregando = reactiveVal(FALSE)
  
  output$yahooTable <- renderDT({
    dataYahoo = scrapeYahooFinanceTable()
    dataYahoo$Change = as.numeric(sub("^[+]", "", dataYahoo$Change))
    dataYahoo$`% Change` = as.numeric(sub("^[+]", "", gsub("%", "", dataYahoo$`% Change`)))
    
    datatable(dataYahoo, options = list(pageLength = 10)) %>%
      formatStyle(
        c("Change", "% Change"),
         color = styleInterval(c(0, Inf), c("red", "black", "green"))
      )
  })
  
  observeEvent(input$acao_select, {
    precos(NULL)
  })
  
  observeEvent(input$acao, {
    carregando(TRUE)
    acao = tickers_descricao[tickers_descricao$names == input$acao_select,2] |> unlist()
    precos1 <- yf_get(tickers = acao, first_date = data_ini, last_date = data_fim)
    precos(precos1)
    carregando(FALSE)
  })
  
  output$verificacao <- renderText({
    if(input$acao_select == ""){
      return("Nenhuma ação selecionada")
    }
    
    if(carregando()){
      while(carregando()){
        x = 1
      }
    }
    
    if(!is.null(precos())){
      if(nrow(precos()) == 0){
        return("Ação não disponível no momento, tente novamente")
      }
      else{
        return(paste0("Você selecionou a ação ", tickers_descricao[tickers_descricao$names == input$acao_select,1]))
      }
    }
  })
  
  #Dados no formato tsibble e sem datas faltantes
  precos_tsibble <- reactive({
    req(input$acao)
    n <- nrow(precos())
    precos_tsibble <- precos() %>% 
      mutate(ref_date = seq(as.Date("2010/01/01"), by = "day", length.out = n)) %>%
      as_tsibble(index = ref_date) %>%
      as_tsibble(index = ref_date)
  })
  #Dados para os gráficos interativos
  don <- reactive({
    req(input$acao)
    xts(x = precos()$price_adjusted, order.by = precos()$ref_date)})
  don2 <- reactive({
    req(input$acao)
    xts(x = precos()$ret_adjusted_prices, order.by = precos()$ref_date)})
  output$plot <- renderDygraph({
    req(input$acao)
    dygraph(don()) %>%
      dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors= "#5b75f6") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1)
  })
  removeModal()
  output$ret <- renderDygraph({
    req(input$acao)
    dygraph(don2()) %>%
      dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#5b75f6") %>%
      dyRangeSelector() %>%
      dyCrosshair(direction = "vertical") %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
      dyRoller(rollPeriod = 1)
  })
  removeModal()
  
  output$acf <- renderPlot({
    req(input$acao)
    plot(acf(na.omit(precos()$ret_adjusted_prices), plot = FALSE), main = "Retornos")})
  output$pacf <- renderPlot({
    req(input$acao)
    plot(pacf(na.omit(precos()$ret_adjusted_prices), plot = FALSE), main = "Retornos")})
  
  #Análise
  observeEvent(input$rodar_GARCH, {
    
    output$result = renderPlot({({return(ar_garch(isolate(input$AR), isolate(input$MA),
                                                  isolate(input$GARCH_P),
                                                  isolate(input$GARCH_Q),
                                                  isolate(input$dist),
                                                  precos()$ret_adjusted_prices[-1]))})})})
  observeEvent(input$rodar_ARIMA, {
    output$result_ARIMA = renderPlot({({return(funcao_arima(isolate(input$AR),
                                                            isolate(input$I),
                                                            isolate(input$MA),
                                                            precos()$ret_adjusted_prices[-1]))})})})
  output$result_NAIVE <- renderPlot({
    req(input$acao)
    fit_Naive <- na.omit(precos_tsibble()) |> model(NAIVE(ret_adjusted_prices))
    fit_Naive %>% gg_tsresiduals() 
  })
  observeEvent(input$rodar_SNAIVE, {output$result_SNAIVE = renderPlot({({return(funcao_snaive(isolate(input$sazonalidade),
                                                                                              precos_tsibble()))})})})
  output$result_DRIFT <- renderPlot({
    req(input$acao)
    fit_drift <- precos_tsibble() |> model(RW(ret_adjusted_prices ~ drift()))
  fit_drift %>% gg_tsresiduals()})
  output$result_MEAN <- renderPlot({
    req(input$acao)
    fit_Mean <- precos_tsibble() |> model(MEAN(ret_adjusted_prices))
    fit_Mean %>% gg_tsresiduals()})
  output$result_regress <- renderPlot({
    req(input$acao)
    fit_reg <- precos_tsibble() |> model(TSLM(ret_adjusted_prices ~ trend() + season() + price_low + volume))
    fit_reg %>% gg_tsresiduals()
    })
  output$result_SE <- renderPlot({
    req(input$acao)
    fit_se <- na.omit(precos_tsibble()) |> model(ETS(ret_adjusted_prices ~ error("A")))
  fit_se %>% gg_tsresiduals()})
  output$result_HWA <- renderPlot({
    req(input$acao)
    fit_hwa <- na.omit(precos_tsibble()) |> model(ETS(ret_adjusted_prices ~ error("A") + trend("Ad") + season("A")))
    fit_hwa %>% gg_tsresiduals()})
  output$diag0 <- renderPlot({
    req(input$acao)})
}
shinyApp(ui, server)
