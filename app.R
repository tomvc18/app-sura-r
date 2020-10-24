library(shiny)
library(shinydashboard)
library(plotly)
source('funciones.R')

header <- dashboardHeader(
  title = "SURA"
)

sb <- dashboardSidebar(
  sidebarMenu(
    menuItem("TRM  y Petróleo", tabName = "trm_oil", icon=icon("coins")),
    menuItem("S&P 500", tabName = "spy", icon=icon("chart-line"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "trm_oil",
      fluidRow(
        box(dateInput(inputId = "fecha", label = "Fecha de corte:",
                      value= as.Date.character(Sys.Date() - 1),
                      min = as.Date.character(Sys.Date() - 365*10),
                      max = as.Date.character(Sys.Date() - 1)),
            radioButtons(inputId = "freq", label = "Frecuencia:",
                         choices = c("Diario", "Mensual", "Anual"),
                         selected = "Diario", inline = TRUE),
            plotlyOutput("trm_oil_graf", height = 250),
          width = 12
        ),
        box(
          plotlyOutput("box_plot_trm", height = 250),
          width = 12
        ),
        box(
          dateInput(inputId = "fecha_corr", label = "Correlación desde:",
                    value= as.Date.character(Sys.Date() - 365),
                    min = as.Date.character(Sys.Date() - 365*10),
                    max = as.Date.character(Sys.Date() - 365)),
          plotlyOutput("trm_corr", height = 350), width = 6
        ),
        box(
          tableOutput("trm_table"),
          width = 6
        )
      )
    ),
    tabItem(
      tabName = "spy",
      fluidRow(
        box(
          dateInput(inputId = "fecha_acc", label = "Fecha de corte:",
                    value= as.Date.character(Sys.Date() - 1),
                    min = as.Date.character(Sys.Date() - 365*5),
                    max = as.Date.character(Sys.Date() - 1)),
          selectInput(inputId = "stock", label="Stock:",
                      choices = c("S&P"="^GSPC", "Apple"="AAPL", "Microsoft"="MSFT", 
                                  "Amazon"="AMZN","Facebook"="FB","Google"="GOOG", 
                                  "Berkshire Hathaway"="BRK-B","Johnason & Johnson"="JNJ",
                                  "Visa"="V","Procter & Gamble"="PG"),
                      selected = "^GSPC"), width = 6
        ),
        box(
          checkboxGroupInput(inputId = "ind1", label = "Panel Principal",
                             choices = c("SMA(14)"="SMA14", "SMA(50)"="SMA50",
                                         "SMA(200)"="SMA200", "Bollinger Bands"="BB"),
                             selected = "SMA14", inline = TRUE),
          radioButtons(inputId = "ind2", label = "Panel Secundario",
                       choices = c("RSI"="RSI", "MACD"="MACD", "Ultimate Oscillator"="UO"),
                       selected = "MACD", inline = TRUE), width = 6
        ),
        box(
          radioButtons(inputId = "ht", label = "Horizonte de tiempo:",
                       choices = c("1M", "3M", "6M", "1Y", "5Y"), 
                       selected = "1Y", inline = TRUE),
          plotlyOutput("stock_graf", height = 250),
          plotlyOutput("tec_graf", height = 200),
          plotlyOutput("vol_graf", height = 200),
          width = 12
        )
      )
    )
  )
)

ui <- dashboardPage(header, sb, body)

server <- function(input, output, session) {
  df_trm_oil <- reactive({
    df <- get_trm_oil(input$fecha)
    df <- filter_data(df, input$freq)
  })
  #TAB TRM y Petroleo-----------
  #Grafica TRM y Petróleo
  output$trm_oil_graf <- renderPlotly({
    ay <- list(
      tickfont = list(color="red"),
      overlaying = "y",
      side = "right",
      title = "Petróleo (USD)"
    )
    
    fig <- plot_ly(x=df_trm_oil()[["Fecha"]], y=df_trm_oil()[["TRM"]], type="scatter", 
                   mode="lines", name="TRM") %>% 
      add_lines(x=df_trm_oil()[["Fecha"]], y=df_trm_oil()[["OIL"]], 
                name="Petróleo", yaxis="y2") %>% 
      layout(title="TRM y Petróleo", yaxis2 = ay, xaxis = list(title="Fecha"))
    fig
  })
  
  #BoxPlot TRM
  output$box_plot_trm <- renderPlotly({
    df <- get_trm_oil(Sys.Date() - 1)
    df <- subset(df, Fecha > as.Date("2009-12-31"))
    df$Fecha <- format(df$Fecha, "%Y-%m")
    fig <- plot_ly(y=df$TRM, color = df$Fecha, type="box")
    fig <- fig %>% layout(title="BoxPlot TRM 2010 - 2020",
                          yaxis=list(title="TRM(COP)"),
                          xaxis=list(title="Fecha"))
    hide_legend(fig)
  })
  
  #Tabla TRM
  output$trm_table <- renderTable({
    df <- tabla_trm(input$fecha)
    df
  })
  
  #Grafica Correlacion
  output$trm_corr <- renderPlotly({
    df <- returns_data(get_trm_oil(input$fecha), input$fecha_corr)
    fig <- plot_ly(x=df[["rTRM"]], y=df[["rOIL"]], type="scatter",
                   mode="markers") %>% layout(xaxis = list(ticksuffix = "%"),
                                              yaxis = list(ticksuffix = "%"),
                                              title = paste("TRM vs Petróleo, Corr = ", 
                                                            round(corr(df), digits=3)))
    fig
  })
  
  s <- reactive({
    df <- get_data(input$stock, input$fecha_acc)
    df <- df_ind(df)
  })
  #TAB S&P 500--------------
  
  #Grafica Acciones
  output$stock_graf <- renderPlotly({
    dfa <- s()
    h <- switch(input$ht, "1M"=30, "3M"=90, "6M"=180, "1Y"=365, "5Y"=365*5)
    f <- input$fecha_acc - h
    dfa <- subset(dfa, Fecha >= as.Date(f))
    fig <- plot_ly(x=dfa$Fecha, y=dfa$close, type="scatter", mode="lines",
                   name=input$stock)
    if ("SMA14" %in% input$ind1){
      fig <- fig %>% add_trace(y=dfa$SMA14, mode="lines", name="SMA(14)")
    }
    if ("SMA50" %in% input$ind1){
      fig <- fig %>% add_trace(y=dfa$SMA50, mode="lines", name="SMA(50)")
    }
    if ("SMA200" %in% input$ind1){
      fig <- fig %>% add_trace(y=dfa$SMA200, mode="lines", name="SMA(200)")
    }
    if ("BB" %in% input$ind1){
      fig <- fig %>% add_trace(y=dfa$BBp, mode="lines", name="BB+")
      fig <- fig %>% add_trace(y=dfa$BBd, mode="lines", name="BB-")
    }
    fig <- fig %>% layout(
      title=paste(input$stock, "-", input$ht),
      xaxis = list(title="Fecha"),
      yaxis=list(title=input$stock)
    )
    fig
  })
  output$tec_graf <- renderPlotly({
    dft <- s()
    h <- switch(input$ht, "1M"=30, "3M"=90, "6M"=180, "1Y"=365, "5Y"=365*5)
    f <- input$fecha_acc - h
    dft <- subset(dft, Fecha >= as.Date(f))
    if (input$ind2 == "RSI"){
      fig <- plot_ly(x=dft$Fecha, y=dft$rsi, type="scatter", mode="lines", name="RSI")
      dft$OverBought <- 70
      dft$OverSold <- 30
      fig <- fig %>% add_trace(y=dft$OverBought, type="scatter", mode="lines", name="OverBought (70)")
      fig <- fig %>% add_trace(y=dft$OverSold, type="scatter", mode="lines", name="OverSold (30)")
    } else if (input$ind2 == "MACD"){
      fig <- plot_ly(x=dft$Fecha, y=dft$macd, type="scatter", mode="lines", name="MACD")
      fig <- fig %>% add_trace(y=dft$macd_signal, mode="lines", name="MACD_Signal")
    } else if (input$ind2 == "UO"){
      fig <- plot_ly(x=dft$Fecha, y=dft$UO, type="scatter", mode="lines", name="UO")
      dft$OverBought <- 70
      dft$OverSold <- 30
      fig <- fig %>% add_trace(y=dft$OverBought, type="scatter", mode="lines", name="OverBought (70)")
      fig <- fig %>% add_trace(y=dft$OverSold, type="scatter", mode="lines", name="OverSold (30)")
    }
    fig <- fig %>% layout(yaxis=list(title=input$ind2))
    fig
  })
  output$vol_graf <- renderPlotly({
    dfv <- s()
    h <- switch(input$ht, "1M"=30, "3M"=90, "6M"=180, "1Y"=365, "5Y"=365*5)
    f <- input$fecha_acc - h
    dfv <- subset(dfv, Fecha >= as.Date(f))
    fig <- plot_ly(x=dfv$Fecha, y=dfv$volume, type="bar", name="Volumen")
    fig <- fig %>% layout(yaxis=list(title="Volumen"))
    fig
  })
}

shinyApp(ui, server)