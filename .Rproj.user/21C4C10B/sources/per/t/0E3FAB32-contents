library("shiny")
library("shinydashboard")
library("plotly")
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