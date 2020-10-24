my_p = c("shiny", "shinydashboard", "plotly", "RSocrata", "tidyverse", "tidyquant",
         "dplyr", "lubridate", "QuantTools", "TTR")

install_if_missing <- function(p){
  if (p %in% rownames(installed.packages()) == FALSE){
    install.packages(p)
  }
}

invisible(sapply(my_p, install_if_missing))