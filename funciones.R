library("RSocrata")
library("tidyverse")
library("tidyquant")
library("dplyr")
library("lubridate")
library("QuantTools")
library("TTR")

get_trm_oil <- function(fecha){
  trm <- read.socrata("https://www.datos.gov.co/resource/32sa-8pi3.json")
  trm$Fecha <- as.Date(as.character(trm$vigenciadesde), "%Y-%m-%d")
  trm$Fecha <- trm$Fecha - 1
  trm <- subset(trm, Fecha >= as.Date("2008-01-01"))
  trm <- trm[order(trm$Fecha),] %>%
    select(Fecha, valor) %>% 
    rename(TRM = valor)
  s <- tq_get("CL=F", from="2008-01-01")
  s <- s[order(s$date),] %>% 
    select(date, close) %>% 
    rename(Fecha = date, OIL = close)
  p <- merge(x=trm, y=s, by="Fecha", all.x = TRUE)
  p <- na.locf(p, fromLast = TRUE)
  p$TRM <- as.numeric(p$TRM)
  p <- subset(p, Fecha <= fecha)
  return(p)
}

get_data <- function(nemo, fecha){
  s <- tq_get(nemo, from="2010-01-01")
  s <- subset(s, date <= fecha)
  s <- s[order(s$date),] %>% 
    rename(Fecha=date)
  return(s)
}

filter_data <- function(data, freq){
  cols <- colnames(data)[2:length(colnames(data))]
  frec <- switch(freq, "Mensual"="month", "Anual"="year", "Diario"="day")
  data <- data %>% group_by(Fecha=floor_date(Fecha, frec)) %>%
    summarize_at(cols, tail, 1)
  data$Fecha <- as.Date(data$Fecha, "%Y-%m-%d")
  return(data)
}

tabla_trm <- function(fecha){
  df <- get_trm_oil(fecha)[c("Fecha", "TRM")] %>%
    group_by(Fecha=year(Fecha)) %>%
    summarise_all(c("min", "max", "mean", "sd")) %>% 
    rename("Minimo"="min", "Maximo"="max", "Media"="mean", "DesvStd"="sd")
  return(df)
}


returns_data <- function(df, fecha){
  df <- subset(df, Fecha >= fecha)
  trm <- df %>% 
    tq_transmute(select = TRM,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "rTRM"
    )
  oil <- df %>% 
    tq_transmute(select = OIL,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "rOIL"
  )
  return(merge(trm, oil, by="Fecha"))
}

corr <- function(df){
  r <- cor.test(df$rTRM, df$rOIL, method = "pearson")
  return(r$estimate)
}

boll_bands <- function(df){
  df$TP <- (df$high + df$close + df$low) / 3
  df$STD <- roll_sd(df$TP, 20)
  df$BBp <- sma(df$TP, 20) + (df$STD * 2)
  df$BBd <- sma(df$TP, 20) - (df$STD * 2)
  return(df)
}

macd_serie <- function(df){
  df$EMA12 <- ema(df$close, 12)
  df$EMA26 <- ema(df$close, 26)
  df$macd <- df$EMA12 - df$EMA26
  return(df)
}

df_ind <- function(df){
  df$SMA14 <- sma(df$close, 14)
  df$SMA50 <- sma(df$close, 50)
  df$SMA200 <- sma(df$close, 200)
  df <- boll_bands(df)
  x <- MACD(df[, "close"], 12, 26, 9, maType = "EMA")
  df$macd <- x[1:nrow(x), 1]
  df$macd_signal <- x[1:nrow(x), 2]
  df$rsi <- RSI(df$close)
  df$UO <- ultimateOscillator(df[,c("high", "low", "close")])
  return(df %>% select(Fecha, close, SMA14, SMA50, SMA200, BBp, BBd, 
                       macd, macd_signal, rsi, UO, volume)) 
}