library(dplyr)
library(lubridate)
library(udpipe)
library(quanteda)
library(topicmodels)
library(ggplot2)
library(tidytext)
library(stm)
library(tidyverse)
library(data.table)
library(ggplot2)
library(plotly)
library(tibble)
library(caret)
library(LDAvis)
library(reshape2)
library(dplyr)
library(wordcloud2)
library(tsne)
library(servr)


# # Datei laden
# file_path <- "C:/Users/Veritas/Documents/Uni/python/data/speeches.csv"
# data <- read.csv(file_path, stringsAsFactors = FALSE, 
#                  fileEncoding = "UTF-8")
# 
# # Metadaten analysieren
# metadata <- list(
#   Spaltennamen = names(data),
#   Anzahl_der_Zeilen = nrow(data),
#   Anzahl_der_Spalten = ncol(data),
#   Datentypen = sapply(data, class)
# )
# 
# # Ausgabe der Metadaten
# print(metadata)
# 
# # Zusätzlich: Überblick über die ersten Zeilen
# head(data)
# 
# # Zusammenfassung der Daten
# summary(data)
# 
# tail(data, 1)
# 
# # Zeigt nur das Datum der letzten Rede an
# tail(data$date, 1)
# 
# 
# glimpse(data)
