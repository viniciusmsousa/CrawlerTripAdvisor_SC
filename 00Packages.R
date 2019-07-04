
lista_pacotes <- c("data.table","readr","tidyverse","rvest","stringr","rebus","lubridate",
                   "zoo","drake","tm","stringr","furrr","tidytext","stm","shiny",
                   "shinydashboard","shinyWidgets","shinycssloaders")
nao_instalados <- lista_pacotes[!(lista_pacotes %in% installed.packages()[,"Package"])]
if(length(nao_instalados)) install.packages(nao_instalados)
lapply(lista_pacotes,function(x){library(x,character.only=TRUE)}) 
rm(lista_pacotes,nao_instalados)
gc()
