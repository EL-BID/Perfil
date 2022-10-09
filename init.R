##### Perfil da Dívida Ativa do IPTU #######
##
## Autor: Rodrigo Franklin
##
## BR-T1496
##
####

## Variáveis de configurações ####
source("config.R")

# Verificar e instalar lista de pacotes
# carregar pacotes
library(shiny)
library(rmarkdown)
library(DBI)
library(odbc)
library(dplyr)

## Fatias ####

# Coleta
source("coleta/coleta.R")

# Manipulação
source("manipulacao/manipulacao.R")

# Visualização
runApp("visualizacao")
