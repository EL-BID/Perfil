# coleta todas as informações necessárias

dir.create("coleta/temp", showWarnings = FALSE)
dir.create("coleta/dados", showWarnings = FALSE)

# dados do Data Warehouse via ODBC
source("coleta/dw_via_odbc.R")

# Dados do IBGE (malha de setores, censo e IPCA)
source("coleta/ibge.R")

# Base dos dados (método alternativo para Censo 2010)
# source("coleta/basedosdados.R")

# Dados fornecidos pela prefeitura
source("coleta/dados_internos.R")

# openstreetmap (massa de água e georreferenciamento restante)
source("coleta/openstreetmap.R")
