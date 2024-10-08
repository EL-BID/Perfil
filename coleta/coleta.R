# coleta todas as informações necessárias

dir.create("coleta/temp", showWarnings = FALSE)
dir.create("coleta/dados", showWarnings = FALSE)

# dados do Data Warehouse via ODBC
if (teste_ver) {
  source("coleta/dw_via_xlsx.R")
} else {
  source("coleta/dw_via_odbc.R")
}

# Dados do IBGE (malha de setores, censo e IPCA)
source("coleta/ibge.R")

# Dados fornecidos pela prefeitura
source("coleta/dados_internos.R")

# openstreetmap (massa de água e georreferenciamento restante)
source("coleta/openstreetmap.R")
