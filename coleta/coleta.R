# coleta todas as informações necessárias

# dados do Data Warehouse via ODBC
source("coleta/dw_via_odbc.R")

# malhas dos setores (ibge)
source("coleta/shapes_setores.R")

# malha dos bairros (fornecido pela prefeitura)
source("coleta/shapes_bairros.R")

# georreferenciamento dos imóveis
source("coleta/imoveis_geocode.R")

# dados censitários
source("coleta/censo_2010.R")
