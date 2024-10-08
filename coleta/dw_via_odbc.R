# Conectar e baixar dados do DW da prefeitura via ODBC

print("Carga de dados do DataWarehouse...")

library(DBI)
library(odbc)
library(dplyr)

print("Carga de dados do DataWarehouse... Conectando...")
con_dw <- dbConnect(odbc(), dsn= dsn_name, encoding = dw_encoding)
print("Carga de dados do DataWarehouse... Conexão estabelecida... lendo tabelas...")

imoveis <- 
  dbReadTable(con_dw, "BI_DadosInscricaoImobiliaria")

# completa alguns dados
# essa parte do código precisa ser integrada diretamente no DW e retirada daqui
temp_inscricoes <- 
  dbReadTable(con_dw, Id(
    database = "bdpmv", 
    schema = "dbo", 
    table = "Inscricoes_Cadastrais"))

temp_logradouro <- 
  dbReadTable(con_dw, Id(
    database = "bdpmv", 
    schema = "dbo", 
    table = "Logradouro_Postal"))

temp_tipos_logradouro <- 
  dbReadTable(con_dw, Id(
    database = "bdpmv", 
    schema = "dbo", 
    table = "tipo_logradouro"))

# retira os espaços vazios de cod_logradouro
temp_logradouro$Tipo_Logradouro <- sub(" ","",temp_logradouro$Tipo_Logradouro)
temp_logradouro$Tipo_Logradouro <- sub(" ","",temp_logradouro$Tipo_Logradouro)
temp_tipos_logradouro$tipo_logradouro <- sub(" ","",temp_tipos_logradouro$tipo_logradouro)
temp_tipos_logradouro$tipo_logradouro <- sub(" ","",temp_tipos_logradouro$tipo_logradouro)

# retira logradouros duplicados
temp_tipos_logradouro <- 
  temp_tipos_logradouro[!duplicated(temp_tipos_logradouro$tipo_logradouro),]

temp_logradouro <-
  left_join(temp_logradouro,
            temp_tipos_logradouro, 
            by = c("Tipo_Logradouro" = "tipo_logradouro"))

temp_inscricoes$tipo_logradouro <-
  left_join(temp_inscricoes,
            temp_logradouro, 
            by = c("Cod_Logradouro" = "Cod_Logradouro"))$Descr_Tipo_Logradouro

imoveis$tipoLogradouro <- 
  temp_inscricoes[
    match(imoveis$inscricaoCadastral,temp_inscricoes$Inscricao_Cadastral),
    "tipo_logradouro"]

# grava dados
imoveis |> saveRDS("coleta/dados/imoveis.RDS")

dbDisconnect(con_dw)
print("Fim da carga de dados do DataWarehouse.")
