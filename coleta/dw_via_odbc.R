# Conectar e baixar dados do DW da prefeitura via ODBC

library(DBI)
library(odbc)
library(dplyr)

con_dw <- dbConnect(odbc(), dsn= dsn_name, encoding = dw_encoding)

imoveis <- 
  dbReadTable(con_dw, "BI_DadosInscricaoImobiliaria")

pessoas <- 
  dbReadTable(con_dw, "BI_PessoaImobiliario")

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

temp_unidades_imobiliarias <- 
  dbReadTable(con_dw, Id(
    database = "bdpmv", 
    schema = "dbo", 
    table = "Unidades_Imobiliarias"))

temp_ocupacao <- 
  dbReadTable(con_dw, Id(
    database = "bdpmv", 
    schema = "dbo", 
    table = "Ocupacoes"))

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

imoveis$ocupacao <- 
  temp_unidades_imobiliarias[
    match(imoveis$inscricaoCadastral,temp_unidades_imobiliarias$Inscricao_Cadastral),
    "Cod_Ocupacao"]

# grava dados
saveRDS(imoveis,
        file = "coleta/dados/imoveis.RDS")

saveRDS(pessoas,
        file = "coleta/dados/pessoas.RDS")

dbDisconnect(con_dw)
