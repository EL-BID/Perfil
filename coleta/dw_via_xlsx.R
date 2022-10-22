# Exemplo de leitura do DW via arquivo xlsx

print("Carga de dados do DataWarehouse...")

library(xlsx)

print("Carga de dados do DataWarehouse...")

imoveis <- read.xlsx("dados/amostra_dw.xlsx","BI_DadosInscricaoImobiliaria")

# grava dados
imoveis |> saveRDS("coleta/dados/imoveis.RDS")

print("Fim da carga de dados do DataWarehouse.")
