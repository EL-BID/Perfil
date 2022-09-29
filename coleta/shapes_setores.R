# Obtém shapes dos bairros e dos setores censitários

library(rgdal)

# baixa a malha de todo o estado
setor_url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2010/setores_censitarios_kmz/32-ES.kmz"
download.file(setor_url, "coleta/temp/setor_estado.zip", mode = "wb")
unzip("coleta/temp/setor_estado.zip", exdir = "coleta/temp")

# carrega a malha dos setores da cidade
setores <- readOGR("coleta/temp/doc.kml", layer = "VITÓRIA")

saveRDS(setores, file = 
          "coleta/dados/setores.RDS")