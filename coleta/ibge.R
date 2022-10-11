# Dados do IBGE

print("Carga de dados do IBGE...")

library(sidrar)
library(rgdal)
library(XML)
library(sf)
library(raster)

## baixa a malha de setores de todo o estado ####
print("Carga de dados do IBGE... malha dos setores censitários...")
setor_url <- 
  "http://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2010/setores_censitarios_kmz/"
lista <- getHTMLLinks(setor_url)
arquivos <- grep(paste0("-",UF), lista)
if (arquivos |> length() == 1) {
  download.file(
    paste0(setor_url, lista[arquivos]), 
    "coleta/temp/setor_estado.zip", mode = "wb")
  unzip("coleta/temp/setor_estado.zip", exdir = "coleta/temp")
  
  # carrega a malha dos setores da cidade
  setores <- readOGR("coleta/temp/doc.kml", layer = CIDADE)
} else {
  # Alguns estados possuem mais de 1 arquivo. Nesses casos, carrega todos os
  # arquivos para verificar em qual está a cidade
  for (x in arquivos) {
    download.file(
      paste0(setor_url, lista[x]), 
      "coleta/temp/setor_estado.zip", mode = "wb")
    unzip("coleta/temp/setor_estado.zip", exdir = "coleta/temp")
    # carrega a malha dos setores da cidade
    temp_setores <- readOGR("coleta/temp/doc.kml", layer = CIDADE) |>
      try (silent = TRUE)
    if (class(temp_setores) != "try-error") {
      setores <- temp_setores
    }
  }
}

saveRDS(setores, file = 
          "coleta/dados/setores.RDS")

## IPCA para atualização dos dados do censo ####
print("Carga de dados do IBGE... IPCA...")
IPCA <- get_sidra(1737, variable = 2266, period = c("201006", "202208"))
IPCA <- IPCA$Valor[2]/IPCA$Valor[1]

saveRDS(IPCA,
        file = "coleta/dados/IPCA.RDS")

## Baixa dados do censo ####
print("Carga de dados do IBGE... Censo...")
censo_url <- 
  "http://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios/"
lista <- getHTMLLinks(censo_url)
arquivos <- grep(paste0(UF,"_"), lista)
if (arquivos |> length() == 1) {
  download.file(
    paste0(censo_url, lista[arquivos]), 
    "coleta/temp/censo_estado.zip", mode = "wb")
  
  unzip("coleta/temp/censo_estado.zip", 
        exdir = "coleta/temp/censo",
        junkpaths = TRUE,
        unzip = "unzip")
  
} else {
  # Alguns estados possuem mais de 1 arquivo
  # Não implementado
}

# Para carregar o dado direto no R
domicilio_renda_censo2010 <- 
  read.csv2(paste0("coleta/temp/censo/DomicilioRenda_",UF,".csv"))

domicilio_moradores_censo2010 <- 
  read.csv2(paste0("coleta/temp/censo/Domicilio02_",UF,".csv"))

domicilio_basico_censo2010 <- 
  read.csv2(paste0("coleta/temp/censo/Basico_",UF,".csv"))

# filtra pelos setores de vitória
domicilio_renda_censo2010 <- 
  domicilio_renda_censo2010[domicilio_renda_censo2010$Cod_setor %in% 
                              setores$Name,]

domicilio_moradores_censo2010 <- 
  domicilio_moradores_censo2010[domicilio_moradores_censo2010$Cod_setor %in% 
                                  setores$Name,]

domicilio_basico_censo2010 <- 
  domicilio_basico_censo2010[domicilio_basico_censo2010$Cod_setor %in% 
                               setores$Name,]

saveRDS(domicilio_renda_censo2010, 
        file = "coleta/dados/domicilio_renda_censo2010.RDS")

saveRDS(domicilio_moradores_censo2010, 
        file = "coleta/dados/domicilio_moradores_censo2010.RDS")

saveRDS(domicilio_basico_censo2010,
        file = "coleta/dados/domicilio_basico_censo2010.RDS")

### Preterido pelos dados do OpenStreetMap
# ## shape das massas de água ####
# options(timeout=2400)
# download.file(
#   'ftp://geoftp.ibge.gov.br/cartas_e_mapas/bases_cartograficas_continuas/bc250/versao2019/shapefile/bc250_shapefile_06_11_2019.zip',
#   'coleta/temp/bc250_shapefile_06_11_2019.zip')
# fileslist <-
#   unzip('coleta/temp/bc250_shapefile_06_11_2019.zip', 
#         exdir = "coleta/temp", 
#         list=TRUE)
# fileslist<-c(fileslist$Name[grep('hid_massa_dagua_a', fileslist$Name)])
# unzip('coleta/temp/bc250_shapefile_06_11_2019.zip',
#       exdir='coleta/temp',
#       files=fileslist,
#       junkpaths=TRUE)
# 
# massa_agua <- readOGR('coleta/temp/hid_massa_dagua_a.shp')
# bb <- st_bbox(setores) # Caixa com os limites da cidade
# bb <- c(bb$xmin,bb$ymin,bb$xmax,bb$ymax)
# massa_agua <- crop(massa_agua,bb) # Corta a massa de água 
# 
# setores <- erase(setores, massa_agua)

print("Fim da carga de dados do IBGE.")
