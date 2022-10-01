# Cria uma única variável com os dados dos imóveis

library(dplyr)
library(raster)
library(magrittr)

imoveis <- readRDS("coleta/dados/imoveis.RDS")
imoveis_geo <- readRDS("coleta/dados/imoveis_geo.RDS")
imoveis_geo_interno <- readRDS("coleta/dados/imoveis_geo_interno.RDS")
setores <- readRDS("coleta/dados/setores.RDS")
bairros <- readRDS("coleta/dados/bairros.RDS")


imoveis$long <- NA
imoveis$lat <- NA

# acrescenta as coordenadas dos imóveis, com prioridade para a fonte interna.
imoveis[,c("long","lat")] <-
  coordinates(imoveis_geo_interno)[
    match(imoveis$inscricaoCadastral,
          imoveis_geo_interno$inscricao),c(1,2)]

imoveis[imoveis$long|>is.na(),c("long","lat")] <-
  imoveis_geo[
    match(imoveis$inscricaoCadastral[imoveis$long|>is.na()],
          imoveis_geo$inscricaoCadastral),c("long","lat")]

# Elimina os imóveis sem georreferenciamento
imoveis <- imoveis[!is.na(imoveis$long),]

# Converte para SpatialPointsDataFrame
coordinates(imoveis) <- c("long","lat")
proj4string(imoveis) <- "+proj=longlat +datum=WGS84 +no_defs"

# cruza com polígonos dos setores e bairros
imoveis$setor <-
  setores$Name[imoveis |> over(setores |> as("SpatialPolygons"))]
imoveis$bairro <-
  bairros$Name[imoveis |> over(bairros |> as("SpatialPolygons"))]

# elimina imóveis que não pertencem a bairros ou setores
imoveis <- imoveis[imoveis$setor |> is.na() |> not(),]
imoveis <- imoveis[imoveis$bairro |> is.na() |> not(),]

saveRDS(imoveis, 
        file = "manipulacao/dados/imoveis.RDS")
