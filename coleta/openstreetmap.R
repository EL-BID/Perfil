# Dados do OpenStreetMaps

print("Carga de dados do OpenStreetMap...")

library(cartography)
library(osmdata)
library(raster)
library(sf)
library(rgdal)
library(tidygeocoder)

## Massas de água ####
print("Carga de dados do OpenStreetMap... massas de água...")
agua <- setores |> 
  st_bbox() |>
  opq() |>
  add_osm_features(c(
    "\"natural\"=\"strait\"",
    "\"natural\"=\"bay\""
  )) |>
  osmdata_sf()

agua <- agua$osm_multipolygons |> as("Spatial")

saveRDS(agua,
        "coleta/dados/agua.RDS")

## consulta as coordenadas dos imóveis para os quais não há informações internas
print("Carga de dados do OpenStreetMap... geocode dos imóveis...")
imoveis_geo <- readRDS("coleta/dados/imoveis.RDS")
imoveis_geo_interno <- readRDS("coleta/dados/imoveis_geo_interno.RDS")

imoveis_geo <- 
  imoveis_geo[
    which(!(imoveis_geo$inscricaoCadastral %in% 
              imoveis_geo_interno$inscricao)),]

if (imoveis_geo[1,1] |> is.na() |> not()) {
  imoveis_geo$endereco <-
    paste0(imoveis_geo$tipoLogradouro, " ",
           imoveis_geo$nomeLogradouro,", ",
           imoveis_geo$numero," ",
           imoveis_geo$nomeBairro,", ",
           "VITORIA"," - ",
           "ES",", ",
           "BRASIL")
  
  imoveis_geo <- imoveis_geo[,c("inscricaoCadastral", "endereco")] |>
    geocode(address = endereco,
            method = 'osm')
}

saveRDS(imoveis_geo,
        file = "coleta/dados/imoveis_geo.RDS")

print("Fim da carga de dados do OpenStreetMap.")
