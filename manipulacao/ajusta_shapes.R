# trata os setores, eliminando regiões não habitadas.

library(cartography)
library(osmdata)
library(raster)

setores <- readRDS("coleta/dados/setores.RDS")
bairros <- readRDS("coleta/dados/bairros.RDS")
imoveis <- readRDS("manipulacao/dados/imoveis.RDS")

# obtém os corpos de água naturais para retirar dos setores
agua <- bairros |> 
  st_bbox() |>
  opq() |>
  add_osm_features(c(
    "\"natural\"=\"strait\"",
    "\"natural\"=\"bay\""
  )) |>
  osmdata_sf()

agua <- agua$osm_multipolygons |> as("Spatial")

# retira regiões não habitadas de setores
setores <- erase(setores, agua)
setores <- intersect(setores, bbox(bairros))

# retira setores que não possuem imoveis
setores <- setores[setores$Name %in% unique(imoveis$setor),]

saveRDS(setores,
        file = "manipulacao/dados/setores.RDS")
