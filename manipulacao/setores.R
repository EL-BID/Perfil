# trata os setores, eliminando regiões não habitadas.

library(raster)

setores <- readRDS("coleta/dados/setores.RDS")
bairros <- readRDS("coleta/dados/bairros.RDS")
bairros <- readRDS("coleta/dados/agua.RDS")
imoveis <- readRDS("manipulacao/dados/imoveis.RDS")

# retira regiões não habitadas de setores
setores <- erase(setores, agua)
setores <- intersect(setores, bbox(bairros))

# retira setores que não possuem imoveis
setores <- setores[setores$Name %in% unique(imoveis$setor),]

saveRDS(setores,
        file = "manipulacao/dados/setores.RDS")