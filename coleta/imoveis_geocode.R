# georreferenciamento dos imóveis

library(rgdal)
library(tidygeocoder)

# carrega georreferenciamento das unidades imobiliárias
imoveis_geo_interno <- readOGR("dados/UnidadeImobiliaria.kml")

# remove duplicados
imoveis_geo_interno <- 
  imoveis_geo_interno[!duplicated(imoveis_geo_interno$Name),]

# extrai dados presente na descrição
imoveis_geo_interno$inscricao <- 
  sub('</td>.*', "", 
      sub(".*inscricaoC</td> <td>", "", imoveis_geo_interno$Description)) |>
  as.numeric()

imoveis_geo_interno$ocupacao <- 
  sub('</td>.*', "", 
      sub(".*ocupacao</td> <td>", "", imoveis_geo_interno$Description))

imoveis_geo_interno$complemento <- 
  sub('</td>.*', "", 
      sub(".*Complement</td> <td>", "", imoveis_geo_interno$Description))

imoveis_geo_interno$bairro <- 
  sub('</td>.*', "", 
      sub(".*bairro</td> <td>", "", imoveis_geo_interno$Description))

saveRDS(imoveis_geo_interno, file = 
          "coleta/dados/imoveis_geo_interno.RDS")

# consulta as longitudes e latidudes dos endereços a partir do OpenStreetMap
imoveis_geo <- readRDS("coleta/dados/imoveis.RDS")

# consulta apenas os imóveis que já não possuem informações internas
imoveis_geo <- 
  imoveis_geo[
    which(!(imoveis_geo$inscricaoCadastral %in% 
              imoveis_geo_interno$inscricao)),]

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

# converte
# imoveis_geo <- 
#   SpatialPointsDataFrame(imoveis_geo[,c("long", "lat")], imoveis_geo[,1:30])

saveRDS(imoveis_geo,
        file = "coleta/dados/imoveis_geo.RDS")
