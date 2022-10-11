### Carrega informações fornecidas internamente

print("Carga de dados do internos...")

library(rgdal)

## malhas dos bairros ####
print("Carga de dados do internos... malha dos bairros...")
bairros <- readOGR(paste0("dados/",arquivo_bairros))

# extrai dados presente na descrição
bairros$populacao <- 
  sub('</td>.*', "", 
      sub(".*populacao</td> <td>", "", bairros$Description)) |>
  as.numeric()

saveRDS(bairros, file = 
          "coleta/dados/bairros.RDS")


## georreferenciamento das unidades imobiliárias ####
print("Carga de dados do internos... unidades imobiliárias...")
imoveis_geo_interno <- readOGR(paste0("dados/",arquivo_unidades))

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

print("Fim da carga de dados do internos.")
