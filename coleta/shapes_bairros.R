library(rgdal)

# carrega as malhas dos bairros
bairros <- readOGR("dados/BairroPopulacaoCenso2010.kml")

bairros$populacao <- 
  sub('</td>.*', "", 
      sub(".*populacao</td> <td>", "", bairros$Description)) |>
  as.numeric()

bairros$area <- 
  sub(",",".",
      sub('</td>.*', "", 
          sub(".*areaBairro</td> <td>", "", bairros$Description))) |>
  as.numeric()

saveRDS(bairros, file = 
          "coleta/dados/bairros.RDS")
