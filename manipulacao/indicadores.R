# calcula todos os indicadores

print("Calculando indicadores...")

# carrega dados
bairros <- readRDS("coleta/dados/bairros.RDS")
setores <- readRDS("manipulacao/dados/setores.RDS")
imoveis <- readRDS("manipulacao/dados/imoveis.RDS")
domicilio_renda_censo2010 <- 
  readRDS("coleta/dados/domicilio_renda_censo2010.RDS")
domicilio_basico_censo2010 <- 
  readRDS("coleta/dados/domicilio_basico_censo2010.RDS")
IPCA <- readRDS("coleta/dados/IPCA.RDS")

# Cria variáveis com os imóveis residenciais:
# 3 = residências
# 11 = garagens residenciais
# 13 = lazer residenciar
domicilios <- imoveis |> subset(ocupacao %in% c(3))
imoveis_residenciais <- imoveis |> subset(ocupacao %in% c(3,11,13))

# retira setores e bairros que não possuem imoveis residenciais
setores <- setores |> subset(setores$Name %in% unique(domicilios$setor))
bairros <- bairros |> subset(bairros$Name %in% unique(domicilios$bairro))

# ordena pelo nome
setores <- setores[setores$Name |> order(),]
bairros <- bairros[bairros$Name |> order(),]

# inicialização da lista de indicadores
indicadores <- NULL

# Número de domicílios (base interna)
indicadores <- rbind(indicadores,
                 c("Nº de Domicílios","NumDomicilios","inteiro","soma"))

domicilios$NumDomicilios <- 1

setores$NumDomicilios <- 
  domicilios$NumDomicilios |>
  tapply(domicilios@data$setor, sum, na.rm = TRUE)

bairros$NumDomicilios <- 
  domicilios$NumDomicilios |>
  tapply(domicilios@data$bairro, sum, na.rm = TRUE)

# Número de domicílios (censo)
indicadores <- rbind(indicadores,
                 c("Nº de Domicílios (censo)","NumDomiciliosCenso","inteiro","soma"))

setores$NumDomiciliosCenso <- 
  domicilio_basico_censo2010$V001[
    match(setores$Name,domicilio_basico_censo2010$Cod_setor)] |>
  as.numeric()

domicilios$NumDomiciliosCensoAtual <-
  setores$NumDomiciliosCenso[match(domicilios$setor, setores$Name)] /
  setores$NumDomicilios[match(domicilios$setor, setores$Name)]

bairros$NumDomiciliosCenso <- 
  domicilios$NumDomiciliosCensoAtual |>
  tapply(domicilios@data$bairro, mean, na.rm = TRUE) *
  bairros$NumDomicilios

# Renda domiciliar média
indicadores <- rbind(indicadores,
                 c("Renda Domiciliar Média (censo)","RendaDomicilioMedia","moeda","media"))

setores$RendaDomicilioMedia <- 
  IPCA * 
  (domicilio_renda_censo2010$V002[
    match(setores$Name, domicilio_renda_censo2010$Cod_setor)] |>
  as.numeric()) /
  setores$NumDomiciliosCenso

domicilios$RendaDomicilioMedia <- setores@data$RendaDomicilioMedia[
  match(domicilios$setor, setores@data$Name)]

bairros$RendaDomicilioMedia <- 
  (domicilios$RendaDomicilioMedia |>
  tapply(domicilios@data$bairro, mean, na.rm = TRUE))

# Renda total média
indicadores <- rbind(indicadores,
                 c("Renda Total (censo)","RendaTotal","moeda","soma"))

setores$RendaTotal <- 
  IPCA * (domicilio_renda_censo2010$V002[match(setores$Name,
    domicilio_renda_censo2010$Cod_setor)]  |> as.numeric())

bairros$RendaTotal <- 
  bairros$RendaDomicilioMedia *
  bairros$NumDomicilios
  # (domicilios$RendaDomicilioMedia |>
  #    tapply(domicilios@data$bairro, sum, na.rm = TRUE))

# Variação número de domicílios entre base interna e censo
indicadores <- rbind(indicadores,
                 c("Variação % do nº de domicílios","VariacaoNumDomicilios","percentual","media"))

setores$VariacaoNumDomicilios <-
  (setores$NumDomicilios / 
  setores$NumDomiciliosCenso * 100) -100

bairros$VariacaoNumDomicilios <- 0

# Número de domicílios criados ou destruídos desde o censo 2010
indicadores <- rbind(indicadores,
                 c("Variação absoluta de domicílios (= base - censo)","NovosDomicilios","inteiro","soma"))

setores$NovosDomicilios <-
  setores$NumDomicilios - 
  setores$NumDomiciliosCenso

bairros$NovosDomicilios <- 0
# Dívida total
indicadores <- rbind(indicadores,
                 c("Dívida Ativa Total","DividaTotal","moeda","soma"))

setores$DividaTotal <- 
  imoveis_residenciais$vlDebitoDA |>
  tapply(imoveis_residenciais@data$setor, sum, na.rm = TRUE)

bairros$DividaTotal <- 
  imoveis_residenciais$vlDebitoDA |>
  tapply(imoveis_residenciais@data$bairro, sum, na.rm = TRUE)

# Número de domicílios inadimplentes
indicadores <- rbind(indicadores,
                 c("Total de Inscritos em Dívida Ativa","Inadimplentes","inteiro","soma"))

domicilios$Inadimplentes <- 0
domicilios$Inadimplentes[domicilios$vlDebitoDA>0] <- 1

imoveis_residenciais$Inadimplentes <- 0
imoveis_residenciais$Inadimplentes[imoveis_residenciais$vlDebitoDA>0] <- 1

setores$Inadimplentes <- 
  domicilios$Inadimplentes |>
  tapply(domicilios@data$setor, sum, na.rm = TRUE)

bairros$Inadimplentes <- 
  domicilios$Inadimplentes |>
  tapply(domicilios@data$bairro, sum, na.rm = TRUE)

# Taxa de inadimplência
indicadores <- rbind(indicadores,
                 c("Percentual de Inscritos em Dívida Ativa","TxInadimplencia","percentual","media"))

setores$TxInadimplencia <- 
  setores$Inadimplentes /
  setores$NumDomicilios *100

bairros$TxInadimplencia <- 
  bairros$Inadimplentes /
  bairros$NumDomicilios *100

# Dívida média por domicílio devedor
indicadores <- rbind(indicadores,
                 c("Dívida Ativa média","DividaMedia","moeda","media"))

setores$DividaMedia <- 
  setores$DividaTotal /
  setores$Inadimplentes

bairros$DividaMedia <- 
  bairros$DividaTotal /
  bairros$Inadimplentes

# Valor venal total (acrescenta ao valor do domicílio, o valor das garagens e lazer)
indicadores <- rbind(indicadores,
                 c("Valor venal total","ValorVenalTotal","moeda","soma"))

setores$ValorVenalTotal <- 
  imoveis_residenciais$vlVenalImovel |>
  tapply(imoveis_residenciais@data$setor, sum, na.rm = TRUE)

bairros$ValorVenalTotal <- 
  imoveis_residenciais$vlVenalImovel |>
  tapply(imoveis_residenciais@data$bairro, sum, na.rm = TRUE)

# Valor venal médio
indicadores <- rbind(indicadores,
                 c("Valor venal médio","ValorVenalMedio","moeda","media"))

setores$ValorVenalMedio <- 
  setores$ValorVenalTotal /
  setores$NumDomicilios

bairros$ValorVenalMedio <- 
  bairros$ValorVenalTotal /
  bairros$NumDomicilios

# Valor IPTU total
indicadores <- rbind(indicadores,
                 c("Valor total do IPTU","ValorIPTUTotal","moeda","soma"))

setores$ValorIPTUTotal <- 
  imoveis_residenciais$vlIPTU |>
  tapply(imoveis_residenciais@data$setor, sum, na.rm = TRUE)

bairros$ValorIPTUTotal <- 
  imoveis_residenciais$vlIPTU |>
  tapply(imoveis_residenciais@data$bairro, sum, na.rm = TRUE)

# Valor IPTU médio
indicadores <- rbind(indicadores,
                 c("Valor médio do IPTU","ValorIPTUMedio","moeda","media"))

setores$ValorIPTUMedio <- 
  setores$ValorIPTUTotal /
  setores$NumDomicilios

bairros$ValorIPTUMedio <- 
  bairros$ValorIPTUTotal /
  bairros$NumDomicilios

# Relação IPTU/renda mensal
indicadores <- rbind(indicadores,
                 c("Valor do IPTU (% da renda mensal)","IPTURendaMensal","percentual","media"))

setores$IPTURendaMensal <- 
  setores$ValorIPTUTotal /
  setores$RendaTotal *100

bairros$IPTURendaMensal <- 
  bairros$ValorIPTUTotal /
  bairros$RendaTotal *100

# Relação IPTU/valor venal
indicadores <- rbind(indicadores,
                 c("Valor do IPTU (% do valor venal)","IPTUValorVenal","percentual","media"))

setores$IPTUValorVenal <- 
  setores$ValorIPTUTotal /
  setores$ValorVenalTotal *100

bairros$IPTUValorVenal <- 
  bairros$ValorIPTUTotal /
  bairros$ValorVenalTotal *100

# Renda total dos domicílios inadimplentes
indicadores <- rbind(indicadores,
                     c("Renda total dos domicílios inadimplentes","RendaInadimplentes","moeda","soma"))

domicilios$RendaInadimplentes <- 
  domicilios$RendaDomicilioMedia * domicilios$Inadimplentes

setores$RendaInadimplentes <- 
  domicilios$RendaInadimplentes |>
  tapply(domicilios@data$setor, sum, na.rm = TRUE)

bairros$RendaInadimplentes <- 
  domicilios$RendaInadimplentes |>
  tapply(domicilios@data$bairro, sum, na.rm = TRUE)

# Relação Dívida/renda mensal
indicadores <- rbind(indicadores,
                 c("Valor da DA (% da renda mensal)","DividaRendaMensal","percentual","media"))

setores$DividaRendaMensal <- 
  setores$DividaTotal /
  setores$RendaInadimplentes * 100

bairros$DividaRendaMensal <- 
  bairros$DividaTotal /
  bairros$RendaInadimplentes * 100

# Valor Venal Total dos domicílios inadimplentes
indicadores <- rbind(indicadores,
                     c("Valor venal dos domicílios inscritos em DA","vlVenalImovelInadimplentes","moeda","soma"))

imoveis_residenciais$vlVenalImovelInadimplentes <- 
  imoveis_residenciais$vlVenalImovel * imoveis_residenciais$Inadimplentes

setores$vlVenalImovelInadimplentes <- 
  imoveis_residenciais$vlVenalImovelInadimplentes |>
  tapply(imoveis_residenciais@data$setor, sum, na.rm = TRUE)

bairros$vlVenalImovelInadimplentes <- 
  imoveis_residenciais$vlVenalImovelInadimplentes |>
  tapply(imoveis_residenciais@data$bairro, sum, na.rm = TRUE)

# Valor venal médio do inadimplente
indicadores <- rbind(indicadores,
                     c("Valor venal médio dos inscritos em DA","vlVenalImovelMedioInadimplentes","moeda","media"))

setores$vlVenalImovelMedioInadimplentes <- 
  setores$vlVenalImovelInadimplentes /
  setores$Inadimplentes

bairros$vlVenalImovelMedioInadimplentes <- 
  bairros$vlVenalImovelInadimplentes /
  bairros$Inadimplentes

# Relação Dívida/valor venal
indicadores <- rbind(indicadores,
                 c("Valor da DA (% do valor venal)","DividaValorVenal","percentual","media"))

setores$DividaValorVenal <- 
  setores$DividaTotal /
  setores$vlVenalImovelInadimplentes * 100

bairros$DividaValorVenal <- 
  bairros$DividaTotal /
  bairros$vlVenalImovelInadimplentes * 100

# Projeção da população a partir do censo
indicadores <- rbind(indicadores,
                 c("População (projeção)","PopulacaoProjecao","inteiro","soma"))

domicilios$PopulacaoProjecao <- 
  domicilio_basico_censo2010$V003[
    match(domicilios$setor, domicilio_basico_censo2010$Cod_setor)] |> 
  as.numeric()

setores$PopulacaoProjecao <- 
  domicilios$PopulacaoProjecao |>
  tapply(domicilios@data$setor, sum, na.rm = TRUE)

bairros$PopulacaoProjecao <- 
  domicilios$PopulacaoProjecao |>
  tapply(domicilios@data$bairro, sum, na.rm = TRUE)

# Valor IPTU Total dos domicílios inadimplentes
indicadores <- rbind(indicadores,
                     c("Valor do IPTU dos domicílios inscritos em DA","vlIPTUInadimplentes","moeda","soma"))

imoveis_residenciais$vlIPTUInadimplentes <- 
  imoveis_residenciais$vlIPTU * imoveis_residenciais$Inadimplentes

setores$vlIPTUInadimplentes <- 
  imoveis_residenciais$vlIPTUInadimplentes |>
  tapply(imoveis_residenciais@data$setor, sum, na.rm = TRUE)

bairros$vlIPTUInadimplentes <- 
  imoveis_residenciais$vlIPTUInadimplentes |>
  tapply(imoveis_residenciais@data$bairro, sum, na.rm = TRUE)

# Valor IPTU médio do inadimplente
indicadores <- rbind(indicadores,
                     c("Valor médio do IPTU (inadimplente)","ValorIPTUMedioInadimplente","moeda","media"))

setores$ValorIPTUMedioInadimplente <- 
  setores$vlIPTUInadimplentes /
  setores$Inadimplentes

bairros$ValorIPTUMedioInadimplente <- 
  bairros$vlIPTUInadimplentes /
  bairros$Inadimplentes

# Desvio IPTU devedor
indicadores <- rbind(indicadores,
                     c("Desvio IPTU do devedor (em relação à média)","DesvioIPTUInadimplente","percentual","media"))

setores$DesvioIPTUInadimplente <- 
  (setores$ValorIPTUMedioInadimplente /
  setores$ValorIPTUMedio *100) -100

bairros$DesvioIPTUInadimplente <- 
  (bairros$ValorIPTUMedioInadimplente /
     bairros$ValorIPTUMedio *100) -100

# Desvio Valor Venal devedor
indicadores <- rbind(indicadores,
                     c("Desvio valor venal do devedor (em relação à média)","DesvioVenalInadimplente","percentual","media"))

setores$DesvioVenalInadimplente <- 
  (setores$vlVenalImovelMedioInadimplentes /
     setores$ValorVenalMedio *100) -100

bairros$DesvioVenalInadimplente <- 
  (bairros$vlVenalImovelMedioInadimplentes /
     bairros$ValorVenalMedio *100) -100

# Valor da DA (% do valor total do município)
indicadores <- rbind(indicadores,
                     c("Valor da DA (% do valor total)","DAPercentual","percentual","media"))

setores$DAPercentual <- 
  (setores$DividaTotal /
     sum(setores$DividaTotal) *100)

bairros$DAPercentual <- 
  (bairros$DividaTotal /
     sum(bairros$DividaTotal) *100)

# Domícilios inscritos em DA (% do total de inscritos)
indicadores <- rbind(indicadores,
                     c("Domícilios inscritos em DA (% do total)","InadimplentesPercentual","percentual","media"))

setores$InadimplentesPercentual <- 
  (setores$Inadimplentes /
     sum(setores$Inadimplentes) *100)

bairros$InadimplentesPercentual <- 
  (bairros$Inadimplentes /
     sum(bairros$Inadimplentes) *100)

# IRDA
indicadores <- rbind(indicadores,
                     c("Índice de recorrência de inscrição em DA","IRDA","numeral","media"))

domicilios$IRDA <-
  rowSums(domicilios@data[,c("temDebitoDA2018",
                             "temDebitoDA2019",
                             "temDebitoDA2020",
                             "temDebitoDA2021",
                             "temDebitoDA2022")])

setores$IRDA <- 
  domicilios@data$IRDA |>
  tapply(domicilios@data$setor, mean, na.rm = TRUE)

bairros$IRDA <- 
  domicilios@data$IRDA |>
  tapply(domicilios@data$bairro, mean, na.rm = TRUE)

#IRPG
# indicadores <- rbind(indicadores,
#                      c("Índice de recorrência de inscrição em DA","IRDA","numeral","media"))
# 
# domicilios$IRDA <-
#   rowSums(domicilios[,c("temDebitoDA2018",
#                         "temDebitoDA2019",
#                         "temDebitoDA2020",
#                         "temDebitoDA2021",
#                         "temDebitoDA2022")])
# 
# setores$IRDA <- 
#   domicilios$IRDA |>
#   tapply(domicilios$setor, mean, na.rm = TRUE)
# 
# bairros$IRDA <- 
#   domicilios$IRDA |>
#   tapply(domicilios$bairros, mean, na.rm = TRUE)



# Eliminar valores infinitos

for (i in (1:(setores@data[1,] |> length()))) {
  setores@data[setores@data[,i]|> is.infinite(),i] <- NA
}

for (i in (1:(bairros@data[1,] |> length()))) {
  bairros@data[bairros@data[,i]|> is.infinite(),i] <- NA
}

# Grava na pasta de visualização
indicadores <- indicadores |> as.data.frame()
names(indicadores) <- c("rotulo","nome","tipo","agregado")

# domicilios <- domicilios[,"Inadimplentes"]


saveRDS(imoveis,
        file = "visualizacao/dados/imoveis.RDS")
saveRDS(imoveis_residenciais,
        file = "visualizacao/dados/imoveis_residenciais.RDS")
saveRDS(indicadores,
        file = "visualizacao/dados/indicadores.RDS")
saveRDS(domicilios,
        file = "visualizacao/dados/domicilios.RDS")
saveRDS(bairros,
        file = "visualizacao/dados/bairros.RDS")
saveRDS(setores,
        file = "visualizacao/dados/setores.RDS")
