## Dados para apresentação em São Paulo

# library
library(odbc)
library(DBI) 
library(dplyr) 
library(dbplyr) 
library(DescTools) #para gini
library(basedosdados) #para censo
library(rgdal) # para kml
library(REAT) # para lorenz




# Gini da dívida ativa do IPTU (com os zeros)
Gini(domicilios$vlDebitoDA)


gini(imoveis@data$vlDebitoDA[imoveis@data$vlDebitoDA>0])
lorenz(imoveis@data$vlDebitoDA[imoveis@data$vlDebitoDA>0])


Debito_DA <- BI_DadosInscricaoImobiliaria$vlDebitoDA[
  -which(IsZero(BI_DadosInscricaoImobiliaria$vlDebitoDA))]
# Gini da dívida entre devedores (domicílios)
domicilios_inadimplentes <- 
  domicilios[order(-domicilios@data$vlDebitoDA),] |> 
  subset(vlDebitoDA>0)
Gini(domicilios_inadimplentes$vlDebitoDA)
lorenz(domicilios_inadimplentes$vlDebitoDA)
tamanho <- length(domicilios_inadimplentes$vlDebitoDA)



plot(domicilios_inadimplentes$vlDebitoDA[order(domicilios_inadimplentes$vlDebitoDA)])

# sem 3 maiores
dom_inad_sem3 <- domicilios_inadimplentes[c(3:tamanho),]
plot(dom_inad_sem3$vlDebitoDA)
gini(dom_inad_sem3$vlDebitoDA)
lorenz(dom_inad_sem3$vlDebitoDA)

# Relação entre dívida dos 5 maiores devedores e restante
Small(Debito_DA, k = tamanho*0.71) |> sum() > Large(Debito_DA) |> sum()

# Relação 50/50 (ponto que divide metade dos maiores e metade dos menores)
dv <- 0.9754 # ponto de divisão
Small(Debito_DA, k = tamanho*dv) |> 
  sum() > 
  Large(Debito_DA, k = tamanho*(1-dv)) |> 
  sum()

# População total dos domicilios
sc_domicilio_moradores$v001


dados <- setores_2010$Sector |> as.numeric() |> as.data.frame()
names(dados) <- "setor"

dados$renda <- 
  sc_domicilio_renda[
    match(dados$setor, 
          (sc_domicilio_renda$id_setor_censitario |> as.numeric())),]$v002  * 2.03277310

dados$unidades <- 
  sc_domicilio_renda[
    match(dados$setor, 
          (sc_domicilio_renda$id_setor_censitario |> as.numeric())),c(3,7:16)] |>
  rowSums()

dados$renda_un <- dados$renda/dados$unidades


dados$populacao <- 
  sc_domicilio_moradores[
    match(dados$setor, (sc_domicilio_moradores$id_setor_censitario |> as.numeric())),]$v001

dados$renda_pc <- dados$renda/dados$populacao

dados <- dados[c(1:572),]

# Gini de renda do município
Gini(dados$renda, n = dados$populacao)

# Gini de pessoas de Vitória
pessoas <- read.csv2("data/microdados_pessoa_vitoria.csv")
Gini(pessoas$v6525, na.rm = T)

# Bairros e população CENSO 2010
Bairros <- readOGR("data/BairroPopulacaoCenso2010.kml")
names(Bairros) <- c("Bairro", "B.Descrição")
Bairros$Bairro

# Arquivo contendo as coordenadas centrais de cada inscrição imobiliária
Unidade <- readOGR("data/UnidadeImobiliaria.kml")
Unidade$InscricaoC <- 
  sub('</td>.*', "", 
      sub(".*inscricaoC</td> <td>", "", Unidade$Description)) |>
  as.numeric()
# Elimina duplicados
Unidade <- Unidade[-which(duplicated(Unidade$Name)),]




# Agora, calcular o peso da dívida e do valor do IPTU na renda das pessoas
### CALMA LÁ! Preciso trabalhar só com imóveis residenciais....



# Analisando exclusivamente imóveis residenciais
##############################################
# da base bdpmv
con <- dbConnect(odbc(), dsn= "PMVBigData", database = "bdpmv")
Unidades_Imobiliarias <- tbl(con, "Unidades_Imobiliarias") |>
  collect() |>
  as.data.frame()

IC_Residencias <- 
  Unidades_Imobiliarias[
    which(Unidades_Imobiliarias$Cod_Ocupacao == 3),]$Inscricao_Cadastral

# Separa apenas os imóveis residenciais
Dados_IC_R <- 
  BI_DadosInscricaoImobiliaria[
    BI_DadosInscricaoImobiliaria$inscricaoCadastral %in% IC_Residencias,]
Unidade_R <- Unidade[Unidade$InscricaoC %in% IC_Residencias,]

# Separa imóves residenciais com dívida
Debito_DA_R <- 
  BI_DadosInscricaoImobiliaria[
    BI_DadosInscricaoImobiliaria$inscricaoCadastral %in% IC_Residencias,]
Debito_DA_R <- Debito_DA_R[
  -which(IsZero(Debito_DA_R$vlDebitoDA)),]

# Gini da dívida dos devedores
Gini(Debito_DA_R$vlDebitoDA)
Gini(Debito_DA_R$vlVenalImovel)


# Relação população por domicílio do censo de 2010
sum(dados$populacao)/sum(sc_domicilio_renda[,c(3,7:16)])

# Débito DA de residências e total
sum(Debito_DA_R$vlDebitoDA)
sum(Debito_DA)

# Valor do débito com relação à taxa em relação ao valor venal
Teste <- Debito_DA_R[order(-Debito_DA_R$vlDebitoDA),]
plot(Teste$vlIPTU/Teste$vlVenalImovel)
# Não tem realmente uma relação. Ou seja, o montante do débito não está 
# relacionado com a taxa.
plot(dom_inad_sem3$vlIPTU/dom_inad_sem3$vlVenalImovel)

###
## Separação dos dados espaciais
Unidade_DA_R <- Unidade[Unidade$InscricaoC %in% Debito_DA_R$inscricaoCadastral,]
Unidade_DA_R$vlDebitoDA <- Debito_DA_R[
  match(Unidade_DA_R$InscricaoC, Debito_DA_R$inscricaoCadastral),]$vlDebitoDA
Unidade_DA_R$vlIPTU <- Debito_DA_R[
  match(Unidade_DA_R$InscricaoC, Debito_DA_R$inscricaoCadastral),]$vlIPTU
Unidade_DA_R$vlVenalImovel <- Debito_DA_R[
  match(Unidade_DA_R$InscricaoC, Debito_DA_R$inscricaoCadastral),]$vlVenalImovel

un_sc <- over(Unidade_DA_R, setores_2010 |> as("SpatialPolygons"))
Unidade_DA_R$renda_un <- dados[un_sc,]$renda_un

# dívida e iptu da unidade em relação à renda média do setor
Unidade_DA_R$dv_rd <- Unidade_DA_R$vlDebitoDA/Unidade_DA_R$renda_un
Unidade_DA_R$iptu_rd <- Unidade_DA_R$vlIPTU/Unidade_DA_R$renda_un
Unidade_DA_R$iptu_vn <- Unidade_DA_R$vlIPTU/Unidade_DA_R$vlVenalImovel


# Mostrar a relação dívida/renda ordenada pelo absoluto da dívida
teste <- Unidade_DA_R[order(-Unidade_DA_R$vlDebitoDA),]
# Retira as 10 maiores dívidas
teste <- teste[10:length(teste),]
plot(teste$dv_rd)
plot(teste$vlDebitoDA, teste$dv_rd)
dom_inad_sem3$dv_rd <- dom_inad_sem3$vlDebitoDA/dom_inad_sem3$RendaDomicilioMedia
plot(dom_inad_sem3$dv_rd, dom_inad_sem3$RendaDomicilioMedia)
plot(dom_inad_sem3[c(17000:18400),]$dv_rd)
dom_inad_sem3[c(17000:18400),] |> plot()

dom_inad_sem3[c(16991:18500),]$vlDebitoDA
dom_inad_sem3[c(16991:18500),]$RendaDomicilioMedia

plot(setores$ValorIPTUMedio, setores$TxInadimplencia)

options(max.print = 2000)
# Mostrar a relação dívida/renda ordenada pelo absoluto da renda
# retirando as 10 maiores dívidas
teste <- Unidade_DA_R[order(-Unidade_DA_R$vlDebitoDA),]
teste <- teste[10:length(teste),]
teste <- teste[order(-teste$renda_un),]
plot(teste$dv_rd)



# Mostrar a relação iptu/renda ordenada pelo absoluto do IPTU
# retirando as 10 maiores dívidas
teste <- Unidade_DA_R[order(-Unidade_DA_R$vlIPTU),]
teste <- teste[10:length(teste),]
teste <- teste[order(teste$vlIPTU),]
plot(teste$iptu_rd)
# relação iptu/renda pela renda
plot(teste$renda_un, teste$iptu_rd)


# Mostrar a relação iptu/valor venal ordenada pelo valor venal
# retirando as 10 maiores dívidas
teste <- Unidade_DA_R[order(-Unidade_DA_R$vlDebitoDA),]
teste <- teste[10:length(teste),]
teste <- teste[order(teste$vlVenalImovel),]
plot(teste$iptu_vn)
plot(teste$vlVenalImovel, teste$iptu_vn)

# Mostrar a relação iptu/valor venal ordenada pela renda
teste <- teste[order(teste$renda_un),]
plot(teste$iptu_vn)
plot(teste$renda_un, teste$iptu_vn)

setores$VariacaoNumDomicilios[order(setores$VariacaoNumDomicilios)]
plot(setores_estaveis$RendaDomicilioMedia, setores_estaveis$IPTURendaMensal)
plot(setores_estaveis$RendaDomicilioMedia, setores_estaveis$ValorIPTUMedio)
plot(setores_estaveis$RendaDomicilioMedia, setores_estaveis$ValorVenalMedio)


plot(setores$RendaDomicilioMedia, setores$IPTURendaMensal)
plot(setores_estaveis$RendaDomicilioMedia, setores_estaveis$IPTURendaMensal)
plot(setores_estaveis$RendaDomicilioMedia, setores_estaveis$IPTURendaMensal)

setores_estaveis <- setores@data |> subset(VariacaoNumDomicilios<100)

setores_estaveis <- setores |> subset(VariacaoNumDomicilios>50)

sum(setores_estaveis$ValorIPTUMedio)/sum(setores_estaveis$ValorVenalMedio)

setores_estaveis <- setores_estaveis[order(setores_estaveis$RendaDomicilioMedia),]
plot(setores_estaveis$ValorIPTUMedio)


dom_inad_sem3$vlIPTUTeorico <- 0.0015 * dom_inad_sem3$vlVenalImovel

dom_inad_sem3_est <- dom_inad_sem3[dom_inad_sem3$setor %in% setores_estaveis$Name,]

setores_estaveis$valorIPTUTeorico <-
  dom_inad_sem3_est$vlIPTUTeorico |>
  tapply(dom_inad_sem3_est$setor, sum, na.rm = TRUE) /
  setores_estaveis$NumDomicilios

setores_estaveis$valorIPTUTeorico <- 0.0015 * setores_estaveis$ValorVenalMedio

domicilios$vlIPTUTeorico <- 0.0015 * domicilios$vlVenalImovel
setores$valorIPTUTeorico <-
  domicilios$vlIPTUTeorico |>
  tapply(domicilios$setor, sum, na.rm = TRUE) /
  setores$NumDomicilios
setores <- setores[order(setores$RendaDomicilioMedia),]
plot(setores$ValorIPTUMedio, type = "l")
lines(setores$valorIPTUTeorico, col = "red")



# Agora, calcular os dados da dívida por bairro
un_br <-  over(Unidade_DA_R, Bairros |> as("SpatialPolygons"))
Unidade_DA_R$unidade <- 1
Bairros$un_devedoras <- NA
Bairros$un_devedoras[-61] <- tapply(Unidade_DA_R$unidade, un_br, sum)
# Bairros$vlIPTU <- tapply(Unidade_DA_R$vlIPTU, un_br, sum)


### Calcular todos os dados por bairro
#######

# Primeiro, carregar nos pontos as informações dos setores e da DA e IPTU
Unidade_R$vlDebitoDA <- Dados_IC_R[
  match(Unidade_R$InscricaoC, Dados_IC_R$inscricaoCadastral),]$vlDebitoDA
Unidade_R$vlIPTU <- Dados_IC_R[
  match(Unidade_R$InscricaoC, Dados_IC_R$inscricaoCadastral),]$vlIPTU
Unidade_R$vlVenalImovel <- Dados_IC_R[
  match(Unidade_R$InscricaoC, Dados_IC_R$inscricaoCadastral),]$vlVenalImovel
# Separar pontos por setores
IC_sc <- over(Unidade_R, setores_2010 |> as("SpatialPolygons"))
Unidade_R$renda_un <- dados[IC_sc,]$renda_un
# dívida e iptu da unidade em relação à renda média do setor
Unidade_R$dv_rd <- Unidade_R$vlDebitoDA/Unidade_R$renda_un
Unidade_R$iptu_rd <- Unidade_R$vlIPTU/Unidade_R$renda_un
Unidade_R$unidade <- 1


# Agrega todos os pontos do baiorro (tira o bairro 61 q não é residencial)
Bairros$renda_un <- NA
Bairros$vlDebitoDA <- NA
Bairros$vlIPTU <- NA
Bairros$vlVenalImovel <- NA
Bairros$dv_iptu <- NA
Bairros$unidades <- NA
Bairros$iptu_teorico <- NA

# Retirar as 5 maiores dívidas
Unidade_R <- Unidade_R[order(-Unidade_R$vlDebitoDA),]
Unidade_R <- Unidade_R[5:length(Unidade_R$Name),]



IC_br <-  over(Unidade_R, Bairros |> as("SpatialPolygons"))
Bairros$renda_un[-61] <- tapply(Unidade_R$renda_un, IC_br, mean, na.rm = T)
Bairros$vlDebitoDA[-61] <- tapply(Unidade_R$vlDebitoDA, IC_br, mean, na.rm = T)
Bairros$vlIPTU[-61] <- tapply(Unidade_R$vlIPTU, IC_br, mean, na.rm = T)
Bairros$vlVenalImovel[-61] <- tapply(Unidade_R$vlVenalImovel, IC_br, mean, na.rm = T)
Bairros$unidades[-61] <- tapply(Unidade_R$unidade, IC_br, sum, na.rm = T)
Bairros$dv_rd <- Bairros$vlDebitoDA/Bairros$renda_un
Bairros$iptu_rd <- Bairros$vlIPTU/Bairros$renda_un
Bairros$iptu_vn <- Bairros$vlIPTU/Bairros$vlVenalImovel
Bairros$dv_iptu <- Bairros$vlDebitoDA/Bairros$vlIPTU
Bairros$inadp <- Bairros$un_devedoras/Bairros$unidades


teste <- Bairros[order(Bairros$renda_un),]
# Retirando Ilha do Frade e Ilha do Boi
teste <- teste[c(-1,-2),]

# Relação renda média da undiade e iptu/renda
plot(teste$renda_un, teste$iptu_rd)

#Relação renda média da unidade e dívida/renda
plot(teste$renda_un, teste$dv_rd)

#relação renda média da unidade e valor absoluto do IPTU
plot(teste$renda_un, teste$vlIPTU)

#relação renda média da unidade e valor absoluto do IPTU
plot(teste$renda_un, teste$iptu_vn)

#relação renda média da unidade e valor absoluto do IPTU
plot(teste$vlVenalImovel, teste$iptu_vn)

#relação dívida/valor do IPTU
plot(teste$renda_un[1:78], teste$dv_iptu[1:78])

# taxa inadimplencia
plot(teste$renda_un[1:78], teste$inadp[1:78])

# taxa inadimplencia por taxa de iptu_renda
plot(teste$iptu_rd[1:78], teste$inadp[1:78])

plot(teste$vlVenalImovel)

teste$Bairro
Bairros$Bairro



# Como poderíamos reduzir a inadimplência sem alterar nada +

# cenário 1: Cobrando uma única taxa sobre o valor venal

tx <- sum(Unidade_R$vlIPTU, na.rm = T)/sum(Unidade_R$vlVenalImovel, na.rm = T)

Unidade_R$iptu_teorico <- tx * Unidade_R$vlVenalImovel

Bairros$iptu_teorico[-61] <- tapply(Unidade_R$iptu_teorico, IC_br, mean, na.rm = T)

# a inadimplência hj:
indp_hj <- (Bairros$vlIPTU * Bairros$inadp * Bairros$unidades) |> sum(na.rm = T)

# a inadimplência com a nova taxa:
indp_torico <- (Bairros$iptu_teorico * Bairros$inadp * Bairros$unidades) |> sum(na.rm = T)

# Diferença
indp_hj-indp_torico

# Mas, a mudança do iptu gera dois efeitos:
# 1 - cobrar menos de quem paga mal
# 2 - reduzir própria taxa inadimplencia # essa parte eu não estou conseguindo fazer


iptu_hj <- (Bairros$vlIPTU * Bairros$unidades) |> sum(na.rm = T)


(Bairros$vlIPTU * Bairros$unidades) |> sum(na.rm = T)

Bairros$iptt_rd <- Bairros$iptu_teorico/Bairros$renda_un

# Relação entre iptu teórico e atual
teste <- Bairros[order(Bairros$renda_un),]
plot(teste$iptu_teorico[1:78], type = "l", col = "blue")
lines(teste$vlIPTU[1:78], col = "red")

# Relação entre iptu/renda teórico e atual
plot(teste$iptt_rd[1:78], type = "l", col = "blue")
lines(teste$iptu_rd[1:78], col = "red")


plot(teste$iptu_rd[1:78], teste$inadp[1:78])
points(Bairros$iptu_rd[1:78], Bairros$inadp_est[1:78], col = "red")
points(Bairros$iptt_rd[1:78], Bairros$inadp_teo[1:78], col = "blue")


library(car)
install.packages("car")

regressao <- lm(log(teste$inadp) ~ (teste$iptu_rd))
regressao$model

summary(regressao)

Bairros$inadp_est <- exp(-2.5538 + ((Bairros$iptu_rd) * 15.1261))
Bairros$inadp_teo <- exp(-2.5538 + ((Bairros$iptt_rd) * 15.1261))


# a inadimplência estimada hj:
indp_hj <- (Bairros$vlIPTU * Bairros$inadp_est * Bairros$unidades) |> sum(na.rm = T)

# a inadimplência com a nova taxa:
indp_torico <- (Bairros$iptu_teorico * Bairros$inadp_teo * Bairros$unidades) |> sum(na.rm = T)

# Diferença
indp_hj-indp_torico


teste$Bairro



Dados_UN <- Unidade_R[,c(3,4,5,6,7,8,9,11)] |> as.data.frame()
Dados_BR <- Bairros[,c(1,3:15)] |> as.data.frame()


arquivo <- list(Dados_UN, Dados_BR)


writexl::write_xlsx(arquivo, "dados_sp.xlsx")
write.csv(Debito_DA, "debito.csv")
