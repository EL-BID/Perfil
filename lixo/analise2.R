library(ggplot2)
library(scales)
library(magrittr)

### Configuração ####

# Quantidade de domicílios que serão considerados pontos fora das curvas +1
pfc_domicilios <- 13

# Ponto de corte para a variação dos domicílios dos setores
pfc_setores <- 100

### Carrega informações ####
imoveis <- readRDS("visualizacao/dados/imoveis.RDS")
domicilios <- readRDS("visualizacao/dados/domicilios.RDS")
setores <- readRDS("visualizacao/dados/setores.RDS")

imoveis <- imoveis@data
domicilios <- domicilios@data
setores <- setores@data

## Cálculos para os domicílios ####
# Domicílios que possuem documento
domicilios$temDoc <- domicilios$cpfCnpjResponsavel |> is.na() |> not()

# Percentuais de domicílios com docs conforme dívida
domicilios_inadimplentes <- domicilios |> subset(vlDebitoDA>0)
domicilios_adimplentes <- domicilios |> subset(vlDebitoDA==0)

domicilios_inadimplentes$temDoc |> sum()/
  domicilios_inadimplentes$temDoc |> length()

domicilios$temDoc |> sum()/
  domicilios$temDoc |> length()

domicilios_adimplentes$temDoc |> sum()/
  domicilios_adimplentes$temDoc |> length()

# IRDA
domicilios$IRDA <- rowSums(domicilios[,c("temDebitoDA2018",
                                         "temDebitoDA2019",
                                         "temDebitoDA2020",
                                         "temDebitoDA2021",
                                         "temDebitoDA2022")])

# IRPG
domicilios$NumPG <- rowSums(domicilios[,c("pgDebitoDA2018",
                                          "pgDebitoDA2019",
                                          "pgDebitoDA2020",
                                          "pgDebitoDA2021",
                                          "pgDebitoDA2022")])
domicilios$IRPG <- 
  domicilios$NumPG /
  domicilios$IRDA

# Domicílios que não possuem dívida não entram nesse cálculo
domicilios$IRPG[is.nan(domicilios$IRPG)] <- NA

# Separação dos domicílios que pagam DA conforme documento
domicilios$PGcomDoc <- NA
domicilios$PGcomDoc[domicilios$temDoc] <- domicilios$IRPG[domicilios$temDoc]
domicilios$PGsemDoc <- NA
domicilios$PGsemDoc[!domicilios$temDoc] <- domicilios$IRPG[!domicilios$temDoc]

# elimina os domicilios de maior dívida
tamanho <- domicilios$inscricaoCadastral |> length()
domicilios <- domicilios[order(-domicilios$vlDebitoDA),]
domicilios <- domicilios[pfc_domicilios:tamanho,]

## Cálculos para os setores ####

# Ordenação dos setores conforme algumas variáveis
num_setores <- setores$Name |> length()

setores <- setores[order(setores$DividaMedia),]
setores$ordemDAMedia <- 1:num_setores

setores <- setores[order(setores$DividaTotal),]
setores$ordemDATotal <- 1:num_setores

setores <- setores[order(setores$IPTURendaMensal),]
setores$ordemIPTURenda <- 1:num_setores

setores <- setores[order(setores$RendaDomicilioMedia),]
setores$ordemRenda <- 1:num_setores

setores <- setores[order(setores$Name),]

# Percentual de domicílios com documento
setores$temDocP <- 
  domicilios$temDoc |>
  tapply(domicilios$setor, mean, na.rm = TRUE)

# IRDA do setor
setores$IRDA <- 
  domicilios$IRDA |>
  tapply(domicilios$setor, mean, na.rm = TRUE)

# IRPG do setor
setores$IRPG <- 
  domicilios$IRPG |>
  tapply(domicilios$setor, mean, na.rm = TRUE)

# Percentual de pagamentos entre domicílios com documento
setores$PGcomDoc <- 
  domicilios$PGcomDoc |>
  tapply(domicilios$setor, mean, na.rm = TRUE)

# Percentual de pagamentos entre domicílios sem documento
setores$PGsemDoc <- 
  domicilios$PGsemDoc |>
  tapply(domicilios$setor, mean, na.rm = TRUE)
# Alguns setores não possuem pagantes sem doc pq não existem domicílios
# sem doc que possuem dívida
setores$PGsemDoc[is.nan(setores$PGsemDoc)] <- NA

# Taxa de pagamento dos munícipes inscritos apenas 1 vez
setores$TXPGIRDA1 <-
  domicilios$IRPG[(domicilios$IRDA==1)] |> 
  tapply(domicilios$setor[(domicilios$IRDA==1)], mean, na.rm = TRUE)

# Dívida ativa de domicílios sem documento por setor
temp <- 
  domicilios[!domicilios$temDoc,"vlDebitoDA"] |>
  tapply(domicilios$setor[!domicilios$temDoc], sum, na.rm = TRUE)

setores$DASemDoc <- 
  temp[match(setores$Name, names(temp))]

setores$DASemDoc[is.na(setores$DASemDoc)] <- 0

setores <- setores[order(setores$DASemDoc),]
setores$ordemDASemDoc <- 1:num_setores

setores <- setores[order(setores$Name),]


# Dívida ativa teórica dos domicílios sem documento, se cadastro fosse completado
# Esse cálculo é feito aplicando a taxa mais elevada de pagamento de cada setor
# entre os pagantes sem e com documento

setores$TxMaxPG <- pmax(setores$PGsemDoc, setores$PGcomDoc, na.rm = TRUE)


setores$PGSDocSNA <- setores$PGsemDoc
setores$PGSDocSNA[is.na(setores$PGSDocSNA)] <- 0

setores$DATeorica <- 
  setores$DASemDoc *
  (1-setores$TxMaxPG) /
  (1-setores$PGSDocSNA)

# Os setores que não possuem dívida sem doc, continuaria 0
setores$DATeorica[is.nan(setores$DATeorica)] <- 0

setores$recuperacao <- setores$DASemDoc - setores$DATeorica

# seleção apenas dos setores estáveis
setores <- setores[order(setores$Name),]
setores_est <- setores |> subset(VariacaoNumDomicilios<pfc_setores)

## Demais variáveis ####

# IRDA da cidade
domicilios$IRDA |> mean()

# Qual o percentual de imóveis com doc q pagam?
PGcomDoc <- 
  domicilios[
    domicilios$temDoc==TRUE
    & domicilios$RendaDomicilioMedia > 60000
    ,"IRPG"] |> 
  mean (na.rm = TRUE)

# Qual o percentual de imóveis sem doc q pagam?
PGsemDoc <- 
  domicilios[
    domicilios$temDoc==FALSE
    & domicilios$RendaDomicilioMedia >10000
    ,"IRPG"] |> 
  mean (na.rm = TRUE)

# Total da DA da cidade de domicílios sem doc
domicilios[!domicilios$temDoc,"vlDebitoDA"] |> sum() # total da cidade

# Total da DA da cidade (dos minicípios atualmente sem doc) se atualizar cadastro
setores$DATeorica |> sum(na.rm = TRUE)

# Número de domicílios sem doc
(!domicilios$temDoc) |> sum()

# Esse é o montante de recuperação possível de uma ampla campanha de 
# cadastramento de CPF
setores$recuperacao |> sum()


## Visualizando... ####
# Dá pra perceber que existe uma forte correlação entre ter documento e 
# pagar o IPTU.
# setores |> 
#   ggplot(aes(x = temDocP, y = IRDA, colour = ordemRenda))+
#   geom_point()

# Na verdade, a correlação deve ser maior em pagar e ter renda
setores |> 
  ggplot(aes(colour = IPTUValorVenal, y = IRDA, x = ordemRenda))+
  geom_point() +
  stat_smooth()

# só pra perceber que nas regiões de mais alta renda, as pessoas tem doc registrado
# na pmv
setores |>
  ggplot(aes(y = temDocP, x = ordemRenda)) +
  geom_point() +
  stat_smooth()

# alta correlação entre IRPG, temDocP e renda média
# na verdade, a correlação entre pagar e não pagar é o temDocP
setores |>
  ggplot(aes(x = temDocP, y = IRPG, colour = ordemRenda)) +
  geom_point() +
  stat_smooth()

# Elevada correlação negativa entre IRDA e IRPG e renda
# Ou seja, quem deixa de pagar uma vez apenas tem mais chance de pagar
# ATENÇÃO: a correlação negativa desse gráfico é necessária. Afinal, 
# a qtd de débitos em atráso indica a possibilidade de fracionamento do 
# IRPG. uma reta de 45º é esperada, nesse sentido. Mas... tb é preciso observar
# q todos eles podem incorrer em 0(não pagar) e 1 (pagar tudo).
setores |>
  ggplot(aes(x = IRDA, y = IRPG, colour = ordemRenda)) +
  geom_point() +
  stat_smooth()

setores |>
  ggplot(aes(x = IRDA, y = IRPG, colour = temDocP)) +
  geom_point() +
  stat_smooth()

# Duas conclusões:
# 1- é mais fácil recuperar a dívida de quem deixou de pagar pela primeira vez
# P. ex.: enviar uma informação antes de terminar o ano para quem tem débito
# em aberto e não possui histórico de DA, q se deixar de pagar vai entra em DA,
# com informações sobre a multa. Isso deve ter um efeito significativo.
# 2- para recuperar a dívida antiga, é preciso registrar o CPF dos devedores


# Causas do não pagamento:
# renda e ausência de doc


# A questão é: entre os bairros de baixa renda (ordemRenda<200), quem paga
# temDoc ou não?
setores_br <- setores |> subset (ordemRenda<200)

# isso mostra que a inadimplência é difusa nesse grupo.
setores_br |>
  ggplot(aes(x = InadimplentesPercentual, y = ordemRenda)) +
  geom_point() +
  geom_smooth()

# isso mostra que a recorrência de pg tem a ver com o documento
setores_br |>
  ggplot(aes(x = temDocP, y = IRPG)) +
  geom_point() +
  geom_smooth()
  
# As pessoas q tem doc pagam menos por causa da renda
setores |>
  ggplot(aes(y = PGcomDoc, x = ordemRenda)) +
  geom_point() +
  stat_smooth()

setores |>
  ggplot(aes(y = PGcomDoc, x = IRDA, colour = ordemRenda)) +
  geom_point() +
  stat_smooth()

# nos setores de alta renda, as pessoas pagam mesmo sem doc!
# Mas, tb deixam de pagar sem doc, embora paguem com doc.
setores |>
  ggplot(aes(y = PGsemDoc, x = ordemRenda)) +
  geom_point()

setores |>
  ggplot(aes(y = PGsemDoc, x = IRDA, colour = ordemRenda)) +
  geom_point() +
  stat_smooth()


# Agora, o ponto é: achar estimativa para as duas ações propostas

# Na ação 1: a possibilidade de impedir as pessoas de entrarem em DA
# é limitada pela taxa de pagamento onde IRDA = 1
domicilios$IRPG[(domicilios$IRDA==1) & 
                  (domicilios$RendaDomicilioMedia > 60000)] |> 
  mean( na.rm = TRUE)

domicilios$IRPG[domicilios$IRDA==1] |> mean()
domicilios$IRPG[domicilios$IRDA==2] |> mean()
domicilios$IRPG[domicilios$IRDA==3] |> mean()
domicilios$IRPG[domicilios$IRDA==4] |> mean()
domicilios$IRPG[domicilios$IRDA==5] |> mean()

setores |>
  ggplot(aes(y = TXPGIRDA1, x = ordemRenda)) +
  geom_point() +
  stat_smooth()

# Na ação 2: aplicar índices de pagamentos para as dívidas sem cpf
# Atualmente, a dívida sem DOC é de:


(setores |>
    subset(DASemDoc>0) |>
    ggplot(aes(y = DASemDoc, x = ordemDASemDoc)) +
    geom_point() +
    geom_point(aes(y = DATeorica, x = ordemDASemDoc), color = "red")) |>
  ggplotly()


setores |>
  ggplot(aes(y = DATeorica, x = ordemRenda)) +
  geom_point() +
  stat_smooth()

# Gráfico da recuperação
setores |>
  ggplot(aes(y = recuperacao, x = ordemRenda)) +
  geom_point() +
  stat_smooth()

