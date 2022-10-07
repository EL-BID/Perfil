library(ggplot2)
library(scales)

### Configuração

# Quantidade de domicílios que serão considerados pontos fora das curvas +1
pfc_domicilios <- 11

# Ponto de corte para a variação dos domicílios dos setores
pfc_setores <- 100

### Carrega informações
imoveis <- readRDS("visualizacao/dados/imoveis.RDS")
domicilios <- readRDS("visualizacao/dados/domicilios.RDS")
setores <- readRDS("visualizacao/dados/setores.RDS")

imoveis <- imoveis@data
domicilios <- domicilios@data
setores <- setores@data

##
setores_estaveis <- setores |> subset(VariacaoNumDomicilios<pfc_setores)
domicilios_inadimplentes <- domicilios |> subset(vlDebitoDA>0)
domicilios_adimplentes <- domicilios |> subset(vlDebitoDA==0)

# Alguns dados básicos sobre a dívida:

# Valor total
DA_Total <- imoveis$vlDebitoDA |> sum() |> format(big.mark = ".", decimal.mark = ",", nsmall = 2)

# Valor residencial (moeda e percentual)
DA_Residencial <- domicilios$vlDebitoDA |> sum() |> format(big.mark = ".", decimal.mark = ",", nsmall = 2)

DA_Residencial_P <- 
  ((domicilios$vlDebitoDA |> sum()) /
     (imoveis$vlDebitoDA |> sum()) *100) |> 
  round(digits = 2) |> 
  format(decimal.mark = ",")

# Taxa de inadimplência 2022: valor e unidades

IPTU_TOTAL <- domicilios$vlIPTU |> sum(na.rm = TRUE)

IPTU_N_Pago <- domicilios$vlIPTU |>
  tapply(domicilios$temDebitoDA2022, sum, na.rm = TRUE)

TX_inadimplencia_valor <- IPTU_N_Pago[2]/IPTU_TOTAL

Tx_inadimplencia_unidades <- (domicilios$temDebitoDA2022 |> sum(na.rm = TRUE)) / (domicilios$temDebitoDA2022 |> length())


## Sobre caracterização da dívida:

# Domicílios ordenados pela dívida
tamanho <- domicilios$inscricaoCadastral |> length()

domicilios <- 
  domicilios[order(-domicilios$vlDebitoDA),]
domicilios$ordemVlDA <- tamanho:1

g1 <- domicilios[c(pfc_domicilios:tamanho),] |>
  subset(vlDebitoDA>0) |> 
  ggplot(aes(x = ordemVlDA, y = vlDebitoDA)) +
  geom_point(shape = 21, size = 1) +
  # geom_density()+
  labs(
    x = "Domicílios ordenados pelo valor da dívida ativa",
    y = "Valor da dívida ativa (em R$)",
    title = "Distribuição da dívida ativa do IPTU (imóveis residenciais)",
    colour = NULL
  ) +
  theme_gray() +
  scale_x_continuous(
    expand = c(0,0),
    labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(
    expand = c(0,0),
    labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme(aspect.ratio = .5)

# Setoes ordenados pela dívida
num_setores <- setores$Name |> length()

setores$DividaTotal <- 
  domicilios[c(pfc_domicilios:tamanho),"vlDebitoDA"] |>
  tapply(domicilios[c(pfc_domicilios:tamanho),"setor"], sum, na.rm = TRUE)

setores$Inadimplentes <- 
  domicilios[c(pfc_domicilios:tamanho),"Inadimplentes"] |>
  tapply(domicilios[c(pfc_domicilios:tamanho),"setor"], sum, na.rm = TRUE)

setores$DividaMedia <- 
  setores$DividaTotal /
  setores$Inadimplentes

setores$DAMax <- 
  domicilios[c(pfc_domicilios:tamanho),"vlDebitoDA"] |>
  tapply(domicilios[c(pfc_domicilios:tamanho),"setor"], max, na.rm = TRUE)

setores$DAMin <- 
  domicilios_inadimplentes$vlDebitoDA |>
  tapply(domicilios_inadimplentes$setor, min, na.rm = TRUE)

domicilios$temDoc <- domicilios$cpfCnpjResponsavel |> is.na() |> not()

setores$temDoc <- 
  domicilios[c(pfc_domicilios:tamanho),"temDoc"] |>
  tapply(domicilios[c(pfc_domicilios:tamanho),"setor"], sum, na.rm = TRUE)

setores$temDocP <- 
  setores$temDoc /
  setores$NumDomicilios

setores <- setores[order(setores$DividaMedia),]
setores$ordemDAMedia <- 1:num_setores

setores <- setores[order(setores$ValorIPTUMedio),]
setores$ordemIPTUMedio <- 1:num_setores

setores <- setores[order(setores$RendaDomicilioMedia),]
setores$ordemRenda <- 1:num_setores

setores_estaveis <- setores |> subset(VariacaoNumDomicilios<pfc_setores)


# Setores ordenados pela dívida, com bandas 
g2 <- setores_estaveis |>
  ggplot(aes(x = ordemDAMedia, y = DividaMedia)) +
  geom_line() +
  geom_ribbon(aes(ymin = DAMin, ymax = DAMax), alpha = 0.3) +
  labs(
    x = "Setores ordenados pela média da dívida ativa dos domicílios inadimplentes",
    y = "Valor da dívida ativa",
    # title = "Distribuição da dívida ativa do IPTU (imóveis residenciais)",
    colour = NULL
  ) +
  theme_gray() +
  scale_x_continuous(
    expand = c(0,0),
    labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(
    expand = c(0,0),
    labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme(aspect.ratio = .5)


# Regressividade do imposto

# Podemos ver que o valor do IPTU aumenta com o valor da renda
setores$IPTUValorVenal
g3 <- setores_estaveis |>
  ggplot(aes(x = RendaDomicilioMedia, y = ValorIPTUMedio)) +
  geom_point() +
  stat_smooth() +
  labs(
    # x = "Setores ordenados pela média da dívida ativa dos domicílios inadimplentes",
    # y = "Valor da dívida ativa",
    # title = "Distribuição da dívida ativa do IPTU (imóveis residenciais)",
    colour = NULL
  ) +
  theme_gray() +
  scale_x_continuous(
    expand = c(0,0),
    labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(
    expand = c(0,0),
    labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme(aspect.ratio = .5)


# No entanto, aumenta menos do que proporcionalmente, de modo que a 
# do IPTU em termos do valor venal do imóvel é decrescente
setores_estaveis |>
  ggplot(aes(x = RendaDomicilioMedia, y = IPTUValorVenal)) +
  geom_point() +
  stat_smooth() +
  labs(
    # x = "Setores ordenados pela média da dívida ativa dos domicílios inadimplentes",
    # y = "Valor da dívida ativa",
    # title = "Distribuição da dívida ativa do IPTU (imóveis residenciais)",
    colour = NULL
  ) +
  theme_gray() +
  scale_x_continuous(
    expand = c(0,0),
    labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(
    expand = c(0,0),
    labels = percent) +
  theme(aspect.ratio = .5)


# O valor efetivo do IPTU opera por faixas:
Modas <- domicilios$NumDomicilios |>
  tapply(domicilios$vlIPTU, sum, na.rm = TRUE)
Modas <- Modas |> subset(Modas > 100)
Modas <- Modas[order(-as.numeric(names(Modas)))]
Faixas <- Modas |> names() |> as.numeric()

domicilios$IPTUSuperior <- domicilios$vlIPTU > Faixas[1]
domicilios[
  domicilios$vlVenalImovel > 4000000 & 
    domicilios$vlIPTU < Faixas[1],
  "IPTUSuperior"] <- 2

IPTU_FAIXAS <- 
  domicilios[c(pfc_domicilios:tamanho),] |>
  ggplot(aes(x = vlVenalImovel, y = vlIPTU, colour = IPTUSuperior))

for (x in Faixas) {
  IPTU_FAIXAS <- IPTU_FAIXAS +
    geom_hline(yintercept = x, color = "red", alpha = .2)
}

# Nesse gráfico, duas informações se destacam: primeiro, as faixas de valores de
# IPTU, que estabelecem um teto para a cobrança para imóveis de alto valor
# Segundo, os valores de IPTU que superam essa faixa incidem sobre imóveis de 
# mais baixo valor
# Ainda há um terceiro: imóveis de alto valor com baixa taxa de IPTU
g3 <- IPTU_FAIXAS  +
  geom_point() +
  labs(
    # x = "Setores ordenados pela média da dívida ativa dos domicílios inadimplentes",
    # y = "Valor da dívida ativa",
    # title = "Distribuição da dívida ativa do IPTU (imóveis residenciais)",
    colour = NULL
  ) +
  theme_gray() +
  scale_x_continuous(
    expand = c(0,0),
    labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(
    expand = c(0,0),
    labels = percent) +
  theme(aspect.ratio = 1)


# Isso significa que o IPTU compromente uma parcela maior da renda da população
# mais pobre
setores_estaveis |>
  ggplot(aes(x = RendaDomicilioMedia, y = IPTURendaMensal)) +
  geom_point() +
  stat_smooth()


# Efeitos sobre inadimplência:
setores_estaveis$IPTUValorVenal
# A inadimplência se concentra nos setores de mais baixa renda
setores_estaveis |>
  ggplot(aes(x = RendaDomicilioMedia, y = TxInadimplencia)) +
  geom_point() +
  stat_smooth()

# É possível observar a existência de coorelação entre as taxas e a inadimplência

setores_estaveis |>
  ggplot(aes(x = IPTUValorVenal, y = TxInadimplencia)) +
  geom_point() +
  stat_smooth(method = "lm")

# O que a análise sugere é que parte da inadimplência se explica pelo impacto que
# o IPTU gera para os contribuintes.

# Portanto, uma forma de reduzir a inadimplência é tornando a taxa proporcional 
# ao valor venal do imóvel, de modo a impactar menos a população de mais baixa
# renda; Exemplo de imposto neutro sem impacto no montanto cobrado

taxa <- domicilios$vlIPTU |> sum() / 
  (domicilios |> subset(vlIPTU >0))$vlVenalImovel |> sum()

domicilios$vlIPTUTeorico <- taxa * domicilios$vlVenalImovel
# É preciso zerar IPTU teórico de quem não paga IPTU
domicilios[domicilios$vlIPTU == 0,"vlIPTUTeorico"] <- 0

setores <- setores[order(setores$Name),]
setores$valorIPTUTeoricoTotal <-
  (domicilios[c(pfc_domicilios:tamanho),"vlIPTUTeorico"] |>
     tapply(domicilios[c(pfc_domicilios:tamanho),"setor"], sum, na.rm = TRUE))
setores$valorIPTUTeorico <-
  setores$valorIPTUTeoricoTotal /
  (domicilios[c(pfc_domicilios:tamanho),"NumDomicilios"] |>
  tapply(domicilios[c(pfc_domicilios:tamanho),"setor"], sum, na.rm = TRUE))

setores <- setores[order(setores$RendaDomicilioMedia),]
plot(setores$ValorIPTUMedio, type = "l")
lines(setores$valorIPTUTeorico, col = "red")

setores |>
  ggplot(aes(x = ordemRenda, y = ValorIPTUMedio)) +
  geom_point() +
  geom_point(aes(x = ordemRenda, y = valorIPTUTeorico), colour = "red")

# O resultado em termos de taxa de inadimplência seria assim:
# valor total do IPTU x Tx de Inadimplência de cada setor
setores_estaveis |>
  ggplot(aes(x = ordemRenda, y = ValorIPTUMedio, colour = TxInadimplencia)) +
  geom_point()

domicilios$inad_teorica <- domicilios$temDebitoDA2022 * domicilios$vlIPTUTeorico
domicilios$inad_atual <- domicilios$temDebitoDA2022 * domicilios$vlIPTU

# Resultado da ampliação da arrecadação como redução da inadimplência.
domicilios$inad_atual |> sum() - 
  domicilios$inad_teorica |> sum()

#Resultado em termos de redução da taxa de inadimplência
# do total de:
domicilios$inad_teorica |> sum() /
domicilios$vlIPTUTeorico |> sum()




# Isso daqui vai servir para a análise 2! Relacionar a existência de documento 
# com a inadimplência
setores_estaveis |>
  ggplot(aes(x = temDocP, y = TxInadimplencia)) +
  geom_point()

# Percentuais de domicílios com docs conforme dívida
domicilios_inadimplentes$temDoc |> sum()/
  domicilios_inadimplentes$temDoc |> length()

domicilios$temDoc |> sum()/
  domicilios$temDoc |> length()

domicilios_adimplentes <- domicilios |> subset(vlDebitoDA == 0)
domicilios_adimplentes$temDoc |> sum()/
  domicilios_adimplentes$temDoc |> length()

setores_estaveis |>
  ggplot(aes(y = temDocP, x = RendaDomicilioMedia, colour = TxInadimplencia)) +
  geom_point()

setores$temDocP |> plot()


# Aqui, estou tentando estimar alguma modificação na taxa de inadimplência.
# Problema é que a taxa está aumentando nos setores de mais alta renda, reduzindo
# O efeito sobre a arrecadação

teste <- 
  lm(setores$TxInadimplencia ~ setores$temDocP + setores$RendaDomicilioMedia)
predict(teste)
summary(teste)


teste2 <- 
  lm(setores_estaveis$TxInadimplencia ~ setores_estaveis$temDocP + setores_estaveis$IPTURendaMensal + 0)

setores_estaveis$ITRM <- 
  setores_estaveis$valorIPTUTeorico/setores_estaveis$RendaDomicilioMedia
predicao <- setores_estaveis[,c("temDocP", "ITRM")]
names(predicao) <- c("temDocP", "IPTURendaMensal")
setores_estaveis$NovaTx <- predict(teste2, predicao)
summary(teste2)

setores_estaveis |>
  ggplot(aes(x = ordemRenda, y = valorIPTUTeorico, colour = NovaTx)) +
  geom_point()

setores_estaveis |>
  ggplot(aes(x = ordemRenda, y = TxInadimplencia)) +
  geom_point() +
  geom_point(aes(x = ordemRenda, y = NovaTx, colour = "Red"))


setores$RendaDomicilioMedia
setores |>
  ggplot(aes(x = ValorVenalMedio, y = RendaDomicilioMedia)) +
  geom_point() +
  stat_smooth()

