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

