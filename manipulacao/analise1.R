
# Carrega informações

imoveis <- readRDS("visualizacao/dados/imoveis.RDS")
imoveis_residenciais <- readRDS("visualizacao/dados/imoveis_residenciais.RDS")
indicadores <- readRDS("visualizacao/dados/indicadores.RDS")
domicilios <- readRDS("visualizacao/dados/domicilios.RDS")
bairros <- readRDS("visualizacao/dados/bairros.RDS")
setores <- readRDS("visualizacao/dados/setores.RDS")

# Alguns dados básicos sobre a dívida:

# Valor total
DA_Total <- imoveis@data$vlDebitoDA |> sum() |> format(big.mark = ".", decimal.mark = ",", nsmall = 2)

# Valor residencial (moeda e percentual)
DA_Residencial <- imoveis_residenciais@data$vlDebitoDA |> sum() |> format(big.mark = ".", decimal.mark = ",", nsmall = 2)

DA_Residencial_P <- 
  ((imoveis_residenciais@data$vlDebitoDA |> sum()) /
     (imoveis@data$vlDebitoDA |> sum()) *100) |> 
  round(digits = 2) |> 
  format(decimal.mark = ",")

# Taxa de inadimplência 2022: valor e unidades

IPTU_TOTAL <- imoveis_residenciais$vlIPTU |> sum(na.rm = TRUE)

IPTU_N_Pago <- imoveis_residenciais$vlIPTU |>
  tapply(imoveis_residenciais$temDebitoDA2022, sum, na.rm = TRUE)

TX_inadimplencia_valor <- IPTU_N_Pago[2]/IPTU_TOTAL

Tx_inadimplencia_unidades <- (imoveis_residenciais$temDebitoDA2022 |> sum(na.rm = TRUE)) / (imoveis_residenciais$temDebitoDA2022 |> length())

