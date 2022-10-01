library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(RColorBrewer)
library(magrittr)
library(htmlwidgets)
library(sp)
library(stringr)
library(rmarkdown)
library(knitr)

# Carrega dados
domicilios <- readRDS("dados/domicilios.RDS")
setores <- readRDS("dados/setores.RDS")
bairros <- readRDS("dados/bairros.RDS")
dic_indicadores <- readRDS("dados/indicadores.RDS")
indicadores <- dic_indicadores$nome
names(indicadores) <- dic_indicadores$rotulo

# indicadores_tabela <- (dic_indicadores$agregado == "soma") |> which()
indicadores_tabela <- c(
  "NumDomicilios",
  "RendaDomicilioMedia",
  "RendaTotal",
  "ValorIPTUMedio",
  "ValorIPTUMedioInadimplente",
  "Inadimplentes",
  "InadimplentesPercentual",
  "TxInadimplencia",
  "DividaTotal",
  "DAPercentual",
  "DividaMedia",
  "DividaRendaMensal",
  "ValorVenalTotal",
  "ValorVenalMedio",
  "vlVenalImovelMedioInadimplentes",
  "IPTUValorVenal"
)

# cria as linhas da tabela de dados
tabela <- NULL
for (x in 1:length(indicadores_tabela)) {
  celula_1 <- tags$td(
    style = "padding-left: 5px;",
    textOutput(paste0("nome_indicador_",x)))
  celula_2 <- tags$td(
    width = "140px",
    style = "padding-right: 5px;",
    textOutput(paste0("indicador_",x)))
  if (x %% 2 != 0 ) {
    linha <- tags$tr(celula_1, celula_2)
  } else {
    linha <- tags$tr(bgcolor="#F7F7F7", celula_1, celula_2)
  }
  tabela <- tagList(tabela, linha)
}


#
paleta <- function(intervalo) {
  # Cria paleta de cores conforme o intervalo dos dados
  # se os dados são negativos e positivos, a paleta é divergente centrada em 0
  
  qtd <- intervalo |> length()
  
  negativos <- colorRampPalette(colors = c("#C80000", "#FFF5F0"))(qtd)
  positivos <- colorRampPalette(colors = c("#F7FBFF", "#0000C8"))(qtd)
  sopositivos <- colorRampPalette(colors = c("#FFF5F0", "#C80000"))(qtd)
  
  maximo <- max(intervalo, na.rm = TRUE) *1.1
  minimo <- min(intervalo, na.rm = TRUE) *1.1
  
  if (minimo < 0 & maximo >0) {
    if (maximo > abs(minimo)) {
      dominio <- c(-maximo,maximo)
    } else {
      dominio <- c(minimo,-minimo)
    }
    cores <- c(negativos, positivos)
  } else if (maximo < 0) {
    dominio <- c(minimo,0)
    cores <- negativos
  } else {
    dominio <- c(0,maximo)
    cores <- sopositivos
  }
  
  colorNumeric(
    cores,
    domain = dominio, 
    na.color="transparent")
}

formatar <-  function (x, ind = NULL) {
  if (x |> is.na()) {
    x
  } else {
    tipo <- dic_indicadores$tipo[dic_indicadores$nome == ind]
    sufixo <- ""
    if (x > 1000000000) {
      x <- x/1000000000
      sufixo <- " bilhões"
    } else if (x > 1000000) {
      x <- x/1000000
      sufixo <- " milhões"
    }
    x <- round(x, 2)
    if (ind |> is.null() | tipo == "numeral") {
      x <- 
        paste0(format(x, big.mark = ".", decimal.mark = ",", nsmall = 2), sufixo)
    } else if (tipo == "moeda") {
      x <- 
        paste0("R$ ", x |> format(big.mark = ".", decimal.mark = ",", nsmall = 2), sufixo)
    } else if (tipo == "percentual") {
      x <- 
        paste0(x |> format(big.mark = ".", decimal.mark = ",", nsmall = 2), "%", sufixo)
    } else if (tipo == "inteiro") {
      x <- 
        paste0(format(x, big.mark = ".", big.interval = 3, decimal.mark=",", nsmall = 0), sufixo)
    }
    x
  }
}

formatar_lista <- function(z, ind) {
  lapply(
    1:length(z), 
    function(i, w = z, nome = ind){
      formatar(w[i], nome)
    }
  )
}
# definições de estilos:
# botão de ferramenta
btn_pressionado <- 
  "z-index:4000;
  right: 10px;
  width: 34px;
  height: 34px;
  background-color: #C3C3C3;
  border-bottom-left-radius: 4px;
  border-bottom-right-radius: 4px;
  border-top-left-radius: 4px;
  border-top-right-radius: 4px;
  border: 2px solid rgba(0,0,0,0.2);"

btn_normal <-
  "z-index:3000;
  right: 10px;
  width: 34px;
  height: 34px;
  background-color: #fff;
  border-bottom-left-radius: 4px;
  border-bottom-right-radius: 4px;
  border-top-left-radius: 4px;
  border-top-right-radius: 4px;
  border: 2px solid rgba(0,0,0,0.2);"

btn_rotulo <- 
  "z-index:5000;
  right: 10px;
  width: 34px;
  height: 34px;
  font-size: 22px;
  text-align: center;
  background-color: transparent;"
