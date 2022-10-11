# trata as informações coletadas

library(rmarkdown)

dir.create("manipulacao/dados", showWarnings = FALSE)
dir.create("visualizacao/dados", showWarnings = FALSE)

# Cruza os georreferenciamento dos imóveis
source("manipulacao/imoveis.R")

# Retira setores que não possuem imóveis
source("manipulacao/setores.R")

# Cálculo dos indicadores
source("manipulacao/indicadores.R")

# Roda as análises
render("manipulacao/analise1.Rmd", output_dir = "visualizacao/www")
render("manipulacao/analise2.Rmd", output_dir = "visualizacao/www")
