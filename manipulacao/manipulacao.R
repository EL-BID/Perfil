# trata as informações coletadas
library(rmarkdown)

dir.create("manipulacao/dados", showWarnings = FALSE)
dir.create("visualizacao/painel/dados", showWarnings = FALSE)
dir.create("visualizacao/analise1/dados", showWarnings = FALSE)
dir.create("visualizacao/analise2dados", showWarnings = FALSE)

# Cruza os georreferenciamento dos imóveis
source("manipulacao/imoveis.R")

# Retira setores que não possuem imóveis
source("manipulacao/setores.R")

# Cálculo dos indicadores
source("manipulacao/indicadores.R")

# Roda as análises
# knit("manipulacao/analise1.Rmd", output = "visualizacao/analise1.md")
render("manipulacao/analise1.Rmd", output_dir = "visualizacao/www")
