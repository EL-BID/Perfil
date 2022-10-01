# trata as informações coletadas

# Cruza os georreferenciamento dos imóveis
source("manipulacao/imoveis.R")

# Retira setores que não possuem imóveis
source("manipulacao/setores.R")

# Cálculo dos indicadores
source("manipulacao/indicadores.R")

# Roda as análises
# knit("manipulacao/analise1.Rmd", output = "visualizacao/analise1.md")