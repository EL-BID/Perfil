##### Perfil da Dívida Ativa do IPTU #######
##
## Autor: Rodrigo Franklin
##
## BR-T1496
##
####

## Variáveis de configurações ####
source("config.R", encoding = "UTF-8")

## instalando e carregando pacotes requeridos ####
source("pacotes.R")

## Fatias ####
# Coleta
source("coleta/coleta.R")

# Manipulação
source("manipulacao/manipulacao.R")

# Visualização
file.create("visualizacao/restart.txt")
print("Aplicação atualizada...")