# Baixa dados do censo a partir do basedosdados
# Método alternativo ao download via FTP presente em ibge.R

library("basedosdados")

# alternativamente, pode ser obtido direto de:
# http://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios/ES_20171016.zip

# Defina o seu projeto no Google Cloud
gcloud_id <- "pmvbigdata"

# Defina o seu projeto no Google Cloud
set_billing_id(gcloud_id)

# Para carregar o dado direto no R
domicilio_renda_censo2010 <- 
  bdplyr("br_ibge_censo_demografico.setor_censitario_domicilio_renda_2010") |>
  bd_collect()

domicilio_moradores_censo2010 <- 
  bdplyr("br_ibge_censo_demografico.setor_censitario_domicilio_moradores_2010") |>
  bd_collect()

domicilio_basico_censo2010 <- 
  bdplyr("br_ibge_censo_demografico.setor_censitario_basico_2010") |>
  bd_collect()

# filtra pelos setores de vitória
domicilio_renda_censo2010 <- 
  domicilio_renda_censo2010[domicilio_renda_censo2010$id_setor_censitario %in% 
                              setores$Name,]

domicilio_moradores_censo2010 <- 
  domicilio_moradores_censo2010[domicilio_moradores_censo2010$id_setor_censitario %in% 
                                  setores$Name,]

domicilio_basico_censo2010 <- 
  domicilio_basico_censo2010[domicilio_basico_censo2010$id_setor_censitario %in% 
                               setores$Name,]

# Altera nomes para compatibilizar com o IBGE:
# Tudo maiúsculo
# id_setor_censitario -> Cod_setor


saveRDS(domicilio_renda_censo2010, file = 
          "coleta/dados/domicilio_renda_censo2010.RDS")

saveRDS(domicilio_moradores_censo2010, file = 
          "coleta/dados/domicilio_moradores_censo2010.RDS")

saveRDS(domicilio_basico_censo2010, file = 
          "coleta/dados/domicilio_basico_censo2010.RDS")
