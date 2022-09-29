# Baixa dados do censo a partir do basedosdados

library("basedosdados")

# Defina o seu projeto no Google Cloud
set_billing_id("pmvbigdata")

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

saveRDS(domicilio_renda_censo2010, file = 
          "coleta/dados/domicilio_renda_censo2010.RDS")

saveRDS(domicilio_moradores_censo2010, file = 
          "coleta/dados/domicilio_moradores_censo2010.RDS")

saveRDS(domicilio_basico_censo2010, file = 
          "coleta/dados/domicilio_basico_censo2010.RDS")
