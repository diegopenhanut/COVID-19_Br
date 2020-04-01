#!/usr/bin/env RScript

args = commandArgs(trailingOnly=TRUE)

texto <- args[[1]]

library(tidyverse)
library(lubridate)

mapa_sigla <-
	structure(list(sigla = c("AC", "AL", "AP", "AM", "BA", "CE",             
							 "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE",        
							 "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),             
				   nome = c("ACRE", "ALAGOAS", "AMAPÁ", "AMAZONAS", "BAHIA",            
							"CEARÁ", "DISTRITO FEDERAL", "ESPÍRITO SANTO", "GOIÁS",              
							"MARANHÃO", "MATO GROSSO", "MATO GROSSO DO SUL", "MINAS GERAIS",     
							"PARÁ", "PARAÍBA", "PARANÁ", "PERNAMBUCO", "PIAUÍ", "RIO DE JANEIRO",
							"RIO GRANDE DO NORTE", "RIO GRANDE DO SUL", "RONDÔNIA",              
							"RORAIMA", "SANTA CATARINA", "SÃO PAULO", "SERGIPE", "TOCANTINS"     
							)), row.names = c(NA, -27L), class = "data.frame")                   


mapa_sigla

hoje <- Sys.Date()

linhas <- read_lines(texto)
linhas <- c('nome', linhas)
nome <- toupper(linhas[seq(from = 5, to = 108, by = 4)])
confirmados <- linhas[seq(from = 6, to = 109, by = 4)]
obitos <- linhas[seq(from = 7, to = 110, by = 4)]

output <- tibble(dia = hoje, nome, confirmados, obitos)


output <- arrange(output, nome)

output <- left_join(output, mapa_sigla) %>%
select(dia, sigla, nome, confirmados, obitos)

write_csv2(x = output, 
		   path = paste0("dados/from_mar_29/", hoje, ".csv")
		   ) 
