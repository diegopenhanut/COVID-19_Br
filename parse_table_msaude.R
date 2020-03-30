#!/usr/bin/env RScript
texto <- 
"Confirmados
Óbitos
Letalidade
Acre
42
0
0%
Alagoas
17
0
0%
Amapá
8
0
0%
Amazonas
151
1
0,7%
Bahia
176
1
0,6%
Ceará
372
5
1,3%
Distrito Federal
312
1
0,3%
Espírito Santo
72
0
0%
Goiás
61
1
1,6%
Maranhão
23
1
4,3%
Mato Grosso
18
0
0%
Mato Grosso do Sul
44
0
0%
Minas Gerais
261
1
0,4%
Paraná
155
3
1,9%
Paraíba
15
0
0%
Pará
21
0
0%
Pernambuco
78
6
7,7%
Piauí
16
3
18,8%
Rio Grande do Norte
77
1
1,3%
Rio Grande do Sul
241
3
1,2%
Rio de Janeiro
657
18
2,7%
Rondônia
6
0
0%
Roraima
16
0
0
Santa Catarina
197
1
0,5%
Sergipe
16
0
0%
São Paulo
1517
113
7,4%
Tocantins
10
0
0%"

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


output <- arrange(output, nomes)

output <- left_join(output, mapa_sigla) %>%
select(dia, sigla, nome, confirmados, obitos)

write_csv2(x = output, 
		   path = paste0("dados/from_mar_29/", hoje, ".csv")) 
