#!/usr/bin/env Rscript

# get data from convid, Ceara State

library(tidyverse)
library(lubridate)
library(rjson)

get_ce_data <- function(tipo, 
						first_day = ymd("2020-03-14"), 
						last_day = Sys.Date(), 
						wait = 2){
	out <- list()
	d <- seq(first_day, last_day, 1)

	base_query <- 
		"https://indicadores.integrasus.saude.ce.gov.br/api/coronavirus/qtd-por-municipio?data=" 

	l_query <- paste0("&tipo=", tipo, "&idMunicipio=")

	for(i in as.list(d)){
		print(paste0("getting ", tipo, " data from ", i))

		query <- URLencode(paste0(base_query, i, l_query))

		json_data <- fromJSON(file = query) %>%
			bind_rows() %>%
			mutate(dia = i)

		out[[as.character(i)]] <- json_data
		Sys.sleep(wait)
	}
	bind_rows(out) 
}

obito <- get_ce_data("Óbito", wait = 1)
obito <- obito %>% 
	rename(obito = qtdObito) %>%
	select(-tipo) 


confirmado <- get_ce_data("Confirmado", wait = 1)
confirmado <- confirmado %>% rename(confirmado = qtdConfirmado) %>%
	select(-tipo) 


suspeito <- get_ce_data("Suspeito", wait = 1)

suspeito <- suspeito %>% rename(suspeito = qtdSuspeito) %>%
	select(-tipo) 

out <- left_join(confirmado, suspeito)

out <- left_join(out, obito, by = c("idMunicipio", "municipio", "dia"))

write_csv2(x = out, path = "output/ce_municipio.csv")


out_sum <- out %>% 
	select(-idMunicipio, -municipio) %>%
	group_by(dia) %>%
	summarize(
			  suspeito = sum(suspeito, na.rm = TRUE),
			  confirmado = sum(confirmado, na.rm = TRUE),
			  obito = sum(obito, na.rm = TRUE),
			  )

write_csv2(x = out_sum, path = "output/ce.csv")


incidencia <- fromJSON(file = 
					   "https://indicadores.integrasus.saude.ce.gov.br/api/coronavirus/qtd-por-dia-tipo?tipo=Confirmado"	   
				   ) %>%
bind_rows() %>%
select(-tipo) %>% 
rename(dia = data)

write_csv2(x = incidencia, path = "output/ce_novos_casos.csv")
