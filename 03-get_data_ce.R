#!/usr/bin/env RScript

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

		query <- URLencode(paste0(f_query, i, l_query))

		json_data <- fromJSON(file = query) %>%
			bind_rows() %>%
			mutate(dia = i)

		out[[as.character(i)]] <- json_data
		Sys.sleep(wait)
	}
	bind_rows(out) 
}

obito <- get_ce_data("Óbito")
obito <- obito %>% 
	rename(obito = quantidade) %>%
	select(-tipo) 


confirmado <- get_ce_data("Confirmado")
confirmado <- confirmado %>% rename(confirmado = quantidade) %>%
	select(-tipo) 


suspeito <- get_ce_data("Suspeito")
suspeito <- suspeito %>% rename(suspeito = quantidade) %>%
	select(-tipo) 

out <- left_join(confirmado, suspeito)

out <- left_join(out, obito, by = c("idMunicipio", "municipio", "dia"))

write_csv2(x = out, path = "output/ce.csv")
