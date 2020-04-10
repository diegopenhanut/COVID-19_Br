#!/usr/bin/env Rscript

# Carregar pacotes

library("brazilmaps")
library("tidyverse")
library('lubridate')
library("gganimate")
library("magick")
library("stringi")

theme_set(theme_minimal())

ce_map <- get_brmap(geo = "City",
					geo.filter = list(State = 23),
					class = "sf")
plot_brmap(ce_map)

ce_map <- mutate(ce_map, nome = stri_trans_general(nome, id = "Latin-ASCII")) 

mun <- ce_map$nome

ce <- read_csv2("output/ce_municipio.csv")

d <- unique(ce$dia)

backbone <- list()

for(i in mun){
backbone[[i]] <- tibble(municipio = i, dia = d)
}

backbone <- bind_rows(backbone)

ce <- left_join(backbone, ce)

ultimo_dia <- ce %>% 
	filter(dia == max(dia))


ce_map <- left_join(ce_map, ce, by = c('nome' = 'municipio'))

conf <- ggplot(ce_map) +  
	geom_sf(aes(fill = confirmado)) +
	coord_sf(datum = NA) +
	labs(title = "CE - Casos confirmados de COVID-19", 
		 subtitle = paste0(
						   ' casos confirmados em ',
						   "{frame_time}"
						   )) +
	ylab("") +
	xlab("") +
	transition_time(dia) +
	scale_fill_viridis_c(na.value = 0, trans = 'log10', guide = "legend") 

conf

anim_save(filename = "animações/ce_confirmados_mapa.gif")

ob <- ggplot(ce_map) +  
	geom_sf(aes(fill = obito)) +
  coord_sf(datum = NA) +
  labs(title = "CE - Óbitos confirmados por COVID-19", 
       subtitle = paste0(
         ' óbitos em ',
         "{frame_time}"
       )) +
  ylab("") +
  xlab("") +
  transition_time(dia) +
  scale_fill_viridis_c(na.value = 0, trans = 'log10', guide = "legend") 

ob

anim_save(filename = "animações/ce_obitos_mapa.gif")

sus <- ggplot(ce_map) +  
	geom_sf(aes(fill = suspeito)) +
	coord_sf(datum = NA) +
	labs(title = "CE - Casos suspeitos de COVID-19", 
		 subtitle = paste0(
						   ' casos suspeitos em ',
						   "{frame_time}"
						   )) +
	ylab("") +
	xlab("") +
	transition_time(dia) +
	scale_fill_viridis_c(na.value = 0, trans = 'log10', guide = "legend") 

sus

anim_save(filename = "animações/ce_suspeitos_mapa.gif")
