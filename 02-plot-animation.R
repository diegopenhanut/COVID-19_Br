#!/usr/bin/env RScript
start_time <- Sys.time()
print(paste0('começando: ', start_time))

library("brazilmaps")
library("tidyverse")
library('lubridate')
library("gganimate")
library("magick")
library('sf')
# install.package('transformr')

# Lendo arquivos com casos

# casos <- read_csv2("output/casos_compilados.csv")

covid <- read_csv2("dados/COVID19.csv",
				   local = locale(encoding = "latin1")) %>%
#mutate(data = dmy(data)) %>%
mutate(sigla = estado)

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


covid <- left_join(covid, mapa_sigla)

covid 

# Variáveis que serão usadas depois

hoje <- Sys.Date()

ultimo_dia <- max(covid$data)

primeiro_dia <- ymd("2020-02-26")

# total de dias de registro
ultimo_dia - primeiro_dia  


covid <- filter(covid, data >= primeiro_dia)

# Animações
# gráfico de linha

brasil <- covid %>%
  group_by(data) %>%
  summarise(confirmados = sum(casosAcumulados, na.rm = TRUE),
            obitos = sum(obitosAcumulados, na.rm = TRUE)) %>%
  gather(key = tipo, value = casos, -data) %>%
  mutate(tipo = factor(x = tipo, levels = c("obitos", "confirmados")))

brasil

brasil <- mutate(brasil, casos = if_else(casos == 0, as.numeric(NA), casos)) 

ani_brasil <- ggplot(brasil, aes(x = data, y = casos, color = tipo)) +
  geom_line() +
  geom_point(size = 2) + 
  transition_reveal(data) +
  labs(title = 'Casos confirmados no Brasil - COVID-19') +
  theme_minimal() +
  xlab("Dia") + 
  ylab("Confirmados") 

#animate(plot = ani_brasil, nframes = 200)

magick::image_write(
					animate(ani_brasil), 
					"animações/brasil_linear.gif"
)



estados_anim <- covid %>%
  select(sigla, casosAcumulados, obitosAcumulados, data) %>%
  gather(key = 'tipo', value = 'casos', -sigla, -data) %>%
  mutate(tipo = factor(x = tipo, levels = c("obitosAcumulados", "casosAcumulados")))


anim_estado_bar <- ggplot(estados_anim, aes(x = sigla, y = casos, fill = tipo)) +
  geom_col(position = 'dodge') +
  transition_time(data) +
  labs(title = 'Casos confirmados no Brasil - COVID-19',
     subtitle = 'dia {frame_time}') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

magick::image_write(
					animate(anim_estado_bar),
					"animações/estados_barras.gif"
)

# preparando mapa

br <- get_brmap(geo = "State",
          geo.filter = NULL,
          class = "sf")

# ggplot(br) +
# 	geom_sf()

#selected_days <- 
#	covid %>% filter(data %in% seq(primeiro_dia, ultimo_dia, length.out=4))

#selected_days <- 
#	covid %>% filter(data == ultimo_dia)

# https://github.com/tidyverse/ggplot2/issues/2799
# cf <- coord_fixed()
# cf$default <- TRUE

lala <- left_join(br, covid, by = c('nome' = 'nome'))  
# 
anim_mapa <- ggplot(lala, aes(fill = casosAcumulados)) +
 	geom_sf() +
 	coord_sf(datum = NA) +
 	labs(title = "Brasil - COVID-19", 
 		 subtitle = paste0(
 						   ' casos confirmados em ',
 						   "{frame_time}"
 						   )) +
 ylab("") +
 xlab("") +
	transition_time(data) +
# scale_fill_viridis_c(na.value = 0, trans = 'log10', guide = "legend") +
 theme_minimal()

#anim_mapa

magick::image_write(
					animate(anim_mapa),
					"animações/brasil_mapa.gif"
)
# animate(plot = anim_mapa, nframes = 50, detail = 2)
# anim_save(anim_mapa, filename = "animações/brasil_mapa.gif")
# 
# anim_mapa <- ggplot(lala, aes(fill = casosAcumulados)) +
#  	geom_sf()+
#  	coord_sf(datum = NA) +
#  	labs(title = "Brasil - COVID-19", 
#  		 subtitle = paste0(
#  						   ' casos confirmados em ',
#  						   "{frame_time}"
#  						   )) +
#  ylab("") +
#  xlab("") +
# # transition_time(data) +
#  scale_fill_viridis_c(na.value = 0, trans = 'log10', guide = "legend") +
#  theme_minimal()
# 
# anim_mapa
# 
# anim_mapa <- 
#   plot_brmap(br, data_to_join = selected_days, 
#              join_by = c("nome" = "nome"),
#              var = "casosAcumulados") + 
# #  cf +
#   coord_sf(datum = NA) +
#   labs(title = "Brasil - COVID-19", 
#        subtitle = paste0(
#          ' casos confirmados em ',
#          "{frame_time}"
#        )) +
#   ylab("") +
#   xlab("") +
#   transition_time(data) +
#   scale_fill_viridis_c(na.value = 0, trans = 'log10', guide = "legend") +
#   theme_minimal()
# 
# #anim_mapa
# 
# magick::image_write(
# 					animate(anim_mapa),
# 					"animações/brasil_mapa.gif"
# )
# # animate(plot = anim_mapa, nframes = 50, detail = 2)
# anim_save(anim_mapa, filename = "animações/brasil_mapa.gif")


# Caso o output pretendido seja vídeo

# library("av")

# vid_temp <- animate(ani_brasil, renderer = av_renderer()) 
# anim_save("animações/brasil_linear.mp4", vid_temp)

# vid_estado_bar <- animate(anim_estado_bar, renderer = av_renderer()) 
# anim_save("animações/estados_barras.mp4", vid_estado_bar)

# vid_map <- animate(anim_mapa, renderer = av_renderer()) 
# anim_save("animações/brasil_mapa.mp4", vid_map)
end_time <- Sys.time()

print(paste0('terminando em: ', end_time))

print(paste0('duração: ', end_time - start_time))

