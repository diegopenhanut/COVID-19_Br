#!/usr/bin/env Rscript

# Carregar pacotes

library("brazilmaps")
library("tidyverse")
library('lubridate')
library("gganimate")
library("magick")




# Lendo arquivos com casos

# casos <- read_csv2("output/casos_compilados.csv")

covid <- read_csv2("dados/COVID19.csv") %>%
	mutate(data = dmy(data)) %>%
	filter(data >= ymd("2020-02-26"))

mapa_sigla <-
	structure(list(estado = c("AC", "AL", "AP", "AM", "BA", "CE",             
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


# gráfico de linha

brasil <- covid %>%
  group_by(data) %>%
  summarise(confirmados = sum(casosAcumulados, na.rm = TRUE),
            obitos = sum(obitosAcumulados, na.rm = TRUE)) %>%
  gather(key = tipo, value = casos, -data) %>%
  mutate(tipo = factor(x = tipo, levels = c("obitos", "confirmados")))

brasil

brasil <- mutate(brasil, casos = if_else(casos == 0, as.numeric(NA), casos)) 

# filter(brasil, tipo == 'óbitos')

# sem animação, brasil

simples_brasil <- ggplot(brasil, aes(x = data, y = casos, color = tipo)) +
  geom_line() +
  theme_minimal() +
  xlab("Dia") + 
  ylab("Confirmados") +
  xlim(c(primeiro_dia, ultimo_dia))

simples_brasil

ggsave(filename = "plots/brasil_linear.png", simples_brasil, device = "png")


# tentando extrapolar usando log para uma semana de casos

predicao_brasil <- filter(brasil, tipo == "confirmados") %>%
  ggplot(aes(x = data, y = casos, color = tipo)) +
  scale_y_continuous(trans='log10') + 
  geom_point() +
  xlim(primeiro_dia, ultimo_dia + 7) +
  stat_smooth(method="lm", 
              #formula = 'x ~ log(y)',
              fullrange=TRUE) + 
  labs(title = 'Predição do número casos para os próximos 7 dias - COVID-19',
       subtitle = paste0('último dia: ', ultimo_dia, ', predição até: ', ultimo_dia + 7) ) +
  theme_minimal()
  
predicao_brasil

ggsave(filename = "plots/brasil_predicao.png", predicao_brasil, device = "png")

# gráfico de barras, com último dia

estados <- covid %>%
	filter(data == ultimo_dia) %>%
  select(estado, casosAcumulados, obitosAcumulados) %>%
  gather(key = 'tipo', value = 'casos', -estado) %>%
  mutate(tipo = factor(x = tipo, levels = c("obitosAcumulados", "casosAcumulados")))


# sem animação, estados

simples_estados <- ggplot(estados, aes(x = estado, y = casos, fill = tipo)) +
  geom_col(position = 'dodge') +
  ggtitle(paste0("Casos confirmados por estado em ", ultimo_dia, " COVID-19")) + 
  xlab("Estados") + 
  ylab("Confirmados") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

simples_estados

ggsave(filename = "plots/estados_barra.png", simples_estados, device = "png")

# com animação, brasil

ani_brasil <- ggplot(brasil, aes(x = data, y = casos, color = tipo)) +
  geom_line() +
  geom_point(size = 2) + 
  transition_reveal(data) +
  labs(title = 'Casos confirmados no Brasil - COVID-19') +
  theme_minimal() +
  xlab("Dia") + 
  ylab("Confirmados") 

ani_brasil

anim_save(filename = "animações/brasil_linear.gif")


estados_anim <- covid %>%
  select(estado, casosAcumulados, obitosAcumulados, data) %>%
  gather(key = 'tipo', value = 'casos', -estado, -data) %>%
  mutate(tipo = factor(x = tipo, levels = c("obitosAcumulados", "casosAcumulados")))


anim_estado_bar <- ggplot(estados_anim, aes(x = estado, y = casos, fill = tipo)) +
  geom_col(position = 'dodge') +
  transition_time(data) +
  labs(title = 'Casos confirmados no Brasil - COVID-19',
     subtitle = 'dia {frame_time}') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

anim_estado_bar

anim_save(filename = "animações/estados_barras.gif")

# preparando mapa

br <- get_brmap(geo = "State",
          geo.filter = NULL,
          class = "sf")

anim_mapa <- 
  plot_brmap(br, data_to_join = covid, 
             join_by = c("nome" = "nome"),
             var = "casosAcumulados") + 
  coord_sf(datum = NA) +
  labs(title = "Brasil - COVID-19", 
       subtitle = paste0(
         ' casos confirmados em ',
         "{frame_time}"
       )) +
  ylab("") +
  xlab("") +
  transition_time(data) +
  scale_fill_viridis_c(na.value = 0, trans = 'log10', guide = "legend") +
  theme_minimal()

anim_mapa

anim_save(filename = "animações/brasil_mapa.gif")


# Caso o output pretendido seja vídeo

# library("av")

# vid_temp <- animate(ani_brasil, renderer = av_renderer()) 
# anim_save("animações/brasil_linear.mp4", vid_temp)

# vid_estado_bar <- animate(anim_estado_bar, renderer = av_renderer()) 
# anim_save("animações/estados_barras.mp4", vid_estado_bar)

# vid_map <- animate(anim_mapa, renderer = av_renderer()) 
# anim_save("animações/brasil_mapa.mp4", vid_map)


