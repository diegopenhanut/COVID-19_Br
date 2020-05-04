#!/usr/bin/env Rscript

# Carregar pacotes

library("brazilmaps")
library("tidyverse")
library('lubridate')
#library("gganimate")
#library("magick")
library('sf')

# Depois é melhor separar a parte com a o cleaning em outro arquivo
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


# tentando extrapolar usando log para uma semana de casos, usando dados do último mês

dias_passados <- ultimo_dia - 15
dias_futuros <- ultimo_dia + 7

predicao_brasil <- brasil %>%
	ggplot(aes(x = data, y = casos, color = tipo)) +
	scale_y_continuous(trans='log10') + 
	geom_point() +
	xlim(primeiro_dia, dias_futuros) +
	facet_wrap(~ tipo, scales = 'free') 
#	geom_vline(xintercept = dias_passados) 

predicao_brasil <- predicao_brasil + 
	geom_smooth(data = filter(brasil, data > dias_passados),
				aes(x = data, y = casos),
				fullrange=T,
				method = "glm", formula = y~x,
				method.args = list(family = gaussian(link = 'log'))) +
  ylim(0, NA) + 
	  labs(title = paste0('Predição do número casos para os próximos ',
						  7, 
						  ' dias 
						  utilizando dados dos últimos ',
						  15 ,
						  ' dias - COVID-19'),
		   subtitle = paste0('último dia: ', ultimo_dia, ', predição até: ', dias_futuros) ) +
theme_minimal() 
  
predicao_brasil

ggsave(filename = "plots/brasil_predicao.png", predicao_brasil, device = "png")

# gráfico de barras, do último dia

estados <- covid %>%
	filter(data == ultimo_dia) %>%
  select(sigla, casosAcumulados, obitosAcumulados) %>%
  gather(key = 'tipo', value = 'casos', -sigla) %>%
  mutate(tipo = factor(x = tipo, levels = c("obitosAcumulados", "casosAcumulados")))


# sem animação, estados

simples_estados <- ggplot(estados, aes(x = sigla, y = casos, fill = tipo)) +
  geom_col(position = 'dodge') +
  ggtitle(paste0("Casos confirmados por sigla em ", ultimo_dia, " COVID-19")) + 
  xlab("Estados") + 
  ylab("Confirmados") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

simples_estados

ggsave(filename = "plots/estados_barra.png", simples_estados, device = "png")




