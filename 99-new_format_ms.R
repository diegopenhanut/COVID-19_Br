#!/usr/bin/env Rscript

library(tidyverse)
library(readxl)
library(lubridate)
library(gganimate)

start_time <- Sys.time()
print(paste0('começando: ', start_time))

tab <- read_xlsx("dados/HIST_PAINEL_COVIDBR_20200516.xlsx", 
				 col_types = 
					 c("text", "text", "text", "numeric", "text", "text", 
					   "text", "guess", "numeric", "numeric", "numeric", 
					   "numeric", "numeric", "numeric"))

tab <- mutate(tab, data = ymd(data))

ibge <- read_csv("dados/codigo_ibge.csv")

tab <- tab %>%
	select(regiao, data, coduf, semanaEpi, casosAcumulado, obitosAcumulado, Recuperadosnovos, emAcompanhamentoNovos)


tab <- left_join(tab, ibge)
ibge <- NULL

head(tab)
tail(tab)

br <- filter(tab, regiao == "Brasil") 

after_reading <- Sys.time()
print(paste0('Dados lidos e processados em ', after_reading - start_time))

#  %>%
#  select(data, casosAcumulado, obitosAcumulado) %>%
#  gather('tipo', 'casos', -data)

l <- min(br$data)
h <- max(br$data)
hp <- h + 15

ani_linear <- ggplot(br, aes(y = casosAcumulado, x = data)) +   
	geom_line() +
	geom_point(size = 2) + 
	xlim(c(l, hp)) +
	geom_segment(aes(xend = h,
					 yend = casosAcumulado),
				 linetype = 2,
				 colour = 'grey') + 
	geom_text(aes(x = hp, label = casosAcumulado), hjust = 0) + 
	transition_reveal(data) +
	theme(plot.margin = margin(5.5, 40, 5.5, 5.5))

#	theme_minimal()

animate(ani_linear)

anim_save(filename = "animações/teste_ani_linear.gif")

final <- Sys.time()

print(paste0('Tempo de ploting : ', final - after_reading))
print(paste0('Temino em : ', final))
print(paste0('Tempo total: ', final - start_time))
