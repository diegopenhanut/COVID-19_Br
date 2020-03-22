#!/usr/bin/env Rscript

# requisitos
# libudunits2
#➜  ~ sudo apt install libudunits2-dev
# gdal
#!/usr/bin/env Rscript


#➜  ~ sudo apt install libgdal-dev
#  libmagick++-dev 
# install.packages("brazilmaps")
# install.packages(transformr)
 

# Carregar pacotes

library("brazilmaps")
library("tidyverse")
library('lubridate')
library("gganimate")
library("magick")




# Lendo arquivos com casos

# site 
# http://plataforma.saude.gov.br/novocoronavirus/
# blob:http://plataforma.saude.gov.br/940f521d-d3cb-4ebc-b52b-378cfd2112aa
# não acho que dê para fazer o download automaticamente, o 'id' do blob muda sempre

file_list <- list.files(path = "dados/", pattern = "*.csv", full.names = TRUE)
#rm(files)
files <- vector(mode = "list", length = length(file_list))

for (i in 1:length(file_list)) {
  print("lendo...")
  print(i)
  print(file_list[i])
  files[[i]] <- read.delim2(file_list[i], fileEncoding = "UTF-8") %>%
	rename(Abrangência = 1, Nome = 2) %>%
  select("Abrangência", "Nome", "Casos.confirmados", "Óbitos") %>%
    mutate(dia = file_list[i], Óbitos = as.numeric(Óbitos)) 

  print("Leitura terminada")
  }

casos <- bind_rows(files)

casos <- filter(casos, Abrangência == 'Unidade da Federação')


casos <- casos %>% 
  separate(col = Nome,
           into = c('nome', 'sigla', NA),
           sep = "[\\(\\)]" )

casos <- 
  casos %>% mutate(dia = 
                     str_extract(string = dia,
                                 pattern = "\\d{4}.\\d{2}.\\d{2}"))

casos <-
  casos %>%
  mutate(dia = ymd(dia))

casos <-
  casos %>%
  mutate(Óbitos = if_else(Óbitos == 0, as.numeric(NA), Óbitos))


summary(casos$Óbitos)

str(casos$dia) 
head(casos$dia) 

casos <- casos %>% mutate(nome = toupper(str_trim(nome)))
casos <- casos %>% mutate(confirmados = Casos.confirmados)

write_csv2(casos, 'casos_compilados.csv')

# Variáveis que serão usadas depois

hoje <- format(Sys.time(), "%A, %d de %B de %Y")

ultimo_dia <- max(casos$dia)

primeiro_dia <- min(casos$dia)


# gráfico de linha

brasil <- casos %>%
  group_by(dia) %>%
  summarise(confirmados = sum(confirmados, na.rm = TRUE),
            óbitos = sum(Óbitos, na.rm = TRUE)) %>%
  gather(key = tipo, value = casos, -dia) %>%
  mutate(tipo = factor(x = tipo, levels = c("óbitos", "confirmados")))

brasil <- mutate(brasil, casos = if_else(casos == 0, as.numeric(NA), casos)) 

filter(brasil, tipo == 'óbitos')

# sem animação, brasil

simples_brasil <- ggplot(brasil, aes(x = dia, y = casos, color = tipo)) +
  geom_line() +
  theme_minimal() +
  xlab("Dia") + 
  ylab("Confirmados") 

simples_brasil

ggsave(filename = "plots/brasil_linear.png", simples_brasil, device = "png")


# tentando extrapolar usando log para uma semana de casos

filter(brasil, tipo == "confirmados") %>%
  
ggplot(aes(x = dia, y = casos, color = tipo)) +
  scale_y_continuous(trans='log10') + 
  geom_point() +
  xlim(primeiro_dia, ultimo_dia + 7) +
  stat_smooth(method="lm", 
              #formula = 'x ~ y',
              fullrange=TRUE)


# gráfico de barras, com último dia

estados <- casos %>%
	filter(dia == ultimo_dia) %>%
  select(sigla, confirmados = Casos.confirmados, óbitos = Óbitos) %>%
  gather(key = 'tipo', value = 'casos', -sigla) %>%
  mutate(tipo = factor(x = tipo, levels = c("óbitos", "confirmados")))


# sem animação, estados

simples_estados <- ggplot(estados, aes(x = sigla, y = casos, fill = tipo)) +
  geom_col(position = 'dodge') +
  ggtitle(paste0("Casos confirmados por estado em ", ultimo_dia, " COVID-19")) + 
  xlab("Estados") + 
  ylab("Confirmados") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

simples_estados

ggsave(filename = "plots/estados_barra.png", simples_estados, device = "png")

# com animação, brasil

ani_brasil <- ggplot(brasil, aes(x = dia, y = casos, color = tipo)) +
  geom_line() +
  geom_point(size = 2) + 
  transition_reveal(dia) +
  labs(title = 'Casos confirmados no Brasil - COVID-19') +
  theme_minimal() +
  xlab("Dia") + 
  ylab("Confirmados") 

ani_brasil

anim_save(filename = "animações/brasil_linear.gif")


estados_anim <- casos %>%
  select(sigla, confirmados = Casos.confirmados, óbitos = Óbitos, dia) %>%
  gather(key = 'tipo', value = 'casos', -sigla, -dia) %>%
  mutate(tipo = factor(x = tipo, levels = c("óbitos", "confirmados")))


anim_estado_bar <- ggplot(estados_anim, aes(x = sigla, y = casos, fill = tipo)) +
  geom_col(position = 'dodge') +
  transition_time(dia) +
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
  plot_brmap(br, data_to_join = casos, 
             join_by = c("nome" = "nome"),
             var = "confirmados") + 
  coord_sf(datum = NA) +
  labs(title = "Brasil - COVID-19", 
       subtitle = paste0(
         ' casos confirmados em ',
         "{frame_time}"
       )) +
  ylab("") +
  xlab("") +
  transition_time(dia) +
  scale_fill_viridis_c(na.value = 0, trans = 'log10', guide = "legend") +
  theme_minimal()

anim_mapa

anim_save(filename = "animações/brasil_mapa.gif")

# Caso queira vídeo

# library("av")

# vid_temp <- animate(ani_brasil, renderer = av_renderer()) 
# anim_save("animações/brasil_linear.mp4", vid_temp)

# vid_estado_bar <- animate(anim_estado_bar, renderer = av_renderer()) 
# anim_save("animações/estados_barras.mp4", vid_estado_bar)

# vid_map <- animate(anim_mapa, renderer = av_renderer()) 
# anim_save("animações/brasil_mapa.mp4", vid_map)



