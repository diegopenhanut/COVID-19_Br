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

# Variáveis que serão usadas depois

#hoje <- format(Sys.time(), "%A, %d de %B de %Y")
#hoje

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
  files[[i]] <- read.delim2(file_list[i]) %>%
#	rename(Abrangência = 1, Nome = 2) %>%
  select("Abrangência", "Nome", "Casos.suspeitos", "Casos.confirmados") %>%
    filter(Abrangência == 'Unidade da Federação') %>%
    mutate(dia = file_list[i]) 

  print("Leitura terminada")
  }

casos <- bind_rows(files)

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

casos <- casos %>% mutate(nome = toupper(str_trim(nome)))
casos <- casos %>% mutate(suspeitos = Casos.suspeitos)
casos <- casos %>% mutate(confirmados = Casos.confirmados)


# gráfico de linha

brasil <- casos %>%
  group_by(dia) %>%
  summarise(confirmados = sum(confirmados))

brasil

# sem animação, brasil
simples_brasil <- ggplot(brasil, aes(x = dia, y = confirmados)) +
  geom_line() +
  theme_minimal() 

ggsave(filename = "plots/brasil_linear.png", simples_brasil, device = "png")

# com animação, brasil

ani_brasil <- ggplot(brasil, aes(x = dia, y = confirmados)) +
  geom_line() +
  geom_point(size = 2) + 
  #coord_cartesian(clip = 'off') + 
  transition_reveal(dia) +
  labs(title = 'Casos confirmados no Brasil - COVID-19') +
  theme_minimal()  

ani_brasil

anim_save(filename = "animações/brasil_linear.gif")

anim_estado_bar <- ggplot(casos, aes(x = sigla, y = confirmados)) +
  geom_col() +
  transition_time(dia) +
  labs(title = 'Casos confirmados no Brasil - COVID-19',
     subtitle = 'dia {frame_time}') +
  theme_minimal()  

anim_estado_bar

anim_save(filename = "animações/estados_barras.gif")

# preparando mapa

br <- get_brmap(geo = "State",
          geo.filter = NULL,
          class = "sf")

#ggplot(br) + 
#  geom_sf(color = "black", fill = "lightgreen")


# mcs <- plot_brmap(br, data_to_join = casos, 
#            join_by = c("nome" = "nome"),
#            var = "suspeitos") +
#   labs(title = "Brasil - COVID-19",
#        subtitle = paste0(
#          sum(casos$suspeitos),
#          ' casos suspeitos em ', 
#          hoje
#        )) +
#   scale_fill_viridis_c(na.value = 0)

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
    scale_fill_viridis_c(na.value = 0, trans = 'log') +
    theme_minimal()
    
anim_mapa

anim_save(filename = "animações/brasil_mapa.gif")

# combinando múltiplos mapas

# a_gif <- animate(ani_brasil)
# b_gif <- animate(anim_estado_bar)
# c_gif <- animate(anim_mapa)
# 
# 
# a_mgif <- image_read(a_gif)
# b_mgif <- image_read(b_gif)
# c_mgif <- image_read(c_gif)
# 
# new_gif <- image_append(c(a_mgif[1], b_mgif[1], c_mgif[1]))
# for(i in 2:100){
#   combined <- image_append(c(a_mgif[i], b_mgif[i], c_mgif))
#   new_gif <- c(new_gif, combined)
# }
# 
# new_gif
# 
