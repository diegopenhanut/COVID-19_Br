#!/usr/bin/env Rscript


# Carregar pacotes
library("tidyverse")
library('lubridate')

# Lendo arquivos com casos

# maiores info sobre origem dos dados, procurar em `dados/readme`

file_list <- list.files(path = "dados/before_mar_28/", 
						pattern = "*.csv",
						full.names = TRUE)
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
casos <- casos %>% select(dia, sigla, nome, confirmados, obitos = Óbitos)


from_mar <- read.delim2("dados/from_mar_29/brasil.csv") %>%
	mutate(dia = ymd(dia))

from_mar2 <- read_csv2("dados/from_mar_29/2020-03-30.csv") 
from_mar3 <- read_csv2("dados/from_mar_29/2020-03-31.csv") 

compilado <- bind_rows(casos, from_mar, from_mar2, from_mar3) %>%
	arrange(dia, nome)

write_csv2(compilado, 'output/casos_compilados.csv')

