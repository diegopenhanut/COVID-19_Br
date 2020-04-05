# Número de casos do COVID-19 no Brasil

Esse repositório é um esforço no sentido de preservar banco de dados da
evolução na quantidade no número de casos do COVID-19 no Brasil. Comecei a
coletar os dados do ministério da saúde a partir de 14 de março. Para conseguir
os dados anteriores, procurei em grandes portais de notícia. Detalhes sobre
isso podem ser encontrados na pasta `dados/`

Uma parte dos gráficos foi feita com animações, de modo que possa ter maior
impacto, mostrando as pessoas o quão rápido o número de casos vêm aumentando.

## Requisitos (Ubuntu 18.04)
- Para compilar pacotes
`sudo apt install r-base-dev`
- libudunits2
`sudo apt install libudunits2-dev`
- gdal
`sudo apt install libgdal-dev libmagick++-dev` 
- libsodium
`sudo apt install libsodium-dev`
## Gráficos sem animação

![](plots/brasil_linear.png)

![](plots/estados_barra.png)


## Gráficos com animação

### Mesmo gráfico mostrado no tópido anterior, mas animado

![](animações/brasil_linear.gif)

### Número de casos por estado

![](animações/estados_barras.gif)


### Número de casos por estado e tempo, escala de cor
![](animações/brasil_mapa.gif)

## Observações
- Vou tentar manter atualizado, tão logo o Ministério da Saúde continue
  liberando as tabelas com número de casos