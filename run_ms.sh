#!/usr/bin/env bash

Rscript 01-plot_anim.R
R -e "rmarkdown::render('02-model.Rmd',output_file='02-model.html', output_format ='html_document')"


