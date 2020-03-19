#!/usr/bin/env bash

convert animações/brasil_linear.gif -resize '240x240' -repage 480x240 -coalesce null: \( animações/estados_barras.gif -resize '240x240' -coalesce \) -geometry +240+0 -layers Composite animações/anim1+2.gif
convert animações/anim1+2.gif -repage 720x240 -coalesce null: \( animações/brasil_mapa.gif -resize '240x240' -coalesce \) -geometry +480+0 -layers Composite animações/anim_final.gif

