#On répertorie et lance tout les packages nécessaires pour faire tourner l'application
library(shiny)
library(pxweb)
library(shinyWidgets)
library(shinyBS)
library(tidyverse)
library(beeswarm)
library(DT)


#define global (code à faire tourner une seule fois avant de lancer l'application)

#permet de récupérer les différents thèmes présent sur pxweb pour ainsi les mettre à disposition dans le premier input de l'application, labelisé "choix du sujet" et créé dans la partie ui
url <- "https://px.web.ined.fr:443/BDPD/api/v1/fr/"
px_levels1 <- pxweb_get(url)
choix_database <- list()
for (i in 1:length(px_levels1)) {
  choix_database[i] <- (px_levels1[[i]]$id)
}

