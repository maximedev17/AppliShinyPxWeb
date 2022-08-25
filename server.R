#
# La partie serveur, ce script, permet d'implémenter tout les outputs/fonctions/visualisations dépendant des informations données par l'utilisateur
# Vous pouvez lancer l'application en cliquant sur le bouton 'Run App' en haut à droite.
#
# Pour plus d'information sur la création de d'une appli Shiny, vous pouvez visiter le site suivant :
#
#    http://shiny.rstudio.com/
#


#Les objets créés grâce à des functions reactives permettent de stocker ce qui est renvoyé par cette fonction
#De plus la fonction ne sera réexécutée que si l'un des input/élément qu'elle utilise est modifié (l'objet renvoyé sera donc modifié à ce moment-là uniquement)
#Les fonctions réactives sont notamment efficaces pour minimiser le temps nécessaire à faire tourner l'application
#Aussi, elles permettent de stocker des variables à la différences des outputs utilisés pour afficher/renvoyer des tableaux/graphiques... 


# Definition de "server"
shinyServer(function(input, output) {
  
  #1) Variables réactives créées lors de l'import des données
  
  #data est créé ou modifié uniquement lorsque l'on clique sur le bouton valider
  #La fonction remplissant data importe la base de donnée de pxweb sélectionnée
  #data sera ensuite utilisés pour construire tout le reste de l'application
  data <- eventReactive(input$go, {
    url <- paste0("https://px.web.ined.fr:443/BDPD/api/v1/fr/",input$selectbdd,"/",input$choose_data)
    px_levels3 <- pxweb_get(url)
    #création de la requête à faire sur les données situés dans pwxweb (on importe tout)
    pxweb_query_list <- list()
    for (i in 1:length(px_levels3$variables)) {
      pxweb_query_list[px_levels3$variables[[i]]$text] <- c("*")
      #cette ligne de code est nécessaire pour pouvoir importer les données qui sont trop importantes pour être importés directement 
      px_levels3$variables[[i]]$elimination <- TRUE
    }
    pxq <- pxweb_query(pxweb_query_list)
    #Importation des données puis passage au format dataframe
    px_data <- pxweb_advanced_get(url, pxq, pxmdo = px_levels3)
    px_dataframe <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
    #On en profite pour enregistrer les noms de variables, cette variable pourra être réutilisée par la suite
    #avant cela on pivote le tableaux pour que nos "Indicateurs" apparaissent comme autant de variables (le format de la base de données après importation n'est pas utilisable sinon dans ce cas)
    if ("Indicateur" %in% colnames(px_dataframe)){
      px_dataframe <- pivot_wider(px_dataframe, names_from = Indicateur, values_from = colnames(px_dataframe)[length(px_dataframe)])
    } 
    nomcol_data <<- colnames(px_dataframe)
    return(px_dataframe)
  })
  
  
  #renvoie dans une liste les différentes base de données disponibles selon le thème choisie (theme = selectbdd)
  output$datachoice <- renderUI({
    url <- paste0("https://px.web.ined.fr:443/BDPD/api/v1/fr/",input$selectbdd,"/")
    px_levels2 <- pxweb_get(url)
    choix_donnees <- list()
    for (i in 1:length(px_levels2)) {
      choix_donnees[i] <- (px_levels2[[i]]$id = px_levels2[[i]]$text)
    }
    selectInput(inputId="choose_data",
                label="choix de la base de données",
                choices=choix_donnees)
  })
  
  
  #retourne une liste des indicateurs en retirant les noms des variables temporelles et territoriales des noms de colonne (= variable) de la base de données 
  nom_indicateurs <- reactive({
    data()
    nomcol_data[! nomcol_data %in% c(input$temporellevar,input$territoirevar)]
  })
  
  
  #Au moment où l'on importe nos première données, cet output prend la valeur "Oui" et on affiche le reste de l'application dépendant des données (voir partie UI)
  output$afficherassociation <- reactive({
    if (is.null(data()) == FALSE) {
      "Oui"
    }
  })
  #Après avoir bloqué l'affichage des output rendu par le code dépendant de afficherassociation, on précise aussi qu'il ne faut tout simplement pas lancer le code (Optimisation)
  outputOptions(output, "afficherassociation", suspendWhenHidden=FALSE)
  
  
  #Affiche des inputs demandant à l'utilisateur de spécifier une variable temporelle et une variable pour le niveau géographique 
  #(comme le choix pour ces inputs dépend d'un autre input, la base de donnée téléchargée, ils sont codées partie serveur)
  output$association <- renderUI({
    data()
    tagList(
      selectInput('temporellevar','Quel est la variable caractérisant l année de l observation ?', choices = c("",nomcol_data)),
      selectInput('territoirevar','Quel est la variable caractérisant le lieu de l observation ?', choices = c("",nomcol_data)), 
    )
  })
  
  
  #Une fois les variables temporelles et territoriales spécifiées, afficher prend la valeur 'Oui et on affiche le reste de l'application
  output$afficher <- reactive({
    if (input$temporellevar != "" & input$territoirevar != "") {
      #la création d'une variable comptabilisant le nombre d'individus permet plus tard d'empêcher la création de graphiques n'ayant pas d'utilité sans un certain nombre d'individus
      nb_individus <<- length(unique(data()[[input$territoirevar]]))
      "Oui"
    }
  })
  outputOptions(output, "afficher", suspendWhenHidden=FALSE)
  
  
  
  #2)Les différents outputs utilisés si page Tableau sélectionnée sont codés dans le script appelé ci-dessous
  
  source('ServeurTableau.R', local = TRUE)
  
  
  #3)Les différents outputs utilisés si page Graphique sélectionnée sont codés dans les scripts appelés ci-dessous
  # 'ServeurOutputGraphique' contient uniquement les output permettant la création des différents graphiques, le reste est dans 'ServeurInputGraphique' (noms peut-être à modifier car pas clair)
  
  source('ServeurInputGraphique.R', local = TRUE)
  source('ServeurOutputGraphique.R', local = TRUE)
  
  #4)Les différents outputs utilisés si page Carte sélectionnée sont codés dans le script appelé ci-dessous
  
  source('ServeurCartes.R', local = TRUE)
})
