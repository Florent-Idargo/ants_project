library(ggplot2)
library(dplyr)

#Notes:
# -les noms en fr sont des variables globales définie sur tout les fichier et les noms en anglais sont juste définie de manière local (juste dans la fonction ou ils sont appellés)
# -on renome les colonnies et les traitements avec des noms sans espace pour pouvoir générer dynamiquement à partir de leur nom et des traitements les variables correspondantes

#Obtention des données
deaths <- read.csv(file = "/Users/fouad/Desktop/Charlotte/deaths.csv",header= T, dec= ",",sep=";")
deaths <- data.frame(deaths[,0:8],stringsAsFactors = FALSE)
lala <- deaths
deaths %>% mutate_if(is.factor, as.character) -> deaths #pour passer les strings étant des facteurs comme des charactères (ainsi les rendants modifiables)

#Initialisation des variables globales
colonies <- list("Lyon 8","St-Sulpice 4","Ours 14 Av","Cully 4","EPFL 3","Ours 26 Av") #ces deux listes avec les noms complet des colonnes du fichier .csv servent à accéder directement aux données dans le fichier
traitements <- list("dead.TRT.0","dead.TRT.0.02","dead.TRT.0.2","dead.TRT.control")

colonies_ <- list("lyon8","st-suplice_4","ours_14_Av","cully_4","epfl_3","ours_26_Av") #on crée ces 2 listes pour générer dynamiquement les noms des dataset d'arrivé pour faire les boxplots
traitements_ <- list("0","0.02","0.2","control")

combinaisons <- c()
index_combinaisons <- 1

#Organisation des données: un dataframe par combinaison de traitement-colonie
  

  #Création des 24 dataFrames d'arrivé 
  for(colony in colonies_) {
    for (treatment in traitements_) {
      assign(paste(colony,treatment,sep="_"), data.frame(matrix(nrow=16,ncol=1)))
      combinaisons[index_combinaisons] <- paste(colony,treatment,sep="_")
      index_combinaisons <- 1 + index_combinaisons
    }
  }
  #mise en index de la date dans les dataFrames d'arrivé

  #on redonne un nom sans espace pour les colonies dans le dataFrame deaths 
  for(line in seq(96)) { #96 le nombre de lignes dans deaths.csv A MODIFIER par une variable globale
    if ("Lyon 8 " %in% toString(deaths[line,3])) {
      deaths[line,3] <- "lyon8"
    }
    else if ("St-Sulpice 4 " %in% toString(deaths[line,3])) {
      deaths[line,3] <- "st-suplice_4"
    }
    else if ("Ours 14 Av " %in% toString(deaths[line,3])) {
      deaths[line,3] <- "ours_14_Av"
    }
    else if ("Cully 4 " %in% toString(deaths[line,3])) {
      deaths[line,3] <- "cully_4"
    }
    else if ("EPFL 3" %in% toString(deaths[line,3])) {
      deaths[line,3] <- "epfl_3"
    }
    else if ("Ours 26 Av " %in% toString(deaths[line,3])) {
      deaths[line,3] <- "ours_26_Av"
    }
    else {
      print(paste("Erreur: une colonnie n'a pas pu être renomée, son index est:", line))
    }
  }

  #on renome les colonnes pour avoir des noms sans espace
  names(deaths) <- c("GENUS","species","colony","0","0.02","0.2","control","check_day") #soit c() une fonction qui permet de crée des listes
  

  #On définie la fonction constructor() pour remplir automatiquement les dataFrame des boxplots en fonction des données du fichier deaths.csv
  constructor <- function (colony,treatment) { #colony et treatment on besoin d'être des strings
    index_dataFrame_colonie <- 1 #les index commencent à 1 dans les data frame R et non à 0
    match <- 0 #match est juste une variable pour vérifier qu'aucune erreur n'arrive, on vérifie le nombre d'entrées par colonie. Si tes données bouges et ne sont pas cohérente t'auras une erreur qui te l'indiquera directement comme ca
    arrivalOutput <- data.frame(matrix(nrow=16,ncol=1))
    
    for (line in 1:96) {
      if (colony %in% toString(deaths[line,3])) {
        match <- 1+match
        
        arrivalOutput <- get(paste(colony,treatment,sep="_"))
        arrivalOutput[index_dataFrame_colonie,1] <- deaths[line,treatment] 
        assign(paste(colony,treatment,sep="_"),arrivalOutput, envir = .GlobalEnv)
        
        index_dataFrame_colonie <- 1+index_dataFrame_colonie
      }
    }
    #print(arrivalOutput)
    if (match != 16) { #si tu rajoutes des dates, ce nombre doit être égale au nombres de dates (ou au nombres de lignes/nombres de colonies)
      stop("ERREUR: Le nombre d'occurence de cette colonie dans le dataframe n'est pas égale à 16.") # comme dits précédemment, on vérifie qu'on a des données cohérentes avant de conitnuer, c'est à dire 16 entrées de données par colonie
    }
  }
  
  #On appelle constructor() 24 fois grâce aux boucles sur "traitements" et "colonies" pour avoir l'ensemble des données qu'on utilisera pour les boxplots
  for (colony in colonies_) {
    for (treatment in traitements_) {
      constructor(colony,treatment)
    }
  }


#Boxplot avec 24 entrées reprenant la colonie et le traitement agencé par groupe de traitements, équivalent des 4 graphes qui suivront mis bouts à bouts 
  
  #ggplot ne prends que des objets de type list, donc on va transformer notre dataFrame qui était facile en une liste
  #vu qu'on veut représenter les 24 possibilités, on va construire un dataFrame qui les inclut tous sous la forme travaillée
  deaths_24 <- data.frame(matrix(nrow = 16,ncol = 24))
  names(deaths_24) <- combinaisons
  for (colonne in seq(24)) {
    deaths_24[,colonne] <- get(combinaisons[colonne])
  }
  ldeaths_24 <- list(deaths_24)
  

# Segmentation en 4 graphs

# Données statistiques par traitements







