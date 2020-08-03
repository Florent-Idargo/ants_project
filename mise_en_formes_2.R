library(ggplot2)

deaths <- read.csv(file = "/Users/fouad/Desktop/Charlotte/deaths.csv",header= T, dec= ",",sep=";") #On utilise read.csv() avec l'argument sep=";" car excel nous donne des fichier .csv dont les valeurs sont séparés par des ";" et non "," comme prévu par défaut dans la fonction read.csv()
deaths <- data.frame(deaths[,0:8]) #on fait le subset des colonnes qui nous intéresse

colonies <- list("Lyon 8 ","St-Sulpice 4 ","Ours 14 Av ","Cully 4 ","EPFL 3","Ours 26 Av ") #ces deux listes avec les noms complet des colonnes du fichier .csv servent à accéder directement aux données dans le fichier
traitements <- list("dead.TRT.0","dead.TRT.0.02","dead.TRT.0.2","dead.TRT.control")

colonies_ <- list("lyon8","st-suplice_4","ours_14_Av","cully_4","epfl_3","ours_26_Av") #on crée ces 2 listes pour générer dynamiquement les noms des dataset d'arrivé pour faire les boxplots
traitements_ <- list("0","0.02","0.2","control")


#La démarche: on aura 24 combinaisons possible de traitements/colonies. On créerra pour chaque combinaison un dataFrame (tableau).
#Pour pouvoir les retrouver facilement on standardise la manière dont on les nommes: d'abord le nom de la colonie puis le nom du traitement comme dans les listes lignes 9 et 10
#Avec la démarche on pourra accéder à l'ensemble de ces 24 combinaison avec la fonction get()
#la fonction get() va chercher avec la chaîne de charactères qu'on lui fournit comme input.
#la fonction paste() "colle" l'ensemble des chaînes de charactères qui lui sont fournit pour faire une seule grande chaîne de charactère
#Ainsi avec get(paste(colonies_[1],traitements_[1],sep="_")) on pourra aller chercher le dataFrame lyon8_0
#Ce qu'on vient de faire au dessus est utilisé dans chaque boucle du script à partir de la ligne 32.

inc <- function(x){
  eval.parent(substitute(x <- x + 1))
} #cette fonction équivaut à faire l'opération x <- 1 + x, mais vu qu'on répète ce type d'opération à chaque fois qu'on navigue dans un dataframe faire une fonction nous gagne du temps.

combinaisons <- c() #on crée le vecteur vide qui acceuillera la liste des 24 combinaisons traitements/colonie
index_combinaisons <- 1 #on crée un objet index pour naviguer au fur et à mesure qu'on complète l'index, ici il ira de 1 à 24.
for(colony in colonies_) {
  for (treatment in traitements_) {
    combinaisons[index_combinaisons] <- paste(colony,treatment,sep="_") #paste() crée une chaine de charactère comme: "lyon8_0" -> lyon8 vient de colonies_, le "_",le séparateur qu'on a spécifié entre les arguments et "0" est le nom de traitement pris dans traitements_
    inc(index_combinaisons) #équivaut à index_combinaisons <- 1 + index_combinaisons
  }
}

détermine_colonie <- function (label) { #par défaut on a des noms avec espace dans le .csv, le rôle de cette fonction c'est de prendre en entrée un nom avec espace qu'on trouve dans .csv et de retourner son nom sans espace, avec lequel on peut retrouver les dataframe qu'on vient de crée comme lyon8_0
  if ("Lyon 8 " %in% toString(label)) {
    return("lyon8")
  }
  else if ("St-Sulpice 4 " %in% toString(label)) {
    return("st-suplice_4")
  }
  else if ("Ours 14 Av " %in% toString(label)) {
    return("ours_14_Av")
  }
  else if ("Cully 4 " %in% toString(label)) {
    return("cully_4")
  }
  else if ("EPFL 3" %in% toString(label)) {
    return("epfl_3")
  }
  else if ("Ours 26 Av " %in% toString(label)) {
    return("ours_26_Av")
  }
  else {
    print(paste("Erreur: une colonnie n'a pas pu être renomée, son label est:", nom))
  }
}

#faire 4 listes (une par traitements)
grande_liste <- data.frame(matrix(ncol=2,nrow=384)) #cet objet est un dataframe, le but est de découper les 4 colonnes de traitements et de les mettres bout à bout pour pouvoir faire le boxplot dessus
levels(grande_liste[,1]) <- combinaisons #ensemble des variables qu'on peut mettre dans le dataFrame 
names(grande_liste) <- c("colonie_traitement","n_death") #renomer les colonnes

index_listes <- 1 #index_listes et la variable qui nous permettra de naviguer dans le dataframe grande_liste, àchaque qu'on rentre une donnée dams le dataframe on l'augmente de 1 pour passer à la ligne suivante.
for(line in seq(96)) { #cette boucle est celle qui va découper le dataframe deaths (ce dataframe reprends l'ensemble des données de deaths.csv) pour les mettres bout à bout dans grande_liste
  #Pourquoi cette démarche ?
  # -> on analyse 3 facteurs, donc on va rendre une information en 3 dimensions
  #   -> première dimension: colonie d'origine
  #   -> seconde dimension: nombre de morts
  #   -> troisième dimension: traitement utilisé
  
  #le boxplot n'as que deux axes et on a 3 informations, on va donc devoir rajouter l'information en trop sur un des axes.
  # à l'ordonnée on a une information numérique tandis que les deux autres sont des chaînes de charactères.
  # on va donc combiner les chaines de charactère, qui sont de même nature (chaîne de charactère) pour donner 2 informations en abscisse et la dernière sera en ordonnée
  
  #pour crée un boxplot on doit fournir une liste de valeurs à ggplot() (plus précisemment on lui donne un dataframe et dans ce dataframe on lui précise avec aes l'intitulé qu'on veut regarder ce qui nous donne une liste de valeur dans le concept.)
  #grande_liste doit donc comporter :
  #   - une colonne combinant colonnie et traitement
  #   - une colonne avec le nombre de mort correspondant à cette combinaison colonie traitement
  
  if (toString(deaths[line,3]) %in% colonies) {
    grande_liste[index_listes,1] <- paste(détermine_colonie(deaths[line,3]),"0",sep="_") #on combine l'information colonie et traitement dans une seule grande chaine de charactère du format spécifié au début du script: colonie_traitement, i.e. lyon8_0
    grande_liste[index_listes,2] <- deaths[line,4] #le nombre de mort
    inc(index_listes) #on passe à la ligne suivante en rajoutant un à l'index
    
    grande_liste[index_listes,1] <- paste(détermine_colonie(deaths[line,3]),"0.02",sep="_") #on refait la même chose pour chaque traitement dans les 3 paragraphes suivant
    grande_liste[index_listes,2] <- deaths[line,5]
    inc(index_listes)
    
    grande_liste[index_listes,1] <- paste(détermine_colonie(deaths[line,3]),"0.2",sep="_")
    grande_liste[index_listes,2] <- deaths[line,6]
    inc(index_listes)
    
    grande_liste[index_listes,1] <- paste(détermine_colonie(deaths[line,3]),"control",sep="_")
    grande_liste[index_listes,2] <- deaths[line,7]
    inc(index_listes)
  }
  else {
    print(paste("Erreur à la ligne",line))
  }
}

## Production du boxplot géant de 24 éléments
boxplot24 <- ggplot(grande_liste, aes(colonie_traitement,n_death)) + geom_boxplot() + theme(axis.text.x=element_text(angle = -80, hjust = 0))

## Sortie des statistiques par combinaison traitement_colonie
  
  #Création des 24 dataFrames d'arrivé type lyon8_0 (on avait fait la liste des combinaisons à la ligne 25 et là on fait les objets qui correspondent à ces noms)
  for(colony in colonies_) {
    for (treatment in traitements_) {
      assign(paste(colony,treatment,sep="_"), data.frame(matrix(nrow=16,ncol=1))) #assign() donne une valeur à un objet qu'on appelle en donnant le string correspondant au nom de l'objet. Le second argument est la valeur qu'on veut donner à l'objet.
      combinaisons[index_combinaisons] <- paste(colony,treatment,sep="_")
      inc(index_combinaisons)
    }
  }
  
  #on redonne un nom sans espace pour les colonies dans le dataFrame deaths 
  deaths %>% mutate_if(is.factor, as.character) -> deaths #pour passer les strings étant des facteurs (donc non modifiable, très chiant) à des charactères (ainsi les rendants modifiables)
  names(deaths) <- c("GENUS","species","colony","t0","t0.02","t0.2","tcontrol","check_day") #renomer les colonnes
  
  for(line in seq(96)) { #96 le nombre de lignes dans deaths.csv 
    
    #Cette boucle renome tous les noms de colonies dans la troisième colonne de death, on leur redonne des noms sans espace car plus facile à manipuler
    
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
  
  #On définie la fonction constructor() pour remplir automatiquement les dataFrame de chaque combinaison traitement+colonie de la liste des morts correspondants sur les 16 jours d'expérience
  constructor <- function (colony,treatment) { 
    index_dataFrame_colonie <- 1 
    arrivalOutput <- data.frame(matrix(nrow=16,ncol=1)) #l'objet arrivalOutput est juste un dataframe local
    #à la fonction qu'on utilise pour ranger les valeurs qu'on obtiendra de death avant de les mettres dans une 
    #variable globale qu'on nommera dynamiquement en fonction de sa combinaison colonie/traitement

    for (line in 1:96) {
      if (colony %in% toString(deaths[line,3])) {
        arrivalOutput <- get(paste(colony,treatment,sep="_"))
        arrivalOutput[index_dataFrame_colonie,1] <- deaths[line,toString(paste0("t",treatment))]
        assign(paste(colony,treatment,sep="_"),arrivalOutput, envir = .GlobalEnv)
        inc(index_dataFrame_colonie) 
      }
    }
  }
  
  #On appelle constructor() 24 fois grâce aux boucles sur "traitements" et "colonies" pour avoir l'ensemble des données qu'on utilisera pour les boxplots
  for (colony in colonies_) {
    for (treatment in traitements_) {
      constructor(colony,treatment)
      #pour construire (mettre les 16 jours de données dans le tableau) lyon8_0 on ferait: constructor("lyon8","0")
    }
  }
  
  #On crée un dataFrame avec le summary de chaque combinaisons traitement + colonie
  récapitulatif <- data.frame(matrix(ncol=24,nrow=7)) #24 colonnes pour le nombre de combinaisons
  rownames(récapitulatif) <- c("Minimum","1st Quartile","Median","Mean","3rd Quartile","Maximum","NA's")
  combinaison <- 1 #variable d'index pour naviguer entre les colonnes de récapitulatif
  for (colony in colonies_) {
    for (treatment in traitements_) {
      nom_de_combinaison <- paste(colony,treatment,sep="_") #on établis dans une chaine de charactère la combinaison qu'on est entrain de réaliser
      names(récapitulatif)[combinaison] <- nom_de_combinaison #on met le nom de colonne qui correspond à la colonne de récapitulatif qu'on va modifier
      récapitulatif[,combinaison] <- as.numeric(sub('.*:', '', summary(get(nom_de_combinaison)))) #summary() retourne une liste de chaine de charactères alors qu'on veut des nombres. as.numeric() nous fera cette conversion 
      #On utilise une regular expression (deux premier arguments de as.numeric()) pour sélectionner les valeurs qui arrivent après leur description
      #fait tourner la fonction summary(lyon8_0) pour voir les mots qu'on essaie d'enlever
      inc(combinaison)
    }
  }

## Production de 4 petits boxplot en grille par traitements
  require(gridExtra) # pour afficher les graphiques dans une grille de 2 par 2, module compris avec ggplot2
  angle <- 20
  plot_t0 <- ggplot(deaths,aes(colony,t0)) + geom_boxplot() + theme(axis.text.x=element_text(angle = -angle, hjust = 0))
  plot_t0.02 <- ggplot(deaths,aes(colony,t0.02)) + geom_boxplot() + theme(axis.text.x=element_text(angle = -angle, hjust = 0))
  plot_t0.2 <- ggplot(deaths,aes(colony,t0.2)) + geom_boxplot() + theme(axis.text.x=element_text(angle = -angle, hjust = 0))
  plot_tcontrol <- ggplot(deaths,aes(colony,tcontrol)) + geom_boxplot() + theme(axis.text.x=element_text(angle = -angle, hjust = 0))
  grille_boxplot <- grid.arrange(plot_t0, plot_t0.02,plot_t0.2,plot_tcontrol, ncol=2,nrow=2)

## Sortie de statistiques par traitements
  récapitulatif_grille <- data.frame(matrix(ncol=4,nrow = 7))
  rownames(récapitulatif_grille) <- c("Minimum","1st Quartile","Median","Mean","3rd Quartile","Maximum","NA's")
  names(récapitulatif_grille) <- c("t0","t0.02","t0.2","tcontrol")
  index_récapitulatif_grille <- 1
  #on refait la meme chose que pour récapitulatif mais seulement une fois par traitement ici
  for (i in 4:7) { #les colonnes correspondant correspondent aux colonnex 4,5,6,7
    récapitulatif_grille[,index_récapitulatif_grille] <- as.numeric( sub('.*:', '', summary(deaths[,i]) ) )
    inc(index_récapitulatif_grille)
  }
  
  
# Résultat
  # récapitulatif
  # boxplot24
  #grille_boxplot
  # récapitulatif_grille






