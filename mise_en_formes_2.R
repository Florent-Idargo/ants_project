library(ggplot2)

deaths <- read.csv(file = "/Users/fouad/Desktop/Charlotte/deaths.csv",header= T, dec= ",",sep=";")
deaths <- data.frame(deaths[,0:8])

colonies <- list("Lyon 8 ","St-Sulpice 4 ","Ours 14 Av ","Cully 4 ","EPFL 3","Ours 26 Av ") #ces deux listes avec les noms complet des colonnes du fichier .csv servent à accéder directement aux données dans le fichier
traitements <- list("dead.TRT.0","dead.TRT.0.02","dead.TRT.0.2","dead.TRT.control")

colonies_ <- list("lyon8","st-suplice_4","ours_14_Av","cully_4","epfl_3","ours_26_Av") #on crée ces 2 listes pour générer dynamiquement les noms des dataset d'arrivé pour faire les boxplots
traitements_ <- list("0","0.02","0.2","control")

inc <- function(x){
  eval.parent(substitute(x <- x + 1))
}

combinaisons <- c()
index_combinaisons <- 1
for(colony in colonies_) {
  for (treatment in traitements_) {
    combinaisons[index_combinaisons] <- paste(colony,treatment,sep="_")
    inc(index_combinaisons)
  }
}

détermine_colonie <- function (label) {
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
grande_liste <- data.frame(matrix(ncol=2,nrow=96)) #en toute logique grande_liste est un dataFrame ^^'..
levels(grande_liste[,1]) <- combinaisons
names(grande_liste) <- c("colonie_traitement","n_death")

index_listes <- 1
for(line in seq(96)) { 
  if (toString(deaths[line,3]) %in% colonies) {
    grande_liste[index_listes,1] <- paste(détermine_colonie(deaths[line,3]),"0",sep="_")
    grande_liste[index_listes,2] <- deaths[line,4]
    inc(index_listes)
    
    grande_liste[index_listes,1] <- paste(détermine_colonie(deaths[line,3]),"0.02",sep="_")
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
  #Création des 24 dataFrames d'arrivé 
  for(colony in colonies_) {
    for (treatment in traitements_) {
      assign(paste(colony,treatment,sep="_"), data.frame(matrix(nrow=16,ncol=1)))
      combinaisons[index_combinaisons] <- paste(colony,treatment,sep="_")
      index_combinaisons <- 1 + index_combinaisons
    }
  }
  
  #on redonne un nom sans espace pour les colonies dans le dataFrame deaths 
  deaths %>% mutate_if(is.factor, as.character) -> deaths #pour passer les strings étant des facteurs comme des charactères (ainsi les rendants modifiables)
  names(deaths) <- c("GENUS","species","colony","t0","t0.02","t0.2","tcontrol","check_day") 
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
  
  #On définie la fonction constructor() pour remplir automatiquement les dataFrame de chaque combinaison traitement+colonie
  constructor <- function (colony,treatment) { 
    index_dataFrame_colonie <- 1 
    arrivalOutput <- data.frame(matrix(nrow=16,ncol=1))

    for (line in 1:96) {
      if (colony %in% toString(deaths[line,3])) {
        print(index_dataFrame_colonie)

        arrivalOutput <- get(paste(colony,treatment,sep="_"))
        arrivalOutput[index_dataFrame_colonie,1] <- deaths[line,toString(paste0("t",treatment))]
        assign(paste(colony,treatment,sep="_"),arrivalOutput, envir = .GlobalEnv)
        inc(index_dataFrame_colonie) 
      }
    }
    #print(arrivalOutput)
    # if (index_dataFrame_colonie != 16) {
    #   stop(paste("ERREUR: Le nombre d'occurence de la colonie", colony, "avec le traitement", treatment, "n'est pas égale à 16.")) # comme dits précédemment, on vérifie qu'on a des données cohérentes avant de conitnuer, c'est à dire 16 entrées de données par colonie
    # }
  }
  
  #On appelle constructor() 24 fois grâce aux boucles sur "traitements" et "colonies" pour avoir l'ensemble des données qu'on utilisera pour les boxplots
  for (colony in colonies_) {
    for (treatment in traitements_) {
      constructor(colony,treatment)
    }
  }
  
  #On crée un dataFrame avec le summary de chaque combinaisons traitement + colonie
  récapitulatif <- data.frame(matrix(ncol=24,nrow=7))
  rownames(récapitulatif) <- c("Minimum","1st Quartile","Median","Mean","3rd Quartile","Maximum","NA's")
  combinaison <- 1
  for (colony in colonies_) {
    for (treatment in traitements_) {
      nom_de_combinaison <- paste(colony,treatment,sep="_")
      names(récapitulatif)[combinaison] <- nom_de_combinaison
      récapitulatif[,combinaison] <- as.numeric(sub('.*:', '', summary(get(nom_de_combinaison))))
      inc(combinaison)
    }
  }

## Production de 4 petits boxplot en grille par traitements
  require(gridExtra) # pour afficher les graphiques dans une grille de 2 par 2
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
  for (i in 4:7) {
    récapitulatif_grille[,index_récapitulatif_grille] <- as.numeric( sub('.*:', '', summary(deaths[,i]) ) )
    inc(index_récapitulatif_grille)
  }
  
  
# Résultat
  # récapitulatif
  # boxplot24
  #grille_boxplot
  #récapitulatif_grille






