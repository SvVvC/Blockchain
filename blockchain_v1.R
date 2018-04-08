library("digest")   #Permet d'importer le hasheur SHA256
library("stringr")  #gestion des strings pour le hash
library("stringi")  #gestion des strings pour le hash
##############################################################################################################################################################################
##############################################################################################################################################################################
###############################                         POW                                                     ########################################################
##############################################################################################################################################################################
##############################################################################################################################################################################

myPreuve_de_travail <- function(block,difficulte){
  
  # Initialisation des paramètres utiles #
  
  time1=Sys.time()        #On recupere le temps système pour pouvoir calculer la durée d'exécution de l'algorithme
  preuve_de_travail= 0
  block_boucle="block_base"
  debut_hash=""
  k=0
  while (k!=difficulte){
    debut_hash<-paste("0",debut_hash, sep="")
    k=k+1
  }
  ###################################
  # Coeur de la fonction de hashage #
  #########################################################################################################
  while (stri_compare(str_sub(digest(block_boucle,"sha256"),1,difficulte), debut_hash )!=0){              
    
    preuve_de_travail <- preuve_de_travail + 1                                                            
    block_boucle <- paste(toString(preuve_de_travail),block)                                              
  }                                                                                                       
  #########################################################################################################
  
  block_final <- paste(toString(preuve_de_travail),block)
  time2=Sys.time()
  retour= list(block= block_final, hash=  digest(block_final,"sha256"), preuve_de_travail= preuve_de_travail, duree_execution= difftime(time2, time1, units="secs"), Sys.Date)
  return(retour)
}


###################################################
# Analyse de la difficulte du problème de hashage #
###################################################
## En remplacant la valeur de la difficulté, on demande à l'ordinateur de résoudre le problème avec des difficultés plus ou moins élevées.

#difficulte= 4
#données="données test"
#Durée_execution=list()
#difficulté=list()
#k=1
#while(k<=difficulte){
#  Durée_execution=c(Durée_execution, as.numeric(myPreuve_de_travail(données,k)[4]))
#  difficulté=c(difficulté,k)
#  k=k+1
#}
#plot(difficulté, Durée_execution, type="b")

## A faire: interpolaton avec exponentielle. 

############################################################################################################
############################################################################################################
#                                   Premier bloc de la chaine                                              #
############################################################################################################
############################################################################################################
difficulte = 3 # Définit la difficulté des problèmes de hashage.
n=0
bloc_nom = paste(paste("bloc_", toString(n)),".txt")
file.create(bloc_nom)
cat("################################################################################################################################################################################

Ceci est le premier bloc de la chaine
Ceci est le premier bloc de la chaine. 
Il ne contient pas de donnees significatives, mais est utile pour construire le reste de la chaine.

################################################################################################################################################################################",file=bloc_nom,sep="",append=TRUE)

# Maintenant, on va travailler directement sur le hash du bloc pour sécuriser le bloc, en s'inspirant des arbres de Merkle
bloc= readLines(bloc_nom)
hash_bloc_brut=digest(bloc,"sha256")
Preuve_de_travail_rendu= myPreuve_de_travail(hash_bloc_brut, difficulte)

Preuve_de_travail = Preuve_de_travail_rendu[3]
hash_bloc_final<<-Preuve_de_travail_rendu[2]
cat("",file=bloc_nom,sep="\n",append=TRUE)
cat("################           Données annexes              ################",file=bloc_nom,sep="\n",append=TRUE)




cat(paste("le hash brut du bloc est: ",hash_bloc_brut),file=bloc_nom,sep="\n",append=TRUE)                  #  On a travaillé sur ce hash pour obtenir la preuve de travail
cat(paste("le hash travaillé ce bloc est: ",hash_bloc_final),file=bloc_nom,sep="\n",append=TRUE)            #  On fournit le nouveau hash, qui sera réutilisé par la suite pour construire la chaine
cat(paste("la preuve de travail associée est: ", Preuve_de_travail),file=bloc_nom,sep="\n",append=TRUE)              #  On fournit la preuve de travail 
cat(paste("la date de validation du bloc est le: ",Sys.Date()),file=bloc_nom,sep="\n",append=TRUE)                        #  On marque le bloc de la date du système 


"Notes : 
            - On ne travail pas sur le bloc, mais sur le hash du bloc par soucis de simplicité. Dans le fond, cela ne change rien.
            - Ici, on utilise Sys.Date pour mettre le timestamp du bloc.
            
"
############################################################################################################
#                                                                                                          #
#                                   Creer un nouveau bloc                                                  #
#                                                                                                          #
############################################################################################################
" 
Ici, nous allons proposer la méthode qui permet d'ajouter de nouveaux blocs à la chaîne.

Le nouveau bloc doit nécessairement contenir: 
                                                - Le hash du bloc précédent qui est incérré avant le calcul de résolution du problème de hashage. 
                                                  Grâce à cette anteriorité, les blocs sont liés lors du nouveau hash.
Le nouveau bloc doit nécessairement proposer: 

                                                - La preuve de la résolution du problème de hashage
                                                - Le hash validé
                                                - La date à laquelle il a été validé
                                                - Les données qu'il utilise

"

##############################################################################################################################################################################
#                                                                                                                                                                            #
#                                       Création du bloc                                                                                                                     #
#                                                                                                                                                                            #
##############################################################################################################################################################################

creation_bloc_nplus1 <- function(dataN, hash_precedent, preuve_precedente, numero_bloc){
  n=numero_bloc
  bloc_nom = paste(paste("bloc_", toString(n)),".txt")
  bloc_nom_précédent = paste(paste("bloc_", toString(n-1)),".txt")
  
  file.create(bloc_nom)
# On ecrit les donnees du bloc 
  
  cat("################################################################################################################################################################################",file=bloc_nom,sep="\n",append=TRUE)
  cat(paste("ceci est bloc n°:",toString(n)),file=bloc_nom,sep="\n",append=TRUE)
  cat("################################################################################################################################################################################",file=bloc_nom,sep="\n",append=TRUE)
  cat(paste("Hash du bloc précédent (NE PAS SUPPRIMER): ",hash_precedent),file=bloc_nom,sep="\n",append=TRUE)   #  c'est le lien entre les blocs de la chaine
  cat("################################################################################################################################################################################",file=bloc_nom,sep="\n",append=TRUE)
  
  cat(databloc, file=bloc_nom,sep="\n",append=TRUE)    # Données du bloc
  
  cat("################################################################################################################################################################################",file=bloc_nom,sep="",append=TRUE)

  bloc= readLines(bloc_nom)
  hash_bloc_brut=digest(bloc,"sha256")
  Preuve_de_travail_rendu= myPreuve_de_travail(hash_bloc_brut, difficulte)
  
  Preuve_de_travail = Preuve_de_travail_rendu[3]
  hash_bloc_final<<-Preuve_de_travail_rendu[2]
  cat("",file=bloc_nom,sep="\n",append=TRUE)
  cat("################           Données annexes              ################",file=bloc_nom,sep="\n",append=TRUE)
  
  cat(paste("le hash brut du bloc est: ",hash_bloc_brut),file=bloc_nom,sep="\n",append=TRUE)                  #  On a travaillé sur ce hash pour obtenir la preuve de travail
  cat(paste("le hash travaillé de ce bloc est: ",hash_bloc_final),file=bloc_nom,sep="\n",append=TRUE)            #  On fournit le nouveau hash, qui sera réutilisé par la suite pour construire la chaine
  cat(paste("la preuve de travail associée est: ", Preuve_de_travail),file=bloc_nom,sep="\n",append=TRUE)              #  On fournit la preuve de travail 
  cat(paste("la date de validation du bloc est le: ",Sys.Date()),file=bloc_nom,sep="\n",append=TRUE)                        #  On marque le bloc de la date du système
  
  }

#Exemple d'utilisation: 
databloc="
Données du bloc n°1
"
creation_bloc_nplus1(databloc, hash_bloc_final,preuve_precedente,1)
databloc="
Données du bloc n°2
"
creation_bloc_nplus1(databloc, hash_bloc_final,preuve_precedente,2)
databloc="
Données du bloc n°3
"
creation_bloc_nplus1(databloc, hash_bloc_final,preuve_precedente,3)


##############################################################################################################################################################################
"


Comment s'assurer de la validité d'un bloc ?
Pour vérifier que le bloc est bon, il suffit de recalculer le hash avec la preuve proposée.
Pour ce faire, on suivrait les étapes décrites ci-dessous: 
  
  1. On recalcul le hash du  bloc considéré avec la preuve de travail :  digest( paste(toString(preuve_de_travail), digest(readLines(bloc_nom),sha256), sha256)). 
                                                                                 Pour cette étape, il faut faire attention de n'utiliser que le contenu du bloc et pas les données annexes (qui doivent pour cette version être supprimées manuellement), point à améliorer.

Il y a alors deux possibilités: 
          - Si le résultat ne commence pas un nombre de zeros correspondant à la difficulté de la chaîne, on peut s'arrếter là: la chaîne est corrompue. 
                                                                                 - Sinon, on peut passer à l'étape suivante.

2. On vérifie que le hash du bloc précédent est bien celui inscrit dans le bloc actuel (calcul puis comparaison) et on recommence l'étape 1 jusqu'à arriver au premier bloc de la chaîne."

## Mise en pratique
#Soit N l'indice du block à tester

testeur_de_chaine <- function(numéro_bloc_testé){
N=numéro_bloc_testé+1
k=0 # 0= chaine ok, 1=chaine corrompue.
liste_de_hash <- vector("list", (N))
num_bloc=1

#Le premier bloc est un peu particulié
bloc_testé_nom = "bloc_ 0 .txt"
bloc_testé = readLines(bloc_testé_nom)
bloc_originel = bloc_testé[1:(length(bloc_testé)-5)]              # On récupère le bloc brut
Preuve_de_travail= substr(sub(".*:", "", bloc_testé[(length(bloc_testé)-1)]),3,50)
hash_brut = digest(bloc_originel,"sha256")                        # On récupère le hash brut du bloc
hash_travaillé = digest(paste(toString(Preuve_de_travail), hash_brut), "sha256")

liste_de_hash[num_bloc] <- list(paste(toString(bloc_testé_nom), hash_travaillé)) # On vérifie que ce résultat est cohérent avec la difficulté choisie.
hash_travaillé = digest(paste(toString(Preuve_de_travail), hash_brut), "sha256")

hash_précédent_réel=substr(sub(".*:", "", bloc_testé[(length(bloc_testé)-2)]),3,75)

if(substr(hash_travaillé,0,difficulte)=="000"){
  print("bloc ok")
}
else{
  liste_de_hash[num_bloc]<-paste("PROBLEME",toString(liste_de_hash[num_bloc]))
  k=1
  print("La chaîne a été corrompue")
}

for(num_bloc in (2:N)){
    bloc_testé_nom = paste(paste("bloc_", toString(num_bloc-1)),".txt")
    bloc_testé = readLines(bloc_testé_nom)
    bloc_originel = bloc_testé[1:(length(bloc_testé)-5)]              # On récupère le bloc brut
    Preuve_de_travail= substr(sub(".*:", "", bloc_testé[(length(bloc_testé)-1)]),3,50)  #On récupère la preuve de travail proposée
    hash_brut = digest(bloc_originel,"sha256")                        # On récupère le hash brut du bloc
    hash_travaillé = digest(paste(toString(Preuve_de_travail), hash_brut), "sha256")  #On recalcul le hash avec la preuve de travail
    
    liste_de_hash[num_bloc] <- list(paste(toString(bloc_testé_nom), hash_travaillé)) # On vérifie que ce résultat est cohérent avec la difficulté choisie.
    hash_travaillé = digest(paste(toString(Preuve_de_travail), hash_brut), "sha256")
    
    hash_précédent_réel=substr(sub(".*:", "", bloc_testé[(length(bloc_testé)-2)]),3,75)
    
    if(num_bloc!=N){bloc_testé_nom = paste(paste("bloc_", toString(num_bloc)),".txt")
    bloc_testé = readLines(bloc_testé_nom)
    hash_précédent_annoncé= substr(sub(".*:", "", bloc_testé[4]),3,75)}

    
    
    if(substr(hash_travaillé,0,difficulte)=="000" && (((hash_précédent_annoncé!=hash_précédent_réel)== FALSE && hash_précédent_réel==hash_travaillé)||num_bloc==N)){
      print("bloc ok")
    }
    else{
      k=1
      print("La chaîne a été corrompue")
      
    }
  }
if(k==0){print("La chaîne est ok")}
return(k)
}
testeur_de_chaine(3)


##############################################################################################################################################################################