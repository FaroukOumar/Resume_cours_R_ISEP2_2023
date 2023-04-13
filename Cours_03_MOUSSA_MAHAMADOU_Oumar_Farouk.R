
##Importation de donnees

library("readxl")
mybase=read.csv("C:/Users/ROSSA/Desktop/R/classe/Programmation avec R/Cours R 2023/Cours_004_Base.csv", sep=";")
View(mybase)

mabase=read.table("C:/Users/ROSSA/Desktop/R/classe/Programmation avec R/Cours R 2023/Cours_004_Base.txt", header=TRUE)
View(mabase)

mydata=read_excel("C:/Users/ROSSA/Desktop/R/classe/Programmation avec R/Cours R 2023/Cours_004_Base.xlsx")

View(mydata)

##Manipulation de donnees

#Selection de variables

age=mybase$Age
sexe=mabase$Sexe


#Filtrage de donnees

indiv_age=mabase[mabase$Age<35,]
hommes=mabase[mabase$Sexe=="M",]


#Agregation de donnees
moyenne_revenu_par_sexe= aggregate(mabase$Revenu, by = list(mabase$Sexe), FUN = mean)
moyenne_revenu_par_sexe


#Fusion de donnees


jeune_et_pauvre= mabase %>%
  filter(Age < 40, Revenu< 100000)
jeune_et_pauvre



##Statistiques descriptives
moyenne_rev=mean(mabase$Revenu)
moyenne_rev

median_rev=median(mabase$Revenu)
median_rev

ecart_type_age=sd(mabase$Age)
ecart_type_age

summary(mabase)



##Manipulation de donnees avec dplyr et tidyr
install.packages("dplyr")
install.packages("tidyr")
library("dplyr")
library("tidyr")

?gather
#donnees_longues <- gather(donnees, variable, valeur, -id_client)



##Visualisation de données avec ggplot2
install.packages("ggplot2")
library("ggplot2")


# Histogramme de l'âge des indiv

(monhist=ggplot(mabase, aes(x = Age)) + 
  geom_histogram())



# Diagramme en barres de la Mention
(mon_dia_barre=ggplot(mabase, aes(x = Mention)) + 
  geom_bar())


# Nuage de points de l'âge par rapport au revenu
(mon_nuagep=ggplot(mabase, aes(x = Revenu, y = Age)) + 
  geom_point())


## Graphique en boîtes de l'âge par rapport à la Mention
(ma_boite=ggplot(mabase, aes(x = Mention, y = Age)) + 
  geom_boxplot())



##Exportation de donnees

write.csv(mybase, "base_manipulee.csv", row.names = FALSE)


write.csv(mabase, "base_manipulee.txt", row.names = FALSE)

write.csv(mybase, "base_manipulee.csv", row.names = FALSE)



##Visualisation de donnees

#Histogramme

hist(mabase$Revenu, main = "Histogramme", xlab = "Valeurs", ylab = "Fréquence")


#Diagramme en boîtes

boxplot(mabase$Age, main = "Diagramme en boîtes des données", ylab = "Valeurs")


#Nuage de points

plot(mabase$Age, mabase$Revenu, main = "Nuage de points", xlab = "Age", ylab = "Revenu")


#Graphique en barres
barplot(mabase$Age, names.arg = mabase$Sexe, main = "Graphique en barres des ages", xlab = "Données", ylab = "Fréquences")

#Graphique en secteurs

pie(mabase$Age, labels = mabase$Sexe, main = "Graphique en secteurs des données")


#Carte thermique

matrice <- matrix(c(2, 5, 3, 6, 4, 7, 5, 8, 6), ncol = 3)
heatmap(matrice, main = "Carte thermique des données")


####Exercices

Base=read.csv2("C:/Users/ROSSA/Desktop/R/classe/Programmation avec R/Cours R 2023/Cours_005_CM.csv")
Base
View(Base)
str(Base)

classe_age=Base$Age
sous_base_femme=Base[Base$Sexe=="F",]
sous_base_femme
summary(Base$Age)
Base$Cage=ifelse(Base$Age<35,1, ifelse(Base$Age>=35 & Base$Age<50,2,ifelse(Base$Age>=50,3,0)))


barplot(Base$Age, names.arg = Base$Sexe, main = "Graphique en barres des ages", xlab = "Données", ylab = "Fréquences")

(boite=ggplot(Base, aes(x = Sexe, y = Age)) + 
    geom_boxplot())



########################################Resume cours_04
##Booleen
(x=TRUE)
(class(x))
(y=F)
print(class(y))

#comparaison
#renvoie True or false

# >, <, ==, !=,>=, <=


##Operateurs logiques

#& =et renvoie True ssi les 2 contions sont vraies
# |= ou renvoie True si au moins une des contions est vraie
# ! :  renvoie l'opposé d'un elt


# on peut ultiliser ! avec des fonctions
x= 3 + 0i
print(!is.numeric(x)) #=TRUE
print(is.numeric(x)) #=FALSE

#il est possible de combiner les operateurs

x="Oumar"

print(!is.character(x) & (x=="oumar" | x=="Oumar")) #= FALSE
        #FALSE             FALSE         TRUE

## Condition if___else

#cette instruction permet d'excuter les codes suivant certaines conditions

#if seul
a=23
if (a > 0) {
  print("positif")
}

#if_else
a=-1
if (a > 0) {
  print("positif")
}else{
  print("negatif")
}
#="negatif"


#instructions break et next
#break est utilisé pour mettre fin à une boucle
#next sauter certaines iteration et passer à une autre

# plusieurs if__else
#il s'agit de mettre plusieurs contitions à la fois

m=0

if (m > 0) {
  print("positif")
} else if (m < 0) {
  print("negatif")
} else {
  print("nul")
}
#="nul"



#else_if imbriqués
mon_nbr=-3
if (mon_nbr > 0) {
  
  if (mon_nbr %% 2 == 0) {
    print("mon_nbr est pair et positif")
  } else {
    print("mon_nbrnest impair et positif")
  }
} else {
  
  if (mon_nbr %% 2 == 0) {
    print("mon_nbr est pair et negatif")
  } else {
    print("mon_nbr est impair et negatif")
  }
}
###Fonction ifelse
#renvoie une valeur suivant qu'une condition soit vraie et une autre si la condition est fausse

#ifelse(test_expression, x, y)

p_im=c(112, 9, 73, 14, 0)
ifelse(p_im %% 2 == 0, "pair", "impair")
#="pair"   "impair" "impair" "pair"   "pair"
#lorsque la condition conserne un vecteur, la comparaison est faites avec tous 
#les elts du vecteur

#cherchons les nombres positifs d'un vecteur
v1=c(-2,1,6,9,-6,0,-17)
ifelse(v1 < 0, "FAUX", "VRAI")
#="FAUX" "VRAI" "VRAI" "VRAI" "FAUX" "VRAI" "FAUX"


###Boucle while

#permet de repeter une action tant qu'une condition est verifiée

##syntaxe
while (test_expression) {
  # instructions
}

#si le resultat du test est vrai, les instructions sont executees sinon ça passe


nbr = 1
i = 0
while(nbr <= 7) {
  
  i = i + nbr
  nbr = nbr + 1
}

print(i) #somme des 7 premiers nombres


#Il est possible de combiner break et while

n = 1
while(n <= 10) {
  print(n*2)
  n = n + 1 
  if (n == 5) {
    break
  }  
}

#on affiche le double de n tant que n <= 10 et 
#on arrete quand n=5
# le double de 5 n'est pas pris en compte

#On peut faire pareil avec l'instruction next

i = 1
while(i <= 10) {
  
  if (i %% 2 == 0) {  
    i = i + 1
   next
  }
  print(i)
  i = i + 1  
}


#quand i est pair on incremente à 1 et passe au block suivant
#ici on affiche les chiffres pairs inferieurs à 10



### Boucle for
#permet de executer des blocks avec un nbre d'iteration connu

#afficher le carré de tous les elts d'un vecteur par exple
m = c(1, 2, 3, 4, 5,6)
for (x in m) {
  print(x^2)
}

#compter le nombre de chiffres impairs dans un vecteur
num = c(2, 3, 1, 6, 5, 7)
compteur = 0
for (i in num) {
  if (i %% 2 != 0) {
    compteur = compteur + 1
  }
}
print(compteur) #=4

#Suivant une condition, on peut egalement utiliser break (ou next) pour mettre fin à la boucle(sauter une iteration)


### For imbriquée

#table de multiplication de 1 2 et 3
t1=c(1,2,3)
t2=1:10

for(i in t1){
  for(j in t2){
    print(i*j)
  }
}


####instruction break et next
(vecto=1:11)
for (i in vecto) {
  if(i==8) {
    break
  }
  print(i*2)
}
#le chiffre 8 dans le test n'est pas pris en compte
#on donne le double des chiffre ve vecto strictement inferieurs à 8


#instruction break dans les boucles imbriquées

(a=c(1,2,4))
(b=c(5,6,7))

for(i in a){
  for(j in b){
    if(i==4 & j==6){
      break
    }
    print( paste(i,"-",j))
  }
}


##instruction next

(y=1:14)
for(i in y){
  if(i%%2==1){
    next
  }
  print(i)
}


##Boucle repeter
#on utile cette boucle pour effectuer plusieurs boucles
#ici il est necessaire d'utilisser une une instruction break
#pour arreter la boucle


expert="Farouk"
i=1
repeat {
  print(expert)
  
  if(i==3){
    break
  }
  i=i+1
}

#il s'agit ici de repeter Farouk 3 fois


#warning: si on oublie de mettre l'instrution break
#le bolck de code peut etre executer à l'infini

########defense d'executer
#x = 1
#sum = 0
#repeat {
  
 # sum = sum + x
 # print(sum)
 # x = x + 1
#}


#Boucle repeter et l'instruction next

x = 1
repeat {
  
  if ( x == 4) {
    break
  } 
  
  if ( x == 2 ) {
    x = x + 1
    next
  }
  print(x)
  x = x + 1
  
}


#ici on arrete l'execution lorsque x=4 et sauter l'iteration lorsque x=2



###Fonction R
#c'est un block de code qu'on peut utiliser partout dans le script
#ça permet d'eviter les repetitions de fonction


mon_min=function(a,b){
  ifelse(a<b, print(a),print(b))
}

# appel de la fonction

mon_min(4,3)   #[1] 3
               #[1] 3

#si on veut changer l'ordre des parametres on le precise lors de l'appel de la fonction
#exple

#mon_min(b=4, a=3)

#OU

#mon_min(b=4, 3)


#On peut aussi mettre des parametres constant dans la fonction

demi_racine=function(a=2, b){
  print(sqrt(b)/a)
}

demi_racine(2, 9) #=1.5

#il faut, ce pendant, specifier le parametre non constant
demi_racine(b=4) #=1

#Au lieu de print on peut retourner la valeur de la fonction

demi_racine2=function(a=2, b){
  return(sqrt(b)/a)
}

demi_racine2(b=9) #=1.5

##Fonction imbriquée

#on peut imbriquer des fonctions de differentes manieres

#en appellant dans une autre fonction ou
#en l'ecrivant dans la fonction

even=function(c=2,d){
  return(c*d)
}

print(even(d=4)) #=8



demi_rac=function(a){
  
  prod=function(b){
    return(a*b)
  }
  return(prod)
}
result=demi_rac(3)
print(result(3)) #=9



######CHAINES DE CARACTERES SUR R

#les caracteres sont specifiés avec les cotes "" ou ''

let="on produira à un niveau d'elite"
print(let)

#Plusieurs manipulation peuvent etre faites avec les strings
#determiner la taille du string
#concatener 2 strings
#les comparer
#ou changer la casse


##taille
#nchar() permet le nbre de caracteres du string
nchar(let) #=31

##concatenation
#paste() ou paste0() pour concatener
me='Farouk va produire'
mee="à un niveau d'elite"
paste(me,mee) #="Farouk va produire à un niveau d'elite"

##Comparaison

m1="R genius"
m2="R expert"
print(m1==m2) #=FALSE


##Changement de casse
toupper() # convertir en majuscule
tolower() #convertir en muniscule

toupper(m1) #"R GENIUS"
tolower("MALDIVE") #="maldive"

#NB: plusieurs autres manip sont possibles

#si par exple on veut mettre des cotes sur mot dans un string

#example1 <- "This is "R" programming" ici on ne peut pas mettre 2 cotes à la fois

(ex1 <- "This is \"R\" programming")
#="This is \"R\" programming"

cat(ex1) # pour masquer les antislash
#=This is "R" programming

#on utilisera alor
# \b : pour l'espace un mot
# \\: 
#\t :
#\n :
#\"


#Definir le string sur plusieurs lignes
(essai <- "R is awesome
It is a powerful language
R can be used in data science")
#="R is awesome\nIt is a powerful language\nR can be used in data science"

#pour garder ça sur plusieurs lignes on utilise cat()
cat(essai)
#=R is awesome
# It is a powerful language
# R can be used in data science



##################################################
####VECTEUR R
#c'est une suite d'elt de meme type
#ça peut etre des strings,des chiffres,des booleens,etc.

#on utilise la fonction c() pour creer un vecteur

presidents=c("Issoufou","Macki","Ouatara")
presidents #="Issoufou"  "Macki"   "Ouatara"


#pour acceder à un elt de la base on utilise []

#pour le 2eme elt du vecteur presidents
print(presidents[2]) #="Macki"


#il est possible de modifier un elt vecteur ou d'en ajouter
presidents=c("Issoufou","Macki","Ouatara")
titre='SE'
cat("les presidents en 2020 sont:", paste(titre,presidents))
#=les presidents en 2020 sont: SE Issoufou SE Macki SE Ouatara

presidents[1]="Bazoum"
cat("les anciens presidents :", paste(titre,presidents))
#=les anciens presidents : SE Bazoum SE Macki SE Ouatara

##Boucle sur un vecteur
for(i in 1:7){
  print(i)
} # afficher elts un à un

#La fonction length nous permet d'avoir nbre d'elts d'un vecteur

cat("Nos",length(presidents),"presidents sont:",paste(titre,presidents))
#=Nos 3 presidents sont: SE Bazoum SE Macki SE Ouatara

###################################################################################
### MATRICES DANS R

#creation
#on utilise la fonction matrix()
#matrix(vector, nrow, ncol, byrow)

#nrow= nbre de lignes de la matrice
#ncol= nbre de colonnes de la matrice
#byrow : pour specifier l'ordre de remplissage (true or false)

(matrix1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE))

##Acces aux elts d'une matrice

#matrix[n1, n2] : elt de ligne n1 et colonne n2

matrix1 <- matrix(c("Issoufou","Macki","Ouatara","Paul"), nrow = 2, ncol = 2)

print(matrix1)
print(matrix1[1, 2]) #="Ouatara"

#Pour avoir acces à une ligne ou une colonne
#on utilise
# [n, ] ligne
# [ ,n] colonne

print(matrix1[1, ])
#="Issoufou" "Ouatara" 

print(matrix1[, 2])
#="Ouatara" "Paul" 


#Pour avoir acces à plusieurs lignes ou colonnes

(mat=matrix(c(10, 20, 30, 40, 50, 60,11,23,0), nrow = 3, ncol = 3))
cat("lere et 3eme lignes:", mat[c(1,3), ])

##Modifier les elts d'une matrice
mat[1,1]=111 #remplacer l'elt de ligne 1 col 1 par 111
mat


##Combinaison de deux matrices

#on utilise pour ce faire
# cbind() pour colonne
#rbind() pour ligne


(pa=matrix(c(2, 4, 6, 8), nrow = 2, ncol = 2))
(im=matrix(c(1, 3, 5, 7), nrow = 2, ncol = 2))

total1 <- cbind(pa,im)
print(total1)

total2 <- rbind(pa, im)
print(total2)


#Verifier si un elt existe dans une matrice
#on utilise
#%in%
#renvoie TRUE si l'elt existe
print(matrix1)
"Paul" %in% matrix1 #=TRUE
"Sankara" %in% matrix1 #=FALSE

####################################################
## LISTES DANS R

#Une liste est une collection d'elts de plusieurs types

#pour la creer on utilise
#liste()

(lst1=list(12,23,1+1i)) #liste d'entiers et de complex

(lst2=list(12,1+1i,"R","pyton",T)) #liste d'elts de type differents


###Acces à un elt de la liste
the_second=lst2[2]
print(the_second)#=1+1i il s'agit du 2eme elt de la liste

#il est à noter que le premier elt est indexé par 1


###Modifier un elt de la liste

lst2[2]='complex'

print(lst2)


### Ajouter un elt dans une liste

#pour ce faire on utilise la fonction append()

print(lst2)

append(lst2,"Java") # ajout de Java dans la liste

###Suppresion d'elt de la liste
print(lst2[-5]) # on enleve le 5eme elt de la liste

lst3=lst2[-5] #creation de nouvelle liste ne contenant pas le 5eme elt
print(lst3)

###Longueur d'une liste
#pour connaitre la longueur d'une liste on utilise length()

length(lst2) #=5 : lst2 contient 5 elts
length(lst3) #=4 : lst3 contient 4 elts


#comme pour les vecteurs, on peut appliquer une boucle sur une ligne

for(i in lst3){
  print(i)
}

# [1] 12
# [1] "complex"
# [1] "R"
# [1] "pyton"

#comme pour les matrices, il est possible verifier si un elt existe dans une liste ou pas

"Jamovi" %in% lst2 #FALSE: Jamovi n'est pas dans liste
"R" %in% lst2 #=TRUE : R est present dans la liste



#############################################################################################
#### LES TABLEAUX DANS R

#c'est une structure de données dans info de meme type peuvent etre stockées sur plusieurs dimensions

#la difference entre les vecteurs, les matrices et les tableaux se situe au niveau des dimensions
#les 1er ont une dimension, les sseconds 2 et les tableaux plus de 2


#pour creer un tableau on utilse la fonction Array()

#Syntaxe
#array(vector, dim = c(nrow, ncol, nmat))

#vector : les données monotypes à stocker
#nrow : nombre le lignes
#ncol : nombre de colonnes
#nmat : nombre de matrice de (nrow*ncol) dimension

tab= array(c(1:12), dim = c(2,3,2)) #tableau à 2 matrices ayant 2 lignes et 3 col
tab


##Acces à un elt du tableau
#syntaxe

#array[n1, n2, mat_level]
#n1 : ligne
#n2 : colonne
#mat_level : la matrice concernée
print(tab)
print(tab[1, 3, 2]) #=11 :elt de la 2eme matrice, à la 1ere ligne et 3eme col



#Il est egalement possible d'extraire une ligne ou une colonne dans une matrice du tableau
#on utilise

# [c(n), ,mat_level] pour avoir une ligne
# [ ,c(n), mat_level] pour avoir une colonne

print(tab)
print(tab[c(1), , 2])#=7  9 11 : ligne 1 de la matrice 2

print(tab[, c(3), 1]) #=5 6 : colonne 3 de la matrice 1

###Verification de l'existence d'un elt dans un tableau

8 %in% tab #=TRUE : 8 est dzns le tableau


###Nombre d'elts dans un tableau
print(length(tab))#=12 : le tableau contient 12 elts



###############################################################################################
###DATAFRAME DANS R ou jeu de données

#pour creer un dataframe on utilise la fonction data.frame()

#Synstaxe
# dataframe1 <- data.frame(
#   first_col  = c(val1, val2, ...),
#   second_col = c(val1, val2, ...),
#    ...)

mon_jeu= data.frame (
  Name = c("FALL", "RAZA", "JULIEN","CREPIN"),
  Age = c(22, 30, 17,16),
  Vote = c(TRUE,TRUE, FALSE, TRUE),
  Ville=c("Conacry","Fatick","Dosso","Diffa"))

print(mon_jeu)

#on a des variables en colonnes avec leurs modalités en lignes



###Acces aux elts d'un jeu de données

#Pour ce faire on utilse: [ ],[[ ]] ou $
# [ ] : pour avoir une colonne avec la varible
# [[ ]] ou $ : pour la liste des modalités d'une colonne
  
print(mon_jeu[4]) #colonne 4 avec le nom de la var

print(mon_jeu[["Age"]]) #(1) on affiche la liste des modalites en specifiant le nom de la var 

print(mon_jeu$Age) # fait meme chose que (1)


##Combinaison de 2 dataframes

#on le fait avec les fonctions rbind() et cbind()

data1=data.frame (
  Nom = c("Juan", "Alcaraz"),
  Age = c(22, 15))

data2 =data.frame (
  Nom = c("Yiruma", "Bach"),
  Age = c(46, 89))

#combinaison en ligne
# il faut que les 2 matrices aient le nbre de var
DATA=rbind(data1, data2)
print(DATA)


####
dta1=data.frame (
  Nom = c("Juan", "Alcaraz"),
  Age = c(22, 15))

dta2=data.frame (
  Loisir = c("Tennis", "Piano"))

#combinaison en colonne
# il faut que les 2 matrices aient le nbre de ligne
DTA=cbind(dta1,dta2)
print(DTA)


#la fonction length() nous permet d'avoir le nbre de colonne d'un jeu de données

print(length(dta1)) #=2
print(length(dta2))#=1
print(length(DTA)) #=3




##############################################################################################

# LES FACTEURS DANS R
# C'est un objet contenant des données categorielles
#il s'agit exemple de la situation matrimoniale

##creation de facteur avec la fonction factor()
#synataxe
#factor() : il prend vecteurs comme arguments

sexe=factor(c("masculin", "feminin", "masculin", "feminin", "feminin","masculin"))
print(sexe)

#=masculin feminin  masculin feminin  feminin  masculin 
#=Levels: feminin masculin


##Acces aux elts d'un facteur
print(sexe)
print(sexe[5]) #=feminin : 5eme elt du facteur


## Modifier un elt d'un facteur
sit_mat=factor(c("marie", "celibataire", "celibataire", "divorce", "marie"))

sit_mat[3]="marie"

print(sit_mat[3]) #=marie

#######################################################################################
#######################GRAPHIQUES
#######################################################################################

##Diagramme en barre

##on utilise la fonction barplot(), pour ce faire

temp=c(22, 27, 26, 24, 23, 26, 28)

barre=barplot(temp) # diagramme en barre temp#

#On peut ajouter au graphique on utilise d'autres elts comme:
#un titre avec: main
#etiquette en abscisse avec: xlab
#etiquette en ordonnée avec: ylab

barre=barplot(temp,main="Temperatures max dans la semaine",xlab="Degré celsius",ylab="jours")

#pour nommer les barres on utilise names.arg

bar=barplot(temp,main="Temperatures max dans la semaine",xlab="Degré celsius",ylab="jours",  names.arg = c("Lun", "Mar", "Mer", "Jeu", "Ven", "Sem", "Dim"))


#on peut mettre un couleur aux barres avec "col"

bar=barplot(temp,main="Temperatures max dans la semaine",xlab="Degré celsius",ylab="jours", 
            names.arg = c("Lun", "Mar", "Mer", "Jeu", "Ven", "Sem", "Dim"),
            col="skyblue")

#Il est possible de changer la texture des barres avec "density"

bar=barplot(temp,main="Temperatures max dans la semaine",
            xlab="Degré celsius",
            ylab="jours",
            names.arg = c("Lun", "Mar", "Mer", "Jeu", "Ven", "Sem", "Dim"),
            col="skyblue",
            density=30) # graphique avec une autre texture

#Pour avoir ce graphique de maniere horizontale on utilise "horiz"

ba=barplot(temp,main="Temperatures max dans la semaine",
            xlab="Degré celsius",
            ylab="jours",
            names.arg = c("Lun", "Mar", "Mer", "Jeu", "Ven", "Sem", "Dim"),
            col="skyblue",
            density=30,
            horiz=TRUE)


##Stacked Bar Plot in R
#On peut creer un graphique....avec R


titanic=matrix(c(122, 203, 167, 118, 528, 178, 673, 212),
                        nrow = 2, ncol = 4)

grap=barplot(titanic,
                  main = "Survivant de chaque classe sociale ",
                  xlab = "Classe sociale",
                  names.arg = c("1er", "2eme", "3eme", "Groupe"),
                  col = c("yellow","green")) # notre diagramme empilé

#R donne la possibilité d'ajouter une legende sur les graphiques
legend("topleft",
       c("Decedés","Survivant"),
       fill = c("yellow","green"))
#"topleft" pour mettre la legende en haut à gauche du graphique



###########################################################################################""
#HISTOGRAMME

#On utilise les histogrammes pour representer de var continues 

#On cree un histogramme avec la fonction hist()
temp
myhist=hist(temp)

#pour ajouter de
#etiquette on utilise xlab
#titre: main
#couleur: col
myhist=hist(temp,
            main = "Histogramme de Temperature",
            xlab="temperature en degre kelvin",
            col="green")


#On peut faire varier les echelles en abscisse et en ordonnée avec xlim et ylim

myhist=hist(temp,
            main = "Histogramme de Temperature",
            xlab="temperature en degre kelvin",
            col="green",
            xlim=c(20,30),
            ylim=c(0,5))

####################################################################################################
# DIAGRAMME CIRCULAIRE

#La creation d'un diagramme circulaire se fait avec la fonction pie()

for_pi=c(600, 300, 150, 100, 200)
circul=pie(for_pi)

#Il est possible d'ajouter un titre, des labels, des couleurs aux differentes portions du diagramme

#Titre : main
#labelle : labels
#couleur : col

circul2=pie(for_pi,
            main='Expedition',
            labels=c("Nourriture","habille","Voiture","Electro","Autres"),
            col = c("red", "orange", "yellow", "blue", "green"))

#Il existe une autre forme de diagramme circulaire en 3D

#On charge d'abord le paquet qui contient la fonction

install.packages("plotrix")
library(plotrix)

circu3D=pie3D(for_pi,
            main='Expedition',
            labels=c("Nourriture","habille","Voiture","Electro","Autres"),
            col = c("red", "orange", "yellow", "blue", "green"))


#########################################################################################

#BOITE A MOUSTACHE

#C'est un graphique qui represente la dispersion des valeurs

Base=read.csv2("C:/Users/ROSSA/Desktop/R/classe/Programmation avec R/Cours R 2023/Cours_005_CM.csv")
View(Base)

#Creation de la boite

boxplot(Base$Age) # boite à moustache de l'age

#On peut ajouter un titre, les etiquettes, de la couleur au boxplot

boxplot(Base$Age,
        main="Boxplot de l'age",
        ylab="Age des individus",
        col="orange")


#On peut faire le boxplot de 2 variables croisees

boxplot( Base$Age ~ Base$Sexe,
        main = "Boxplot de l'age et du sexe",
        ylab = "Age",
        xlab = "Sexe",
        col = "orange")


#On peut changer la forme de la boite
#On utilise notch

boxplot( Base$Age ~ Base$Sexe,
         main = "Boxplot de l'age et du sexe",
         ylab = "Age",
         xlab = "Sexe",
         col = "blue",
         notch=TRUE)

#############################################################################

##Graphique en bande
#il permet d'affiche des données numériques sur une seule bande

install.packages("questionr")
library("questionr")

Base=read.csv2("C:/Users/ROSSA/Desktop/R/classe/Programmation avec R/Cours R 2023/Cours_005_CM.csv")
View(Base)

#La fonction qui permet d'avoir ce graphique est stripchart()

stripchart(Base$Age) #=il s'agit du graphique en bande de l'age

##Ajout un titre, une étiquette, une couleur au graphique

stripchart(Base$Age,
           main="Age des individus",
           xlab="Age en année",
           col="blue")

#on peut faire en sorte que les carrés soient decales les uns des autres
#On utilise le parametre jitter

stripchart(Base$Age,
           main="Age des individus",
           xlab="Age en année",
           col="blue",
           method="jitter")

#Il est possible de creer le graphique en bande de plusieurs variable
Base$Note=c(12,14,15,16,17,19,18,20,15,19,16,16,11,15,16,14,15,17,16)
View(Base)

mylist=list(Base$Age,Base$Note) 

stripchart(mylist,
           main="Age et note des individus",
           xlab="Abscisse",
           col=c("blue","orange"),
           method="jitter")

#Il s'agit du graphiques  en bande des variables Age et Note 


#########################################################################

## Fonction plot()

#Cette fonction permet de tracer le nuage de point de deux varaibles avec la fonction plot()


x=c(2, 4, 6, 8) 
y=c(1, 3, 5, 7)
plot(x, y)  # nuage de point entre x et y

#On peut tracer le nuage d'une sequence de point

plot(2:8) # on a la sequence 2:8 en ordonnée


#On peut changer le type de notre courbe avec l'argument "type"

plot(2:8, type='l') # on a une ligne à ce niveau

#Pour les autres types de variables on utilise

#"p" les points
#"l" ligne
#"b" points et traits combinés
#"s" tracé en escaliers

plot(2:8, type='s')

#"n" aucun graphe
#"h" Tracé de type histogramme


#On peut ajouter un titre et des etiquettes

plot(2:8, type='s',
     main="Graphique en scalier", 
     xlab="Abscisse", 
     ylab="Ordonnée") #Graphique en escalier titré


#Tracer de fonction trigonométrique dans R

x = seq(-pi,pi,0.1) #Abscisse de -pi à pi avec un pas de 0,1
y = sin(x) # fonction sinus

plot(x,y) #Fonction sinus sur -pi pi


######################################################################################
##Sauvegarde des graphiques

#Il s'agit de savoir sauvegarder les graphiques produits

#On peut les sauvegarder comme image en jpeg, png

library(plotrix)
setwd("C:/Users/ROSSA/Desktop/R/classe/Programmation avec R/Cours R 2023/Cours_03_Moussa_Mahamadou_Oumar_Farouk")
getwd()

#enregistrer le graphique au format jpeg dans le répertoire courant
x = seq(-pi,pi,0.1)
y = sin(x)
sinus=plot(x,y)

jpeg(file="sinus.jpeg")#Enregistrement du graphique sinus format jpeg
plot(x,y)
dev.off() #cette fonction permet sauvegarder le graphique


png(file="sinus.png",
    width=600, height=350) #enregistrer en format png
plot(x,y)
dev.off()

#On peut aussi enregistrer notre graphique au format pdf ou postscript
#On utilise respectivement les fonctions pdg() et postscript()

#Enregistrement au format pdf
pdf(file="sinus.pdf")
plot(x,y)
dev.off()


##################################################################
##Les Couleurs en R

#Pour ajouter une couleur au graphique, on utilise le parametre "col"


Base=read.csv2("C:/Users/ROSSA/Desktop/R/classe/Programmation avec R/Cours R 2023/Cours_005_CM.csv")
View(Base)
hist(Base$Age, main="Histogramme de l'age", col="cadetblue")

#pour avoir les couleurs

colors() # couleurs disponibles sur R


#Au lieu de donner le nom de la couleur on peut utiliser les codes hexadecimales

#comme RR pour le red, GG pour le vert, BB pour le bleu ...

hist(Base$Age, main="Histogramme de l'age", col="#c00000") #couleur Rouge sombre
hist(Base$Age, main="Histogramme de l'age", col="#AE4371") #couleur violet

##On peut Utiliser des valeurs RVB pour les couleurs
#On utilse la fonction rgb()

rgb(0, 1, 0) #Afficher "#00FF00"
hist(Base$Age, main="Histogramme de l'age", col="#00FF00") #couleur violet
hist(Base$Age, main="Histogramme de l'age", col=rgb(0, 1, 0)) #couleur violet

#Pour un diagramme en barre par exemple, il est possible de mettre une couleur differente pour chaque barre
temp=c(12,13,3,9,6,11,4,7)
barplot(temp, main="Barplot", col=c("sienna", "coral", "snow4", "turquoise", "pink", "wheat","slateblue","seashell"))


##Utilisation de la palette de couleurs dans R
#R propose 4 palettes de couleurs qui peuvent être utilisées pour générer 
#des vecteurs de couleur de la longueur souhaitée

#On utilise
#rainbow(), heat.colors(), terrain.colors(),topo.colors()

#pour générer une palette 5 couleurs avec rainbow() par exemple

rainbow(4) #="#FF0000" "#80FF00" "#00FFFF" "#8000FF"

barplot(temp, main="Barplot", col=rainbow(8))
barplot(temp, main="Barplot", col=heat.colors(5))
barplot(temp, main="Barplot", col=terrain.colors(5))
barplot(temp, main="Barplot", col=topo.colors(5))



###############################################################################
##### Manipulation de données avec R

##Importation et exportation de données sous format csv

#On va utiliser la base cours_004_Base


df=read.csv("Cours_004_Base.csv", sep=";") #import de la base avec le separateur ;
print(df)

#Au cas ou la base n'est pas dans le repertoire courant, on peut specifier le chemin ou se trouve la base



##Nombre de colonnes et de lignes du fichier CSV
#On utilise ncol() pour les colonnes et nrow() pour les lignes

cat("Nombre de colonnes est: ", ncol(df))
#=Nombre de colonnes est:  9

cat("Nombre de lignes est: ", nrow(df))
#=Nombre de lignes est:  19


#On peut chercher le min et le max d'une variable
#On utilise les fonctions min() et max()
(le_min=min(df$Age)) #=15

(le_max=max(df$Age)) #=88

#On peut extraire un sous ensemble de la base suivant des conditions avec la fonction subset()

ext_df=subset(df, df$Age>44) #individus ayant un age sup à 44
View(ext_df)



###Export d'une base
#On utilise la fonction write.csv()

write.csv(ext_df, "Age_sup.csv") #export de la base extraite sous le nom Age_sup
#Avec cette methode, les observations sont mises entre cotes

#On peut choisir de les enlever avec le parametre quote=FALSE
write.csv(ext_df, "Age_sup2.csv", quote = FALSE)




###Importation et exportation de données sous format xlsx

#Il est possible d'importer des fichiers Excel, on va impoter Cours_004_Base

#Il est necessaire d'installer un package (xlsx)

install.packages("xlsx")
install.packages("readxl")
library("readxl")

dta=read_xlsx("Cours_004_Base.xlsx", sheetIndex = 1)

View(dta)

#sheetIndex = 1 lire la feuille de travail spécifiée, c'est-à-dire 1

#On peut utiliser la fonction read.xlsx2() si l'ensemble de données sur lequel nous travaillons est plus grand


#On peut lire une plage spécifique de données 

data=read_xlsx("Cours_004_Base.xlsx", 
                       sheetIndex = 1,
                       rowIndex = 1:5)

#rowIndex lire une plage spécifique de lignes

#colIndex lire une plage spécifique de colonnes



#le fichier Excel peut contenir des en-têtes au début que nous ne souhaitons peut-être pas inclure
#On peut specifier la ligne à partir de laquelle les données seront prises
#Pour ce faire on utilise l'argument startRow

data=read_xlsx("Cours_004_Base.xlsx", 
               sheetIndex = 1,
               startRow=3)


#Export des données xlsx avec la fonction write.xlsx()
write.xlsx(data, "fichier_test.xlsx")


#On peut aussi exporter en renommant la feuille courante excel avec le parametre sheetName

write.xlsx(data, "fichier_test.xlsx", sheetName="Test")


########################################################################################################
##base de données R
#Il s'agit d'une colection de données presentées sous forme de tableau

#Il existe de nombreuses bases de données utilisable dans R

#comme par exemple airquality,AirPassengers, iris etc.

#Utilisons airquality dans la suite de notre travail

data("airquality") # chargement du dataset airquality
View(airquality)
bs=airquality

##Interroger la base de données voir ses dimensions...

dim(bs) #dimension de la base bs : 153 lignes et 6 colonnes
nrow(bs) #nbre de lignes: 153
ncol(bs) #nbre de colonnes : 6

names(bs) #Nom des varaibles
#="Ozone"   "Solar.R" "Wind"    "Temp"    "Month"   "Day"


#On peut avoir acces à une variable en utilisant le symbole $

print(bs$Ozone) # voir les obs de la variable Ozone de la base bs


#Un tri peut egalement etre fait sur une variable avec la fonction sort()
#l'ordre par defaut est croissant

sort(bs$Temp)
sort(bs$Temp,decreasing = TRUE) # ordre decroissant

##statistiques sommaires des données dans R
#pour avoir ces stat on utilise la fonction summary()
#celle-ci retourne le min, le max, la moyenne, les 3 quartiles


summary(bs$Temp) #statistiques sommaires de la var Temp

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max
#56.00   72.00   79.00   77.88   85.00   97.00


##################################################################

#Fonction min() et max()
#nous pouvons trouver la valeur minimale ou maximale d'un vecteur ou d'une trame de données


nbr=c(2,4,6,8,10)
min(nbr)  #=2
max(nbr) #=10

carac=c("s", "a", "p", "b")
min(carac)  #= "a" est le caractere qui vient le premier suivant l'alphabet dans le vecteur carac
max(carac) #= "s" est le caractere qui vient en derniere position suivant l'alphabet dans le vecteur carac


#Dans le cas ou le vecteur contient des valeurs manquantes, les fonctions min et max renvoient NA

vect=c(11, 1987, 6, 7, NA, 10)
min(vect) #=NA
max(vect) #=NA

#On peut alors chercher le min ou max des valeurs qui ne manquent pas
#Pour cela, on utilise l'argument na.rm( NA remove) pour mettre les NA

min(vect, na.rm = TRUE) #=6
max(vect, na.rm = TRUE) #=1987

##min() et max() dans un Data Frame

(temp_minimale=min(bs$Temp, na.rm = TRUE)) #=56 : temperature min de de 56

(Ozone_maximale=max(bs$Ozone, na.rm = TRUE)) #=168 : quantité d'ozone max est de 168



################################################################################################"
# La Moyenne, la mediane et le mode dans R
#Il s'agit d'indicateurs de tendance centrale, ils peuvent calculer sur des vecteurs ou des dataframes


#Calcul de la moyenne d'age
Base

(moy_age=mean(Base$Age)) #=45.63158 est la moyenne d'age 

#Calcul de l'age median
median(Base$Age) #=38 est l'age median

#calcul du mode
#il n'y a pas de fonction intégrée pour calculer le mode
#il faut créer une fonction qui determine le mode


mode = function() {
  return(names(sort(-table(Base$Age)))[1])
}
mode() #=23: le mode est de 23

#la fonction table() pour créer une représentation catégorique des données avec
#les noms de variables et la fréquence sous la forme d'un tableau.
#on trie les ages par ordre décroissant et on renvient la 1ère valeur parmi les valeurs triées

#######################################################################################################################################"

##Les centiles dans R

#Il s'agit d'une donnee qui indique la valeur en dessous de laquelle
#se situe un pourcentage de données

#Pour ce faire on utilise la fonction quantile()

perc=quantile(Base$Age, 0.70)
perc #= 56.8 : 70% des individus ont moins de 56,8 ans et 30% en ont plus

#On peut calculer plusieurs centiles en meme temps

multi_perc=quantile(Base$Age, c(0.5,0.70,0.9))
multi_perc  #=50%  70%  90%
            #=38.0 56.8 79.0




###################################################################################################
#Manipulation des dates avec R

#Il peut arriver qu'on traite des données contenant des dates et des heures.
#Il existe diverses fonctions dans R pour gerer les dates


##la date et l'heure actuelles du système dans R
#Pour cela on utilise les fonctions Sys.Date()et Sys.time()

Sys.Date() # pour la date du jour = "2023-04-13"
Sys.time() # pour date du jour et l'heure actuelle="2023-04-13 18:46:54 UTC"

#Utilisation du package lubridate
#celui-ci rend l'extraction et la manipulation de certaines parties de la valeur de la date plus efficaces

library(lubridate)

now() # pour avoir  l'heure et la date actuelles: "2023-04-13 18:50:33 UTC"

##On peut extraire le jour, le mois ou l'année d'une variable date

dates= c("2023-01-23", "2001-09-21", "2004-03-08")

year(dates) # extraction des années: 2023 2001 2004
month(dates) # extraction des mois: 1 9 3
mday(dates) # extraction des jours: 23 21  8

#Autres manipulations

print(dates + years(1)) # ajout d'un an aux années
print(dates + months(1)) # ajout d'un mois aux mois

mday(dates)= c(2, 9, 4) #ajout de 2, 9 et 4jours pour les 3 dates
print(dates) 


##Pour mettre à jour plusieurs dates à la fois on utilise la fonction update()
dates= c("2023-01-23", "2001-09-21", "2004-03-08")
a_j_dates=update(dates, year = c(2023, 2002, 2019), month = c(9, 12, 1), day = c(21, 2, 13))

########################################################################################################3

##Objet et classe sur R

#R est un langage fonctionnel qui utilise des concepts d'objets et de classes
#Un objet est une collection de données (variables) et de méthodes (fonctions)

##Système de classe dans R
#R a3 systemes de classe: S3 class, S4 class et reference class

##S3 Class
#La plupart des classes prédéfinies dans R sont de ce type

elev1=list(nom = "Inoussa", age = 21, GPA = 3.5)
class(elev1)="info_eleve" #creation de la class info_eleve
elev1



##S4 Class
#c'est une amélioration par rapport à la classe S3
#On utilise la fonction setClass()


setClass("info_etudiant", slots=list(nom="character", age="numeric", GPA="numeric"))
etudiant1=new("info_etudiant", nom = "IVANA", age = 21, GPA = 3.5)
etudiant1



##Reference Class in R
#il s'agit un peu de la programmation orientée objet
#Pour definir cette classe, on utilise la fonction setRefClass()


eleve_Info=setRefClass("info_eleve",
                            fields = list(nom = "character", age = "numeric", GPA = "numeric"))
stu=eleve_Info(nom = "Sarah", age = 21, GPA = 3.5)

stu
