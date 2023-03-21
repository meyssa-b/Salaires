### Projet Statistiques


###### Librairies à importer
library(tidyverse)
library(readxl)
library(wooldridge)
library(ggplot2)
library(car)
library(ISLR) 
library(tidyverse)
library(broom)
library(qdapTools)
library(data.table)
library(purrr)


# Import des données
salaire <- read_excel('BD.xlsx')

# Création d'une base des salariés Portugais
portugal <- salaire %>% filter(country == "Portugal")

### Question 1 : Distibution et écart de salaire homme-femme
# Observation de des quartiles
summary(salaire$inc_euro)
summary(portugal$inc_euro)

# Création d'une base européenne filtrée sur le salaire (inférieur ou égal à 6000€ mensuel)
salaire_Clean <- salaire %>% filter(inc_euro <= 6000)
summary(salaire_Clean$inc_euro)

# Courbe de distibution du salaire en Europe
ggplot(salaire_Clean, aes(x=inc_euro)) + geom_density() + xlab("Salaire mensuel net (en euro)") +
  ylab("Densité") + ggtitle("Courbe de distribution du salaire en Europe en 2015") +
  theme(
    axis.title.x = element_text(size=12, face="bold", vjust = -1),
    axis.title.y = element_text(size=12,face="bold", vjust = 3)) +
  geom_density(color="darkblue", fill="lightblue")

# Courbe de distibution du salaire au Portugal
ggplot(portugal, aes(x=inc_euro)) + geom_density() + xlab("Salaire mensuel net (en euro)") +
  ylab("Densité") + ggtitle("Courbe de distribution du salaire au Portugal en 2015") +
  theme(
    axis.title.x = element_text(size=12, face="bold", vjust = -1),
    axis.title.y = element_text(size=12, face="bold", vjust = 3)) +
  geom_density(color="bisque4", fill="bisque")

# Boite à monstache de la répartiotion du salaire entre les hommes et les femmes en Europe
salaire_Clean <- salaire_Clean %>% mutate(genre = ifelse(gender == 0, "homme", "femme"))

ggplot(salaire_Clean, aes(x = genre, y = inc_euro, fill = genre)) + geom_boxplot() +
  ggtitle("Répartition du salaire en fonction du genre en Europe en 2015") +
  xlab("Genre") + ylab("Salaire") + theme(
    axis.title.x = element_text(size=12, face="bold", vjust = -1),
    axis.title.y = element_text(size=12, face="bold", vjust = 3),
    legend.position = "none")

# Boite à monstache de la répartiotion du salaire entre les hommes et les femmes au Portugal
portugal <- portugal %>% mutate(genre = ifelse(gender == 0, "homme", "femme"))

ggplot(portugal, aes(x = genre, y = inc_euro, fill = genre)) + geom_boxplot() +
  ggtitle("Répartition du salaire en fonction du genre au Portugal en 2015") +
  xlab("Genre") + ylab("Salaire") + theme(
    axis.title.x = element_text(size=12, face="bold", vjust = -1),
    axis.title.y = element_text(size=12, face="bold", vjust = 3),
    legend.position = "none")

# Observation de la répartition des salaire par genre
tapply(salaire$inc_euro , salaire$gender, summary)
tapply(portugal$inc_euro , portugal$gender, summary)

### Question 2 : Céation d'un tableau comparatif par genre
# Création d'une base homme et d'une base femme
portF <- portugal %>% filter(genre == "femme")
portH <- portugal %>% filter(genre == "homme")

# Création des variables
Tx_indiv <- as.matrix(round(table(portugal$genre)/1110*100,0)) %>% t() %>%
  as.data.frame() %>% mutate(variable = "taux d'individus") # Taux d'individus
age_moy <- data.frame(femme = round(mean(portF$age),0), homme = round(mean(portH$age),0), variable = "âge moyen") # Age moyen
sal_moy <- data.frame(femme = round(mean(portF$inc_euro),0),
                      homme = round(mean(portH$inc_euro),0), variable = "salaire moyen") # Salaire moyen
sal_med <- data.frame(femme = median(portF$inc_euro),
                      homme = median(portH$inc_euro), variable = "salaire médian") # Salaire median
tenure_moy <- data.frame(femme = round(mean(portF$tenure_firm),0),
                         homme = round(mean(portH$tenure_firm),0), variable = "ancienneté moyenne") # Anciennete moyenne
tx_CDI <- data.frame(femme = round(portF %>% filter(permanent==1) %>% nrow()/nrow(portF)*100, 0),
                     homme = round(portH %>% filter(permanent==1) %>% nrow()/nrow(portH)*100, 0),
                     variable = "taux d'emploi en CDI") # Taux d'emploi en CDI
tx_CDD<- data.frame(femme = round(portF %>% filter(permanent==2) %>% nrow()/nrow(portF)*100, 0),
                    homme = round(portH %>% filter(permanent==2) %>% nrow()/nrow(portH)*100, 0),
                    variable = "taux d'emploi en CDD") # Taux d'emploi en CDD
tx_interim<- data.frame(femme = round(portF %>% filter(permanent==3) %>% nrow()/nrow(portF)*100, 1),
                        homme = round(portH %>% filter(permanent==3) %>% nrow()/nrow(portH)*100, 1),
                        variable = "taux d'emploi en interim") # Taux d'emploi en interim
tx_alternance<- data.frame(femme = round(portF %>% filter(permanent==4) %>% nrow()/nrow(portF)*100, 0),
                           homme = round(portH %>% filter(permanent==4) %>% nrow()/nrow(portH)*100, 0),
                           variable = "taux d'emploi en alternance/stage") # Taux d'emploi en alternance ou en stage
tx_prive<- data.frame(femme = round(portF %>% filter(Private==1) %>% nrow()/nrow(portF)*100, 0),
                      homme = round(portH %>% filter(Private==1) %>% nrow()/nrow(portH)*100, 0),
                      variable = "taux de secteur privé") # Taux de travailleurs dans le secteur prive
tx_public<- data.frame(femme = round(portF %>% filter(Private==0) %>% nrow()/nrow(portF)*100, 0),
                       homme = round(portH %>% filter(Private==0) %>% nrow()/nrow(portH)*100, 0),
                       variable = "taux de secteur public") # Taux de travailleurs dans le secteur public
tx_etranger<- data.frame(femme = round(portF %>% filter(nationality==0) %>% nrow()/nrow(portF)*100, 1),
                         homme = round(portH %>% filter(nationality==0) %>% nrow()/nrow(portH)*100, 1),
                         variable = "taux de travailleurs étrangers") # Taux de travailleurs etrangers
tx_satisfaction<- data.frame(femme = round(portF %>% filter(job_satisfaction==1) %>% nrow()/nrow(portF)*100, 0),
                             homme = round(portH %>% filter(job_satisfaction==1) %>% nrow()/nrow(portH)*100, 0),
                             variable = "taux de satisfaction au travail") # Taux de satisfaction au travail

# Compilation par lignes
resume <- rbind(Tx_indiv, age_moy, sal_moy, sal_med, tenure_moy, tx_CDI, tx_CDD,
                tx_interim, tx_alternance, tx_prive, tx_public, tx_etranger, tx_satisfaction)

# Nettoyage de l'environnement de travail
rm(age_moy, portF, portH, sal_med, sal_moy, tenure_moy, tx_alternance,
   tx_CDD, tx_CDI, tx_etranger, Tx_indiv, tx_interim, tx_prive, tx_public, tx_satisfaction)

# Changement de l'odre des colonnes
resume <- resume[,c(3, 1, 2)]

# Ajout des unités
for(i in 1:13){
  resume$incr[i] <- i
}

for(i in 1:13){
  if(resume$incr[i] != 2 && resume$incr[i] != 3 && resume$incr[i] != 4 && resume$incr[i] != 5){
    n <- "%"
  }
  else{
    ifelse(resume$incr[i] == 3 || resume$incr[i] == 4,
           n <- "€",
           n <- " ans")
  }
  resume$Hommes[i] <- paste0(resume$homme[i], n)
  resume$Femmes[i] <- paste0(resume$femme[i], n)
}

# Suppression des colonnes inutiles et suppression du nom de la colonne "variable"
resume <- resume[-c(2:4)]
names(resume)[1] <- ""
colnames(resume)

# Export du tableau
library(kableExtra)
resume %>% kbl() %>%
  kable_classic_2(full_width = T, html_font = "Cambria", font_size = 20) %>%
  column_spec(1, bold = T, color = "darkblue") %>%
  save_kable("tableau_Q2.png")


### Question 3.a : Etude de l'impact et de la significativité des variables sur le salaire

# Binarisation de la variable catégorielle concernant le type de contrat
# Retrait d'une des modalité (permanent == 4, c'est à dire les contrats en intérim) afin d'éviter une colinéarité stricte
portugal <- portugal %>% mutate(CDI = ifelse(permanent==1, 1, 0)) %>%
  mutate(CDD = ifelse(permanent==2, 1, 0)) %>%
  mutate(interim = ifelse(permanent==3, 1, 0))

# Modification de la classe des variables catégorielles (numériques en facteurs)
portugal$gender <- as.factor(portugal$gender)
portugal$job_satisfaction <- as.factor(portugal$job_satisfaction)
portugal$nationality <- as.factor(portugal$nationality)
portugal$permanent <- as.factor(portugal$permanent)
portugal$CDI <- as.factor(portugal$CDI)
portugal$CDD <- as.factor(portugal$CDD)
portugal$interim <- as.factor(portugal$interim)
portugal$Private <- as.factor(portugal$Private)

# Création d'un modèle linéaire incluant les variables souhaitées et visant à expliquer le salaire des travailleurs portugais
reg1 <- lm(inc_euro~age+gender+nationality+CDI+CDD+interim+Private, data=portugal)

# Stockage des coefficients dans un dataframe
reg1_coef <- as.data.frame(summary(reg1)[["coefficients"]])

# Affichage des résultats
summary(reg1)

# Affichage de l'intervale de confiance
reg1_confint <- as.data.frame(confint(reg1))
reg1_confint

# Compilation des coefficients et de l'intervale de confiance
reg1_coefconfint <- cbind(reg1_coef, reg1_confint)

# Export du tableau
reg1_coefconfint %>% kbl() %>%
  kable_classic_2(full_width = T, html_font = "Cambria", font_size = 20) %>%
  row_spec(0, bold = T) %>% column_spec(1, bold = T) %>%
  save_kable("tableau_Q3a.png")


###### TOUs les pays
data_all=salaire


## Filtrage sur les données du Portugal

data=portugal


### Conversion en factor pour les variables qualitatives encodées en character
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                           as.factor)


# Informations globales sur les données
str(data)
summary(data)

## Création du dataset Portugal - Filtrage sur les données du Portugal

portugal <- data %>%
  filter(country == "Portugal")


# Descriptive statistics by groups# 

tapply(data$inc_euro , data$gender, summary)
tapply(portugal$inc_euro , portugal$gender, summary)


portugal %>% split(.$gender) %>% map(summary)




# Régression linéaire multiple sur le salaire - Evaluation de l'interaction genre-âge
result2=lm(inc_euro~age*gender+nationality+Private+CDI+CDD+interim, data=portugal)
summary(result2)


## Analyse de la variance
### Salaire moyen au Portugal
moy <- mean(portugal$inc_euro)
### Variance expliquée
result1=reg1
SCE <- sum((fitted(result1)-moy)**2)
### Variance totale
SCT <- sum((portugal$inc_euro-moy)**2)
### Variance résiduelle
SCR  <- SCT - SCE

### Part de la variance expliquée par rapport à la variance totale
SCE/SCT
### Part de la variance résiduelle par rapport à la variance totale
SCR/SCT



## Qualité globale du modèle
broom::glance(result1)


## Analyse des résidus

### Répartition des résidus
par(mfrow=c(2,2))
plot(result1)

### Analyse des Résidus standardisés
#### calcul du résidu standardisé
res.standard <- rstandard(result1)
#### Test bilatéral sur résidu standardisé 
alpha <- 0.1
seuil.standard <- qt(1-alpha/2,1110-7-1)
#### construction du graphique des résidus standardisés
layout(1)
plot(portugal$inc_euro,res.standard, main ="Résidus standardisés")
abline(h=-seuil.standard)
abline(h=+seuil.standard)
abline(h=0)
#### Tableau des obs. atypiques
ab.standard <- portugal[res.standard < -seuil.standard | res.standard > +seuil.standard,]
print(ab.standard)
#### Caractéristiques des obs. atypiques
summary(ab.standard)
#### Proportion du dataset concerné 
nrow(ab.standard)/nrow(portugal)


### calcul des résidus studentisés
res.student <- rstudent(result1)
#### calcul du seuil 
alpha <- 0.1
seuil.student <- qt(1-alpha/2,1110-7-2)
#### construction du graphique des résidus standardisés
layout(1)
plot(portugal$inc_euro,res.student, main ="Résidus studentisés") 
abline(h=-seuil.student)
abline(h=+seuil.student)
abline(h=0)
#### détection des points en dehors des tuyaux
ab.student <- portugal[res.student < -seuil.student | res.student > +seuil.student,]
print(ab.student)
#### Caractéristiques des observations en dehors des tuyaux
summary(ab.student)
#### Comparaison observations extraites de l'analyse des résidus standardisés et celles des résidus studentisés
ab.student != ab.standard


### Points leviers
#### Récupération des indicateurs d'influence
atypiques <- influence.measures(result1)
#### Matrice infmat
print(atypiques$infmat)
#### Récupération de la colonne "hat" qui correspond au levier
res.hat <- atypiques$infmat[,"hat"]
#### Calcul du seuil
seuil.hat <- 2*(7+1)/1110
#### Détection des points atypiques (au sens du levier)
ab.hat <- portugal[res.hat > seuil.hat,]
print(ab.hat)
#### Nombre de points atypiques détectés
nrow(ab.hat)
#### Caractéristiques de ces points atypiques
summary(ab.hat)
#### Création de la variable factorielle "permanent"
portugal$permanent_f <- as.factor(portugal$permanent)
levels(portugal$permanent_f) <- c("CDI", "CDD",'Interim',"Apprentissage/Stage")
#### Visualisation des points leviers selon la nature du contrat de travail
layout(1)
plot(portugal$permanent_f,res.hat, main ="Points leviers selon la nature du contrat") 
abline(h=-seuil.hat)
abline(h=+seuil.hat)
abline(h=0)

### Distance de cook, résidus studentisés et leverage
influencePlot(result1, main="Influence Plot", 
              sub="Circle size is proportional to Cook's Distance" )
infIndexPlot(result1)




#### f) Pour améliorer la qualité du modèle :

#vecteur booléen indicateur de suspicion pour résidu standardisé
b.standard <- (res.standard < -seuil.standard | res.standard > +seuil.standard)
#vecteur booléen indicateur de suspicion pour résidu studentisé
b.student <- (res.student < -seuil.student | res.student > +seuil.student)
#vecteur booléen indicateur de suspicion pour le levier
b.hat <- (res.hat > seuil.hat)
#booléen indicateur de détection au moins une fois ; Note : règle contraignante, faut voir surtout si ça a un impact sur la qualité de la régression
b.suspicious <- b.standard | b.student | b.hat
#booléen indicateur de non détection
b.not.suspicious <- !b.suspicious
#data.frame  sans les données suspectes 
portugal.clean <- portugal[b.not.suspicious,]
print(nrow(portugal.clean))
print(portugal.clean)
portugal.Nclean <- portugal[b.suspicious,]
print(nrow(portugal.Nclean))
#régression sur les données "clean"
result3<-lm(inc_euro~age+gender+nationality+Private+CDI+CDD+tenure_firm, data=portugal.clean)
summary(result3)





#############################################################4  Estimation du modèle sur tous les pays#############################################################
#Equation global sur le portugal
result_p=lm(inc_euro~age+gender+nationality+CDI+CDD+interim+Private, data=data)
result_p
summary(result_p)
confint(result_p)

data_all <- data_all %>% mutate(CDI = ifelse(permanent==1, 1, 0)) %>%
  mutate(CDD = ifelse(permanent==2, 1, 0)) %>%
  mutate(interim = ifelse(permanent==3, 1, 0))


#Equation global sur tous les pays
result_all=lm(inc_euro~age+gender+nationality+CDI+CDD+interim+Private, data=data_all)
result_all
summary(result_all)
confint(result_all)

########Variance sur le portugal
fitted(result_p)
residuals(result_p)

#somme des carrés des
# écarts totale         = somme des carrés des  écarts expliquée + somme des carrés des écarts résiduelle

# Variance expliquée
SCE_p <- sum((fitted(result_p)-moy)**2)
# Variance totale
SCT_p <- sum((data$inc_euro-moy)**2)
# Variance résiduelle
SCR_p  <- SCT_p - SCE_p

anova(result_p)   # Analyse de variance


# Part de la variance expliquée par rapport à la variance totale
SCE_p/SCT_p
# Part de la variance résiduelle par rapport à la variance totale
SCR_p/SCT_p
  
  
########Variance sur tous les pays
fitted(result_all)
residuals(result_all)
data$inc_euro
moy <- mean(data_all$inc_euro)

#somme des carrés des
# écarts totale         = somme des carrés des  écarts expliquée + somme des carrés des écarts résiduelle

# Variance expliquée
SCE_all <- sum((fitted(result_all)-moy)**2)
# Variance totale
SCT_all <- sum((data_all$inc_euro-moy)**2)
# Variance résiduelle
SCR_all  <- SCT_all - SCE_all

anova(result_all)   # Analyse de variance


# Part de la variance expliquée par rapport à la variance totale
SCE_all/SCT_all
# Part de la variance résiduelle par rapport à la variance totale
SCR_all/SCT_all
  


#############################################################5  COMPARER QUALITE DE REGRESSION ENTRE portugal ET TOUT#############################################################

result_all=lm(inc_euro~age+gender+nationality+permanent+Private, data=data_all)
result_p=lm(inc_euro~age+gender+nationality+permanent+Private, data=data)

###############Comparaison de l'évaluation
list(result_train_all=broom::glance(result_all), result_train_p=broom::glance(result_p))


