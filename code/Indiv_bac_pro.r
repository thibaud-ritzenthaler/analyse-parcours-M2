setwd("C:/Users/ocean/OneDrive/Documents/Baptou")
install.packages("tidyverse")
library(tidyverse)
install.packages("questionr")
library(questionr)
install.packages("haven")
library(readxl)
library(haven)
install.packages("dplyr")
library(dplyr)

indiv <- read_sas("D:/Démographie/trajectoire/indiv10_champcomp.sas7bdat", 
                              NULL)
indiv_bac_pro <- indiv %>%filter(Q35NEW == 3)
indiv_bac_pro2 <- indiv %>%filter(Q40A == 3 )
# Q35NEW, Q40A, Q40B, DIS09 
# Create df for histories with calendar
bac_pro_parcours_pro <- indiv_bac_pro %>% select(IDENT, starts_with('MOIS'), -MOISEDI)
bac_pro_parcours_hab <- indiv_bac_pro %>% select(IDENT, starts_with('HMOIS'))


#Répartition par âge
table(indiv_bac_pro$AGE10)
#De 23 à 35 ans. 
## Recodage de doc$AGE10 en doc$age_rec
indiv_bac_pro$age_rec <- cut(indiv_bac_pro$AGE10,
                   include.lowest = FALSE,
                   right = TRUE,
                   breaks = c(20, 22, 25, 35)

## Recodage de doc$Q1 en doc$sexe
indiv_bac_pro$sexe <- as.character(indiv_bac_pro$Q1)
indiv_bac_pro <- indiv_bac_pro %>%
  mutate(sexe_rec = recode(sexe,
                       "Homme" = "1",
                       "Femme" = "2")

## Recodage de doc$TAPE en doc$acces_emp
library(dplyr)


indiv_bac_pro <- indiv_bac_pro %>%
  mutate(TAPE = if_else(is.na(TAPE), "00", as.character(TAPE)))

indiv_bac_pro <- indiv_bac_pro %>%
  mutate(acces_emp = recode(TAPE,
                            "0" = "Moins de 1 mois",
                            "1" = "Moins de 1 mois",
                            "10" = "7 à 12 mois",
                            "11" = "7 à 12 mois",
                            "12" = "7 à 12 mois",
                            "13" = "Plus d'un an",
                            "14" = "Plus d'un an",
                            "15" = "Plus d'un an",
                            "16" = "Plus d'un an",
                            "17" = "Plus d'un an",
                            "18" = "Plus d'un an",
                            "19" = "Plus d'un an",
                            "2" = "Moins de 3 mois",
                            "20" = "Plus d'un an",
                            "21" = "Plus d'un an",
                            "22" = "Plus d'un an",
                            "23" = "Plus d'un an",
                            "24" = "Plus d'un an",
                            "25" = "Plus d'un an",
                            "26" = "Plus d'un an",
                            "27" = "Plus d'un an",
                            "28" = "Plus d'un an",
                            "29" = "Plus d'un an",
                            "30" = "Plus d'un an",
                            "3" = "3 à 6 mois",
                            "31" = "Plus d'un an",
                            "32" = "Plus d'un an",
                            "33" = "Plus d'un an",
                            "34" = "Plus d'un an",
                            "35" = "Plus d'un an",
                            "36" = "Plus d'un an",
                            "4" = "3 à 6 mois",
                            "5" = "3 à 6 mois",
                            "6" = "3 à 6 mois",
                            "7" = "7 à 12 mois",
                            "8" = "7 à 12 mois",
                            "9" = "7 à 12 mois",
                            "00" = "Jamais"))
         # Replace "NA_category" with the desired category for NA values

na_count <- sum(is.na(indiv_bac_pro$TAPE)) 
cat("Number of NA values in TAPE:", na_count, "\n")

# Suppose your data frame is named indiv_bac_pro and the variable is named TAPE
table_result <- table(indiv_bac_pro$acces_emp)
print(table_result)

indiv_bac_pro$nbfreressoeurs <- as.character(indiv_bac_pro$CA14)
indiv_bac_pro$nbfreressoeurs <- as.factor(indiv_bac_pro$nbfreressoeurs)

indiv_bac_pro <- indiv_bac_pro %>%
  mutate(nbfreressoeurs = recode(nbfreressoeurs,
                                 "0" = "Aucun",
                                 "1" = "Un",
                                 "2" = "Deux",
                                 "3" = "3 ou plus",
                                 "4" = "3 ou plus",
                                 "5" = "3 ou plus",
                                 "6" = "3 ou plus",
                                 "7" = "3 ou plus",
                                 "8" = "3 ou plus",
                                 "9" = "3 ou plus",
                                 "10" = "3 ou plus"))


# Suppose your data frame is named indiv_bac_pro and the variable is named TAPE
table_result <- table(indiv_bac_pro$nbfreressoeurs)
print(table_result)


indiv_bac_pro <- indiv_bac_pro %>%
  mutate(etud_conj = recode(CA2,
                            "1" = "Pas sup",
                            "2" = "Pas sup",
                            "3" = "Pas sup",
                            "4" = "Sup",
                            "5" = "Sup",
                            "6" = "NSP", 
                            "NA" = "N'est pas en couple"))

# Suppose your data frame is named indiv_bac_pro and the variable is named TAPE
table_result <- table(indiv_bac_pro$etud_conj)
print(table_result)





# Suppose your data frame is named indiv_bac_pro and the variable is named TAPE
table_result <- table(indiv_bac_pro$etud_conj)
print(table_result)



# Assuming your data frame is named indiv_bac_pro

# Recodage de indiv_bac_pro$CA0AREGIONF_13 en indiv_bac_pro$region_rec
indiv_bac_pro <- indiv_bac_pro %>%
  mutate(region_rec = recode(CA0AREGIONF_13,
                             "ALSACE" = "HORS IDF",
                             "AQUITAINE" = "HORS IDF",
                             "AUVERGNE" = "HORS IDF",
                             "BASSE-NORMANDIE" = "HORS IDF",
                             "BOURGOGNE" = "HORS IDF",
                             "BRETAGNE" = "HORS IDF",
                             "CENTRE" = "HORS IDF",
                             "CHAMPAGNE-ARDENNE" = "HORS IDF",
                             "CORSE" = "HORS IDF",
                             "FRANCHE-COMTE" = "HORS IDF",
                             "HAUTE-NORMANDIE" = "HORS IDF",
                             "LANGUEDOC-ROUSSILLON" = "HORS IDF",
                             "LIMOUSIN" = "HORS IDF",
                             "LORRAINE" = "HORS IDF",
                             "MIDI-PYR?N?ES" = "HORS IDF",
                             "NORD-PAS-DE-CALAIS" = "HORS IDF",
                             "PAYS DE LA LOIRE" = "HORS IDF",
                             "PICARDIE" = "HORS IDF",
                             "POITOU-CHARENTES" = "HORS IDF",
                             "PROVENCE-ALPES-COTE D'AZUR" = "HORS IDF",
                             "RHONE-ALPES" = "HORS IDF"))

# Recodage de indiv_bac_pro$region_rec
indiv_bac_pro <- indiv_bac_pro %>%
  mutate(region_rec = recode(region_rec, "**" = "HORS IDF"))

# Recodage de indiv_bac_pro$CA0AREGIONF_15 en indiv_bac_pro$region15_rec
indiv_bac_pro <- indiv_bac_pro %>%
  mutate(region15_rec = recode(CA0AREGIONF_15,
                               "ALSACE" = "HORS IDF",
                               "AQUITAINE" = "HORS IDF",
                               "AUVERGNE" = "HORS IDF",
                               "BASSE-NORMANDIE" = "HORS IDF",
                               "BOURGOGNE" = "HORS IDF",
                               "BRETAGNE" = "HORS IDF",
                               "CENTRE" = "HORS IDF",
                               "CHAMPAGNE-ARDENNE" = "HORS IDF",
                               "CORSE" = "HORS IDF",
                               "FRANCHE-COMT?" = "HORS IDF",
                               "HAUTE-NORMANDIE" = "HORS IDF",
                               "LA R?UNION" = "HORS IDF",
                               "LANGUEDOC-ROUSSILLON" = "HORS IDF",
                               "LIMOUSIN" = "HORS IDF",
                               "LORRAINE" = "HORS IDF",
                               "MIDI-PYR?N?ES" = "HORS IDF",
                               "NORD-PAS-DE-CALAIS" = "HORS IDF",
                               "PAYS DE LA LOIRE" = "HORS IDF",
                               "PICARDIE" = "HORS IDF",
                               "POITOU-CHARENTES" = "HORS IDF",
                               "PROVENCE-ALPES-C?TE D'AZUR" = "HORS IDF",
                               "RH?NE-ALPES" = "HORS IDF"))

# Recodage de indiv_bac_pro$region15_rec
indiv_bac_pro <- indiv_bac_pro %>%
  mutate(region15_rec = recode(region15_rec, "**" = "HORS IDF"))

# Recodage de indiv_bac_pro$TH5 en indiv_bac_pro$lieu
indiv_bac_pro <- indiv_bac_pro %>%
  mutate(lieu = recode(as.character(TH5),
                       "1" = "Dans une universit?",
                       "2" = "Dans une ecole d'ingenieur",
                       "3" = "Dans un autre organisme public",
                       "4" = "Dans une entreprise",
                       "5" = "Chez vous"))

table_result <- table(indiv_bac_pro$lieu)
print(table_result)

## Type d'établissement en dernier

indiv_bac_pro <- indiv_bac_pro %>%
  mutate(etab = recode(as.character(TYPE),
                       "A" = "Ecoles ingénieurs",
                       "B" = "Ecoles de commerce",
                       "C" = "Universités",
                       "D" = "Ecoles professions sociales",
                       "E" = "Ecoles professions de la santé",
                       "F" = "DRJS",
                       "H" = "IUFM",
                       "J" = "Ecoles secteur service",
                       "K" = "Ecoles secteur industriel",
                       "L" = "Ecoles formations agricoles",
                       "M" = "Facultés privées",
                       "N" = "Lycées et collèges MEN",
                       "P" = "Centre de Formation des Apprentis",
                       "Q" = "Lycées agricoles",
                       "R" = "Centres privés d'enseignement",
                       "S" = "Ecoles administrations publiques",
                       "T" = "Ecoles Normales Supérieures",
                       "U" = "Ecoles de la DGA",
                       "V" = "IEP",
                       "W" = "Ecoles ministère de la culture",
                       "X" = "CIFRE",
                       "Y" = "ALLOCATAIRES THESES",
                       "Z" = "DGAFP"))

table_result <- table(indiv_bac_pro$etab)
print(table_result)

##Obtention d’un diplôme/titre pro après arrêt des études	


install.packages("readxl")
library(readxl)
habpro_croise1 <- read_excel("C:/Users/ocean/Downloads/habpro_croise1.xlsx")

complet <- merge(habpro_croise1,indiv_bac_pro,by.x = "IDENT", by.y = "IDENT")


library(Rcmdr)
library(FactoMineR)
install.packages("TraMineR")
library(TraMineR)
install.packages("cluster")
library(cluster)
install.packages("questionr")
library(questionr)
install.packages("tidyverse")
library(tidyverse)
install.packages("Amelia")
library(Amelia)
install.packages("haven")
library(haven)
install.packages("explor")
library(explor)
install.packages("dplyr")
library(dplyr)
install.packages("survey")
library(survey)

seq <- seqdef(complet, 2:53)
couts <- seqsubm(seq, method="TRATE", cval = 1)
seq.om <- seqdist(seq, method = "OM", indel = 1, sm = couts)
ordre <- cmdscale(as.dist(seq.om), k = 1)
seq.agnes <- agnes(as.dist(seq.om), method = "ward", keep.diss = FALSE )
plot (sort(seq.agnes$height, decreasing=TRUE) [1:10], type="s",
      xlab="nb de classes", ylab="inertie")
plot(as.dendrogram(seq.agnes), leaflab="none") 







#### ANALYSE DES SEQUENCES ####

#Sélection des différentes variables des séquences 
seq <- seqdef(indiv_bac_pro, 2:53)
couts <- seqsubm(seq, method="TRATE", cval = 1)
seq.om <- seqdist(seq, method = "OM", indel = 1, sm = couts)
ordre <- cmdscale(as.dist(seq.om), k = 1)
seq.agnes <- agnes(as.dist(seq.om), method = "ward", keep.diss = FALSE )
plot (sort(seq.agnes$height, decreasing=TRUE) [1:10], type="s",
      xlab="nb de classes", ylab="inertie")
plot(as.dendrogram(seq.agnes), leaflab="none")   

##   En 4 classes
nbcl <- 4
seq.part4 <- cutree (seq.agnes, nbcl)
seq.part4 <- factor(seq.part4, labels = paste ("classe", 1:nbcl, sep = "."))
#Effectifs par classe 
distri.eff <- table(seq.part4)
distri.eff
# 789 dans classe 1, 301 dans classe 2, 95 dans classe 3 et 27 dans classe 4 






#Calculer la distance moyenne des séquences d'une classe au centre de cette classe
round(aggregate(disscenter(as.dist(seq.om), group = seq.part4), 
                list(seq.part4), mean)[, -1],1)

#Calcul de la courbe d'entropie transversale 
seqHtplot(seq, group=seq.part4, xtlab=1:55, with.legend=T) 

#Visualisation de l'état modal à chaque durée 
par(mar=c(2,2,2,2))
seqmsplot(seq, group=seq.part4, xtlab=1:55, with.legend=T,
          main = "classe")

#Calculer la durée moyenne dans chaque état 
seqmtplot(seq, group=seq.part4, with.legend=T)

#Représentation des chronogrammes 
seqdplot (seq, group=seq.part4, sortv=ordre, xtlab=1:55, tlim=0,
          border=NA, with.legend=T, yaxis=FALSE)

#Représentation des tapis de séquences
seqiplot (seq, group=seq.part4, sortv=ordre, xtlab=1:55, tlim=0,
          border=NA, with.legend=T, yaxis=FALSE) 

#Visualisation des 10 séquences les plus fréquentes 
seqfplot(seq, group=seq.part4, with.legend=T)

#Ajout de la variable séquence typologie 
indiv_bac_pro$seq.part4 <- seq.part4

#Exporter la table avec la variable séquence 
#write.csv2 (seq.part4,file= " seq.part4.csv ") 








####STATISTIQUES DESCRIPTIVES ####



#Tableau croisé séquences avec sexe et test du Khi 2
sexe <- round (100*prop.table(wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$sexe,weights = doc$pondefcomp),2),1)
effsexe <- wtd.table(indiv_bac_pro$seq.part4,doc$sexe,weights = indiv_bac_pro$pondefcomp)
sexe
chisq.test (effsexe)



#Tableau croisé séquences avec catégorie socio pro des parents et test du Khi 2 
catsocio <- round (100*prop.table(wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$SUP, weights = doc$pondefcomp),2),1)
catsocio_eff<- wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$SUP,weights = indiv_bac_pro$pondefcomp)
catsocio
chisq.test (catsocio_eff)


#Tableau croisé séquences avec âge et test du Khi 2 
age <- round (100*prop.table(wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$age_rec),2),1)
age_eff <- wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$age_rec,weights = doc$pondefcomp)
age
chisq.test (age_eff)

#Tableau croisé séquences avec temps accès premier emploi et test du Khi 2 
premier_emp <- round (100*prop.table(wtd.table(doc$seq.part4,$acces_emp, weights = indiv_bac_pro$pondefcomp),2),1)
premier_emp_eff <- wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$acces_emp,weights = indiv_bac_pro$pondefcomp)
premier_emp
chisq.test (premier_emp_eff)

#Tableau croisé séquences avec région et test du Khi 2 
region <- round (100*prop.table(wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$region_rec, weights = indiv_bac_pro$pondefcomp),2),1)
region_eff <- wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$region_rec,weights = indiv_bac_pro$pondefcomp)
region
chisq.test (region_eff)

region <- round (100*prop.table(wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$region15_rec, weights = indiv_bac_pro$pondefcomp),2),1)
region_eff <- wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$region15_rec,weights = indiv_bac_pro$pondefcomp)
region
chisq.test (region_eff)

#Tableau croisé séquences avec domaine du indiv_bac_protorat et test du Khi 2 
phd <- round (100*prop.table(wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$PHD, weights = indiv_bac_pro$pondefcomp),2),1)
phd_eff<- wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$PHD,weights = indiv_bac_pro$pondefcomp)
phd
chisq.test (phd_eff)

#Tableau croisé séquences avec sejour à l'étranger et test du Khi 2 
etranger <- round (100*prop.table(wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$etranger, weights = indiv_bac_pro$pondefcomp),2),1)
etranger_eff<- wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$etranger,weights = indiv_bac_pro$pondefcomp)
etranger
chisq.test (etranger_eff)

#Tableau croisé séquences avec lieu réalisation thèse et test du Khi 2 
lieu <- round (100*prop.table(wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$lieu, weights = indiv_bac_pro$pondefcomp),2),1)
lieu_eff<- wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$lieu,weights = indiv_bac_pro$pondefcomp)
lieu
chisq.test (lieu_eff)

#Tableau croisé séquences avec projet pro et test du Khi 2 
projet <- round (100*prop.table(wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$projetpro, weights = indiv_bac_pro$pondefcomp),2),1)
projet_eff<- wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$projetpro,weights = indiv_bac_pro$pondefcomp)
projet
chisq.test (projet_eff)

#Tableau croisé séquences avec nb freres soeurs et test du Khi 2 
nbfs <- round (100*prop.table(wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$nbfreressoeurs, weights = indiv_bac_pro$pondefcomp),2),1)
nbfs_eff<- wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$nbfreressoeurs,weights = indiv_bac_pro$pondefcomp)
nbfs
chisq.test (nbfs_eff)

#Tableau croisé séquences avec niveau ?tudes conjoint et test du Khi 2 
etudesconj <- round (100*prop.table(wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$conj_etud, weights = indiv_bac_pro$pondefcomp),2),1)
etudesconj_eff<- wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$conj_etud,weights = indiv_bac_pro$pondefcomp)
etudesconj
chisq.test (etudesconj_eff)

#Tableau croisé séquences avec contrat13 et test du Khi 2 
contrat13 <- round (100*prop.table(wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$CONTRAT_13, weights = indiv_bac_pro$pondefcomp),2),1)
contrat13_eff<- wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$CONTRAT_13,weights = indiv_bac_pro$pondefcomp)
contrat13
chisq.test (contrat13_eff)

#Tableau croisé séquences avec contrat15 et test du Khi 2 
contrat15 <- round (100*prop.table(wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$CONTRAT_15, weights = indiv_bac_pro$pondefcomp),2),1)
contrat15_eff<- wtd.table(indiv_bac_pro$seq.part4,indiv_bac_pro$CONTRAT_15,weights = indiv_bac_pro$pondefcomp)
contrat15
chisq.test (contrat15_eff)










#Réalisation de l'ACM
#### ACM1 ####
indiv_bac_pro$acces_emp <- fct_recode(indiv_bac_pro$acces_emp, "sans emploi" = "NA")
data1<-subset(indiv_bac_pro, select=c(sexe, age_rec, SUP, acces_emp, region15_rec, PHD, lieu, projetpro, CONTRAT_15 ,seq.part4, TYPOTRAJ_13, financement))
data2<-as.data.frame(lapply(data1,as.factor))
par(mar=c(1,1,1,1))

modele.mca = MCA(data2,)
dimdesc (modele.mca)

res1 <- explor::prepare_results(modele.mca)
explor::MCA_var_plot(res1, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = NULL,
                     size_range = c(10, 300), labels_size = 13, point_size = 128, transitions = TRUE,
                     labels_positions = "auto", labels_prepend_var = FALSE, xlim = c(-1.59, 6),
                     ylim = c(-4.24, 3.35))
explor(modele.mca)

#### ACM2 ####
# on enlève ceux qui n'ont pas d'emploi et qui tirent l'ACM
doc_emploi <- indiv_bac_pro %>%
  filter(acces_emp != "sans emploi")
data3<-subset(doc_emploi, select=c(sexe, age_rec, SUP, acces_emp, PHD, CONTRAT_15 , seq.part4, TYPOTRAJ_13, financement))
data4<-as.data.frame(lapply(data3,as.factor))
par(mar=c(1,1,1,1))

modele.mca = MCA(data4,)
dimdesc (modele.mca)

res <- explor::prepare_results(modele.mca)
explor::MCA_var_plot(res, xax = 1, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = NULL,
                     size_range = c(10, 300), labels_size = 14, point_size = 128, transitions = TRUE,
                     labels_positions = "auto", labels_prepend_var = FALSE, xlim = c(-1.5, 4.07),
                     ylim = c(-1.47, 4.1))
explor(modele.mca)





 




