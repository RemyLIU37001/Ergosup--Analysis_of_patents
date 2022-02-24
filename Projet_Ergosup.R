rm(list = ls())
library(openxlsx)
library(Rcmdr)
library(colorspace, pos=19)
#Combiner les fichiers de resultat dans un meme fichier en format excel
tb1<- read.csv('Q1.CSV',header= TRUE ,sep = ';')
tb2a <- read.csv('Q2A.CSV',header = TRUE,sep = ';')
tb2b <- read.csv('Q2B.CSV',header = TRUE,sep = ';')
tb3a <- read.csv('Q3A.CSV',header = TRUE,sep = ';')
tb3b <- read.csv('Q3B.CSV',header = TRUE,sep = ';')
tb4 <- read.csv('Q4.CSV',header = TRUE,sep = ';')

result <- list("Question1"=tb1,
               "Question2a"=tb2a,
               "Question2b"=tb2b,
               "Question3a"=tb3a,
               "Question3b"=tb3b,
               "Question4"=tb4) 
write.xlsx(result,"results of questions.xlsx")

#Lecture des donnees
tb1<-read.xlsx('results of questions.xlsx',1)
tb2a<-read.xlsx('results of questions.xlsx',2)
tb2b<-read.xlsx('results of questions.xlsx',3)
tb3a<-read.xlsx('results of questions.xlsx',4)
tb3b<-read.xlsx('results of questions.xlsx',5)
tb4<-read.xlsx('results of questions.xlsx',6)

#Question 1 
#lister des brevets deposes par la firme
##Préparation des données
str(tb1)
tb1$appln_id <- as.factor(tb1$appln_id)
tb1$appln_auth <- as.factor(tb1$appln_auth)
tb1$appln_filing_year<-as.factor(tb1$appln_filing_year)
tb1$docdb_family_id <- as.factor(tb1$docdb_family_id)
tb1$granted <- as.factor(tb1$granted)



table_tb1_idpays <- table(tb1$appln_auth);table_tb1_idpays
with(tb1, piechart(appln_auth, main="Distribution des brevets", 
                   col=rainbow_hcl(7), scale="percent"))
# Cette entreprise a depose 18 brevets aux 7 differents bureau de brevets.
# ils sont : CA:bureau de brevet canadien,
#            EP:bureau de brevet union europeen
#            ES:bureau de brevet espagnol
#            FR:bureau de brevet de la france
#            US:bureau de brevet aux État-Unis
#            WO:bureau de brevet international
#Nous pouvons conclure que la plupart de ses brevets est déposés dans le bureau de brevet international 
#et le bureau de brevet de la france, mais pas dans le bureau de union européenne,
#elle dépose aussi sur les bureau des autres pays comme Canada,Espagne, etc.
table_tb1_granted <- table(tb1$granted);table_tb1_granted


#Parmis les brevets qu'elle a deposé, il y a 11 brevets ne sont pas accordés par le bureau de brevets
with(tb1, Barplot(appln_auth, by=granted, style="divided", 
                  legend.pos="above", xlab="appln_auth", ylab="Frequency",main = 'La situation d\'accord(par bureau)'))
#Nous trouvons que les brevets qui ne sont pas accordés sont les brevets qui sont déposés dans le bureau de brevet
#de la france et de union européen. 
#2/7 de brevets sont accordé par le bureau de brevets de la france et 100%(6/6) de brevets ne sont pas encore accordé par
#le bureau de brevets internationnal
table_tb1_years <-table(tb1$appln_filing_year);table_tb1_years
with(tb1, Barplot(appln_filing_year, 
                  legend.pos="above", xlab="appln_filing_year", ylab="Frequency"))
#L'entreprise a demarré ces activités d'innovations depuis 2010 mais
#le nombre de brevets déposés sont plus important dans les années 2010,2018 et 2019, 
#pendant la période de 2013 à 2017, sauf que 0 brevet est déposé à 2017, chaque année elle dépose au moins
#un brevets.

table_tb1_docdb_fid <- table(tb1$docdb_family_id);table_tb1_docdb_fid

#Question 2 Classes rechro associees a ces brevets :cpc puis ipc 
#Question 2a CPC
summary(tb2a)
tb2a$appln_id<-as.factor(tb2a$appln_id)
tb2a$cpc_class_symbol<-as.factor(tb2a$cpc_class_symbol)
str(tb2a)
table_tb2a_cpc <- table(tb2a$cpc_class_symbol);table_tb2a_cpc
with(tb2a, Barplot(cpc_class_symbol, xlab="cpc_class_symbol", 
                   ylab="Frequency", label.bars=TRUE, main = 'Les catégories de CPC'))
#Nous trouvons que les cpc de ces brevets se situent dans les domaines C F H Y
# C: CHIMIE ; MÉTALLURGIE
### c01b: ÉLÉMENTS NON MÉTALLIQUES ; COMPOSÉS DE CEUX-CI; MÉTALLODES OU COMPOSÉS DE CEUX- CI NON VISÉS PAR LA SOUS- CLASSE C01C
### c25b: PROCEDES ELECTROLYTIQUES OU ELECTROPHORETIQUES POUR LA PRODUCTION DE COMPOSES OU DE NON-METAUX ; 
###       APPAREIL À CET APPAREIL ( protection anodique ou cathodique C23F13/00 ; croissance monocristalline C30B )
### C25C: PROCÉDÉS DE PRODUCTION ÉLECTROLYTIQUE, DE RÉCUPÉRATION OU D'AFFINAGE DES MÉTAUX ; APPAREIL À CET APPAREIL
# F: INGÉNIERIE MÉCANIQUE ; ECLAIRAGE ; CHAUFFAGE ; ARMES ; SAUTAGE
### F04B: MACHINES A DEPLACEMENT POSITIF POUR LIQUIDES ; POMPES ( machines à liquides, ou pompes, à piston rotatif ou oscillant F04C ; pompes non volumétriques F04D ; pompage de fluide par contact direct d'un autre fluide ou en utilisant l'inertie du fluide à pomper F04F )
# H: ÉLECTRICITÉ
### H01M: PROCEDES OU MOYENS, p.ex. BATTERIES, POUR LA CONVERSION DIRECTE DE L'ENERGIE CHIMIQUE EN ENERGIE ELECTRIQUE
# Y: SECTIONS DE L'IPC ; SUJETS TECHNIQUES COUVERTS PAR LES ANCIENNES COLLECTIONS D'ART DE RÉFÉRENCE USPC [XRAC] ET RÉGIME
### Y02E: RÉDUCTION DES ÉMISSIONS DE GAZ À EFFET DE SERRE [GES] LIÉES À LA PRODUCTION, AU TRANSPORT OU À LA DISTRIBUTION D'ÉNERGIE
### Y02p: TECHNOLOGIES D'ATTÉNUATION DU CHANGEMENT CLIMATIQUE DANS LA PRODUCTION OU LA TRANSFORMATION DES MARCHANDISES

#INTERPRÉTATION::

#Question 2b
summary(tb2b)
tb2b$appln_id<-as.factor(tb2b$appln_id)
tb2b$Ipc_class_symbol<-as.factor(tb2b$Ipc_class_symbol)
str(tb2b)
with(tb2b, Barplot(Ipc_class_symbol, xlab="cpc_class_symbol", 
                   ylab="Frequency", label.bars=TRUE, main = 'Les catégories de IPC'))
table_tb2b_Ipc<-table(tb2b$Ipc_class_symbol);table_tb2b_Ipc

#Nous trouvons que les Ipc de ces brevets se situent dans les domaines B C F H 
# B: TECHNIQUES INDUSTRIELLES; TRANSPORTS
### B01D: SÉPARATION (séparation de solides par voie humide B03B, B03D, par tables ou cribles pneumatiques B03B, par voie sèche B07; séparation magnétique ou électrostatique de matériaux solides à partir de matériaux solides ou de fluides, séparation par des champs électriques à haute tension B03C; appareils centrifugeurs B04B; appareils à vortex B04C; presses en soi pour exprimer les liquides des substances qui les contiennent B30B 9/02) [5]
# C: CHIMIE; MÉTALLURGIE
### C01B: ÉLÉMENTS NON MÉTALLIQUES; LEURS COMPOSÉS 
### C25B: PROCÉDÉS ÉLECTROLYTIQUES OU ÉLECTROPHORÉTIQUES POUR LA PRODUCTION DE COMPOSÉS ORGANIQUES OU MINÉRAUX, OU DE NON-MÉTAUX; APPAREILLAGES À CET EFFET 
### C25C:	PROCÉDÉS POUR LA PRODUCTION, LA RÉCUPÉRATION OU L'AFFINAGE ÉLECTROLYTIQUE DES MÉTAUX; APPAREILLAGES À CET EFFET
### C25D: PROCÉDÉS POUR LA PRODUCTION ÉLECTROLYTIQUE OU ÉLECTROPHORÉTIQUE DE REVÊTEMENTS; GALVANOPLASTIE (fabrication de circuits imprimés par dépôt métallique H05K 3/18); JONCTION DE PIÈCES PAR ÉLECTROLYSE; APPAREILLAGES À CET EFFET (protection anodique ou cathodique C23F 13/00; croissance des monocristaux C30B) [6]
# F: MÉCANIQUE; ÉCLAIRAGE; CHAUFFAGE; ARMEMENT; SAUTAGE
### F25B:	MACHINES, INSTALLATIONS OU SYSTÈMES FRIGORIFIQUES; SYSTÈMES COMBINÉS DE CHAUFFAGE ET DE RÉFRIGÉRATION; SYSTÈMES À POMPES À CHALEUR
# H: ÉLECTRICITÉ
### H01M:	PROCÉDÉS OU MOYENS POUR LA CONVERSION DIRECTE DE L'ÉNERGIE CHIMIQUE EN ÉNERGIE ÉLECTRIQUE, p.ex. BATTERIES [2]

#INTERPRÉTATION:

#Question 3:
#Citations recues par ces brevets : liste simple puis ajouter infos sur classes techno.des brevets citants
#Preparation des donnees de question 3a et 3b:
summary(tb3a)
tb3a$appln_id <- as.factor(tb3a$appln_id)
tb3a$docdb_family_id <- as.factor(tb3a$docdb_family_id)
tb3a$citing_appln_id <- as.factor(tb3a$citing_appln_id)
tb3a$citing_family_id <- as.factor(tb3a$citing_family_id)
tb3a$citing_appln_years <- as.factor(tb3a$citing_appln_years)
str(tb3a)
summary(tb3b)
tb3b$appln_id <- as.factor(tb3b$appln_id)
tb3b$docdb_family_id <- as.factor(tb3b$docdb_family_id)
tb3b$citing_ipc4 <- as.factor(tb3b$citing_ipc4)
tb3b$citing_family_id <- as.factor(tb3b$citing_family_id)
str(tb3b)

#3a)
table_tb3a_appid<-table(tb3a$appln_id);table_tb3a_appid
dftable_tb3a_appid <- data.frame(table_tb3a_appid);dftable_tb3a_appid
colnames(dftable_tb3a_appid) = c("Identification","n_citn")
dftable_tb3a_appid$n_citn2 <- dftable_tb3a_appid$n_citn - 1 
#n_citn : comtage brut
#n_citn : comtage nette en eliminant auto-citation


with(tb3a, Barplot(appln_id, xlab="appln_id", ylab="Frequency", 
                   label.bars=TRUE, main = "La situation de la citation des brevets"))
bureau_select=subset(tb1,tb1$appln_id == "363592032"|tb1$appln_id =="365969867",
                     c(appln_id,appln_auth));bureau_select
#Nous trouvons qu'il y a 5 brevets déposés par l'entreprise que nous étudions sont cité par les autres 
#inventeurs, parmis les 5 brevets cités, les 2 brevets qui sont déposé aux etats-unis et korée sont cité 
#le plus de fois par les autres inventeurs.


cpc_select = subset(tb2a,tb2a$appln_id == "363592032"|tb2a$appln_id =="365969867",
                    c(appln_id,cpc_class_symbol));cpc_select
with(cpc_select, piechart(cpc_class_symbol, xlab="", ylab="", 
                          main="cpc_class_symbol", col=rainbow_hcl(7), scale="percent"))
#les 2 brevets cité souvents sont dans la domaine H01M Y02M C25B et C01B.
ipc_select = subset(tb2b,tb2b$appln_id == "363592032"|tb2b$appln_id =="365969867",
                    c(appln_id,Ipc_class_symbol));ipc_select
with(ipc_select, piechart(Ipc_class_symbol, xlab="", ylab="", 
                          main="Ipc_class_symbol", col=rainbow_hcl(7), scale="percent"))
#les 2 brevets cité souvents sont dans la domaine C25B C01B C25D et H01M.
#interpretation::

with(tb3a, piechart(citing_appln_years, xlab="", ylab="", main="citing_appln_years", 
                    col=rainbow_hcl(6), scale="percent"))

#Question 3b
table_tb3b_ctipc4 <- table(tb3b$citing_ipc4);table_tb3b_ctipc4
dftable_tb3b_ctipc4<- data.frame(table_tb3b_ctipc4);dftable_tb3b_ctipc4
colnames(dftable_tb3b_ctipc4) <- c("cting_ipc4","Nbre_cting_ipc4")
dftable_tb3b_ctipc4$Sij = dftable_tb3b_ctipc4$Nbre_cting_ipc4 / sum(dftable_tb3b_ctipc4$Nbre_cting_ipc4)
dftable_tb3b_ctipc4$Sij_carée = I(dftable_tb3b_ctipc4$Sij)^2;dftable_tb3b_ctipc4

#Indice de generalite d<un brevet i
Indice = 1 - sum(dftable_tb3b_ctipc4$Sij_carée);Indice

#Question 4 
summary(tb4)
tb4$appln_id <-as.factor(tb4$appln_id) 
tb4$event_code <- as.factor(tb4$event_code)
tb4$event_publn_year <- as.factor(tb4$event_publn_year)
str(tb4)
colnames(tb4)

table_tb4_evcode_evyear <- table(tb4$event_code,tb4$event_publn_year);table_tb4_evcode_evyear
