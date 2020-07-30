# Data breach notifications analyses ####
# V Looten
# Last modification : 2020-03-29


# Load packages  ####
listepackages<-c("tidyverse","lubridate","compareGroups")
for (pack in listepackages) {
  if (!is.element(pack, installed.packages()[,1])){
    install.packages(pack, dep = TRUE)
  }
  eval(parse(text=paste0("library(",pack,")")))
}
rm(pack)


# Import data and data management ####
breach <- read.csv2("https://www.data.gouv.fr/fr/datasets/r/4c176588-a444-4dc7-b6bf-60390ae7e5be", stringsAsFactors = FALSE)
breach <- breach[,colSums(is.na(breach))<nrow(breach)]
# Definitions of outcomes
# Year: 2018, 2019
breach$year <- year(ymd(paste0(as.character(breach$Date.de.réception.de.la.notification),"-01")))
breach$year <- as.factor(breach$year)
# Type of violation : Loss_of_confidentiality, Loss_of_integrity, Loss_of_availability  
breach$Loss_of_confidentiality <- 0
breach$Loss_of_confidentiality[grep("Perte de la confidentialité", breach$Natures.de.la.violation, fixed = T)] <- 1
breach$Loss_of_integrity <- 0
breach$Loss_of_integrity[grep("Perte de l'intégrité", breach$Natures.de.la.violation, fixed = T)] <- 1
breach$Loss_of_availability <- 0
breach$Loss_of_availability[grep("Perte de la disponibilité", breach$Natures.de.la.violation, fixed = T)] <- 1
breach$Loss_of_availability <- as.factor(breach$Loss_of_availability)
breach$Loss_of_integrity <- as.factor(breach$Loss_of_integrity)
breach$Loss_of_confidentiality <- as.factor(breach$Loss_of_confidentiality)
# Cause (1): internal, external, unknown
breach$cause_1 <- "unknown"
breach$cause_1[grep("interne", breach$Causes.de.l.incident, fixed = T)] <- "Internal"
breach$cause_1[grep("externe", breach$Causes.de.l.incident, fixed = T)] <- "External"
breach$cause_1 <- as.factor(breach$cause_1)
# Cause (2): Accidental, Malicious, unknown
breach$cause_2 <- "unknown"
breach$cause_2[grep("accidentel", breach$Causes.de.l.incident, fixed = T)] <- "Accidental"
breach$cause_2[grep("malveillant", breach$Causes.de.l.incident, fixed = T)] <- "Malicious"
breach$cause_2 <- as.factor(breach$cause_2)
# Institution
# dput(names(table(breach$Secteur.d.activité.de.l.organisme.concerné)))
c("Activités de services administratifs et de soutien", "Activités des ménages en tant qu''employeurs ; activités indifférenciées des ménages en tant que producteurs de biens et services pour usage propre", 
  "Activités extra-territoriales", "Activités financières et d''assurance", 
  "Activités immobilières", "Activités spécialisées, scientifiques et techniques", 
  "Administration publique", "Agriculture, sylviculture et pêche", 
  "Arts, spectacles et activités récréatives", "Autres activités de services", 
  "Commerce ; réparation d''automobiles et de motocycles", "Construction", 
  "Enseignement", "Hébergement et restauration", "Inconnu", "Industrie manufacturière", 
  "Information et communication", "Production et distribution d''eau ; assainissement, gestion des déchets et dépollution", 
  "Production et distribution d''électricité, de gaz, de vapeur et d''air conditionné", 
  "Santé humaine et action sociale", "Transports et entreposage"
)
breach$institution <- "Other"
breach$institution[grep("Santé humaine et action sociale", breach$Secteur.d.activité.de.l.organisme.concerné, fixed = T)] <- "Health and social work"
breach$institution[unique(c(grep("Enseignement",breach$Secteur.d.activité.de.l.organisme.concerné, fixed = T),grep("Activités spécialisées, scientifiques et techniques", breach$Secteur.d.activité.de.l.organisme.concerné, fixed = T)))] <- "Science and education"
breach$institution[unique(c(grep("Administration publique",breach$Secteur.d.activité.de.l.organisme.concerné, fixed = T),grep("Activités de services administratifs et de soutien", breach$Secteur.d.activité.de.l.organisme.concerné, fixed = T)))] <- "Administration"
breach$institution[grep("Activités financières et d''assurance", breach$Secteur.d.activité.de.l.organisme.concerné, fixed = T)] <- "Financial and insurance activities"
breach$institution[grep("Hébergement et restauration", breach$Secteur.d.activité.de.l.organisme.concerné, fixed = T)] <- "Accommodation and food services"
breach$institution[grep("Information et communication", breach$Secteur.d.activité.de.l.organisme.concerné, fixed = T)] <- "Information and communication sectors"
commercial_industrial <- c("Commerce ; réparation d''automobiles et de motocycles",
  "Industrie manufacturière",
  "Production et distribution d''électricité, de gaz, de vapeur et d''air conditionné",
  "Production et distribution d''eau ; assainissement, gestion des déchets et dépollution",
  "Transports et entreposage")
idx_com <- unique(c(grep(commercial_industrial[1],breach$Secteur.d.activité.de.l.organisme.concerné, fixed = T),
         grep(commercial_industrial[2], breach$Secteur.d.activité.de.l.organisme.concerné, fixed = T),
         grep(commercial_industrial[3], breach$Secteur.d.activité.de.l.organisme.concerné, fixed = T),
         grep(commercial_industrial[4], breach$Secteur.d.activité.de.l.organisme.concerné, fixed = T),
         grep(commercial_industrial[5], breach$Secteur.d.activité.de.l.organisme.concerné, fixed = T)   ))
breach$institution[idx_com] <- "Commercial and industrial sectors"
breach$institution <- as.factor(breach$institution)
breach$Nombre.de.personnes <- as.factor(breach$Nombre.de.personnes)
#
breach$NIR <- 0
breach$NIR[grep("NIR",breach$Typologie.des.données.impactées,fixed=T)] <- 1
breach$NIR <- as.factor(breach$NIR)
#
breach$Health <- as.factor(as.numeric(breach$institution=="Health and social work"))
#
breach$number <- as.factor(breach$Nombre.de.personnes.impactées)
#
breach$institution_health <- 0
breach$institution_health[which(breach$institution=="Health and social work")] <- 1
# dput(colnames(breach))

var_analysis <- c("Nombre.de.personnes.impactées","Information.des.personnes", "year", "Loss_of_confidentiality", 
                  "Loss_of_integrity", "Loss_of_availability", "cause_1", "cause_2", 
                  "institution", "NIR") 
resu0 <- compareGroups(institution ~ number + year + Loss_of_confidentiality + Loss_of_integrity + Loss_of_availability + cause_1 + cause_2 + NIR , data = breach[which(!(breach$institution %in% c("Other","Accommodation and food services","Commercial and industrial sectors"))),])
createTable(resu0) %>% export2xls("table1.xls")
resu1 <- compareGroups(NIR ~ number + year + Loss_of_confidentiality + Loss_of_integrity + Loss_of_availability + cause_1 + cause_2 + institution , data = breach)
createTable(resu1) %>% export2xls("table2.xls")
resu2 <- compareGroups(Health ~ number + year + Loss_of_confidentiality + Loss_of_integrity + Loss_of_availability + cause_1 + cause_2 + institution , data = breach)
createTable(resu2) %>% export2xls("table3.xls")
resu3 <- compareGroups(NIR ~ number + year + Loss_of_confidentiality + Loss_of_integrity + Loss_of_availability + cause_1 + cause_2 + institution , data = breach[which(breach$Health==1),])
createTable(resu3) %>% export2xls("table4.xls")
resu4 <- compareGroups(number ~ institution + year + Loss_of_confidentiality + Loss_of_integrity + Loss_of_availability + cause_1 + cause_2 + NIR , data = breach)
createTable(resu4) %>% export2xls("table5.xls")
resu4b <- compareGroups(number ~ institution + year + Loss_of_confidentiality + Loss_of_integrity + Loss_of_availability + cause_1 + cause_2 + NIR , data = breach,byrow = T)
createTable(resu4b) %>% export2xls("table6.xls")

levels(breach$number) <- c("1-Entre 0 et 5 personnes", "4-Entre 301 et 5000 personnes", "3-Entre 51 et 300 personnes", 
                           "2-Entre 6 et 50 personnes", "5-Plus de 5000 personnes")
breach$number <- factor(breach$number, c("1-Entre 0 et 5 personnes","2-Entre 6 et 50 personnes", "3-Entre 51 et 300 personnes", "4-Entre 301 et 5000 personnes", 
                                           "5-Plus de 5000 personnes"))

# dput(levels(breach$number))

# 
# 
# data <- data[data$"Données.sensibles"=="Oui",]
# data$"Typologie.des.données.impactées" <- as.character(data$"Typologie.des.données.impactées")
# 
# table((ymd(paste0(as.character(data$Date.de.réception.de.la.notification),"-01"))))
# table(data$"Nombre.de.personnes.impactées")
# table(data$"Secteur.d.activité.de.l.organisme.concerné")
# table(data$"Natures.de.la.violation")
# data <- data[grep("NIR",data$Typologie.des.données.impactées,fixed=T),]
# write.csv2(table(data$"Typologie.des.données.impactées"),"type.csv")
