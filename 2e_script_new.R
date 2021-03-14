library(tidyverse)
library(tseries)
library("Synth")


setwd("~/ENSAE/projet stat_app/Data/Données")

#import bases
capital=read_csv("Statistical_Capital.csv")
croiss=read_csv("Statistical_Growth-Accounts.csv")
travail=read_csv("Statistical_Labour.csv")
compte=read_csv("Statistical_National-Accounts.csv")
description=read_csv("Variable-Description.csv")
autre_pays=read_csv("WB_data2.csv")

codes <- read_csv("codes_pays.csv") %>% rename(country = `Country Name, Full`, country2 = `ISO2-digit Alpha`, country3 = `ISO3-digit Alpha`)
taux_change <- read_csv("taux_change.csv")
autres_donnees <- read_csv("donnees_complementaires.csv")


#pivot_longer : pour faire passer les annees en lignes
#pivot_wider : pour faire passer les variables en colonnes
#filter(code = "TOT") : pour ne garder que les valeurs agregees
compte <- compte %>% pivot_longer(c(7:29), names_to = "year", values_to = "value") %>%
  pivot_wider(names_from = "var", values_from = "value") %>%
  filter(code == "TOT") %>%
  select(c(country, year, COMP, H_EMPE, LP_QI, VA_PI, VA_Q)) %>%
  rename(country2 = country)


#str_replace_all : pour corriger le pb de nomenclature d'EU KLEMS pour G-B et Grece
compte <- compte %>% mutate(country2 = str_replace_all(compte$country2, c("EL" = "GR", "UK" = "GB"))) %>%
  inner_join(codes, by = "country2")


#meme manip que pour compte
autre_pays <- autre_pays %>% select(-c(Pays_indic, `Country Name`, `Indicator Name`)) %>%
  pivot_longer(c(2:20), names_to = "year", values_to = "value") %>%
  pivot_wider(names_from = "Simple Indicator", values_from = "value") %>%
  rename(country3 = `Country Code`) %>%
  select(country3, year,pop,`unemployment rate`, gdb_hab, FBCF, exports_rate) %>%
  rename(gdp_hab = gdb_hab)

#creation d'un index numérique pour chaque pays
id <- count(compte,country2) %>% mutate(id_country = row_number()) %>% select(-n)

#corriger les pb de type
compte$year <- as.numeric(compte$year)
autre_pays$year <- as.numeric(autre_pays$year)

#join avec autre pays et id
compte <- compte %>% inner_join(autre_pays, by = c("country3", "year")) %>%
  inner_join(id, by = "country2")

compte <- as.data.frame(compte)


#1er modele univarie pour chomage avec juste mean
dataprep.out <- dataprep(foo=compte,
                             predictors = "unemployment rate", #Les variables utilisées en contrôle
                             predictors.op = "mean", #La manière dont on les prend en compte
                             time.predictors.prior = c(2000:2012), #période pré-traitement
                             dependent="unemployment rate", #variable d'intérêt
                             unit.variable = "id_country", #colonne avec les index des pays
                             treatment.identifier = 11, # n° de l'unité traitée
                             controls.identifier = c(1:10,12:30), #n° des unités de contrôles
                             time.variable = "year", #colonne où se trouve la date
                             time.optimize.ssr = c(2000:2012), #période de maximisation
                             time.plot=2000:2017) #période tracée sur le graphique

synth.out <- synth(dataprep.out)

round(synth.out$solution.w,2)

path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)

#fit horrible mais modele hyper simpliste


#idem avec plein d'annees
dataprep.out <- dataprep(foo=compte,
                         special.predictors = list(
                           list("unemployment rate",2000,"mean"),
                           list("unemployment rate",2002,"mean"),
                           list("unemployment rate",2004,"mean"),
                           list("unemployment rate",2006,"mean"),
                           list("unemployment rate",2007,"mean"),
                           list("unemployment rate",2008,"mean"),
                           list("unemployment rate",2009,"mean"),
                           list("unemployment rate",2010,"mean"),
                           list("unemployment rate",2011,"mean"),
                           list("unemployment rate",2012,"mean")),
                         time.predictors.prior = c(2000:2012), #période pré-traitement
                         dependent="unemployment rate", #variable d'intérêt
                         unit.variable = "id_country", #colonne avec les index des pays
                         treatment.identifier = 11, # n° de l'unité traitée
                         controls.identifier = c(1:8,12,14:16,18:30), #n° des unités de contrôles
                         time.variable = "year", #colonne où se trouve la date
                         time.optimize.ssr = c(2000:2012), #période de maximisation
                         time.plot=2000:2017) #période tracée sur le graphique


synth.out <- synth(dataprep.out)

round(synth.out$solution.w,2)

path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)



#autre pays (données Banque Mondiale)


id2 <- count(autre_pays,country3) %>% mutate(id_country = row_number()) %>% select(-n)

autre_pays <- autre_pays %>% inner_join(id2, by = "country3")

autre_pays <- as.data.frame(autre_pays)


#univarié

#chômage

dataprep.out <- dataprep(foo=autre_pays,
                         special.predictors = list(
                           list("unemployment rate",2000,"mean"),
                           list("unemployment rate",2002,"mean"),
                           list("unemployment rate",2004,"mean"),
                           list("unemployment rate",2006,"mean"),
                           list("unemployment rate",2008,"mean"),
                           list("unemployment rate",2009,"mean"),
                           list("unemployment rate",2010,"mean"),
                           list("unemployment rate",2011,"mean"),
                           list("unemployment rate",2012,"mean")),
                         time.predictors.prior = c(2000:2012), #période pré-traitement
                         dependent="unemployment rate", #variable d'intérêt
                         unit.variable = "id_country", #colonne avec les index des pays
                         treatment.identifier = 50, # n° de l'unité traitée
                         controls.identifier = c(1:49,51:145), #n° des unités de contrôles
                         time.variable = "year", #colonne où se trouve la date
                         time.optimize.ssr = c(2000:2012), #période de maximisation
                         time.plot=2000:2018) #période tracée sur le graphique


synth.out <- synth(dataprep.out)

poids <- round(synth.out$solution.w,2) %>% as.data.frame() %>% rownames_to_column() %>%
  rename(id_country = rowname, poids = w.weight) 
poids$id_country <- as.numeric(poids$id_country)
poids <- poids %>% left_join(id2, by = "id_country") %>% relocate(country3) %>% arrange(desc(poids))

path.plot(dataprep.res = dataprep.out,synth.res = synth.out, Ylab = "unemployment")

gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)


#gdp_hab

dataprep.out <- dataprep(foo=autre_pays,
                         special.predictors = list(
                           list("gdp_hab",2000,"mean"),
                           list("gdp_hab",2002,"mean"),
                           list("gdp_hab",2004,"mean"),
                           list("gdp_hab",2006,"mean"),
                           list("gdp_hab",2008,"mean"),
                           list("gdp_hab",2009,"mean"),
                           list("gdp_hab",2010,"mean"),
                           list("gdp_hab",2011,"mean"),
                           list("gdp_hab",2012,"mean")),
                         time.predictors.prior = c(2000:2012), #période pré-traitement
                         dependent="gdp_hab", #variable d'intérêt
                         unit.variable = "id_country", #colonne avec les index des pays
                         treatment.identifier = 50, # n° de l'unité traitée
                         controls.identifier = c(1:49,51:145), #n° des unités de contrôles
                         time.variable = "year", #colonne où se trouve la date
                         time.optimize.ssr = c(2000:2012), #période de maximisation
                         time.plot=2000:2018) #période tracée sur le graphique


synth.out <- synth(dataprep.out)

poids <- round(synth.out$solution.w,2) %>% as.data.frame() %>% rownames_to_column() %>%
  rename(id_country = rowname, poids = w.weight) 
poids$id_country <- as.numeric(poids$id_country)
poids <- poids %>% left_join(id2, by = "id_country") %>% relocate(country3)

path.plot(dataprep.res = dataprep.out,synth.res = synth.out, Ylab = "gdp_hab")

gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)


#gdp_hab base 100

autre_pays <- autre_pays %>% group_by(country3) %>% mutate(gdp_hab2000 = gdp_hab[1]) %>%
  mutate(gdp_hab100 = gdp_hab / gdp_hab2000*100)

autre_pays <- as.data.frame(autre_pays)

dataprep.out <- dataprep(foo=autre_pays,
                         special.predictors = list(
                           list("gdp_hab100",2001,"mean"),
                           list("gdp_hab100",2003,"mean"),
                           list("gdp_hab100",2004,"mean"),
                           list("gdp_hab100",2006,"mean"),
                           list("gdp_hab100",2008,"mean"),
                           list("gdp_hab100",2009,"mean"),
                           list("gdp_hab100",2010,"mean"),
                           list("gdp_hab100",2011,"mean"),
                           list("gdp_hab100",2012,"mean")),
                         time.predictors.prior = c(2000:2012), #période pré-traitement
                         dependent="gdp_hab100", #variable d'intérêt
                         unit.variable = "id_country", #colonne avec les index des pays
                         treatment.identifier = 50, # n° de l'unité traitée
                         controls.identifier = c(1:49,51:145), #n° des unités de contrôles
                         time.variable = "year", #colonne où se trouve la date
                         time.optimize.ssr = c(2001:2012), #période de maximisation
                         time.plot=2000:2018) #période tracée sur le graphique


synth.out <- synth(dataprep.out)

poids <- round(synth.out$solution.w,2) %>% as.data.frame() %>% rownames_to_column() %>%
  rename(id_country = rowname, poids = w.weight) 
poids$id_country <- as.numeric(poids$id_country)
poids <- poids %>% left_join(id2, by = "id_country") %>% relocate(country3)

path.plot(dataprep.res = dataprep.out,synth.res = synth.out, Ylab = "gdp_hab")

gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)

#sans endogénéité

dataprep.out <- dataprep(foo=autre_pays,
                         special.predictors = list(
                           list("gdp_hab100",2002,"mean"),
                           list("gdp_hab100",2004,"mean"),
                           list("gdp_hab100",2006,"mean"),
                           list("gdp_hab100",2008,"mean"),
                           list("gdp_hab100",2009,"mean"),
                           list("gdp_hab100",2010,"mean"),
                           list("gdp_hab100",2011,"mean"),
                           list("gdp_hab100",2012,"mean")),
                         time.predictors.prior = c(2000:2012), #période pré-traitement
                         dependent="gdp_hab100", #variable d'intérêt
                         unit.variable = "id_country", #colonne avec les index des pays
                         treatment.identifier = 50, # n° de l'unité traitée
                         controls.identifier = c(1:49,51:57,59:65,67:69,71:145), #n° des unités de contrôles
                         time.variable = "year", #colonne où se trouve la date
                         time.optimize.ssr = c(2001:2012), #période de maximisation
                         time.plot=2000:2018) #période tracée sur le graphique


synth.out <- synth(dataprep.out)

poids <- round(synth.out$solution.w,2) %>% as.data.frame() %>% rownames_to_column() %>%
  rename(id_country = rowname, poids = w.weight) 
poids$id_country <- as.numeric(poids$id_country)
poids <- poids %>% left_join(id2, by = "id_country") %>% relocate(country3) %>% arrange(desc(poids))

path.plot(dataprep.res = dataprep.out,synth.res = synth.out, Ylab = "gdp_hab")

gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)


#taux croissance

autre_pays <- autre_pays %>% group_by(country3) %>% mutate(d_gdp = (gdp_hab - lag(gdp_hab))/lag(gdp_hab))

autre_pays <- as.data.frame(autre_pays)

dataprep.out <- dataprep(foo=autre_pays,
                         special.predictors = list(
                           list("d_gdp",2001,"mean"),
                           list("d_gdp",2003,"mean"),
                           list("d_gdp",2004,"mean"),
                           list("d_gdp",2006,"mean"),
                           list("d_gdp",2008,"mean"),
                           list("d_gdp",2009,"mean"),
                           list("d_gdp",2010,"mean"),
                           list("d_gdp",2011,"mean"),
                           list("d_gdp",2012,"mean")),
                         time.predictors.prior = c(2001:2012), #période pré-traitement
                         dependent="d_gdp", #variable d'intérêt
                         unit.variable = "id_country", #colonne avec les index des pays
                         treatment.identifier = 50, # n° de l'unité traitée
                         controls.identifier = c(1:49,51:145), #n° des unités de contrôles
                         time.variable = "year", #colonne où se trouve la date
                         time.optimize.ssr = c(2001:2012), #période de maximisation
                         time.plot=2001:2018) #période tracée sur le graphique

synth.out <- synth(dataprep.out)

poids <- round(synth.out$solution.w,2) %>% as.data.frame() %>% rownames_to_column() %>%
  rename(id_country = rowname, poids = w.weight) 
poids$id_country <- as.numeric(poids$id_country)
poids <- poids %>% left_join(id2, by = "id_country") %>% relocate(country3) %>% arrange(desc(poids))

path.plot(dataprep.res = dataprep.out,synth.res = synth.out, Ylab = "d_gdp_hab", Ylim = c(-0.2,0.4))

gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)








#multivarié

autre_pays2 <- autre_pays %>% inner_join(autres_donnees, by = c("country3", "year")) %>%
  select(-id_country)

autre_pays2 <- autre_pays2 %>% group_by(country3) %>%
  mutate(salaire_minimum = mean(`salaire horaire minimum en US$ PPA`, na.rm = TRUE),
         protection_emploi = mean(`indice protection de l'emploi`, na.rm = TRUE),
         diplome = mean(`% des 25-64ans diplomes du superieur`, na.rm = TRUE)) %>%
  filter(!(is.nan(salaire_minimum) | is.nan(protection_emploi) | is.nan(diplome))) %>%
  ungroup()

id2 <- count(autre_pays2,country3) %>% mutate(id_country = row_number()) %>% select(-n)

autre_pays2 <- autre_pays2 %>% inner_join(id2, by = "country3") %>% as.data.frame()



dataprep.out <- dataprep(foo=autre_pays2,
                         predictors = c("taux d'imposition sur bénéfices",
                                        "salaire_minimum",
                                        "protection_emploi",
                                        "diplome",
                                        "gdp_hab"), #Les variables utilisées en contrôle
                         predictors.op = "mean", #La manière dont on les prend en compte
                         special.predictors = list(
                           list("unemployment rate",2012,"mean")),
                         time.predictors.prior = c(2000:2012), #période pré-traitement
                         dependent="unemployment rate", #variable d'intérêt
                         unit.variable = "id_country", #colonne avec les index des pays
                         treatment.identifier = 12, # n° de l'unité traitée
                         controls.identifier = c(1:9,11,13,15:32), #n° des unités de contrôles
                         time.variable = "year", #colonne où se trouve la date
                         time.optimize.ssr = c(2000:2012), #période de maximisation
                         time.plot=2000:2018) #période tracée sur le graphique


synth.out <- synth(dataprep.out)

poids <- round(synth.out$solution.w,2) %>% as.data.frame() %>% rownames_to_column() %>%
  rename(id_country = rowname, poids = w.weight) 
poids$id_country <- as.numeric(poids$id_country)
poids <- poids %>% left_join(id2, by = "id_country") %>% relocate(country3) %>% arrange(desc(poids))


path.plot(dataprep.res = dataprep.out,synth.res = synth.out, Ylab = "unemployment")

gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)




#pour avoir taux de change en 2010 (car grandeurs reelles sont exprimées en prix 2010)

taux_change2010 <- taux_change %>% filter(year == 2010) %>%
  rename(`rate2010 in NAT/US$` = `rate in NAT/US$`) %>% select(-year)

compte <- compte %>% inner_join(taux_change2010, by = "country3")

#creation PIB par hab (VA_Q_hab) en valeur reelle en dollar 2010
compte <- compte %>% mutate(VA_Q_hab = VA_Q/(pop*`rate2010 in NAT/US$`))

#imputation de 3 valeurs manquantes pour VA_Q_hab en supposant même taux de croissance qu'entre les 2 dernières périodes où les données sont disponibles

#Croatie 2017
compte[234,15] <- 0.012891387

#Japon 2016 et 2017
compte[305,15] <- 0.047363271
compte[306,15] <- 0.047973711

