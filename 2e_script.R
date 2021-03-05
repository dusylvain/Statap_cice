library(tidyverse)
library(tseries)
library("Synth")

#import bases
capital=read_csv("Statistical_Capital.csv")
croiss=read_csv("Statistical_Growth-Accounts.csv")
travail=read_csv("Statistical_Labour.csv")
compte=read_csv("Statistical_National-Accounts.csv")
description=read_csv("Variable-Description.csv")
autre_pays=read_csv("WB_data2.csv")

codes <- read_csv("codes_pays.csv") %>% rename(country = `Country Name, Full`, country2 = `ISO2-digit Alpha`, country3 = `ISO3-digit Alpha`)
taux_change <- read_csv("taux_change.csv")


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
  select(country3, year, exports_hab, pop, `unemployment rate`) 

head(autre_pays)
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


#modele bivarie pour VA_Q_hab
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
                           list("unemployment rate",2012,"mean"),
                           list("VA_Q_hab",2000,"mean"),
                           list("VA_Q_hab",2002,"mean"),
                           list("VA_Q_hab",2004,"mean"),
                           list("VA_Q_hab",2006,"mean"),
                           list("VA_Q_hab",2007,"mean"),
                           list("VA_Q_hab",2008,"mean"),
                           list("VA_Q_hab",2009,"mean"),
                           list("VA_Q_hab",2010,"mean"),
                           list("VA_Q_hab",2011,"mean"),
                           list("VA_Q_hab",2012,"mean")),
                         time.predictors.prior = c(2000:2012), #période pré-traitement
                         dependent="VA_Q_hab", #variable d'intérêt
                         unit.variable = "id_country", #colonne avec les index des pays
                         treatment.identifier = 11, # n° de l'unité traitée
                         controls.identifier = c(1:10,12:21,23:30), #n° des unités de contrôles
                         time.variable = "year", #colonne où se trouve la date
                         time.optimize.ssr = c(2000:2012), #période de maximisation
                         time.plot=2000:2017) #période tracée sur le graphique


synth.out <- synth(dataprep.out)

round(synth.out$solution.w,2)

path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)


