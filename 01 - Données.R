#' ---
#' title: "Récupérer les tweets du débat du 20 mars 2017"
#' author: "Florian Gaudin-Delrieu"
#' date: "2017-03-21"
#' output: github_document
#' ---
  
library(rtweet)
library(tidyverse)
library(stringr)
library(lubridate)
library(purrr)

#' # Qu'ont dit les candidats lors du débat ? 
#' 
#' Je voulais voir ce que les
#' candidats ont dit pendant le débat télévisé du 20 mars 2017. Comme il n'y a
#' pas (encore ?) de transcript, je vais récupérer les tweets envoyés par les
#' comptes twitter officiels des 5 candidats, et ensuite nous pourrons analyser
#' les résultats. Avant de commencer par tout le monde, essayons avec un
#' candidat, puis nous généraliserons.
#' 
#' ## Un candidat Récupérons les tweets de Benoit Hamon
hamon <- get_timeline("benoithamon")

#' Y a-t-il les tweets du débat de dimanche ? #' Le débat a commencé à 21h, mais
#' les premiers tweets apparaîssent comme créés à 20h, peut être que le champ
#' `created_at` est en UTC. Pour prendre de la marge, je vais prendre les tweets
#' envoyés entre 19h et 2h du matin le lendemain.

heures_debat <- interval(ymd_hm("20170320_1900"), 
                         ymd_hm("20170321_0200"))

heures_hamon <- interval(min(hamon$created_at), max(hamon$created_at))

heures_debat %within% heures_hamon

#' Le débat est bien inclus dans les heures des tweets récupérés.
#' Filtrons pour récupérer les tweets envoyés pendant le débat.

hamon_debat <- hamon %>% 
  filter(created_at %within% heures_debat)

hamon_debat %>%  
  select(text, hashtags) %>% 
  head

#' Il reste 40 tweets, et ils ont l'air d'être ceux retranscrivant ses paroles
#' pendant le débat.  

#' ## Tous les candidats

candidats <- c("benoithamon", "JLMelenchon","EmmanuelMacron",
               "MLP_officiel", "FrancoisFillon")

tous_tweets <- map(candidats, get_timeline)

#' Vérifions que nous avons bien tout récupéré.
map_lgl(tous_tweets, ~heures_debat %within% interval(min(.x$created_at),
                                                     max(.x$created_at)))
#' C'est bon, nous pouvons filtrer les résultats
tweets_debat <- map_df(tous_tweets, ~filter(.x, .x$created_at %within% heures_debat))

table(tweets_debat$screen_name)
#' Nous pouvons enregistrer les données.

write_csv(tweets_debat, "tweets_debat.csv")
