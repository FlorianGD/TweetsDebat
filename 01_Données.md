Récupérer les tweets du débat du 20 mars 2017
================
Florian Gaudin-Delrieu
2017-03-21

``` r
library(rtweet)
```

    ## Welcome to rtweet v0.4.0!

``` r
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(stringr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(purrr)
```

Qu'ont dit les candidats lors du débat ?
========================================

Je voulais voir ce que les candidats ont dit pendant le débat télévisé du 20 mars 2017. Comme il n'y a pas (encore ?) de transcript, je vais récupérer les tweets envoyés par les comptes twitter officiels des 5 candidats, et ensuite nous pourrons analyser les résultats. Avant de commencer par tout le monde, essayons avec un candidat, puis nous généraliserons.

Un candidat Récupérons les tweets de Benoit Hamon
-------------------------------------------------

``` r
hamon <- get_timeline("benoithamon")
```

Y a-t-il les tweets du débat de dimanche ? \#' Le débat a commencé à 21h, mais les premiers tweets apparaîssent comme créés à 20h, peut être que le champ `created_at` est en UTC. Pour prendre de la marge, je vais prendre les tweets envoyés entre 19h et 2h du matin le lendemain.

``` r
heures_debat <- interval(ymd_hm("20170320_1900"), 
                         ymd_hm("20170321_0200"))

heures_hamon <- interval(min(hamon$created_at), max(hamon$created_at))

heures_debat %within% heures_hamon
```

    ## [1] TRUE

Le débat est bien inclus dans les heures des tweets récupérés. Filtrons pour récupérer les tweets envoyés pendant le débat.

``` r
hamon_debat <- hamon %>% 
  filter(created_at %within% heures_debat)

hamon_debat %>%  
  select(text, hashtags) %>% 
  head
```

    ##                                                                                                                                           text
    ## 1 Je vous propose de voter pour une République bienveillante, pour un futur désirable. Ensemble, faisons battre le cœ… https://t.co/RRZ9wzC8A6
    ## 2 Le 23 avril et le 7 mai, je vous propose un vote utile. Un vote qui vous est utile. Je vous propose de voter "pour"… https://t.co/fuc1sjQvah
    ## 3 Il faut regarder en face les défaillances de la République afin de remédier aux foyers de radicalisation sur notre territoire. #LeGrandDebat
    ## 4           Dans la lutte contre le terrorisme, il est indispensable de poursuivre le renforcement du renseignement territorial. #LeGrandDebat
    ## 5                 Je porterai à 2% du PIB les dépenses en matière de défense et d'actions internationales. #HamonDebat https://t.co/0qww7NPXG8
    ## 6 Mes propositions s’inscrivent dans une volonté claire de maintenir les capacités opérationnelles de la défense fran… https://t.co/1Rpo6zLne1
    ##       hashtags
    ## 1         <NA>
    ## 2         <NA>
    ## 3 LeGrandDebat
    ## 4 LeGrandDebat
    ## 5   HamonDebat
    ## 6         <NA>

Il reste 40 tweets, et ils ont l'air d'être ceux retranscrivant ses paroles pendant le débat.
\#\# Tous les candidats

``` r
candidats <- c("benoithamon", "JLMelenchon","EmmanuelMacron",
               "MLP_officiel", "FrancoisFillon")

tous_tweets <- map(candidats, get_timeline)
```

Vérifions que nous avons bien tout récupéré.

``` r
map_lgl(tous_tweets, ~heures_debat %within% interval(min(.x$created_at),
                                                     max(.x$created_at)))
```

    ## [1] TRUE TRUE TRUE TRUE TRUE

C'est bon, nous pouvons filtrer les résultats

``` r
tweets_debat <- map_df(tous_tweets, ~filter(.x, .x$created_at %within% heures_debat))

table(tweets_debat$screen_name)
```

    ## 
    ##    benoithamon EmmanuelMacron FrancoisFillon    JLMelenchon   MLP_officiel 
    ##             40             33             48            109            115

Nous pouvons enregistrer les données.

``` r
write_csv(tweets_debat, "tweets_debat.csv")
```
