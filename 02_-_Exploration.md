# Débat télévisé du 20 mars 2017
Florian Gaudin-Delrieu  
2017-03-21  
# Qu'ont dit les candidats lors du débat ? 
 
Nous avons récupéré les tweets envoyés par les candidats lors du débat télévisé du 20 mars 2017. Ces tweets par les comptes officiels approximeront le transcript de leurs paroles. Je vais utiliser la librairie `tidytext` pour l'analyse du contenu des tweets.  
_Note_ : les couleurs choisies sur les graphes sont celles de leurs partis respectifs comme définies sur Wikipedia.


```r
library(tidytext)
library(tidyverse)
library(stringr)
library(purrr)
library(forcats)
library(cowplot)
library(RColorBrewer)

tweets_debat <- read_csv("tweets_debat.csv", 
                         col_types = cols(
                           .default = col_character(),
                           user_id = col_integer(),
                           created_at = col_datetime(format = ""),
                           status_id = col_double(),
                           retweet_count = col_integer(),
                           favorite_count = col_integer(),
                           is_quote_status = col_logical(),
                           quote_status_id = col_double(),
                           is_retweet = col_logical(),
                           retweet_status_id = col_double(),
                           in_reply_to_status_status_id = col_double(),
                           in_reply_to_status_user_id = col_integer(),
                           media_id = col_double()
                         ))
tweets_debat <- tweets_debat %>% 
  mutate(nom = factor(paste0("@",screen_name))) %>%
  select(-screen_name, -user_id, -status_id, -lang, -urls, -in_reply_to_status_status_id,
         -in_reply_to_status_user_id, -source, -media_id, -media_url, -media_url_expanded, -c(coordinates:bounding_box_type)) %>% 
  select(nom, everything())

couleurs <- c("#FF8080", "#BA55D3", "#0066CC", "#C6442E", "#C0C0C0")
```

## Exploration rapide des tweets

Qui a le plus tweeté ?

```r
ggplot(tweets_debat, aes(x = nom)) +
  geom_bar(aes(fill = nom), show.legend = FALSE) +
  scale_fill_manual(values = couleurs) +
  theme_minimal() +
  labs(title = "Marine Le Pen et Jean-Luc Mélenchon ont le plus tweeté",
       x = NULL,
       y = NULL)
```

![](02_-_Exploration_files/figure-html/nb_tweets-1.png)<!-- -->

Marine Le Pen et Jean-Luc Mélenchon ont nettement plus tweeté que les autres candidats.  

Y a-t-il une forte proportion de retweets ?

```r
with(tweets_debat, tapply(is_retweet, nom, mean))
```

```
##    @benoithamon @EmmanuelMacron @FrancoisFillon    @JLMelenchon 
##       0.0000000       0.0000000       0.0000000       0.1559633 
##   @MLP_officiel 
##       0.0000000
```

Pour Jean-Luc Mélenchon, environ 16% des tweets envoyés pendant le débat sont des retweets, aucun pour les autres candidats. Nous allons filtrer les retweets, vu qu'ils ne représentent pas ce qu'a dit le candidat pendant le débat. Les colonnes `is_quote_status`, `quote_status_id` et `retweet_status_id` peuvent aussi être filtrées, vu qu'elles n'apportent aucune information.

```r
tweets_debat <- tweets_debat %>% 
  filter(!is_retweet) %>% 
  select(-c(is_quote_status:retweet_status_id))
```

## Utilisation de `tidytext`

Nous allons maintenant pouvoir commencer à analyser le contenu des tweets. J'utilise ici une expression conseillée sur le site [tidy text mining](http://tidytextmining.com/twitter.html#word-frequencies-1). Cela enlève les liens, mais conserve les hashtags et les mentions.


```r
tidy_tweet <- tweets_debat %>%
  select(1:3) %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", 
                                "")) %>% 
  unnest_tokens(mot, text)
```

Quels sont les mots les plus employés par chaque candidat ?

```r
tidy_tweet %>% 
  group_by(nom) %>% 
  count(mot, sort = TRUE)
```

```
## Source: local data frame [2,226 x 3]
## Groups: nom [5]
## 
##                nom          mot     n
##             <fctr>        <chr> <int>
## 1    @MLP_officiel     débattf1   108
## 2    @MLP_officiel legranddébat   106
## 3     @JLMelenchon legranddébat    86
## 4     @JLMelenchon     débattf1    76
## 5    @MLP_officiel           de    66
## 6    @MLP_officiel           la    64
## 7     @JLMelenchon           de    60
## 8  @FrancoisFillon           de    56
## 9     @JLMelenchon           la    52
## 10    @benoithamon           de    49
## # ... with 2,216 more rows
```

Ce résultat n'est pas très intéressant, on trouve les mots clés du débat (débattf1 et legranddébat), ansi que des mots très communs. Nous allons filtrer les mots les plus communs à partir de la liste de `stopwords("french")` du package `tm`. J'ai ajouté les mots "a" et "c'est", qui ne sont pas dans la liste des stopwords, mais n'apportent pas d'information.

```r
tidy_tweet <- tidy_tweet %>% 
  filter(!mot %in% c("débattf1","legranddébat", "legranddebat", "debattf1", "hamondébat", "hamondebat", "marine2017", "a", "c'est", tm::stopwords("french")))
```

Faisons un graphe rapide des mots les plus employés par chaque candidat.

```r
top_split <- tidy_tweet %>% 
  count(mot, nom) %>% 
  group_by(nom) %>% 
  top_n(5, n) %>% 
  ungroup() %>% 
  nest(-nom)  %>% 
  mutate(top_ordre = map(data, "mot")) %>% 
  mutate(top_ordre = map(data, ~mutate(.x, mot = fct_reorder(mot, n, max)))) %>% 
  arrange(nom)

plot_list <- map2(top_split$top_ordre, couleurs, function(x, y) {
  ggplot(x, aes(x = mot, y = n)) + 
    geom_col(fill = y, show.legend = FALSE) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(plot.margin = unit(c(0.6,0,0,0), "cm"))})

plot_grid(plotlist = plot_list, nrow = 2, labels = top_split$nom, hjust = -0.2)
```

![](02_-_Exploration_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Tous les candidats ont le mot "veux" dans le top 5.

## Mots caractéristiques des candidats

Nous allons essayer de trouver des mots caractéristiques pour chaque candidat. Pour ce faire, nous allons regarder la mesure dîte `tf-idf` pour `term frequency, inverse document frequency`. Cela revient à mettre un score élevé à un mot s'il apparaît fréquemment pour un candidat mais pas chez les autres.

```r
mot_tfidf <- tidy_tweet %>% 
  count(mot, nom) %>% 
  bind_tf_idf(mot, nom, n) %>% 
  group_by(nom) %>% 
  top_n(5, tf_idf)

mot_tfidf %>%
  ggplot(aes(x = fct_reorder(mot, tf_idf, max), y = tf_idf)) +
  geom_col(aes(fill = nom), show.legend = FALSE) +
  facet_wrap(~nom, scales = "free") +
  labs(x = NULL,
       y = NULL,
       title = "Mots les moins communs utilisés par chaque candidat") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        strip.text = element_text(size = 10),
        strip.background = element_blank()) +
  scale_fill_manual(values = couleurs)
```

![](02_-_Exploration_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Regardons les tweets qui contiennent les mots arrivant au début des graphes ci-dessus. Cela permettra de les remettre en contexte, et de voir si notre algorithme est plutôt fiable ou non.


```r
add_bold_color <- function(color){
  force(color)
  function(x){
    str_c("<b style=\"background-color :", color, ";\">", x, "</b>")
  }
}

liste_fun <- map(couleurs, add_bold_color)

top_mots <- mot_tfidf %>% 
  top_n(2, tf_idf) %>% 
  select(nom, mot) %>% 
  arrange(nom)

patterns <- top_mots %>% 
  group_by(nom) %>% 
  summarise(pattern = str_c(mot, collapse = "|")) %>% 
  `$`(pattern)

table_tweets <- tweets_debat %>% 
  filter(str_detect(text, str_c(patterns, collapse = "|"))) %>% 
  select(nom, text)

for(i in 1:5){
  table_tweets$text <- str_replace_all(str_to_lower(table_tweets$text), patterns[i], liste_fun[[i]])
}

knitr::kable(table_tweets)
```



nom               text                                                                                                                                                                                                                                                                       
----------------  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
@benoithamon      je porterai à 2% du pib les dépenses en matière de défense et d'actions inter<b style="background-color :#C0C0C0;">nationale</b>s. #hamondebat https://t.co/0qww7npxg8                                                                                                     
@benoithamon      nous devons penser la protection sociale de demain : je propose le <b style="background-color :#ff8080;">revenu</b> <b style="background-color :#ff8080;">universel</b> qui éradiquera la précarité… https://t.co/lmko0ehenu                                               
@benoithamon      avec le <b style="background-color :#ff8080;">revenu</b> <b style="background-color :#ff8080;">universel</b>, un agriculteur ou une assistante maternelle qui percevait 50% du smic verra son pouvoir d’achat augmenter de 397€                                            
@benoithamon      avec le <b style="background-color :#ff8080;">revenu</b> <b style="background-color :#ff8080;">universel</b>, un jeune qui gagnait 231€ gagnera dorénavant 749€ #legranddébat                                                                                              
@benoithamon      je veux penser la protection sociale de demain. le <b style="background-color :#ff8080;">revenu</b> <b style="background-color :#ff8080;">universel</b> éradiquera la précarité.je serai le candidat du… https://t.co/8vuuj5oi3s                                           
@JLMelenchon      la cupidité <b style="background-color :#C0C0C0;">doit</b> céder la place à la vertu. #legranddébat #débattf1 https://t.co/uzdxeztjoi                                                                                                                                      
@JLMelenchon      il faut punir les entreprises qui collaborent avec l'ennemi. lafarge <b style="background-color :#C0C0C0;">doit</b> être puni. #legranddébat #débattf1 https://t.co/alrsioasxx                                                                                             
@JLMelenchon      il faut apprendre à se passer de pétrole et de gaz. le choix des énergies renouvelables, c'est le chemin de la <b style="background-color :#c6442e;">paix</b>. #legranddébat #débattf1                                                                                     
@JLMelenchon      nous ferons une sécurité sociale intégrale qui rembourse à <b style="background-color :#c6442e;">100</b>% les dépenses de #santé. #legranddébat #débattf1… https://t.co/zmwd3hmi68                                                                                         
@JLMelenchon      il faut une sécurité sociale intégrale qui rembourse à <b style="background-color :#c6442e;">100</b>% les dépenses de santé. #legranddébat #débattf1 https://t.co/uzdxeztjoi                                                                                               
@JLMelenchon      le <b style="background-color :#ba55d3;">projet</b> de #macron : «moduler». pour nous, c'est clair : retraite à 60 ans à taux plein avec 40 annuités !… https://t.co/bsu1gojcko                                                                                            
@JLMelenchon      il faut augmenter le smic. il est à peine <b style="background-color :#c6442e;">100</b> euros au dessus du seuil de pauvreté. #legranddébat #débattf1… https://t.co/fv0jyfboec                                                                                             
@JLMelenchon      ma politique est celle de la demande : un plan de <b style="background-color :#c6442e;">100</b> milliards d'euros d'investissements. #legranddébat #débattf1 https://t.co/u3xg1p6sdu                                                                                       
@JLMelenchon      le grand carénage pour continuer le #nucléaire, c'est <b style="background-color :#c6442e;">100</b> milliards. nous proposons 50 milliards pour la transition… https://t.co/oyse0eres2                                                                                     
@JLMelenchon      la laïcité ne <b style="background-color :#C0C0C0;">doit</b> pas être un prétexte pour flétrir une religion. et pour dire les choses clairement : la religion musulmane. #legranddébat                                                                                     
@JLMelenchon      il faut en finir avec l'escalade. il faut revenir au rôle de gardien de la <b style="background-color :#c6442e;">paix</b>. de la <b style="background-color :#c6442e;">paix</b>. #legranddébat #débattf1 https://t.co/uzdxeztjoi                                           
@JLMelenchon      vous vous trompez si vous croyez que l'apprentissage <b style="background-color :#C0C0C0;">doit</b> être la voie royale de l'enseignement professionnel. #legranddébat #débattf1                                                                                           
@JLMelenchon      je serai le président de la <b style="background-color :#c6442e;">paix</b> car je m'inquiète de voir monter la guerre. nous sortirons de l'#otan. #legranddébat #débattf1                                                                                                  
@JLMelenchon      je serai le président écologiste : sortie du nucléaire, <b style="background-color :#c6442e;">100</b>% renouvelables, <b style="background-color :#c6442e;">100</b>% d'agriculture bio. #legranddébat #débattf1                                                            
@EmmanuelMacron   ce <b style="background-color :#ba55d3;">projet</b>, je veux le porter avec vous. l’alternance profonde, c’est notre <b style="background-color :#ba55d3;">projet</b> ! #legranddébat                                                                                      
@EmmanuelMacron   mon <b style="background-color :#ba55d3;">projet</b> est un <b style="background-color :#ba55d3;">projet</b> qui protège celles et ceux qui n’y arrivent pas, qui libère celles et ceux qui veulent entreprendre. #legranddébat                                            
@EmmanuelMacron   le <b style="background-color :#ba55d3;">projet</b> que je porte est un <b style="background-color :#ba55d3;">projet</b> qui a confiance dans le pays et son énergie. 
c’est un <b style="background-color :#ba55d3;">projet</b> porteur d’espoir… https://t.co/smohzlwsyq 
@EmmanuelMacron   <b style="background-color :#ba55d3;">j’ai</b> été ministre et <b style="background-color :#ba55d3;">j’ai</b> vu ce qui bloquait notre pays. des règles hors d’âge, des fonctionnements dépassés. #legranddébat                                                            
@EmmanuelMacron   rien n’était écrit en ce qui me concerne. je suis là parce que <b style="background-color :#ba55d3;">j’ai</b> travaillé, parce que je l’ai voulu. #legranddébat                                                                                                            
@MLP_officiel     "on <b style="background-color :#C0C0C0;">doit</b> expulser les étrangers islamistes fichés s !" #débattf1 #legranddébat #marine2017 #aunomdupeuple https://t.co/eisgk8bww8                                                                                                
@MLP_officiel     "il faut mener la guerre contre le fondamentalisme islamiste, et on <b style="background-color :#C0C0C0;">doit</b> s'en donner les moyens." #débattf1 #legranddébat                                                                                                        
@MLP_officiel     "les fondamentalistes islamistes se sont infiltrés dans les associations : on <b style="background-color :#C0C0C0;">doit</b> aller les chercher." #débattf1 #legranddébat                                                                                                  
@MLP_officiel     "on <b style="background-color :#C0C0C0;">doit</b> faire la liste des organisations islamistes qui menacent la france : cette liste n'est même pas faite !" #débattf1 #legranddébat                                                                                        
@MLP_officiel     "on <b style="background-color :#C0C0C0;">doit</b> expulser les étrangers islamistes fichés s !" #débattf1 #legranddébat                                                                                                                                                   
@MLP_officiel     "le fondamentalisme islamiste ne <b style="background-color :#C0C0C0;">doit</b> plus avoir le droit de cité dans notre pays." #débattf1 #legranddébat                                                                                                                      
@MLP_officiel     "on <b style="background-color :#C0C0C0;">doit</b> retrouver la maîtrise de nos frontières pour savoir qui entre ou pas sur notre territoire." #débattf1 #legranddébat                                                                                                     
@MLP_officiel     "on <b style="background-color :#C0C0C0;">doit</b> aller vers 2% du pib pour le budget de la défense <b style="background-color :#C0C0C0;">nationale</b>, dès 2018." #débattf1 #legranddébat #marine2017 https://t.co/vhhg5rt3qa                                           
@MLP_officiel     "on <b style="background-color :#C0C0C0;">doit</b> aller vers 2% du pib pour le budget de la défense <b style="background-color :#C0C0C0;">nationale</b>, dès 2018. l'armée est aujourd'hui à l'os !" #débattf1 #legranddébat                                              
@MLP_officiel     "la france <b style="background-color :#C0C0C0;">doit</b> décider et personne ne <b style="background-color :#C0C0C0;">doit</b> décider à sa place, je suis attachée à la liberté des français." #débattf1 #legranddébat                                                   
@MLP_officiel     "#fillon a abandonné son <b style="background-color :#ba55d3;">projet</b> de privatiser la sécurité sociale ? il a vu que les français n'en veulent pas." #débattf1 #legranddébat                                                                                          
@MLP_officiel     "les candidats #fillon et #macron se bagarrent pour plus de dérégulation, pour une loi el khomri puissance <b style="background-color :#c6442e;">100</b>0." #débattf1 #legranddébat                                                                                        
@MLP_officiel     "on <b style="background-color :#C0C0C0;">doit</b> baisser le nombre de députés et de sénateurs, à respectivement 300 et 200." #débattf1 #legranddébat                                                                                                                     
@MLP_officiel     "je propose un référendum pour intégrer la priorité <b style="background-color :#C0C0C0;">nationale</b> et la proportionnelle intégrale dans la constitution" #débattf1 #legranddébat                                                                                      
@MLP_officiel     "nous devons retrouver nos frontières <b style="background-color :#C0C0C0;">nationale</b>s, et arrêter l'#immigration : les français n'en peuvent plus."… https://t.co/0hegfe0qm8                                                                                          
@MLP_officiel     "il y a une montée du fondamentalisme islamiste dans notre pays, et on <b style="background-color :#C0C0C0;">doit</b> le dire." #débattf1 #legranddébat                                                                                                                    
@MLP_officiel     "on <b style="background-color :#C0C0C0;">doit</b> mettre fin à la dissémination des #migrants dans les villages, faite sans l'avis des français." #débattf1 #legranddébat                                                                                                 
@MLP_officiel     "nous devons retrouver nos frontières <b style="background-color :#C0C0C0;">nationale</b>s, et arrêter l'immigration dont les français ne peuvent plus." #débattf1 #legranddébat                                                                                           
@MLP_officiel     "les frontières <b style="background-color :#C0C0C0;">nationale</b>s ne sont pas, comme vous le dites m. #fillon, "un leurre" !" #débattf1 #legranddébat                                                                                                                   
@MLP_officiel     "il faut avoir des frontières <b style="background-color :#C0C0C0;">nationale</b>s. on ne pourra pas compter sur la grèce submergée pour gérer l'immigration." #débattf1 #legranddébat                                                                                     
@MLP_officiel     "l'indépendance <b style="background-color :#C0C0C0;">nationale</b>, c'est le droit pour les français de décider pour eux-mêmes." #débattf1 #legranddébat… https://t.co/kcuslvufk2                                                                                         
@MLP_officiel     "l'ecole est le creuset qui fabrique des français, elle ne <b style="background-color :#C0C0C0;">doit</b> pas renvoyer vers des "cultures d'origine"." #débattf1 #legranddébat                                                                                             
@MLP_officiel     "plus aucune décision ne <b style="background-color :#C0C0C0;">doit</b> être prise contre le peuple français." #débattf1 #legranddébat                                                                                                                                     
@MLP_officiel     "il s'agit pour les français de défendre leurs intérêts, pas ceux des banques, pas ceux des multi<b style="background-color :#C0C0C0;">nationale</b>s." #débattf1 #legranddébat                                                                                            
@MLP_officiel     "nous devons défendre notre identité <b style="background-color :#C0C0C0;">nationale</b>, nos valeurs, nos traditions." #débattf1 #legranddébat                                                                                                                            
@MLP_officiel     "l'indépendance <b style="background-color :#C0C0C0;">nationale</b>, c'est le droit pour les français de décider pour eux-mêmes." #débattf1 #legranddébat                                                                                                                  
@MLP_officiel     "je veux être le garant de l'indépendance <b style="background-color :#C0C0C0;">nationale</b>, conformément à l'article 5 de la constitution." #débattf1 #legranddébat                                                                                                     
@FrancoisFillon   un mouvement totalitaire déstabilise une partie du monde. il <b style="background-color :#C0C0C0;">doit</b> être combattu par une alliance avec les forces sur place, russie et iran.                                                                                      
@FrancoisFillon   #immigration la situation économique et sociale de notre pays <b style="background-color :#C0C0C0;">doit</b> nous conduire à limiter le plus possible les ent… https://t.co/jefgytzwxl                                                                                     
@FrancoisFillon   je veux une école <b style="background-color :#0066cc;">primaire</b> qui commence à 5 ans, où 75% du temps des élèves est consacré à l'apprentissage des <b style="background-color :#0066cc;">fondamentaux</b>. #legranddébat                                             
@FrancoisFillon   ma priorité absolue, c'est une école <b style="background-color :#0066cc;">primaire</b> qui enseigne les savoirs <b style="background-color :#0066cc;">fondamentaux</b>. #legranddébat https://t.co/mwmj0stvfj                                                             
@FrancoisFillon   il y a chaque année près de 150.000 orphelins de la république, qui sortent de notre système scolaire en ne maîtrisant pas les <b style="background-color :#0066cc;">fondamentaux</b>.                                                                                     
@FrancoisFillon   nous sommes 11 candidats, 5 sont ici. avec cette règle des sondages, je n'aurais pas pu participer aux débats de la <b style="background-color :#0066cc;">primaire</b>. #legranddébat                                                                                      

Nous voyons que quelques mots caractéristiques pour un candidat se retrouvent chez les autres (par exemple doit qui caractérise Marine Le Pen et se retrouve aussi chez Jean-Luc Mélenchon, mais nettement moins).
Nous pouvons constater :

* Benoît Hamon parle de revenu universel ;
* Jean-Luc Mélenchon donne des chiffres ronds et parle de paix ;
* Marine Le Pen veut des frontières et des priorités nationales et insiste sur le devoir ;
* François Fillon parle d'école primaire et de primaire de la droite (notre algorihtme ne peut pas les distinguer), ainsi que de fondamentaux ;
* et Emmanuel Macron parle de ce qu'il a fait et de son projet (quand il parle de projet, il est deux fois dans le même tweet).



