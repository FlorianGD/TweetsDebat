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
library(DT)

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
  mutate(text = str_replace_all(text,
                                "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https",
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

title <- ggdraw() + draw_label("Mots les plus utilisés par chaque candidat", fontface='bold')

p <- plot_grid(plotlist = plot_list, nrow = 2, labels = top_split$nom,
               label_size = 12, hjust = -0.2)

plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
```

![](02_-_Exploration_files/figure-html/top_mots-1.png)<!-- -->

Tous les candidats ont le mot "veux" dans le top 5.

## Mots caractéristiques des candidats

Nous allons essayer de trouver des mots caractéristiques pour chaque candidat. Pour ce faire, nous allons regarder la mesure dîte `tf-idf` pour `term frequency, inverse document frequency`. Cela revient à mettre un score élevé à un mot s'il apparaît fréquemment pour un candidat mais pas chez les autres.

```r
mot_tfidf_tot <- tidy_tweet %>% 
  count(mot, nom) %>% 
  bind_tf_idf(mot, nom, n)

mot_tfidf <- mot_tfidf_tot %>% 
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

![](02_-_Exploration_files/figure-html/mots_caracteristiques-1.png)<!-- -->

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
  top_n(4, tf_idf) %>% 
  select(nom, mot) %>% 
  arrange(nom)

patterns <- top_mots %>% 
  group_by(nom) %>% 
  summarise(pattern = str_c(mot, collapse = "|")) %>% 
  `$`(pattern)

top_score <- tidy_tweet %>% 
  left_join(mot_tfidf_tot, by = c("nom", "mot")) %>% 
  group_by(nom, created_at) %>% 
  summarise(score = sum(tf_idf)) %>% 
  top_n(n = 3, score) %>% 
  left_join(tweets_debat, by = c("nom", "created_at")) %>% 
  select(nom, score, text)


# table_tweets <- tweets_debat %>% 
#   filter(str_detect(text, str_c(patterns, collapse = "|"))) %>% 
#   select(nom, text)

for(i in 1:5){
  top_score$text <- str_replace_all(str_to_lower(top_score$text), patterns[i], liste_fun[[i]])
}

top_score %>% 
  arrange(nom) %>% 
  group_by(nom) %>% 
  mutate(score = signif(score, 2)) %>% 
  # tidyr::nest(-nom) %>%
  # spread(nom, data) %>%
  datatable(escape = FALSE, 
            caption = htmltools::tags$caption(style = "color: black",
              htmltools::tags$h3("Trois meilleurs scores TF-IDF des tweets de chaque candidat."), 
              "Les mots caractéristiques ont été surlignés de la couleur du candidat."),
            options = list(dom = 't', pageLength = 15))
```

<!--html_preserve--><div id="htmlwidget-395dfb56eb8a5ce35b5e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-395dfb56eb8a5ce35b5e">{"x":{"filter":"none","caption":"<caption style=\"color: black\">\n  <h3>Trois meilleurs scores TF-IDF des tweets de chaque candidat.<\/h3>\n  Les mots caractéristiques ont été surlignés de la couleur du candidat.\n<\/caption>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"],["@benoithamon","@benoithamon","@benoithamon","@EmmanuelMacron","@EmmanuelMacron","@EmmanuelMacron","@FrancoisFillon","@FrancoisFillon","@FrancoisFillon","@JLMelenchon","@JLMelenchon","@JLMelenchon","@MLP_officiel","@MLP_officiel","@MLP_officiel"],[0.065,0.065,0.064,0.08,0.085,0.091,0.044,0.043,0.048,0.055,0.047,0.037,0.05,0.056,0.05],["je veux penser la protection <b style=\"background-color :#c6442e;\">social<\/b>e de demain. le <b style=\"background-color :#ff8080;\">revenu<\/b> <b style=\"background-color :#ff8080;\">universel<\/b> éradiquera la précarité.je serai le candidat du… https://t.co/8vuuj5oi3s","avec le <b style=\"background-color :#ff8080;\">revenu<\/b> <b style=\"background-color :#ff8080;\">universel<\/b>, un agriculteur ou une assistante maternelle qui percevait 50% du smic verra son pouvoir d’achat augmenter de 397€","nous devons penser la protection <b style=\"background-color :#c6442e;\">social<\/b>e de demain : je propose le <b style=\"background-color :#ff8080;\">revenu<\/b> <b style=\"background-color :#ff8080;\">universel<\/b> qui éradiquera la précarité… https://t.co/lmko0ehenu","le <b style=\"background-color :#ba55d3;\">projet<\/b> que je <b style=\"background-color :#ba55d3;\">porte<\/b> est un <b style=\"background-color :#ba55d3;\">projet<\/b> qui a confiance dans le pays et son énergie. \nc’est un <b style=\"background-color :#ba55d3;\">projet<\/b> <b style=\"background-color :#ba55d3;\">porte<\/b>ur d’espoir… https://t.co/smohzlwsyq","je veux que là où 50 à 60% des élèves ne <b style=\"background-color :#ba55d3;\">savent<\/b> pas <b style=\"background-color :#ba55d3;\">lire<\/b>, <b style=\"background-color :#ba55d3;\">écrire<\/b> et compter en <b style=\"background-color :#ba55d3;\">cm2<\/b>, on <b style=\"background-color :#ba55d3;\">porte<\/b> leur nombre à 12 par c… https://t.co/vicq7a2kho","mon <b style=\"background-color :#ba55d3;\">projet<\/b> est un <b style=\"background-color :#ba55d3;\">projet<\/b> qui <b style=\"background-color :#ba55d3;\">protège<\/b> <b style=\"background-color :#ba55d3;\">celles<\/b> et ceux qui n’y arrivent pas, qui libère <b style=\"background-color :#ba55d3;\">celles<\/b> et ceux qui veulent entreprendre. #legranddébat","je veux une <b style=\"background-color :#0066cc;\">école<\/b> <b style=\"background-color :#0066cc;\">primaire<\/b> qui commence à 5 ans, où 75% du temps des élèves est consacré à l'apprentissage des <b style=\"background-color :#0066cc;\">fondamentaux<\/b>. #legranddébat","je mets en garde les français contre l'illusion d'une retraite par points, qui est une manière de baisser chaque année la valeur du point.","je suis le seul qui pourra demain <b style=\"background-color :#0066cc;\">disposer<\/b> d'une majorité cohérente et stable pour <b style=\"background-color :#0066cc;\">conduire<\/b> le <b style=\"background-color :#0066cc;\">redressement<\/b> de notre pays.","je serai le président écologiste : sortie du nucléaire, <b style=\"background-color :#c6442e;\">100<\/b>% renouvelables, <b style=\"background-color :#c6442e;\">100<\/b>% d'agriculture bio. #legranddébat #débattf1","le grand carénage pour continuer le #nucléaire, c'est <b style=\"background-color :#c6442e;\">100<\/b> milliards. nous proposons 50 milliards pour la transition… https://t.co/oyse0eres2","il faut apprendre à se passer de <b style=\"background-color :#c6442e;\">pétrole<\/b> et de <b style=\"background-color :#c6442e;\">gaz<\/b>. le choix des énergies renouvelables, c'est le chemin de la <b style=\"background-color :#c6442e;\">paix<\/b>. #legranddébat #débattf1","\"la france <b style=\"background-color :#C0C0C0;\">doit<\/b> <b style=\"background-color :#C0C0C0;\">décider<\/b> et personne ne <b style=\"background-color :#C0C0C0;\">doit<\/b> <b style=\"background-color :#C0C0C0;\">décider<\/b> à sa place, je suis attachée à la liberté des français.\" #débattf1 #legranddébat","\"on <b style=\"background-color :#C0C0C0;\">doit<\/b> aller vers 2% du pib pour le budget de la défense <b style=\"background-color :#C0C0C0;\">nationale<\/b>, dès 2018. l'armée est aujourd'hui à l'os !\" #débattf1 #legranddébat","\"on <b style=\"background-color :#C0C0C0;\">doit<\/b> aller vers 2% du pib pour le budget de la défense <b style=\"background-color :#C0C0C0;\">nationale<\/b>, dès 2018.\" #débattf1 #legranddébat #marine2017 https://t.co/vhhg5rt3qa"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>nom<\/th>\n      <th>score<\/th>\n      <th>text<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","pageLength":15,"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[10,15,25,50,100]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Nous voyons que quelques mots caractéristiques pour un candidat se retrouvent chez les autres (par exemple sociale est plus caractéristique de Jean-Luc Mélenchon mais se retrouve aussi chez Benoît Hamon).
Nous pouvons constater que :

* Benoît Hamon parle de revenu universel ;
* Jean-Luc Mélenchon donne des chiffres ronds et parle de paix ;
* Marine Le Pen veut des frontières et des priorités nationales et insiste sur le devoir ;
* François Fillon parle d'école primaire et de primaire de la droite (notre algorihtme ne peut pas les distinguer), ainsi que de fondamentaux ;
* et Emmanuel Macron parle de ce qu'il a fait et de son projet (quand il parle de projet, il est deux fois dans le même tweet).

Nous voyons aussi, si l'on classe les tweets par score, qu'Emmanuel Macron est celui qui emploie le plus ses mots caractéristique, et que François Fillon est celui qui les utilise le moins. Ce dernier a donc tweeté plus de mots utilisés aussi par les autres candidats.

