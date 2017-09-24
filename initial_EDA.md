---
title: "The Game is Afoot!"
author: "Julia Silge"
date: '2017-08-22'
output:
  html_document: default
---






```r
library(tidyverse)
library(tidytext)
library(gutenbergr)

my_stop_words <- stop_words %>%
    filter(lexicon == "snowball")

sherlock <- gutenberg_download(1661)

tidy_sherlock <- sherlock %>%
    mutate(line = row_number()) %>%
    unnest_tokens(word, text)
```




```r
sherlock_bigrams <- sherlock %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- sherlock_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts
```

```
## # A tibble: 13,451 x 3
##       word1  word2     n
##       <chr>  <chr> <int>
##  1     said holmes   111
##  2 sherlock holmes    97
##  3       mr holmes    71
##  4       st  simon    39
##  5    baker street    29
##  6     lord     st    28
##  7       st  clair    23
##  8      let     us    22
##  9      red headed    20
## 10    young   lady    20
## # ... with 13,441 more rows
```


```r
library(widyr)

word_pairs <- tidy_sherlock %>%
    filter(!word %in% my_stop_words$word) %>%
    group_by(word) %>%
    filter(n() >= 10) %>%
    ungroup %>%
    pairwise_count(word, line, sort = TRUE)

word_pairs
```

```
## # A tibble: 68,394 x 3
##       item1    item2     n
##       <chr>    <chr> <dbl>
##  1     said   holmes   130
##  2   holmes     said   130
##  3   holmes sherlock    92
##  4 sherlock   holmes    92
##  5       mr   holmes    82
##  6   holmes       mr    82
##  7    simon       st    39
##  8       st    simon    39
##  9       mr     said    35
## 10     said       mr    35
## # ... with 68,384 more rows
```

```r
word_cors <- tidy_sherlock %>%
    filter(!word %in% my_stop_words$word) %>%
    group_by(word) %>%
    filter(n() >= 10) %>%
    pairwise_cor(word, line, sort = TRUE)

word_cors
```

```
## # A tibble: 979,110 x 3
##       item1    item2 correlation
##       <chr>    <chr>       <dbl>
##  1    adler    irene   0.9373948
##  2    irene    adler   0.9373948
##  3    stoke    moran   0.9197695
##  4    moran    stoke   0.9197695
##  5    angel   hosmer   0.8388961
##  6   hosmer    angel   0.8388961
##  7  beeches   copper   0.8003798
##  8   copper  beeches   0.8003798
##  9     yard scotland   0.7534534
## 10 scotland     yard   0.7534534
## # ... with 979,100 more rows
```



```r
library(igraph)
library(networkD3)

network_pairs <- word_pairs %>%
    filter(n > 10)

word_graph <- network_pairs %>%
    graph_from_data_frame()
word_communities <- cluster_walktrap(word_graph)
members <- membership(word_communities)

word_d3 <- igraph_to_networkD3(word_graph, group = members)

forceNetwork(Links = word_d3$links, Nodes = word_d3$nodes,
            Source = "source", Target = "target",
            NodeID = "name", Value = "value",
            Group = "group", 
            opacity = 0.9, charge = -20,
            zoom  = TRUE, fontSize = 24)
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```





