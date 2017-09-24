---
title: "Sherlock Holmes Topic Modeling"
author: "Julia Silge"
date: '2017-09-24'
output:
  html_document: default
---





```r
library(tidyverse)
library(tidytext)
library(gutenbergr)
library(stringr)

my_stop_words <- stop_words %>%
    filter(lexicon == "snowball")

sherlock <- gutenberg_download(1661) %>%
    mutate(story = case_when(str_detect(text, "ADVENTURE") ~ text,
                             TRUE ~ NA_character_)) %>%
    fill(story) %>%
    filter(story != "THE ADVENTURES OF SHERLOCK HOLMES") %>%
    mutate(story = factor(story, levels = unique(story)))

tidy_sherlock <- sherlock %>%
    mutate(line = row_number()) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)

tidy_sherlock
```

```
## # A tibble: 32,030 x 4
##    gutenberg_id                             story  line      word
##           <int>                            <fctr> <int>     <chr>
##  1         1661 ADVENTURE I. A SCANDAL IN BOHEMIA     1 adventure
##  2         1661 ADVENTURE I. A SCANDAL IN BOHEMIA     1   scandal
##  3         1661 ADVENTURE I. A SCANDAL IN BOHEMIA     1   bohemia
##  4         1661 ADVENTURE I. A SCANDAL IN BOHEMIA     5  sherlock
##  5         1661 ADVENTURE I. A SCANDAL IN BOHEMIA     5    holmes
##  6         1661 ADVENTURE I. A SCANDAL IN BOHEMIA     5     woman
##  7         1661 ADVENTURE I. A SCANDAL IN BOHEMIA     5    seldom
##  8         1661 ADVENTURE I. A SCANDAL IN BOHEMIA     5     heard
##  9         1661 ADVENTURE I. A SCANDAL IN BOHEMIA     6   mention
## 10         1661 ADVENTURE I. A SCANDAL IN BOHEMIA     6      eyes
## # ... with 32,020 more rows
```


## Explore tf-df



```r
tidy_sherlock %>%
    count(story, word, sort = TRUE) %>%
    bind_tf_idf(word, story, n) %>%
    arrange(-tf_idf) %>%
    group_by(story) %>%
    top_n(10) %>%
    ungroup %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(word, tf_idf, fill = story)) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ story, scales = "free") +
    coord_flip()
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)




## Implement topic modeling


```r
library(quanteda)
library(stm)

sherlock_dfm <- tidy_sherlock %>%
    count(story, word, sort = TRUE) %>%
    cast_dfm(story, word, n)
save(sherlock_dfm, file = "sherlock_dfm.rda")

topic_model <- stm(sherlock_dfm, K = 24, verbose = FALSE, init.type = "Spectral")
save(topic_model, file = "topic_model.rda")
```



```r
td_beta <- tidy(topic_model)
td_beta
```

```
## # A tibble: 178,512 x 3
##    topic  term        beta
##    <int> <chr>       <dbl>
##  1     1     1 0.000000000
##  2     2     1 0.003377875
##  3     3     1 0.000000000
##  4     4     1 0.000000000
##  5     5     1 0.000000000
##  6     6     1 0.000000000
##  7     7     1 0.000000000
##  8     8     1 0.000000000
##  9     9     1 0.000000000
## 10    10     1 0.000000000
## # ... with 178,502 more rows
```

```r
td_beta %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)



```r
td_gamma <- tidy(topic_model, matrix = "gamma",                    
                 document_names = rownames(sherlock_dfm))
td_gamma
```

```
## # A tibble: 288 x 3
##                                      document topic        gamma
##                                         <chr> <int>        <dbl>
##  1          ADVENTURE I. A SCANDAL IN BOHEMIA     1 2.976082e-05
##  2        ADVENTURE II. THE RED-HEADED LEAGUE     1 1.329666e-05
##  3          ADVENTURE III. A CASE OF IDENTITY     1 1.936038e-05
##  4  ADVENTURE IV. THE BOSCOMBE VALLEY MYSTERY     1 1.430311e-05
##  5          ADVENTURE V. THE FIVE ORANGE PIPS     1 1.693453e-05
##  6 ADVENTURE VI. THE MAN WITH THE TWISTED LIP     1 1.250292e-05
##  7   VII. THE ADVENTURE OF THE BLUE CARBUNCLE     1 1.551410e-05
##  8   VIII. THE ADVENTURE OF THE SPECKLED BAND     1 1.219383e-05
##  9  IX. THE ADVENTURE OF THE ENGINEER'S THUMB     1 1.403023e-05
## 10     X. THE ADVENTURE OF THE NOBLE BACHELOR     1 4.998153e-01
## # ... with 278 more rows
```

```r
ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)


```r
assignments <- augment(topic_model, sherlock_dfm)
assignments
```

```
## # A tibble: 17,774 x 4
##                                      document   term count .topic
##                                         <chr>  <chr> <dbl>  <int>
##  1          ADVENTURE I. A SCANDAL IN BOHEMIA holmes    48     19
##  2        ADVENTURE II. THE RED-HEADED LEAGUE holmes    53     22
##  3          ADVENTURE III. A CASE OF IDENTITY holmes    46      9
##  4  ADVENTURE IV. THE BOSCOMBE VALLEY MYSTERY holmes    47     22
##  5          ADVENTURE V. THE FIVE ORANGE PIPS holmes    25     19
##  6 ADVENTURE VI. THE MAN WITH THE TWISTED LIP holmes    29     19
##  7   VII. THE ADVENTURE OF THE BLUE CARBUNCLE holmes    38     19
##  8   VIII. THE ADVENTURE OF THE SPECKLED BAND holmes    56     22
##  9  IX. THE ADVENTURE OF THE ENGINEER'S THUMB holmes    14     19
## 10     X. THE ADVENTURE OF THE NOBLE BACHELOR holmes    34     19
## # ... with 17,764 more rows
```


