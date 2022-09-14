# Importing Libraries
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(igraph)
library(tidyverse)
library(qdap)
library(gridExtra)
library(ggraph)

# Reading Datasets
df_2017<-read.csv('2017.csv')
df_2018<-read.csv('2018.csv')
df_2019<-read.csv('2019.csv')
df_2020<-read.csv('2020.csv')
df_2021<-read.csv('2021.csv')
df_2022<-read.csv('2022.csv')

## 2017 Dataset

# Selecting only Tweet column to analyze
df_2017 <- subset (df_2017 , select=c(tweet))
text_df <- tibble(text = df_2017$tweet)

# Creating word tokens
Words_2017 <- text_df%>%unnest_tokens(word,text)

# Removing Stop words
stop_words = tidytext::stop_words
df2017 <- Words_2017 %>% filter(!word %in% stop_words$word)

# Top 10 frequency words with atleast 2 letters
Top_ten_2017 <- freq_terms(df2017,top=10,atleast=2)
Top_ten_2017$year <- 2017

# Frequency of all words with atleast 2 letters
freq2017 <- freq_terms(df2017,top=count(df2017),atleast=2,extend=FALSE)
freq2017$total = as.numeric(count(df2017))



## 2018 Dataset

# Selecting only Tweet column to analyze
df_2018 <- subset (df_2018 , select=c(tweet))
text_df <- tibble(text = df_2018$tweet)

# Creating word tokens
Words_2018 <- text_df%>%unnest_tokens(word,text)

# Removing Stop words
stop_words = tidytext::stop_words
df2018 <- Words_2018 %>% filter(!word %in% stop_words$word)

# Top 10 frequency words with atleast 2 letters
Top_ten_2018 <- freq_terms(df2018,top=10,atleast=2)
Top_ten_2018$year <- 2018

# Frequency of all words with atleast 2 letters
freq2018 <- freq_terms(df2018,top=count(df2018),atleast=2,extend=FALSE)
freq2018$total = as.numeric(count(df2018))




## 2019 Dataset

# Selecting only Tweet column to analyze
df_2019 <- subset (df_2019 , select=c(tweet))
text_df <- tibble(text = df_2019$tweet)

# Creating word tokens
Words_2019 <- text_df%>%unnest_tokens(word,text)

# Removing Stop words
stop_words = tidytext::stop_words
df2019 <- Words_2019 %>% filter(!word %in% stop_words$word)

# Top 10 frequency words with atleast 2 letters
Top_ten_2019 <- freq_terms(df2019,top=10,atleast=2)
Top_ten_2019$year <- 2019

# Frequency of all words with atleast 2 letters
freq2019 <- freq_terms(df2019,top=count(df2019),atleast=2,extend=FALSE)
freq2019$total = as.numeric(count(df2019))



## Dataset 2020


# Selecting only Tweet column to analyze
df_2020 <- subset (df_2020 , select=c(tweet))
text_df <- tibble(text = df_2020$tweet)

# Creating word tokens
Words_2020 <- text_df%>%unnest_tokens(word,text)

# Removing Stop words
stop_words = tidytext::stop_words
df2020 <- Words_2020 %>% filter(!word %in% stop_words$word)

# Top 10 frequency words with atleast 2 letters
Top_ten_2020 <- freq_terms(df2020,top=10,atleast=2)
Top_ten_2020$year <- 2020

# Frequency of all words with atleast 2 letters
freq2020 <- freq_terms(df2020,top=count(df2020),atleast=2,extend=FALSE)
freq2020$total = as.numeric(count(df2020))



## 2021 Dataset

# Selecting only Tweet column to analyze
df_2021 <- subset (df_2021 , select=c(tweet))
text_df <- tibble(text = df_2021$tweet)

# Creating word tokens
Words_2021 <- text_df%>%unnest_tokens(word,text)

# Removing Stop words
stop_words = tidytext::stop_words
df2021 <- Words_2021 %>% filter(!word %in% stop_words$word)

# Top 10 frequency words with atleast 2 letters
Top_ten_2021 <- freq_terms(df2021,top=10,atleast=2)
Top_ten_2021$year <- 2021

# Frequency of all words with atleast 2 letters
freq2021 <- freq_terms(df2021,top=count(df2021),atleast=2,extend=FALSE)
freq2021$total = as.numeric(count(df2021))


## 2022 Dataset

# Selecting only Tweet column to analyze
df_2022 <- subset (df_2022, select=c(tweet))
text_df <- tibble(text = df_2022$tweet)

# Creating word tokens
Words_2022 <- text_df%>%unnest_tokens(word,text)

# Removing Stop words
stop_words = tidytext::stop_words
df2022 <- Words_2022 %>% filter(!word %in% stop_words$word)

# Top 10 frequency words
Top_ten_2022 <- freq_terms(df2022,atleast=2,top=10)
Top_ten_2022$year <- 2022

# Frequency of all words
freq2022 <- freq_terms(df2022,atleast=2,top=count(df2022),extend=FALSE)
freq2022$total = as.numeric(count(df2022))


## Plotting top 10 frequency words for each year
gg2017<-ggplot(Top_ten_2017,aes(WORD))+geom_col(aes(x=reorder(as.factor(WORD),-FREQ),y=FREQ))+ggtitle('2017')
gg2018<-ggplot(Top_ten_2018,aes(WORD))+geom_col(aes(x=reorder(as.factor(WORD),-FREQ),y=FREQ))+ggtitle('2018')
gg2019<-ggplot(Top_ten_2019,aes(WORD))+geom_col(aes(x=reorder(as.factor(WORD),-FREQ),y=FREQ))+ggtitle('2019')
gg2020<-ggplot(Top_ten_2020,aes(WORD))+geom_col(aes(x=reorder(as.factor(WORD),-FREQ),y=FREQ))+ggtitle('2020')
gg2021<-ggplot(Top_ten_2021,aes(WORD))+geom_col(aes(x=reorder(as.factor(WORD),-FREQ),y=FREQ))+ggtitle('2021')
gg2022<-ggplot(Top_ten_2022,aes(WORD))+geom_col(aes(x=reorder(as.factor(WORD),-FREQ),y=FREQ))+ggtitle('2022')

grid.arrange(gg2017,gg2018,gg2019,gg2020,gg2021,gg2022,nrow=3)



## Zipf's Law Implementation for each year

freq_by_rank2017 <- freq2017 %>%
  mutate(rank = row_number(),
         `term frequency` = FREQ/total) %>%
  ungroup()
zipf_2017<-freq_by_rank2017 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()+ggtitle('2017')


freq_by_rank2018 <- freq2018 %>%
  mutate(rank = row_number(),
         `term frequency` = FREQ/total) %>%
  ungroup()
zipf_2018<-freq_by_rank2018 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()+ggtitle('2018')


freq_by_rank2019 <- freq2019 %>%
  mutate(rank = row_number(),
         `term frequency` = FREQ/total) %>%
  ungroup()
zipf_2019<-freq_by_rank2019 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()+ggtitle('2019')


freq_by_rank2020 <- freq2020 %>%
  mutate(rank = row_number(),
         `term frequency` = FREQ/total) %>%
  ungroup()
zipf_2020<-freq_by_rank2020 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()+ggtitle('2020')


freq_by_rank2021 <- freq2021 %>%
  mutate(rank = row_number(),
         `term frequency` = FREQ/total) %>%
  ungroup()
zipf_2021<-freq_by_rank2021 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()+ggtitle('2021')

freq_by_rank2022 <- freq2022 %>%
  mutate(rank = row_number(),
         `term frequency` = FREQ/total) %>%
  ungroup()
zipf_2022<-freq_by_rank2022 %>%
  ggplot(aes(rank, `term frequency`)) +
  geom_line(size = 1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()+ggtitle('2022')


grid.arrange(zipf_2017,zipf_2018,zipf_2019,zipf_2020,zipf_2021,zipf_2022,nrow=3)


## BiGram Network

## Bigram for 2017
bigram_2017<-df_2017%>%
  unnest_tokens(bigram,tweet,token='ngrams',n=2)

# bigrams with stop words
bigrams_separated <- bigram_2017 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtering out the stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# New bigram counts
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Visualizing bigrams
bigram_graph <- bigram_counts %>%
  filter(n>7)%>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('2017 Bigram')+
  theme_void()


## Bigram for 2018
bigram_2018<-df_2018%>%
  unnest_tokens(bigram,tweet,token='ngrams',n=2)

# bigrams with stop words
bigrams_separated <- bigram_2018 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtering out the stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# New bigram counts
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Visualizing bigrams
bigram_graph <- bigram_counts %>%
  filter(n>7)%>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('2018 Bigram')+
  theme_void()



## Bigram for 2019
bigram_2019<-df_2019%>%
  unnest_tokens(bigram,tweet,token='ngrams',n=2)

# bigrams with stop words
bigrams_separated <- bigram_2019 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtering out the stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# New bigram counts
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Visualizing bigrams
bigram_graph <- bigram_counts %>%
  filter(n>10)%>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('2019 Bigram')+
  theme_void()



## Bigram for 2020
bigram_2020<-df_2020%>%
  unnest_tokens(bigram,tweet,token='ngrams',n=2)

# bigrams with stop words
bigrams_separated <- bigram_2020 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtering out the stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# New bigram counts
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Visualizing bigrams
bigram_graph <- bigram_counts %>%
  filter(n>16)%>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('2020 Bigram')+
  theme_void()



## Bigram for 2021
bigram_2021<-df_2021%>%
  unnest_tokens(bigram,tweet,token='ngrams',n=2)

# bigrams with stop words
bigrams_separated <- bigram_2021 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtering out the stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# New bigram counts
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Visualizing bigrams
bigram_graph <- bigram_counts %>%
  filter(n>5)%>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('2021 Bigram')+
  theme_void()


## Bigram for 2022
bigram_2022<-df_2022%>%
  unnest_tokens(bigram,tweet,token='ngrams',n=2)

# bigrams with stop words
bigrams_separated <- bigram_2022 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtering out the stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# New bigram counts
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Visualizing bigrams
bigram_graph <- bigram_counts %>%
  filter(n>5)%>%
  graph_from_data_frame()
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('2022 Bigram')+
  theme_void()
