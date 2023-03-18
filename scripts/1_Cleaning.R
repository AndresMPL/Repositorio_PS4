
#------------------------------------------------------------------------------#
#
#                                PROBLEM SET 4
#
#                            "Predicting Tweets"
#
#       Grupo 5:  Isabella Mendez Pedraza.
#                 Manuela Ojeda Ojeda.
#                 Juan Sebastian Tellez Melo.
#                 Andres Mauricio Palacio Lugo.
#
#------------------------------------------------------------------------------#

#Cargamos librerías - Verificamos que ninguna genere conflicto

  library(pacman)
  
  p_load(dplyr, tidyverse, tm, textir, tidytext, wordcloud, SentimentAnalysis, 
         udpipe, syuzhet,stringi,stopwords,textstem,topicmodels, rio, caret, sentimentr,
         janitor, wordcloud2, udpipe,ggcorrplot)

  rm(list=ls())

#Importamos los datos
  
  test   <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS4/main/datasets/test.csv")
  train  <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS4/main/datasets/train.csv")
  
  glimpse(train)
  glimpse(test)
  

#Limpiamos el texto--------------------------------------------------------
  
  #train$name <- as.factor(train$name)
  #levels(train$name)
  
  train_clean <- train %>% 
    mutate(corpus = stri_trans_general(text, id = "Latin-ASCII")) %>%    
    mutate(corpus = removeNumbers(text)) %>%
    mutate(corpus = removePunctuation(corpus)) %>%
    mutate(corpus = tolower(corpus)) %>% 
    mutate(corpus = stripWhitespace(corpus)) %>%
    mutate(corpus = str_replace_all(corpus, "[^[:alnum:]\\s]", ""))  %>%
    mutate(corpus = gsub("[^[:alpha:]#\\s]", " ", corpus)) %>% 
    mutate(corpus = removeWords(corpus, stopwords("es"))) %>%
    mutate(corpus = gsub("\\d+", " ", corpus)) %>%
    mutate(corpus = gsub("http\\S+", " ", corpus)) %>% 
    mutate(corpus = gsub("www\\S+", " ", corpus)) %>%
    mutate(corpus = gsub("\\b\\w{1,2}\\b", " ", corpus)) %>% 
    mutate(corpus = gsub("\\s+", " ", corpus)) 

  
#Tokens----

  train_token <- train_clean %>% unnest_tokens("word", corpus)
  train_token %>% count(word, sort = TRUE) %>% head()
  
  sw <- c()
  for (s in c("snowball", "stopwords-iso", "nltk")) {
       temp <- get_stopwords("spanish", source = s)$word
       sw <- c(sw, temp)
        }
      sw <- unique(sw)
      sw <- unique(stri_trans_general(str = sw, id = "Latin-ASCII"))
      sw <- data.frame(word = sw)
  
  nrow(train_token)
  
  train_token <- train_token %>% anti_join(sw, by = "word")
  
  nrow(train_token)
  