
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
  
#Unificar bases ----------------------------------------------------------
  
  id_test <- test %>% select(id)
  test$name <- NA 
  dt <- rbind(train,test)
  
#Limpiamos el texto--------------------------------------------------------
  
  dt_clean <- dt %>% 
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

  dt_clean <- dt_clean %>% mutate(n_palabras_i = str_count(text, "\\S+"))
  
#Tokens----

  dt_token <- dt_clean %>% unnest_tokens("word", corpus)
  dt_token %>% count(word, sort = TRUE) %>% head()
  
  sw <- c()
  for (s in c("snowball", "stopwords-iso", "nltk")) {
       temp <- get_stopwords("spanish", source = s)$word
       sw <- c(sw, temp)
        }
      sw <- unique(sw)
      sw <- unique(stri_trans_general(str = sw, id = "Latin-ASCII"))
      sw <- data.frame(word = sw)
  
  nrow(dt_token)
  
  dt_token <- dt_token %>% anti_join(sw, by = "word")
  
  nrow(dt_token)
  
  
##Lemma
  
  #udpipe::udpipe_download_model('spanish')
  
  model <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe")
  
  palabras_unicas <- dt_token %>% distinct(word = dt_token$word)
  
  udpipe_results <- udpipe_annotate(model, x = palabras_unicas$word)
  
  udpipe_results <- as_tibble(udpipe_results)
  
  udpipe_results <- udpipe_results %>% select(token, lemma) %>% rename("word" = "token")
  
  dt_token <- dt_token %>% left_join(udpipe_results, by = "word", multiple = "all")
  
  dt_token[is.na(dt_token$lemma), "lemma"] <- dt_token[is.na(dt_token$lemma), "word"]
  
  conteo <- dt_token %>% count(lemma) %>% arrange(desc(n)) %>% tail(1000)
  
  palabras_eliminar <- dt_token %>% count(lemma) %>% filter(n < 10)
  
  dt_token <- dt_token %>% anti_join(palabras_eliminar, by = "lemma") 
  
  dt_clean_2 <- dt_token %>%
    group_by(name, id, n_palabras_i) %>% 
    summarise(text = str_c(lemma, collapse = " ")) %>%
    ungroup()
  
  diferencia <- setdiff(dt_clean$id, dt_clean_2$id) %>% as.data.frame()
  
  dif <- nrow(diferencia)
  inicial <- nrow(dt_clean)
  final <- nrow(dt_clean_2)
  
  inicial - final #debe ser igual a dif
  
  dt_clean %>% filter (id == "ce1464da0f03a61f2659947b") #Ejemplo para validar
  