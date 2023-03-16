
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

#Cargamos librerías

  library(pacman)
  
  p_load(dplyr, tidyverse, tm, textir, tidytext, wordcloud, SentimentAnalysis, 
         udpipe, syuzhet,stringi,stopwords,textstem,topicmodels, rio, caret, sentimentr)

  rm(list=ls())

#Importamos los datos
  
  test   <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS4/main/datasets/test.csv")
  train  <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS4/main/datasets/train.csv")
  
  glimpse(train)
  glimpse(test)
  

#Limpieza del texto--------------------------------------------------------
  
  train$name <- as.factor(train$name)
  levels(train$name)
  
  train_clean <- train %>% 
    mutate(corpus = iconv(text, to = "UTF-8")) %>%    
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
  
  train_token <-train_clean %>% unnest_tokens("word", corpus)
  train_token %>% count(word, sort = TRUE) %>% head()

  train_token$radical <- stemDocument(train_token$word, language="spanish")
  train_token %>% count(radical, sort = TRUE) %>% head()
  
##Histograma de palabras----
  
  train_cloud <- train_token %>% count(name, word) %>% group_by(name) %>% ungroup()
  
  autor_top_word <- train_cloud %>%
                    group_by(name) %>%
                    slice_max(n, n = 10) %>% 
                    ungroup() %>%
                    arrange(name, -n)
  
  imagen_top_autor <- ggplot(autor_top_word, aes(y = reorder(word, n), x = n, fill = factor(name))) +
                      geom_bar(stat = "identity") +
                      ggtitle("Términos más frecuentes") +
                      ylab("Términos") +
                      facet_wrap(~ name, scales = "free") +
                      xlab("Frecuencia")
  
  imagen_top_autor
  
  
##Nube de palabras----
  
  train_cloud <- train_token %>% count(name, word) %>% group_by(name) %>% ungroup()
  
  table(train_cloud$name) %>% as.data.frame()
  
  cloud_lopez <- train_cloud %>% filter(name == "Lopez")
  cloud_uribe <- train_cloud %>% filter(name == "Uribe")
  cloud_petro <- train_cloud %>% filter(name == "Petro")
  
  wordcloud(train_cloud$word, freq = train_cloud$n, min.freq = 80, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  wordcloud(cloud_lopez$word, freq = cloud_lopez$n, min.freq = 80, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  wordcloud(cloud_uribe$word, freq = cloud_uribe$n, min.freq = 80, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  wordcloud(cloud_petro$word, freq = cloud_petro$n, min.freq = 80, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  
  
##Sentimientos de palabras----
  
  #necesita corrección
  tweet_sentimientos <- train_token %>%
    inner_join(get_sentiments("bing", language = ), by = "word") %>%
    group_by(name, sentiment) %>%
    summarise(count = n ()) %>%
    ungroup()
    
    
#Bigramas---------------------------------------------------
  
  tweets_bigrams_h <- train_clean %>% unnest_tokens(bigram, corpus, token = "ngrams", n = 2)
  tweets_bigrams_h <- tweets_bigrams_h %>% filter(str_count(bigram, " ") == 1)
  
  #Buscar bigramas con 1 palabra - Generan NA
  train_clean %>% filter (id == "fc739d769c4bb8ba16961be1")
  una_palabra <- train_clean %>% filter(str_count(text, "\\w+") == 1) 
  
  
##Histograma de bigramas----
  
  bigram_freq <- as.data.frame(tweets_bigrams_h) %>% count(name, bigram, sort = TRUE) 
  
  autor_top_terms <- bigram_freq %>%
      group_by(name) %>%
      slice_max(n, n = 10) %>% 
      ungroup() %>%
      arrange(name, -n)
    
#Gráfico
    
  imagen_top_bigram <- ggplot(autor_top_terms, aes(y = reorder(bigram, n), x = n, fill = factor(name))) +
                        geom_bar(stat = "identity") +
                        ggtitle("Bigramas más frecuentes") +
                        ylab("Bigramas") +
                        facet_wrap(~ name, scales = "free") +
                        xlab("Frecuencia")
   
  imagen_top_bigram
    
##Sentimientos de bigramas----
    
  tweets_bigrams_s <- tweets_bigrams_h %>% separate(bigram, c("word1", "word2"), sep = " ")
  
    
    
#Sentimientos LUCAS----
  
  model <- udpipe_download_model(language = "spanish")
  model <- udpipe_load_model(model$file_model)
  
  reviews <- udpipe_annotate(model, train_token$word)
  reviews_tibble <- as.data.frame(reviews) %>% select(token, upos)
  adjetivos <- reviews_tibble %>% filter(upos == "ADJ") %>% select(token)
  sentimientos_df <- get_nrc_sentiment(adjetivos$token, lang="spanish")
  sentimientos_df$adjetivos  = adjetivos$token
  
  #--Cambiar colores--
  adjetivos_buenos <-sentimientos_df[sentimientos_df$positive==1, "adjetivos"]
  wordcloud(adjetivos_buenos, min.freq = 10000,   colors= c(rgb(72/255, 191/255, 169/255),
                                                            rgb(249/255, 220/255, 92/255), 
                                                            rgb(229/255, 249/255, 147/255)))  
  
  bigramas_positivos <- train_bigrams_sep[train_bigrams_sep$word1 %in% adjetivos_buenos, ]
  wordcloud(bigramas_positivos$word2, min.freq = 10000,   colors= c(rgb(72/255, 191/255, 169/255),
                                                                    rgb(249/255, 220/255, 92/255), 
                                                                    rgb(229/255, 249/255, 147/255))) 

  