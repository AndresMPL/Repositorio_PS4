
#------------------------------------------------------------------------------#
#
#                              2 - ANALISIS
#
#------------------------------------------------------------------------------#

n_tweets = train %>% group_by(name) %>% summarise(n = n()) %>% ungroup()

autor_tweets <- ggplot(n_tweets, aes(name, n, fill = name)) + 
                geom_col() + geom_text(aes(label = n), vjust = -1, colour = "black") + 
                ylim(c(0, 2700)) + theme_bw() + 
                scale_fill_manual(values = c("cadetblue3", "#CCEDB1", "#FFB90F")) + 
                theme(legend.position = "none") +
                labs(x = "Autor", y = "Número de tweets")


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
                      xlab("Frecuencia") + 
                      theme(legend.position = "none")
                    
  imagen_top_autor


##Nube de palabras----

  train_cloud <- train_token %>% count(name, word) %>% group_by(name) %>% ungroup()
  
  table(train_cloud$name) %>% as.data.frame()
  
  cloud_lopez <- train_cloud  %>% filter(name == "Lopez") %>% select(word, n) %>% arrange(desc(n)) %>% head(50)
  cloud_uribe <- train_cloud %>% filter(name == "Uribe") %>% select(word, n) %>% arrange(desc(n)) %>% head(50)
  cloud_petro <- train_cloud %>% filter(name == "Petro") %>% select(word, n) %>% arrange(desc(n)) %>% head(50)
  
  wordcloud(train_cloud$word, freq = train_cloud$n, min.freq = 80, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  wordcloud(cloud_lopez$word, freq = cloud_lopez$n, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  wordcloud(cloud_uribe$word, freq = cloud_uribe$n, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  wordcloud(cloud_petro$word, freq = cloud_petro$n, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  
  wordcloud2(data = cloud_lopez)
  wordcloud2(data = cloud_uribe)
  wordcloud2(data = cloud_petro)

##Lemma
  
  #udpipe::udpipe_download_model('spanish')
  model <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe")
  
  palabras_unicas <- train_token %>% distinct(word = train_token$word)
  
  udpipe_results <- udpipe_annotate(model, x = palabras_unicas$word)
  
  udpipe_results <- as_tibble(udpipe_results)
  
  udpipe_results <- udpipe_results %>% select(token, lemma) %>% rename("word" = "token")
 
  train_token <- train_token %>% left_join(udpipe_results, by = "word", multiple = "all")
  
  train_token[is.na(train_token$lemma), "lemma"] <- train_token[is.na(train_token$lemma), "word"]
  
  train_token %>% count(lemma) %>% arrange(desc(n)) %>% tail(100)

  palabras_eliminar <- train_token %>% count(lemma) %>% filter(n < 10)
  
  train_token <- train_token %>% anti_join(palabras_eliminar, by = "lemma") 
  
  train_clean_2 <- train_token %>%
    group_by(name, id) %>% 
    summarise(text = str_c(lemma, collapse = " ")) %>%
    ungroup()
  
  diferencia <- setdiff(train_clean$id, train_clean_2$id) %>% as.data.frame()
  
  dif <- nrow(diferencia)
  inicial <- nrow(train_clean)
  final <- nrow(train_clean_2)
  
  inicial - final #debe ser igual a dif
  
  train_clean %>% filter (id == "ce1464da0f03a61f2659947b") #Ejemplo para validar
  
  
#Matriz de Términos----
  
  tm_corpus <- Corpus(VectorSource(x = train_clean_2$text))
  str(tm_corpus)
  
  tf_idf <- TermDocumentMatrix(tm_corpus, control = list(weighting = weightTfIdf))
  tf_idf <- as.matrix(tf_idf) %>% t() %>% as.data.frame()
  
  train_clean_2$text[1]
  tf_idf[1, 1:10]
  head(tf_idf)
  dim(tf_idf)
  
  columnas_seleccionadas <- colSums(tf_idf) %>%
    data.frame() %>%
    arrange(desc(.)) %>%
    head(1000) %>%
    rownames()
  
  tf_idf_reducido <- tf_idf %>% select(all_of(columnas_seleccionadas))
  dim(tf_idf_reducido)
  
  save(train_clean, train_clean_2, tf_idf, tf_idf_reducido, file = "scripts//datos_para_modelar.RData")
  
  