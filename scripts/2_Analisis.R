
#------------------------------------------------------------------------------#
#
#                              2 - ANALISIS
#
#------------------------------------------------------------------------------#

n_tweets = train_clean_2 %>% group_by(name) %>% summarise(n = n(), n_palabras = mean(n_palabras_i)) %>% ungroup()

autor_tweets <- ggplot(n_tweets, aes(name, n, fill = name)) + 
                geom_col() + geom_text(aes(label = n), vjust = -1, colour = "black") + 
                ylim(c(0, 4000)) + theme_bw() + 
                scale_fill_manual(values = c("cadetblue3", "#CCEDB1", "#FFB90F")) + 
                theme(legend.position = "none") +
                labs(x = "Autor", y = "Número de tweets", title = "Número de tweets por autor")
autor_tweets

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
                      xlab("Frecuencia") + theme_bw() +
                      theme(legend.position = "none") 
                    
  imagen_top_autor


##Nube de palabras----

  train_cloud <- train_token %>% count(name, word) %>% group_by(name) %>% ungroup()
  
  table(train_cloud$name) %>% as.data.frame()
  
  cloud_lopez <- train_cloud  %>% filter(name == "Lopez") %>% select(word, n) %>% arrange(desc(n)) %>% head(50)
  cloud_uribe <- train_cloud %>% filter(name == "Uribe") %>% select(word, n) %>% arrange(desc(n)) %>% head(50)
  cloud_petro <- train_cloud %>% filter(name == "Petro") %>% select(word, n) %>% arrange(desc(n)) %>% head(50)
  
  wordcloud(train_cloud$word, freq = train_cloud$n, min.freq = 70, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  wordcloud(cloud_lopez$word, freq = cloud_lopez$n, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  wordcloud(cloud_uribe$word, freq = cloud_uribe$n, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  wordcloud(cloud_petro$word, freq = cloud_petro$n, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  
 
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
    head(2000) %>%
    rownames()
  
  tf_idf_reducido <- tf_idf %>% select(all_of(columnas_seleccionadas))
  dim(tf_idf_reducido)
  
  #save(train_clean, train_clean_2, tf_idf, tf_idf_reducido, file = "scripts//datos_para_modelar.RData")
  
  