

#------------------------------------------------------------------------------#
#
#                            2 - LIMPIEZA TEST
#
#------------------------------------------------------------------------------#


#Limpiamos el texto--------------------------------------------------------

test_clean <- test %>% 
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

test_clean <- test_clean %>% mutate(n_palabras_i = str_count(text, "\\S+"))

#Tokens----

test_token <- test_clean %>% unnest_tokens("word", corpus)
test_token %>% count(word, sort = TRUE) %>% head()

sw <- c()
for (s in c("snowball", "stopwords-iso", "nltk")) {
  temp <- get_stopwords("spanish", source = s)$word
  sw <- c(sw, temp)
}
sw <- unique(sw)
sw <- unique(stri_trans_general(str = sw, id = "Latin-ASCII"))
sw <- data.frame(word = sw)

nrow(test_token)

test_token <- test_token %>% anti_join(sw, by = "word")

nrow(test_token)


##Lemma

udpipe::udpipe_download_model('spanish')
model <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe")

palabras_unicas2 <- test_token %>% distinct(word = test_token$word)

udpipe_results2 <- udpipe_annotate(model, x = palabras_unicas2$word)

udpipe_results2 <- as_tibble(udpipe_results2)

udpipe_results2 <- udpipe_results2 %>% select(token, lemma) %>% rename("word" = "token")

test_token <- test_token %>% left_join(udpipe_results2, by = "word", multiple = "all")

test_token[is.na(test_token$lemma), "lemma"] <- test_token[is.na(test_token$lemma), "word"]

conteo2 <- test_token %>% count(lemma) %>% arrange(desc(n)) %>% tail(1000)

#palabras_eliminar2 <- test_token %>% count(lemma) %>% filter(n < 10)
#test_token <- test_token %>% anti_join(palabras_eliminar2, by = "lemma") 

test_clean_2 <- test_token %>%
  group_by(id, n_palabras_i) %>% 
  summarise(text = str_c(lemma, collapse = " ")) %>%
  ungroup()

test_clean_2 <- test_clean_2 %>% mutate(n_palabras_i2 = str_count(text, "\\S+"))

diferencia2 <- setdiff(test_clean$id, test_clean_2$id) %>% as.data.frame()

dif2 <- nrow(diferencia2)
inicial2 <- nrow(test_clean)
final2 <- nrow(test_clean_2)

inicial2 - final2 #debe ser igual a dif2

test_clean %>% filter (id == "cb9ac947c675464803342fc9") #Ejemplo para validar

test_final <- test %>% select(id)
test_final <- test_final %>% left_join(test_clean_2)


#Matriz de Términos----

tm_corpus2 <- Corpus(VectorSource(x = test_final$text))
str(tm_corpus2)

tf_idf2 <- TermDocumentMatrix(tm_corpus2, control = list(weighting = weightTfIdf))
tf_idf2 <- as.matrix(tf_idf2) %>% t() %>% as.data.frame()

test_clean_2$text[1]
tf_idf2[1, 1:10]
head(tf_idf2)
dim(tf_idf2)

tf_idf_reducido2 <- tf_idf2 %>% select(all_of(colnames(X_train)))
dim(tf_idf_reducido2)

