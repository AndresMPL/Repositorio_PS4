
##SUPER LEARNER

library(pacman)

p_load(xgboost,kernlab)

library(readr)
library(tm)
library(SnowballC)
library(SuperLearner)
library(caret)
library(glmnet)
library(kernlab)
library(xgboost)


#Crea una matriz de términos frecuentes

  corpus <- Corpus(VectorSource(train_clean$corpus))
  tdm <- DocumentTermMatrix(corpus, control=list(weighting=weightTfIdf))
  tdm <- removeSparseTerms(tdm, 0.995)


#Modelos a utilizar en el SL
  
  y <- as.factor(train_clean$name)
  x <- as.matrix(tdm)
  x <- as.data.frame(x)
  SL.library <- c("SL.glmnet", "SL.nnet", "SL.kknn", "SL.xgboost")

  
#Ejecutamos el SL

fit <- SuperLearner(y, x, family = "multinomial", SL.library = SL.library, method = "method.NNLS", cvControl = list(V = 5, stratify = TRUE))

#Predicciones

nuevos_tweets_limpio <- tm_map(Corpus(VectorSource(test$text)), content_transformer(tolower))
nuevos_tweets_limpio <- tm_map(nuevos_tweets_limpio, removeNumbers)
nuevos_tweets_limpio <- tm_map(nuevos_tweets_limpio, removePunctuation)
nuevos_tweets_limpio <- tm_map(nuevos_tweets_limpio, stripWhitespace)
nuevos_tweets_limpio <- tm_map(nuevos_tweets_limpio, stemDocument, language = "spanish")
tdm_nuevos_tweets <- TermDocumentMatrix(nuevos_tweets_limpio, control = list(dictionary = Terms(tdm)))
m_nuevos_tweets <- as.matrix(tdm_nuevos_tweets)
predictions <- predict(fit, m_nuevos_tweets)
