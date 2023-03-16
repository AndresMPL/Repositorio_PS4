
#RED NEURONAL

set.seed(10101)

split <- sample.split(train$name, SplitRatio = 0.7)
train <- subset(train, split == TRUE)
test <- subset(train, split == FALSE)

corpus <- Corpus(VectorSource(train_clean$corpus))
dtm <- DocumentTermMatrix(corpus, control=list(weighting=weightTfIdf))
dtm <- removeSparseTerms(dtm, 0.995)

model <- keras_model_sequential() %>%
        layer_embedding(input_dim = nrow(dtm), output_dim = 16, input_length = ncol(dtm)) %>%
        layer_flatten() %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_dense(units = 3, activation = "softmax")

model %>% compile(
          loss = "categorical_crossentropy",
          optimizer = "adam",
          metrics = c("accuracy")
        )

train_x <- as.matrix(dtm[rownames(train), ])
train_y <- to_categorical(as.numeric(factor(train$name)))

model %>% fit(
          train_x, train_y,
          epochs = 10,
          batch_size = 32,
          validation_split = 0.2
        )