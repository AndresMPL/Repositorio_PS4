
#------------------------------------------------------------------------------#
#
#                              4 - NEURAL ENTWORK
#
#------------------------------------------------------------------------------#

library(keras)

#-----------------------------------------------------
  
  #Instalación de TF
      install.packages("tensorflow")
      library(reticulate)
      path_to_python <- install_python()
      virtualenv_create("r-reticulate", python = path_to_python)
      library(tensorflow)
      install_tensorflow(envname = "r-reticulate")
      install.packages("keras")
      library(keras)
      install_keras(envname = "r-reticulate")
      library(tensorflow)
      tf$constant("Hello Tensorflow!")

#------------------------------------------------------

  sum(is.na(train_clean_2$name)) #Validamos que todos los registros están diligenciados
  
  train_clean_2$name2 <- train_clean_2$name  #Guardamos el nombre en una variable nueva para generar factores
  
  train_clean_2$name2 <- ifelse(train_clean_2$name2 == "Lopez", 1, 
                         ifelse(train_clean_2$name2 == "Petro", 2, 3))
  
  train_clean_2$name2 <- as.factor(train_clean_2$name2)
  table(train_clean_2$name2)
  
  Y <- train_clean_2$name2
  Y <- to_categorical(Y)
       class(Y)
       head(Y)
       dim(Y)
       colSums(Y) #Verificamos que coincida con los valores de la tabla de nombres
  
  X <- as.matrix(tf_idf_reducido)
  class(X)
  dim(X)
  
  set.seed(10101)
  
  n <- nrow(train_clean_2)
  data_rows <- floor(0.70 * n)
  train_indices <- sample(1:n, data_rows)
  X_train <- X[train_indices, ]
  X_test <- X[-train_indices, ]
  y_train <- Y[train_indices, ]
  y_test <- Y[-train_indices, ]
  
  n_h = nrow(X_train)/(2*(ncol(X_train) + 5))


##Modelo 1---------------------------------------------------------------------

##1500 palabras

  rm(model_1)
  model_1 <- keras_model_sequential() 
  model_1 %>% 
          layer_dense(units = 2, activation = 'relu', input_shape = ncol(X_train)) %>% 
          layer_dropout(rate = 0.5) %>%
          layer_dense(units = 4, activation = 'softmax')
  
  summary(model_1)
  
  model_1 %>% compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics = c('accuracy'))
  
  history_1 <- model_1 %>% fit(X_train, y_train, epochs = 100, batch_size = 2^8, validation_split = 0.2)
  
  history_plot_1 <- plot(history_1)
  history_plot_1
  
  model_1 %>% evaluate(X_test, y_test)
  
  y_hat_1 <- model_1 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_1), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))


##Modelo 2---------------------------------------------------------------------

  #layer_dense units = 2
  #epochs = 200
  #batch_size = 2^8
  #2296
  
  rm(model_2)
  model_2 <- keras_model_sequential() 
  model_2 %>% 
    layer_dense(units = 2, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_2)
  
  model_2 %>% compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics = c('accuracy'))
  
  history2 <- model_2 %>% fit(X_train, y_train, epochs = 100, batch_size = 2^8, validation_split = 0.2)
  
  history_plot_2 <- plot(history_2)
  history_plot_2
  
  model_2 %>% evaluate(X_test, y_test)
  
  y_hat_2 <- model_2 %>% predict(X_test) %>% k_argmax()
  
  
  #library(caret)
  
  confusionMatrix(data = factor(as.numeric(y_hat_2), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))


##Modelo 3---------------------------------------------------------------------

  rm(model_3)
  model_3 <- keras_model_sequential() 
  model_3 %>% 
    layer_dense(units = 10, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_3)
  
  model_3 %>% compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_3 <- model_3 %>% fit(X_train, y_train, epochs = 100, batch_size = 2^8, validation_split = 0.2)
  
  history_plot_3 <- plot(history_3)
  history_plot_3
  
  model_3 %>% evaluate(X_test, y_test)
  
  y_hat_3 <- model_3 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_3), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))


##Modelo 4---------------------------------------------------------------------

  rm(model_4)
  model4 <- keras_model_sequential() 
  model4 %>% 
    layer_dense(units = 1, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_4)
  
  model_4 %>% compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_4 <- model_4 %>% fit(X_train, y_train, epochs = 200, batch_size = 2^8, validation_split = 0.2)
  
  history_plot_4 <- plot(history_4)
  history_plot_4
  
  model4 %>% evaluate(X_test, y_test)
  
  y_hat_4 <- model4 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_4), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))