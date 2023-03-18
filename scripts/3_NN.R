
#------------------------------------------------------------------------------#
#
#                              4 - NEURAL ENTWORK
#
#------------------------------------------------------------------------------#

install.packages('keras')
library(keras)

#-----------------------------------------------------
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

sum(is.na(train_clean_2$name))

train_clean_2$name2 <- train_clean_2$name #Guardamos el nombre en una variable nueva

train_clean_2$name2 <- ifelse(train_clean_2$name2 == "Lopez", 1, ifelse(train_clean_2$name2 == "Petro", 2, 3))

train_clean_2$name2 <- as.factor(train_clean_2$name2)
table(train_clean_2$name2)

Y <- train_clean_2$name2
Y <- to_categorical(Y)
class(Y)
head(Y)
dim(Y)
colSums(Y)

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

rm(model)
model <- keras_model_sequential() 
model %>% 
        layer_dense(units = 4, activation = 'relu', input_shape = ncol(X_train)) %>% 
        layer_dropout(rate = 0.5) %>%
        layer_dense(units = 4, activation = 'softmax')

summary(model)

model %>% compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))

history <- model %>% fit(X_train, y_train, epochs = 100, batch_size = 2^8, validation_split = 0.2)

history_plot <- plot(history)
history_plot

model %>% evaluate(X_test, y_test)

y_hat <- model %>% predict(X_test) %>% k_argmax()


#library(caret)

confusionMatrix(data = factor(as.numeric(y_hat), levels = 1:3), 
                reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))



