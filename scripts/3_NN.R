
#------------------------------------------------------------------------------#
#
#                              4 - NEURAL ENTWORK
#
#------------------------------------------------------------------------------#

install.packages('keras')
library(keras)

sum(is.na(train_clean_2$name))

table(train_clean_2$name)


train_clean_2$name2 <- ifelse(train_clean_2$name2 == "Lopez ", 1,
                            ifelse(train_clean_2$name2 == "Petro", 2, 3))

#train_clean_2$name2 <- as.factor(train_clean_2$name)

Y <- train_clean_2$name2
Y <- to_categorical(Y, 4)
class(Y)
head(Y)
dim(Y)

X <- as.matrix(tf_idf_reducido)
class(X)
dim(X)

set.seed(10101)
n <- nrow(train_clean_2)
data_rows <- floor(0.80 * n)
train_indices <- sample(1:n, data_rows)
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- Y[train_indices, ]
y_test <- Y[-train_indices, ]

n_h = nrow(X_train)/(2*(ncol(X_train) + 5))

model <- keras_model_sequential() 

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