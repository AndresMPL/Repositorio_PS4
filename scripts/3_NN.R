
#------------------------------------------------------------------------------#
#
#                              4 - NEURAL ENTWORK
#
#------------------------------------------------------------------------------#

install.packages('keras')
library(keras)

sum(is.na(train_clean_2$name))

table(train_clean_2$name)

Y <- as.factor(train_clean_2$name)
Y <- dummy_cols(Y)
Y <- as.matrix(Y)
class(Y)
head(Y)
dim(Y)

X <- as.matrix(tf_idf_reducido)
class(X)
dim(X)
