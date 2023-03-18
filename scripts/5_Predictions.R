
#------------------------------------------------------------------------------#
#
#                              6 - PREDICTIONS
#
#------------------------------------------------------------------------------#

#Prueba 1-----------------------------------------------------------------------

Z <- as.matrix(tf_idf_reducido2)
class(Z)
dim(Z)

y_hat_test <- model2 %>% predict(Z) %>% k_argmax()

predicho <- factor(as.numeric(y_hat_test))
resultados <- data.frame(id = test_final$id, name = predicho)

resultados$name <- ifelse(resultados$name == 1, "Lopez", ifelse(resultados$name == 2, "Petro", "Uribe"))
resultados$name <- as.factor(resultados$name)
write.table(resultados, "submission_1.csv", row.names = FALSE, quote=FALSE, sep = ",")



#Prueba 2-----------------------------------------------------------------------

x_prueba <- as.matrix(tf_idf_reducido2)
y_hat_test <- model2 %>% predict(x_prueba) %>% k_argmax()

resultados <- data.frame(id = test_final$id, name = factor(as.numeric(y_hat_test)))
resultados$name <- ifelse(resultados$name == 1, "Lopez", ifelse(resultados$name == 2, "Petro", "Uribe"))
write.table(resultados, "submission_1.csv", row.names = FALSE, quote=FALSE, sep = ",")
