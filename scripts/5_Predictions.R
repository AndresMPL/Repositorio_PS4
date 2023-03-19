
#------------------------------------------------------------------------------#
#
#                              6 - PREDICTIONS
#
#------------------------------------------------------------------------------#

#Prueba 1-----------------------------------------------------------------------

x_prueba <- as.matrix(tf_idf_reducido2)
y_hat_test <- model_3 %>% predict(x_prueba) %>% k_argmax()

resultados <- data.frame(id = test_final$id, name = factor(as.numeric(y_hat_test)))
resultados$name <- ifelse(resultados$name == 1, "Lopez", ifelse(resultados$name == 2, "Petro", "Uribe"))
write.table(resultados, "submission_5.csv", row.names = FALSE, quote=FALSE, sep = ",")

