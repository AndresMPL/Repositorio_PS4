
#------------------------------------------------------------------------------#
#
#                              6 - PREDICTIONS
#
#------------------------------------------------------------------------------#

#Prueba 1-----------------------------------------------------------------------
tokenizer <- text_tokenizer(num_words = 1000)
tokenizer %>% fit_text_tokenizer(test_final$text)

x_prueba <- texts_to_matrix(tokenizer, test_final$text, mode ="tfidf")

y_hat_test <- model %>% predict(x_prueba) %>% k_argmax()

predicho <- factor(as.numeric(y_hat_test))
resultados <- data.frame(id = test_final$id, name = predicho)

#"Lopez", 1, ifelse(train_clean_2$name2 == "Petro", 2, 3))

resultados$name <- ifelse(resultados$name == 1, "Lopez", ifelse(resultados$name == 2, "Petro", "Uribe"))
resultados$name <- as.factor(resultados$name)
write.table(resultados, "submission_1.csv", row.names = FALSE, quote=FALSE, sep = ",")

#Prueba 2-----------------------------------------------------------------------

x_prueba <- as.matrix(tf_idf_reducido2)
y_hat_test <- model %>% predict(x_prueba) %>% k_argmax()
resultados <- data.frame(id = test_final$id, name = factor(as.numeric(y_hat_test)))
resultados$name <- ifelse(resultados$name == 1, "Lopez", ifelse(resultados$name == 2, "Petro", "Uribe"))
write.table(resultados, "submission_1.csv", row.names = FALSE, quote=FALSE, sep = ",")
