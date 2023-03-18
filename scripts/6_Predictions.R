


X_test_k <- as.matrix(tf_idf2)

X_test_k$y_hat_k <- model %>% predict(X_test_k) %>% k_argmax()
