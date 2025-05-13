library(glmnet)

years <- unique(year(df_input$date_cible))
results <- list()

# Validation croisée avec les modèles Ridge, Lasso et Elastic Net

for (yy_test in years) {
  # Split temporel
  df_input[, train_test := fcase(
    year(date_cible) == yy_test, "test",
    year(date_cible) == 2022 & yy_test != 2022, "out",
    default = "train"
  )]
  
  features <- setdiff(names(df_input), c("date_cible", "FC", "train_test"))
  
  x_train <- as.matrix(df_input[train_test == "train", ..features])
  y_train <- df_input[train_test == "train", FC]
  
  x_test <- as.matrix(df_input[train_test == "test", ..features])
  y_test <- df_input[train_test == "test", FC]
  
  models <- list(
    ridge = cv.glmnet(x_train, y_train, alpha = 0),     # Ridge
    lasso = cv.glmnet(x_train, y_train, alpha = 1),     # Lasso
    elastic = cv.glmnet(x_train, y_train, alpha = 0.5)  # Elastic Net
  )
  
  preds <- data.table(
    date_cible = df_input[train_test == "test", date_cible],
    FC_true = y_test,
    ridge = predict(models$ridge, newx = x_test, s = "lambda.min")[, 1],
    lasso = predict(models$lasso, newx = x_test, s = "lambda.min")[, 1],
    elastic = predict(models$elastic, newx = x_test, s = "lambda.min")[, 1]
  )
  
  results[[as.character(yy_test)]] <- preds
}

# Calcul des RMSE
final_dt <- rbindlist(results, fill = TRUE)

metrics <- final_dt[, 
                    .(
                      rmse_ridge = rmse(FC_true, ridge),
                      rmse_lasso = rmse(FC_true, lasso),
                      rmse_elastic = rmse(FC_true, elastic)
                    ), 
                    by = year(date_cible)
]

## Résultats

print(metrics)
erreurs <- metrics
print(c(mean(erreurs$rmse_elastic),mean(erreurs$rmse_lasso),mean(erreurs$rmse_ridge)))