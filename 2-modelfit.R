library(caret)
library(magrittr)
library(dplyr)
library(ROSE)

library(pROC)
library(ROCR)

library(ggplot2)

rm(list = ls())

# quando for my_data_CTS_filtered, as bases de treinamento e de teste já veem definidas
filename_dataset  <- "ElsaCore_CTF.RData"
filename_savedata <- "Experimento4.RData"

load(filename_dataset)

# Definir a função personalizada para calcular as medidas de desempenho
measure <- function(data, lev = NULL, model = NULL) {
  predictions <- data[, "pred"]
  labels <- data[, "obs"]
  conf_matrix <- table(predictions, labels)

  accuracy <- (conf_matrix[1, 1] + conf_matrix[2, 2]) / sum(conf_matrix)
  precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  sensitivity <-  recall
  specificity <- conf_matrix[1,1] / (conf_matrix[1,1] + conf_matrix[2,1])
  f1_score <- 2 * (precision * recall) / (precision + recall)

  c(accuracy = accuracy, precision = precision, recall = recall, specificity = specificity, f1_score = f1_score)
}

set.seed(123) #definir uma semente aleatória para reprodutibilidade
trainIndex <- createDataPartition(df[[class_name]], p = .7, list = FALSE) #criar índice de treinamento
training <- df[trainIndex, ] #conjunto de treinamento
testing <- df[-trainIndex, ] #conjunto de teste

new_df <- ovun.sample(atts_f, data = training, method = "over", seed = 123)$data
df <- new_df

testing_summ <- testing %>%
  group_by(class_dementia_w8) %>%
  summarize(count = n())

training_summ <- df %>%
  group_by(class_dementia_w8) %>%
  summarize(count = n())

# Defina o controle para o treinamento dos modelos
ctrl <- trainControl(method = "cv", number = 5, savePredictions = TRUE, summaryFunction = measure, verboseIter = FALSE)

models = c("rpart", "rf", "glm", "glmboost", "nnet")
results <- lapply(models, function(m) {
  print(m)
  train(atts_f, data = df, method = m, trControl = ctrl)
})

cm_lst   <- list()
acc_lst  <- list()
prec_lst <- list()
rec_lst  <- list()
f1_lst   <- list()
spec_lst <- list()
roc_lst  <- list()
auc_lst  <- list()

for (i in results) {
  pred <-  predict(i, newdata = testing)

  cm_lst   [[i$method]] <- confusionMatrix(pred, testing[[class_name]])
  acc_lst  [[i$method]] <- cm_lst[[i$method]]$overall["Accuracy"]
  prec_lst [[i$method]] <- cm_lst[[i$method]]$byClass["Precision"]
  rec_lst  [[i$method]] <- cm_lst[[i$method]]$byClass["Recall"]
  f1_lst   [[i$method]] <- cm_lst[[i$method]]$byClass["F1"]
  spec_lst [[i$method]] <- cm_lst[[i$method]]$byClass["Specificity"]
  roc_lst  [[i$method]] <- roc(as.numeric(testing[[class_name]])-1, as.numeric(pred)-1)
  auc_lst  [[i$method]] <- auc(roc(as.numeric(testing[[class_name]])-1, as.numeric(pred)-1))
}

save.image(file = filename_savedata)
