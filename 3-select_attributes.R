library(mlbench)
library(caret)
library(magrittr)
library(dplyr)
library(ROSE)

library(pROC)
library(ROCR)

library(ggplot2)

rm(list = ls())
load("Experimento6.RData")
filename <- "ElsaCore_CTF_Filtered_Over.RData"

new_df <- ovun.sample(atts_f, data = training, method = "over", seed = 123)$data
df <- new_df

#https://jtr13.github.io/cc21fall2/feature-selection-in-r.html
#results[[4]] -> glmboost

# calculate feature importance without models
roc_imp <- filterVarImp(x = df[, 1:441], y = df[, 442])
roc_imp_df <- data.frame(cbind(variable = rownames(roc_imp), score = roc_imp[,1]))
names(roc_imp_df)[1] <- "var"
roc_imp_df$score <- as.double(roc_imp_df$score)
roc_imp_df$norm <- (roc_imp_df$score - min(roc_imp_df$score)) /
  (max(roc_imp_df$score) - min(roc_imp_df$score))

# calculate feature importance with models
var_imp <- varImp(results[[4]], scale = FALSE)
var_imp_df <- as.data.frame(var_imp$importance)
var_imp_df$variavel <- row.names(var_imp_df)
names(var_imp_df)[1] <- "score"
names(var_imp_df)[2] <- "var"
var_imp_df <- var_imp_df[, c("var", "score")]
var_imp_df$norm <- (var_imp_df$score - min(var_imp_df$score)) /
  (max(var_imp_df$score) - min(var_imp_df$score))

# calculate feature importance using rfe
rfeResults <- rfe(atts_f,
                  data = df,
                  sizes = c(1:8),
                  rfeControl = rfeControl(functions = rfFuncs, method = "cv", number = 5))
rfe_df <- rfeResults$variables %>%
  group_by(var) %>%
  summarize(score = mean(Overall))
rfe_df$norm <- (rfe_df$score - min(rfe_df$score)) /
  (max(rfe_df$score) - min(rfe_df$score))

df_sum <- merge(roc_imp_df, var_imp_df, by = "var", all = TRUE)
df_sum <- merge(df_sum, rfe_df, by = "var", all = TRUE)
df_sum$total <- rowSums(df_sum[, c("norm.x", "norm.y", "norm")], na.rm = TRUE)
df_sum <- df_sum[order(df_sum$total,decreasing = TRUE),]

# number of attributes of original ELSA core: 168
class_name <- "class_dementia_w8"
df_sum <- df_sum[1:168,]
df_class <- df[[class_name]]
df <- select(df, one_of(df_sum$var))
atts_f <- as.formula(paste0(class_name, " ~ ", paste(names(df), collapse = " + ")))
df[[class_name]] <- df_class
save(df, atts_f, class_name, testing, file = filename)



