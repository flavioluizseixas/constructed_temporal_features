#setwd("G:/Meu Drive/Projetos/2022 - PosDoc/ELSA datasets and description files/R/constructed_temporal_features/git")

library(tidyr)
library(ggplot2)

rm(list = ls())
acc_df  <- data.frame()
prec_df <- data.frame()
rec_df  <- data.frame()
f1_df   <- data.frame()
spec_df <- data.frame()
auc_df  <- data.frame()

cmp = c(1,2)
filename_ext = "1-2.png"

transformar <- function(a) {
  df_long <- pivot_longer(a, cols = head(names(a), n=length(a)-1),
                          names_to = "coluna", values_to = "valor",
                          names_pattern = "coluna(.+)")
  df_long$coluna = rep(head(names(a), n=length(a)-1), times = 2)
  return(df_long)
}

load("Experimento1.RData")
acc_df  <- setNames(data.frame(matrix(nrow = 0, ncol = length(acc_lst)+1)), c(names(acc_lst), "Experimento"))
prec_df <- setNames(data.frame(matrix(nrow = 0, ncol = length(acc_lst)+1)), c(names(acc_lst), "Experimento"))
rec_df  <- setNames(data.frame(matrix(nrow = 0, ncol = length(acc_lst)+1)), c(names(acc_lst), "Experimento"))
spec_df <- setNames(data.frame(matrix(nrow = 0, ncol = length(acc_lst)+1)), c(names(acc_lst), "Experimento"))
f1_df   <- setNames(data.frame(matrix(nrow = 0, ncol = length(acc_lst)+1)), c(names(acc_lst), "Experimento"))
auc_df  <- setNames(data.frame(matrix(nrow = 0, ncol = length(acc_lst)+1)), c(names(acc_lst), "Experimento"))

for (c in cmp) {
  load(paste0("Experimento", c, ".RData"))
  acc_df  <- rbind(acc_df,  c(acc_lst,  Experimento = c))
  prec_df <- rbind(prec_df, c(prec_lst, Experimento = c))
  rec_df  <- rbind(rec_df,  c(rec_lst,  Experimento = c))
  spec_df <- rbind(spec_df, c(spec_lst,  Experimento = c))
  f1_df   <- rbind(f1_df,   c(f1_lst,   Experimento = c))
  auc_df  <- rbind(auc_df,  c(auc_lst,  Experimento = c))
}

ggplot(transformar(acc_df), aes(x = coluna, y = valor, fill = Experimento)) +
  geom_col(position = "dodge") +
  facet_grid(. ~ Experimento) +
  geom_text(aes(label = sprintf("%.2f", valor)), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle("Accuracy") +
  guides(fill = FALSE)
ggsave(paste0("acc_", filename_ext))

ggplot(transformar(prec_df), aes(x = coluna, y = valor, fill = Experimento)) +
  geom_col(position = "dodge") +
  facet_grid(. ~ Experimento) +
  geom_text(aes(label = sprintf("%.2f", valor)), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle("Precision") +
  guides(fill = FALSE)
ggsave(paste0("prec_", filename_ext))

ggplot(transformar(rec_df), aes(x = coluna, y = valor, fill = Experimento)) +
  geom_col(position = "dodge") +
  facet_grid(. ~ Experimento) +
  geom_text(aes(label = sprintf("%.2f", valor)), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle("Recall") +
  guides(fill = FALSE)
ggsave(paste0("rec_", filename_ext))

ggplot(transformar(spec_df), aes(x = coluna, y = valor, fill = Experimento)) +
  geom_col(position = "dodge") +
  facet_grid(. ~ Experimento) +
  geom_text(aes(label = sprintf("%.2f", valor)), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle("Specificity") +
  guides(fill = FALSE)
ggsave(paste0("spec_", filename_ext))

ggplot(transformar(f1_df), aes(x = coluna, y = valor, fill = Experimento)) +
  geom_col(position = "dodge") +
  facet_grid(. ~ Experimento) +
  geom_text(aes(label = sprintf("%.2f", valor)), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle("F1 score") +
  guides(fill = FALSE)
ggsave(paste0("f1_", filename_ext))

ggplot(transformar(auc_df), aes(x = coluna, y = valor, fill = Experimento)) +
  geom_col(position = "dodge") +
  facet_grid(. ~ Experimento) +
  geom_text(aes(label = sprintf("%.2f", valor)), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle("Area under ROC curve") +
  guides(fill = FALSE)
ggsave(paste0("auc_", filename_ext))
