#setwd("~/Google Drive/Meu Drive/Projetos/2022 - PosDoc/ELSA datasets and description files/R/fev-2023")
setwd("G:/Meu Drive/Projetos/2022 - PosDoc/ELSA datasets and description files/R/constructed_temporal_features/git")

library(foreign)
library(dplyr)
library(readxl)

rm(list = ls())
source('preprocessing-functions.R')

data_core  <- read.arff("../../../ElsacoreDD.arff")
data_nurse <- read.arff("../../../ElsanurseDD.arff")

df <- data_core
#df <- cbind(df, f_extract_feature(df, "indager", f_ratio_between_last_two_measurements, c("w7", "w8")))

attnames <- read_excel("att-list.xlsx", sheet="covariates")

lst_rename <- grep("-", attnames$covariate)
for(i in lst_rename) {
  lst_rename_w <- grep("X", attnames[i, 3:ncol(attnames)])
  new_attname <- gsub("-", "", attnames[i,2])
  for(j in lst_rename_w) {
    ix <- grep(paste0(attnames[i,2], "_", colnames(attnames)[j+2]), colnames(df))
    names(df)[ix] <- paste0(new_attname, "_", colnames(attnames)[j+2])
  }
  attnames[i,2] <- new_attname
}

atts <- c()
for(i in 1:nrow(attnames)) {
#for(i in 1:9) {
attname = attnames[i, 2]
  for (j in 3:ncol(attnames)) {
    if(!is.na(attnames[i,j])) {
      if(attnames[i,j] == "X") {
        atts <- append(atts, paste0(attname, "_", colnames(attnames)[j]))
      }
    }
  }
}
atts <- c(atts, "class_dementia_w8")
df <- df %>% select(atts)

# converter os factor para numérico
for(i in 1:nrow(attnames)) {
  attname = attnames[i, 2]
  attname_w <- grep("X", attnames[i, 3:ncol(attnames)])
  for (j in attname_w) {
    print(attname[[1]])
    ix <- grep(paste0(attnames[i,2], "_", colnames(attnames)[j+2]), colnames(df))
    if(is.factor(df[[ix]])) {
      df[ix] <- as.numeric(as.character(df[[ix]]))
    }
  }
}

atts <- paste(atts, collapse = " + ")
class_name <- "class_dementia_w8"
atts <- paste0(class_name, " ~ ", atts)
atts_f <- as.formula(atts)

