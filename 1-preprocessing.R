setwd("G:/Meu Drive/Projetos/2022 - PosDoc/ELSA datasets and description files/R/constructed_temporal_features")

library(foreign)
library(dplyr)
library(readxl)
library(openxlsx)

rm(list = ls())
source('0-preprocessing-functions.R')

###############################################################################
add_longitudinal_columns_1 <- function(my_env, f_params, att_name, f_op_name) {
  for (i in 1:nrow(f_params)) {
    if(my_env$attnames[[2]][i] == att_name) {
      for (j in seq(3, ncol(f_params), by = 1)) {
        if (!is.na(f_params[[j]][i])) {
          colname_A <- paste0(att_name, "_", f_params[[j]][i])
          if(deparse(substitute(f_op_name)) == "f_agebased_percentile") {
            colname_B <- paste0("indager_", f_params[[j]][i])
            l_df <- my_env$df %>% select(colname_A, colname_B)
          }
          else {
            l_df <- my_env$df %>% select(colname_A)
          }
          my_env$df <- cbind(my_env$df, f_op_name(l_df))
        }
      }
    }
  }
}

###############################################################################
add_longitudinal_columns_2 <- function(my_env, f_params, att_name, f_op_name) {
  for (i in 1:nrow(f_params)) {
    if(my_env$attnames[[2]][i] == att_name) {
      for (j in seq(3, ncol(f_params), by = 2)) {
        if (!is.na(f_params[[j]][i])) {
          colname_A <- paste0(att_name, "_", f_params[[j]][i])
          colname_B <- paste0(att_name, "_", f_params[[j+1]][i])
          l_df <- my_env$df %>% select(colname_A, colname_B)
          my_env$df <- cbind(my_env$df, f_op_name(l_df))
        }
      }
    }
  }
}
###############################################################################
add_longitudinal_columns_3 <- function(my_env, f_params, att_name, f_op_name) {
  for (i in 1:nrow(f_params)) {
    if(my_env$attnames[[2]][i] == att_name) {
      for (j in seq(3, ncol(f_params), by = 3)) {
        if (!is.na(f_params[[j]][i])) {
          colname_A <- paste0(att_name, "_", f_params[[j]][i])
          colname_B <- paste0(att_name, "_", f_params[[j+1]][i])
          colname_C <- paste0(att_name, "_", f_params[[j+2]][i])
          l_df <- my_env$df %>% select(colname_A, colname_B, colname_C)
          my_env$df <- cbind(my_env$df, f_op_name(l_df))
        }
      }
    }
  }
}


rename_names <- function(my_env) {
  lst_rename <- grep("-", my_env$attnames$covariate)
  for(i in lst_rename) {
    lst_rename_w <- grep("X", my_env$attnames[i, 3:ncol(my_env$attnames)])
    new_attname <- gsub("-", "", my_env$attnames[i,2])
    for(j in lst_rename_w) {
      ix <- grep(paste0(my_env$attnames[i,2], "_", colnames(my_env$attnames)[j+2]), colnames(my_env$df))
      names(my_env$df)[ix] <- paste0(new_attname, "_", colnames(my_env$attnames)[j+2])
    }
    my_env$attnames[i,2] <- new_attname
  }
}

###############################################################################

data_core  <- read.arff("../../ElsacoreDD.arff")
data_nurse <- read.arff("../../ElsanurseDD.arff")

my_env <- new.env()
my_env$df <- data_core

filename_params <- "att-list.xlsx"
my_env$attnames <- read_excel(filename_params, sheet="covariates")
rename_names(my_env)

# converter os atributos tipo factor para numeric
for(i in 1:nrow(my_env$attnames)) {
  attname = my_env$attnames[i, 2]
  attname_w <- grep("X", my_env$attnames[i, 3:ncol(my_env$attnames)])
  for (j in attname_w) {
    print(attname[[1]])
    if (colnames(my_env$attnames)[j+2] == "NA") {
      ix <- grep(my_env$attnames[i,2], colnames(my_env$df))
    }
    else {
      ix <- grep(paste0(my_env$attnames[i,2], "_", colnames(my_env$attnames)[j+2]), colnames(my_env$df))
    }
    if(is.factor(my_env$df[[ix]])) {
      my_env$df[ix] <- as.numeric(as.character(my_env$df[[ix]]))
    }
  }
}

###############################################################################
f_params <- read_excel(filename_params, sheet = "f_difference_between_last_two")
for(i in my_env$attnames[[2]]) {
  add_longitudinal_columns_2(my_env, f_params,
                           i, f_difference_between_last_two)
  l <- ncol(my_env$df)
}
print(dim(my_env$df))

###############################################################################
f_params <- read_excel(filename_params, sheet = "f_ratio_between_last_two")
for(i in my_env$attnames[[2]]) {
  add_longitudinal_columns_2(my_env, f_params,
                           i, f_ratio_between_last_two)
  l <- ncol(my_env$df)
}
print(dim(my_env$df))

###############################################################################
f_params <- read_excel(filename_params, sheet = "f_monotonicity")
for(i in my_env$attnames[[2]]) {
  add_longitudinal_columns_3(my_env, f_params,
                           i, f_monotonicity)
  l <- ncol(my_env$df)
}
print(dim(my_env$df))

###############################################################################
f_params <- read_excel(filename_params, sheet = "f_agebased_percentile")
for(i in my_env$attnames[[2]]) {

  add_longitudinal_columns_1(my_env, f_params,
                             i, f_agebased_percentile)
  l <- ncol(my_env$df)
}
print(dim(my_env$df))

atts <- c()
for(i in 1:nrow(my_env$attnames)) {
  attname = my_env$attnames[i, 2]
  for (j in 3:ncol(my_env$attnames)) {
    if(!is.na(my_env$attnames[i,j])) {
      if(my_env$attnames[i,j] == "X") {
        if (colnames(my_env$attnames)[j] == "NA") {
          atts <- append(atts, paste0(attname)[1])
        }
        else {
          atts <- append(atts, paste0(attname, "_", colnames(my_env$attnames)[j]))
        }
      }
    }
  }
}

#inclui os campos com os calculos longitudinais
for (i in grep("^f_", names(my_env$df))) {
  atts <- c(atts, names(my_env$df)[i])
}

atts <- c(atts, "class_dementia_w8")
my_env$df <- my_env$df %>% select(atts)

atts <- paste(atts, collapse = " + ")
class_name <- "class_dementia_w8"
atts <- paste0(class_name, " ~ ", atts)
atts_f <- as.formula(atts)
df <- my_env$df

save(df, atts_f, class_name, file="ElsaCore_CTF.RData")
