library(caret)
library(gridExtra)
library(magrittr)
library(dplyr)
library(e1071)

check_args <- function(input_args) {
  if(length(input_args) == 0){
    stop("USAGE: Rscript run_xgboost.R --train [train.csv] --test [test.csv] --predict [file.csv]", call.=FALSE)
  }else if(length(grep("--train", input_args)) != 1) {
    stop("missing flag", call.=FALSE)
  }else if(length(grep("--test", input_args)) != 1) {
    stop("missing flag", call.=FALSE)
  }else if(length(grep("--predict", input_args)) != 1) {
    stop("missing flag", call.=FALSE)
  }else{
    print("check args pass.")
  }
}

deal_missing <- function(df) {
  for (i in 1:ncol(df)) {
    u <- df[, i]
    if (is.numeric(u)) {
      #df[is.na(u), i] <- median(df[!is.na(u), i])
    } else {
      u[is.na(u)] <- "None"
      df[, i] <- as.factor(u)
    }
  }
  return(df)
}

main <- function() {
  # read parameters
  args = commandArgs(trailingOnly=TRUE)
  check_args(args)
  
  # parse parameters
  i <- 1 
  while(i < length(args))
  {
    if(args[i] == "--train"){
      train_csv <- args[i+1]
      i <- i+1
    }else if(args[i] == "--test"){
      test_csv <- args[i+1]
      i <- i+1
    }else if(args[i] == "--predict"){
      predict_file <- args[i+1]
      i <- i+1
    }else{
      stop(paste("Unknown flag", args[i]), call.=FALSE)
    }
    i <- i+1
  }
  
  # seed setting
  set.seed(87)
  
  # read training data
  training_data <- read.csv(train_csv, stringsAsFactors = F)
  cat('Training shape: ', dim(training_data))
  testing_data <- read.csv(test_csv, stringsAsFactors = F)
  cat('testing shape: ', dim(training_data))
  
  training_data <- training_data[,-1]
  testing_id <- testing_data$Id
  testing_data <- testing_data[,-1]
  testing_data$SalePrice <- NA

  # check correlation
  correlations <- cor(na.omit(training_data[,sapply(training_data[,1:80], typeof) == "integer"]))
  
  #row_indic <- round(abs(correlations[,37]), 1) >= 0.3
  row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
  
  print(dim(correlations[row_indic, row_indic]))
  col_names <- names(training_data)
  cor_col <- names(which(row_indic))
  chr_col <- col_names[!(col_names %in% names(row_indic))]
  col_names <- c(chr_col, cor_col)
  print(length(col_names))
  
  
  # old method for handling missing data.
  training_data <- deal_missing(training_data)
  testing_data <- deal_missing(testing_data)
  
  # new method for handling missing data
  training_data <- training_data[, col_names]
  testing_data <- testing_data[, col_names]
  
  training_data <- training_data[training_data$GrLivArea <= 4000,]
  print(dim(training_data))

  # chr missing
  # na not in train: MSZoning, Utilities, Exterior1st, Exterior2nd, KitchenQual, Functional, SaleType
  testing_data$MSZoning <- replace(testing_data$MSZoning, is.na(testing_data$MSZoning), names(which.max(table(training_data$MSZoning))))
  testing_data$Utilities <- replace(testing_data$Utilities, is.na(testing_data$Utilities), names(which.max(table(training_data$Utilities))))
  testing_data$Exterior1st <- replace(testing_data$Exterior1st, is.na(testing_data$Exterior1st), names(which.max(table(training_data$Exterior1st))))
  testing_data$Exterior2nd <- replace(testing_data$Exterior2nd, is.na(testing_data$Exterior2nd), names(which.max(table(training_data$Exterior2nd))))
  testing_data$KitchenQual <- replace(testing_data$KitchenQual, is.na(testing_data$KitchenQual), names(which.max(table(training_data$KitchenQual))))
  testing_data$Functional <- replace(testing_data$Functional, is.na(testing_data$Functional), names(which.max(table(training_data$Functional))))
  testing_data$SaleType <- replace(testing_data$SaleType, is.na(testing_data$SaleType), names(which.max(table(training_data$SaleType))))
  
  # replace all NA in charactor columns to "None"
  training_data[, chr_col] <- replace(training_data[, chr_col], is.na(training_data[, chr_col]), "None")
  testing_data[, chr_col] <- replace(testing_data[, chr_col], is.na(testing_data[, chr_col]), "None")
  
  # LotFrontage
  training_data$LotFrontage[which(is.na(training_data$LotFrontage))] <- median(training_data$LotFrontage, na.rm = T)
  testing_data$LotFrontage[which(is.na(testing_data$LotFrontage))] <- median(testing_data$LotFrontage, na.rm = T)
  
  # MasVnrArea
  training_data$MasVnrArea[which(is.na(training_data$MasVnrArea))] <- round(mean(training_data$MasVnrArea,na.rm=T), 0)
  testing_data$MasVnrArea[which(is.na(testing_data$MasVnrArea))] <- round(mean(testing_data$MasVnrArea,na.rm=T), 0)
  
  # GarageYrBlt
  training_data$GarageYrBlt[which(is.na(training_data$GarageYrBlt))] <- 0
  testing_data$GarageYrBlt[which(is.na(testing_data$GarageYrBlt))] <- 0
  
  # BsmtFinSF1
  testing_data$BsmtFinSF1[which(is.na(testing_data$BsmtFinSF1))] <- round(mean(testing_data$BsmtFinSF1,na.rm=T), 0)
  
  # TotalBsmtSF
  testing_data$TotalBsmtSF[which(is.na(testing_data$TotalBsmtSF))] <- round(mean(testing_data$TotalBsmtSF,na.rm=T), 0)
  
  # GarageCars 
  testing_data$GarageCars[which(is.na(testing_data$GarageCars))] <- median(testing_data$GarageCars, na.rm = T)
  
  # GarageArea
  testing_data$GarageArea[which(is.na(testing_data$GarageArea))] <- round(mean(testing_data$GarageArea,na.rm=T), 0)
  
  # ex: BsmtFinSF2, BsmtUnfSF, BsmtFullBath, BsmtHalfBath
  testing_data$BsmtUnfSF[which(is.na(testing_data$BsmtUnfSF))] <- round(mean(testing_data$BsmtUnfSF, na.rm = T))
  testing_data$BsmtFullBath[which(is.na(testing_data$BsmtFullBath))] <- median(testing_data$BsmtFullBath, na.rm = T)
  testing_data$BsmtHalfBath[which(is.na(testing_data$BsmtHalfBath))] <- median(testing_data$BsmtHalfBath, na.rm = T)
  
  
  # convert chr to factor
  training_data[sapply(training_data, is.factor)] <- lapply(training_data[sapply(training_data, is.factor)], as.numeric)
  testing_data[sapply(testing_data, is.factor)] <- lapply(testing_data[sapply(testing_data, is.factor)], as.numeric)
  
  train_miss <- c(which(colnames(training_data)=="PoolQC" ), which(colnames(training_data)=="MiscFeature" ), which(colnames(training_data)=="Alley" ), which(colnames(training_data)=="Fence" ), which(colnames(training_data)=="FireplaceQu" ))
  training_data <- training_data[,c(-train_miss)]
  test_miss <- c(which(colnames(testing_data)=="PoolQC" ), which(colnames(testing_data)=="MiscFeature" ), which(colnames(testing_data)=="Alley" ), which(colnames(testing_data)=="Fence" ), which(colnames(training_data)=="FireplaceQu" ))
  testing_data <- testing_data[,c(-test_miss)]

  
  # train model
  #model <- svm(SalePrice ~ ., data = training_data, cost = 1)
  tctrl <- tune.control(sampling = "cross", cross = 5)
  svm_model <- best.tune(svm, SalePrice ~ ., data = training_data, cost = 1, tunecontrol = tctrl)
  summary(svm_model)
  pred <- predict(svm_model, newdata = testing_data)
  tpred <- predict(svm_model, newdata = training_data)
  print(tpred)
  
  target <- training_data[, "SalePrice"]
  mse <- mean((pred - target)^2)
  mean_target <- mean(target)
  rmse <- sqrt(mse)
  rsquared <- 1 - sum((target - tpred)^2) / sum((target - mean(target))^2)
  errors <- abs(pred - target)
  mae <- mean(errors)
  cat("MSE:", mse, "RMSE:", rmse, "R-squared (Training):",  rsquared,"mae:" ,mae , "\n")
  
  
  if (length(strsplit(predict_file,"/")) > 1) {
    create_path <- unlist(strsplit(predict_file,"/"))
    create_path <- create_path[1:(length(create_path)-1)]
    now_path <- "."
    for(path in create_path){
      dir.create(file.path(now_path, path, fsep="/"), showWarnings = FALSE)
      now_path <- file.path(now_path, path, fsep="/")
    }
  }
  
  
  output_data <- data.frame(Id = testing_id, SalePrice = pred)
  print(head(output_data))
  write.csv(output_data, file = predict_file, row.names = FALSE, quote=FALSE)

}

main()