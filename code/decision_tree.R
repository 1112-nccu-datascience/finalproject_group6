library(rpart) #rpart
library(rpart.plot) #rpart.plot
library(caret)
library(gridExtra)
library(corrplot)
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

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

plotCorr <- function(data_in, i){
  data <- data.frame(x = data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data, aes(x = x, y = SalePrice)) + geom_point(shape = 1, na.rm = TRUE, color = "cadetblue") + xlab(paste0(colnames(data_in)[i], '\n', 'R-Squared: ', round(cor(data_in[[i]], data$SalePrice, use = 'complete.obs'), 2))) + theme_light()
  return(suppressWarnings(p))
}

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', linewidth = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
}

plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 45, hjust =1))
  return (p)
}

# Rscript run_xgboost.R --train data/train.csv --test data/test.csv --predict pred.csv
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
  
  # add some columns
  # training_data %>% mutate(YearOld = YrSold - YearBuilt, 
  #                          YearOldReno = YrSold - YearRemodAdd,
  #                          YearGar = YrSold - GarageYrBlt) -> training_data
  # testing_data %>% mutate(YearOld = YrSold - YearBuilt, 
  #                         YearOldReno = YrSold - YearRemodAdd,
  #                         YearGar = YrSold - GarageYrBlt) -> testing_data
  
  # check correlation
  correlations <- cor(na.omit(training_data[,sapply(training_data[,1:80], typeof) == "integer"]))
  #row_indic <- round(abs(correlations[,37]), 1) >= 0.3
  row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
  corrplot(correlations[row_indic, row_indic], method="square")
  print(dim(correlations[row_indic, row_indic]))
  col_names <- names(training_data)
  cor_col <- names(which(row_indic))
  chr_col <- col_names[!(col_names %in% names(row_indic))]
  col_names <- c(chr_col, cor_col)
  print(length(col_names))
  
  training_data <- training_data[, col_names]
  testing_data <- testing_data[, col_names]
  
  training_data <- training_data[training_data$GrLivArea <= 4000,]
  #training_data <- training_data[training_data$SalePrice < 350000,]
  print(dim(training_data))
  # check integer column
  print(summary(training_data[,sapply(training_data[,1:70], typeof) == "integer"]))
  print(ggplot(training_data, aes(x=Neighborhood, y=SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)))
  print(ggplot(training_data, aes(y=SalePrice,x=GrLivArea)) + geom_point())
  doPlots(training_data[,sapply(training_data[,1:70], typeof) == "integer"], fun = plotCorr, ii = 1:6)
  doPlots(training_data[,sapply(training_data[,1:70], typeof) == "integer"], fun = plotCorr, ii = 7:12)
  doPlots(training_data[,sapply(training_data[,1:70], typeof) == "integer"], fun = plotCorr, ii = 13:18)
  doPlots(training_data[,sapply(training_data[,1:70], typeof) == "integer"], fun = plotCorr, ii = 19:24)
  doPlots(training_data[,sapply(training_data[,1:70], typeof) == "integer"], fun = plotCorr, ii = 25:26)
  doPlots(training_data[,sapply(training_data[,1:70], typeof) == "character"], fun = plotHist, ii = 1:6)
  doPlots(training_data[,sapply(training_data[,1:70], typeof) == "character"], fun = plotHist, ii = 7:12)
  doPlots(training_data[,sapply(training_data[,1:70], typeof) == "character"], fun = plotHist, ii = 13:18)
  doPlots(training_data[,sapply(training_data[,1:70], typeof) == "character"], fun = plotHist, ii = 19:24)
  doPlots(training_data[,sapply(training_data[,1:70], typeof) == "character"], fun = plotHist, ii = 25:30)
  doPlots(training_data[,sapply(training_data[,1:70], typeof) == "character"], fun = plotHist, ii = 31:36)
  doPlots(training_data[,sapply(training_data[,1:70], typeof) == "character"], fun = plotHist, ii = 37:42)
  doPlots(training_data[,sapply(training_data[,1:70], typeof) == "character"], fun = plotHist, ii = 43)
  
  # check missing value
  print("train missing:")
  print(colSums(is.na(training_data))[colSums(is.na(training_data)) > 0])
  print("test missing:")
  print(colSums(is.na(testing_data))[colSums(is.na(testing_data)) > 0])
  
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
  
  # YearGar
  # training_data$YearGar[which(is.na(training_data$YearGar))] <- round(median(training_data$YearGar,na.rm=T), 0)
  # testing_data$YearGar[which(is.na(testing_data$YearGar))] <- round(median(testing_data$YearGar,na.rm=T), 0)
  
  # ex: BsmtFinSF2, BsmtUnfSF, BsmtFullBath, BsmtHalfBath
  #testing_data$BsmtFinSF2[which(is.na(testing_data$BsmtFinSF2))] <- round(mean(testing_data$BsmtFinSF2, na.rm = T))
  testing_data$BsmtUnfSF[which(is.na(testing_data$BsmtUnfSF))] <- round(mean(testing_data$BsmtUnfSF, na.rm = T))
  testing_data$BsmtFullBath[which(is.na(testing_data$BsmtFullBath))] <- median(testing_data$BsmtFullBath, na.rm = T)
  testing_data$BsmtHalfBath[which(is.na(testing_data$BsmtHalfBath))] <- median(testing_data$BsmtHalfBath, na.rm = T)
  
  
  # convert chr to factor
  training_data[, chr_col] <- lapply(training_data[, chr_col], as.factor)
  testing_data[, chr_col] <- lapply(testing_data[, chr_col], as.factor)
  testing_data <- testing_data[, -70]
  
  #print(str(testing_data))
  # transform excessively skewed features with log(x + 1)
  # skew_train <- sapply(cor_col,function(x){skewness(training_data[[x]],na.rm = T)})
  # skew_test <- sapply(cor_col[-27],function(x){skewness(testing_data[[x]],na.rm = T)})
  # skew_train <- skew_train[skew_train > 0.75]
  # skew_test <- skew_test[skew_test > 0.75]
  # 
  # for (x in names(skew_train)) {
  #   training_data[[x]] <- log(training_data[[x]] + 1)
  # }
  # 
  # for (x in names(skew_test)) {
  #   testing_data[[x]] <- log(testing_data[[x]] + 1)
  # }
  
  #training_data$SalePrice <- log(training_data$SalePrice + 1) 
  
  # training
  trctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
  
  # tune_grid <- expand.grid(nrounds=c(100,300,500),
  #                          max_depth = c(5:15),
  #                          eta = c(0.01, 0.05),
  #                          gamma = c(0.01),
  #                          colsample_bytree = c(0.75),
  #                          subsample = c(0.50),
  #                          min_child_weight = c(0))
  
  tune_grid <- expand.grid(nrounds = 300,
                           max_depth = 5,
                           eta = 0.05,
                           gamma = 0.01,
                           colsample_bytree = 0.75,
                           min_child_weight = 0,
                           subsample = 0.5)
  
  #print(table(training_data$Condition2))
  #print(table(testing_data$Condition2))
  ###model_tree
  model_tree <- rpart(SalePrice ~ .,data = training_data, method = "anova", minsplit=10)
  #Plotting best size of tree -> on minimum error
  plotcp(model_tree)
  minimum.error <- which.min(model_tree$cptable[, "xerror"])
  optimal.complexity <- model_tree$cptable[minimum.error, "CP"]
  points(minimum.error, model_tree$cptable[minimum.error, "xerror"],
         col = "red", pch = 19)
  model_prune <- prune(model_tree, cp = optimal.complexity)
  rpart.plot(model_tree, type=1, extra=100, box.palette ="-RdYlGn", branch.lty = 2)
  rpart.plot(model_prune, type=1, extra=100, box.palette ="-RdYlGn", branch.lty = 2)
  #how a tree is constructed - node by node
  par(mfrow = c(4,4))
  for(iframe in 1:nrow(model_prune$frame)) {
    cols <- ifelse(1:nrow(model_prune$frame) <= iframe, "deeppink4", "gray")
    prp(model_prune, col = cols, branch.col = cols, split.col = cols)
  }
  pred<- predict(model_tree, testing_data)
  corrplot(correlations[row_indic, row_indic], method="square")
  
  #print(exp(pred))
  # create dir
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
  write.csv(output_data, predict_file, row.names = FALSE, quote=FALSE)
}

main()