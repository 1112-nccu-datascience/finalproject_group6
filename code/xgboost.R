library(xgboost)
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

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
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
  
  # Pie Chart
  SalePrice_cut <- data.frame(SalePrice = cut(training_data$SalePrice, breaks = c(0, 100000, 200000, 300000, max(training_data$SalePrice)), c('0-100k', '100k-200k', '200k-300k', '300k+')))
  SalePrice_freq <- table(SalePrice_cut$SalePrice)
  SalePrice_percentage <- prop.table(SalePrice_freq) * 100
  SalePrice_cut <- data.frame(Category = names(SalePrice_freq), Frequency = as.numeric(SalePrice_freq), Percentage = SalePrice_percentage)
  print(
    ggplot(SalePrice_cut, aes(x = "", y = Frequency, fill = Category)) + 
      geom_bar(width = 1, stat = "identity") + 
      coord_polar("y") + 
      labs(fill = "Sale Price", x = NULL, y = NULL, title = "Pie Chart of Sale Prices") + 
      geom_text(aes(label = paste0(round(Percentage.Freq), "%")), position = position_stack(vjust = 0.5)) + 
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  print(ggplot(training_data, aes(y=SalePrice,x=GrLivArea)) + geom_point() + geom_vline(aes(xintercept=4000), colour="#BB0000", linetype="dashed"))
  print(ggplot(training_data, aes(x=Neighborhood, y=SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)))
  
  training_data <- training_data[training_data$GrLivArea <= 4000,]
  #training_data <- training_data[training_data$SalePrice < 350000,]
  
  # check correlation
  correlations <- cor(na.omit(training_data[,sapply(training_data[,1:80], typeof) == "integer"]))
  
  #row_indic <- round(abs(correlations[,37]), 1) >= 0.3
  row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
  
  corrplot(correlations, method="square")
  corrplot(correlations, method="color", type = "lower")
  print(dim(correlations[row_indic, row_indic]))
  col_names <- names(training_data)
  cor_col <- names(which(row_indic))
  chr_col <- col_names[!(col_names %in% names(row_indic))]
  col_names <- c(chr_col, cor_col)
  print(length(col_names))
  
  training_data <- training_data[, col_names]
  testing_data <- testing_data[, col_names]
  
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
  
  print(
    ggplot(data=training_data, aes(x=factor(PoolQC), fill=PoolQC)) + stat_count() + theme_light() + 
      theme(axis.text.x = element_text(angle = 45, hjust =1)) + 
      labs(x = "PoolQC", y = "Count") + 
      ggtitle("PoolQC in train") + 
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  print(
    ggplot(data=testing_data, aes(x=factor(PoolQC), fill=PoolQC)) + stat_count() + theme_light() + 
      theme(axis.text.x = element_text(angle = 45, hjust =1)) + 
      labs(x = "PoolQC", y = "Count") + 
      ggtitle("PoolQC in test") + 
      theme(plot.title = element_text(hjust = 0.5))
  )
  
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
  #testing_data$GarageCars[which(is.na(testing_data$GarageCars))] <- median(testing_data$GarageCars, na.rm = T)
  testing_data$GarageCars <- replace(testing_data$GarageCars, is.na(testing_data$GarageCars), as.numeric(names(which.max(table(testing_data$GarageCars)))))
  
  # GarageArea
  testing_data$GarageArea[which(is.na(testing_data$GarageArea))] <- round(mean(testing_data$GarageArea,na.rm=T), 0)

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
  # train_miss <- c(which(colnames(training_data)=="PoolQC" ), which(colnames(training_data)=="MiscFeature" ), which(colnames(training_data)=="Alley" ), which(colnames(training_data)=="Fence" ), which(colnames(training_data)=="FireplaceQu" ))
  # training_data <- training_data[,c(-train_miss)]
  # test_miss <- c(which(colnames(testing_data)=="PoolQC" ), which(colnames(testing_data)=="MiscFeature" ), which(colnames(testing_data)=="Alley" ), which(colnames(testing_data)=="Fence" ), which(colnames(training_data)=="FireplaceQu" ))
  # testing_data <- testing_data[,c(-test_miss)]
  # 
  print(
    ggplot(training_data) + geom_density(aes(SalePrice), fill = "blue", alpha = 0.5) +
      labs(x = "SalePrice", y = "Density") +
      ggtitle("Density Plot of SalePrice") +
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  training_data$SalePrice <- log(training_data$SalePrice) 
  
  print(
    ggplot(training_data) + geom_density(aes(log(SalePrice)), fill = "blue", alpha = 0.5) +
      labs(x = "SalePrice", y = "Density") +
      ggtitle("Density Plot of Log(SalePrice)") +
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  
  # filt important features
  # importance_feature <- c("OverallQual", "GrLivArea", "TotalBsmtSF", "GarageArea", "YearBuilt",
  #                         "YearRemodAdd", "LotArea", "BsmtFinSF1", "GarageCars", "OverallCond")
  # training_data <- training_data[, c(importance_feature, "SalePrice")]
  # testing_data <- testing_data[, importance_feature]

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
  
  xgb_model <- train(SalePrice ~., data = training_data, method = "xgbTree",
                 trControl = trctrl,
                 tuneGrid = tune_grid)

  print(xgb_model)
  pred <- predict(xgb_model, newdata = testing_data)

  corrplot(correlations[row_indic, row_indic], method="square")
  corrplot(correlations[row_indic, row_indic], method="color", type = "lower")
  
  # print feature important
  importance <- varImp(xgb_model)
  print(ggplot(importance, top=20) + geom_hline(aes(yintercept=10), colour="#BB0000", linetype="dashed"))
  
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

  output_data <- data.frame(Id = testing_id, SalePrice = exp(pred))
  print(head(output_data))
  write.csv(output_data, predict_file, row.names = FALSE, quote=FALSE)
}

main()