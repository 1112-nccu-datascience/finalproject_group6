# [Group6] House Prices - Advanced Regression Techniques

## Table of contents

- [[Group6] House Prices - Advanced Regression Techniques](#group6-house-prices---advanced-regression-techniques)
  - [Contributors](#contributors)
  - [Goal](#goal)
  - [Quick start](#quick-start)
  - [Folder organization and its related description](#folder-organization-and-its-related-description)
    - [docs](#docs)
    - [data](#data)
    - [code](#code)
    - [results](#results)
  - [References](#references)

## Contributors
|組員|系級|學號|工作分配|
|-|-|-|-|
|黃大維|資科碩一|111753218|Random Forest, 報告, ppt, poster| 
|陳柏翰|資科碩一|111753208|XG Boost, 報告, ppt, poster|
|鄭安翔|資科碩專|110971029|Decision Tree, shiny|
|趙子翔|資科四|108703011|SVM, github|
|郭羿宏|資科四|108703039|無|

## Goal
使用Ames Housing dataset，開發預測房屋價格的回歸模型。  
通過不同的資料前處理，我們致力於改善模型的準確性和解釋力，以提供有價值的市場見解和投資策略，促進房地產市場的發展與效率提升。


## Quick start
You might provide an example commend or few commends to reproduce your analysis, i.e., the following R script
```R
Rscript code/random_forest.R --train [train.csv] --test [test.csv] --predict [file.csv]
Rscript code/xgboost.R --train [train.csv] --test [test.csv] --predict [file.csv]
Rscript code/svm.R --train [train.csv] --test [test.csv] --predict [file.csv]
Rscript code/decision_tree.R --train [train.csv] --test [test.csv] --predict [file.csv]
```


## Folder organization and its related description


### docs
* Presentation: [Group6_finalProject.pptx](docs/Group6_finalProject.pptx)
* Shinyapps: https://anselcheng.shinyapps.io/HousePrice/

### data

* Input
  * Source: [House Prices - Advanced Regression Techniques](https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques)
  * Format: 2個CSV檔案, 訓練資料和測試資料。79個feature欄位, 訓練資料含有SalePrice目標欄位。
  * Size: 訓練資料1460筆，測試資料1459筆。

* Output
  * Format: CSV file, 2欄位 (ID, SalePrice)


### code
* Preprocess
  * 資料刪減  
移除地上生活面積大於4000的資料(離群值)。  

  * 填補缺失值 
對所有欄位的缺失值分別全部填入欄的平均數、中位數、眾數，測試結果為平均數分數最高。  
針對部分欄位做個別的處理。  
ex.	對GarageCars類別進行眾數處理，因為它具有明顯的分類標籤的變量。  
部分欄位以N/A表示一種狀態，填入"None"處裡。

  * 保留高相關度資料  
以 0.3 為標準，保留高相關，並捨棄相關度較低的資料。  
ex. 泳池面積、紗窗門廊面積、其他配套設施費用等。

  * 對目標欄位進行處裡  
從SalePrice的直方圖中發現，他的樣本有偏移的現象。  
透過取對數來將資料拉回使為更集中。


* Methods

  * Random Forest
  * XGBoost
  * Support Vector Machine
  * Decision Tree


### results
* Evaluation

  * Null Model: LM Model
  * 5-fold Cross-validation
  * RMSE
  * R-Squared
  * MAE
 
|Method|RMSE|RSquared|MAE|
|-|-|-|-|
|Random Forest|25027.45|0.8981283|16188.14| 
|XGBoost|22337.72|0.916011|14366.37|
|Support Vector Machine|38866.25|0.7484611|25091.4|
|Decision Tree|47315.27|0.6185629|34599.72|

* Kaggle Score

|Method|Score|
|-|-|
|Random Forest|0.13036|
|XGBoost|0.14878|
|Support Vector Machine|0.22432|
|Decision Tree|0.23917|

* 對SalePrice欄位做對數處理

|Method|Score|
|-|-|
|XGBoost|0.13036|
|XGBoost (log)|0.12740|

* Rplot: [PDF](results/Rplots.pdf)

## References
* Packages you use
  * library(randomForest)
  * library(caret)
  * library(pROC)
  * library(xgboost)
  * library(gridExtra)
  * library(corrplot)
  * library(magrittr)
  * library(dplyr)
  * library(e1071)
  * library(randomForest)
  * library(quantregForest)
  * library(tidyverse)
  * library(skimr)
  * library(Amelia)
  * library(rpart)
  * library(rpart.plot)
  * library(mice)
  * library(ggfortify)

* Related publications  
  * [House Prices - Advanced Regression Techniques](https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques)  
  * [SVM](https://www.kaggle.com/code/mtyxwp/svm-simple)
