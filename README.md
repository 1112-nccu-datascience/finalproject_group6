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
Rscript code/run_xgboost.R --train [train.csv] --test [test.csv] --predict [file.csv]
Rscript code/svm.R --train [train.csv] --test [test.csv] --predict [file.csv]
Rscript code/HousePrice.R --train [train.csv] --test [test.csv] --predict [file.csv]
```

## Folder organization and its related description

### docs
* Your presentation, 1112_DS-FP_groupID.ppt/pptx/pdf (i.e.,1112_DS-FP_group1.ppt), by **06.08**
* Shinyapps: https://anselcheng.shinyapps.io/HousePrice/

### data
* Input
  * Source: [House Prices - Advanced Regression Techniques](https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques)
  * Format: 2個CSV檔案, 訓練資料和測試資料。79個feature欄位, 訓練資料含有SalePrice目標欄位。
  * Size: 訓練資料1460筆，測試資料1459筆。
* Output
  * Format: CSV file, 2欄位 (ID, SalePrice)
  * Size: 訓練資料1460筆，測試資料1459筆

### code
* Analysis steps
* Which method or package do you use? 
  * original packages in the paper
  * additional packages you found

### results
* What is a null model for comparison?
* How do your perform evaluation?
  * Cross-validation, or extra separated data

## References
* Packages you use
* Related publications  
https://www.kaggle.com/code/mtyxwp/svm-simple
