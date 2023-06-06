# [Group6] House Prices - Advanced Regression Techniques

## Contributors
|組員|系級|學號|工作分配|
|-|-|-|-|
|黃大維|資科碩一|111753218|Random Forest, 報告, ppt, poster| 
|陳柏翰|資科碩一|111753208|XG Boost, 報告, ppt, poster|
|鄭安翔|資科碩專|110971029|Decision Tree, shiny|
|趙子翔|資科四|108703011|SVM, github|
|郭羿宏|資科四|108703039|無|

## Goal
預測房屋價格。

## Quick start
You might provide an example commend or few commends to reproduce your analysis, i.e., the following R script
```R
Rscript code/random_forest.R --train [train.csv] --test [test.csv] --predict [file.csv]
Rscript code/run_xgboost.R --train [train.csv] --test [test.csv] --predict [file.csv]
Rscript code/svm.R --train [train.csv] --test [test.csv] --predict [file.csv]
Rscript code/HousePrice.R --train [train.csv] --test [test.csv] --predict [file.csv]
```

## Folder organization and its related description
idea by Noble WS (2009) [A Quick Guide to Organizing Computational Biology Projects.](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1000424) PLoS Comput Biol 5(7): e1000424.

### docs
* Your presentation, 1112_DS-FP_groupID.ppt/pptx/pdf (i.e.,1112_DS-FP_group1.ppt), by **06.08**
* Any related document for the project
  * i.e., software user guide
  * Shinyapps: https://anselcheng.shinyapps.io/HousePrice/
### data
* Input
  * Source
  * Format
  * Size 
* Output

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
