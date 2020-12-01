# Improve Bar's Yelp Ratings!

This repository utilizes Yelp open data to draw insight on ways to improve bars' Yelp ratings. The descriptions of four directories are as following. 


* `Data`: Contains the original and cleaned dataset. `bars.csv` an `business_bars.csv`, `used_data.csv` are the original, cleaned and used-in-models business data set. The `*.RData` files are used for ShinyApp. The `positive - words.txt` and `negative - words.txt` are tokenized positive / negative words from review texts. Descriptions of three subdirectories can be found inside.

* `Code`: Contains R codes for data cleaning, different model construction and figure producing.

* `Figure`: Contains figures for model diagnostics (`Figure/Diagnostic`) of ordinal logistic regression, EDA plots (`Figure/basic plot`) and word related plots (`word related plot`).

* `ShinyApp`: Contains the code to produce shiny app in R.

Our [web-based app](https://xmiao.shinyapps.io/ImproveYelpRatings/) implements our proposed model. 


