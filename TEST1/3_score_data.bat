@echo off
:: Use this line of code for debugging purposes *** Adapt the paths for R installation and R script!!!
::"C:\Program Files\R\R-3.5.1\bin\R.exe" CMD BATCH "C:\LazyTrading\GitHub\R_markettype\TEST1\3_score_data.R"
:: Use this code in 'production'
"C:\Program Files\R\R-3.5.1\bin\Rscript.exe" "C:\LazyTrading\GitHub\R_markettype\TEST1\3_score_data.R"
