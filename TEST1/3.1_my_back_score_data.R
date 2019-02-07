# ----------------------------------------------------------------------------------------
# R Script to score current period of each currency based on the newly entered data
# ----------------------------------------------------------------------------------------

# Expected output: Table containing market type number for every of the 28 currency pairs written as files to the sandboxes of terminals
library(tidyverse)
library(lubridate)
library(h2o)
source("C:/LazyTrading/GitHub/R_markettype/TEST1/evaluate_market_type.R")
source("C:/LazyTrading/GitHub/R_markettype/to_m.R")

# Defining variables to be re-used in the code
sbx <- "C:/Program Files (x86)/ICMarkets MT4 Terminal2/MQL4/Files"
sbx_masterT1 <- "C:/Program Files (x86)/ICMarkets MT4 Terminal1/MQL4/Files"
sbx_slaveT3 <- "C:/Program Files (x86)/ICMarkets MT4 Terminal3/MQL4/Files"
sbx_slaveT4 <- "C:/Program Files (x86)/ICMarkets MT4 Terminal4/MQL4/Files"
sbx_slaveT5 <- "C:/Program Files (x86)/ICMarkets MT4 Terminal5/MQL4/Files"
chart_period <- 15 #this variable will define market type period
num_cols <- 64

#absolute path to store model objects (useful when scheduling tasks)
path_model <- "C:/LazyTrading/GitHub/R_markettype/models"

data_update_path <- "C:/LazyTrading/GitHub/R_markettype/TEST1/data_update"
data_initial_path <- "C:/LazyTrading/GitHub/R_markettype/TEST1/data_initial"

# Vector of currency pairs
# Pairs = c("EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
#           "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
#           "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
#           "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")   

Pairs = c("EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
          "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
          "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
          "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF",
          "USDTHB", "XAUEUR", "XAUUSD", "XBRUSD") 

# Reading the data from the Sandbox of Terminal 2 --> !!!Make sure that DataWriter robot is attached and working in Terminal 2!!!
sbx_price <- file.path(sbx, paste0("AI_CP",chart_period,"-35000.csv"))
sbx_macd <- file.path(sbx, paste0("AI_Macd", chart_period,"-35000.csv"))
#price <- read_csv(sbx_price, col_names = F)
#macd <- read_csv(sbx_macd, col_names = F, col_types = "cdddddddddddddddddddddddddddd")
macd <- read_csv(sbx_macd, col_names = F, col_types = "cdddddddddddddddddddddddddddddddd")
macd$X1 <- ymd_hms(macd$X1)

# Prepare data frame with last 64 observations of all 28 pairs and remove date/time column (16 hours)
#macd_100 <- macd %>% select(c(X2:X29)) %>% head(64)
#macd_100 <- macd %>% select(c(X2:X33)) %>% head(64)
#macd_101 <-macd %>% select(c(X2:X33)) %>% slice(1:6)
#macd_101_date <-macd %>% select(c(X1:X1)) %>% slice(1:1)
# Rename the columns
#names(macd_100) <- Pairs

# initialize the virtual machine
h2o.init()

# test for all columns
for (PAIR in Pairs) {
  # PAIR <- "EURUSD"
  # PAIR <- "GBPUSD"
  # PAIR <- "EURGBP"
  # Extract one column with Indicator data for 1 pair (e.g. "EURUSD")
  my_period <- c(1:1000)
  if(exists("my_data")){
    rm(my_data)
  }
  for(p_period in my_period){
    #p_period <- 1
    #p_period <- 2
    my_end_period <- p_period + 63
    macd_100 <-macd %>% select(c(X2:X33)) %>% slice(p_period:my_end_period)
    macd_100_date <-macd %>% select(c(X1:X1)) %>% slice(p_period:p_period)
    # Rename the columns
    names(macd_100) <- Pairs    
    
    df <- macd_100 %>% select(PAIR)
    # Use function to score the data to the model
    my_market_prediction <- evaluate_market_type(x = df,
                                                 #model_path = "C:/LazyTrading/GitHub/R_markettype/models/regression.bin/DL_Regression",
                                                 model_path = "C:/LazyTrading/GitHub/R_markettype/models/classification.bin/DL_Classification",
                                                 num_cols = 64) 
    # predicted value to write
    my_market <- my_market_prediction  %>% select(predict)
    
    # Join data to the predicted class and save to the temporary dataframe, only if predicted confidence is higher than 0.95
    # get predicted confidence
    my_market_conf <- my_market_prediction %>% select(-1) %>% select(which.max(.))
    
    if(!exists("my_data")){
      my_predict_line <- data.frame(my_market, my_market_conf, macd_100_date)
      my_data <- my_predict_line
    }else if(exists("my_data")){
      my_predict_line <- data.frame(my_market, my_market_conf, macd_100_date)
      my_data <- my_predict_line %>% bind_rows(my_data)
    }
  }
  names(my_data) <- c(PAIR, "conf", "date") 
  # Write obtained result to the sandboxes
  #write_csv(my_market, file.path(sbx, paste0("AI_MarketType_", PAIR, chart_period, ".csv")))
  write_csv(my_data, file.path(sbx, paste0("AI_History_MarketType_", PAIR, chart_period, ".csv")))
  #write_csv(my_market, file.path(sbx_slaveT5,  paste0("AI_MarketType_", PAIR, chart_period, ".csv")))
}

# shutdown  the virtual machine
h2o.shutdown(prompt = F)

