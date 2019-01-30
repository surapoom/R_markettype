# ----------------------------------------------------------------------------------------
# R Script to select and train the Deep Learning model on Financial Asset Time Series Data
# ----------------------------------------------------------------------------------------
# ## Manually select data into big matrix with label of 6 classes
# ----------------------------------------------------------------------------------------
#
# Supervised Deep Learning Classification Modelling
#
# load libraries to use and custom functions
library(tidyverse)
library(h2o)
library(lubridate)
library(plotly)
source("to_m.R")

#### Read asset prices and indicators ==========================================
# load prices of 28 currencies
prices <- read_csv("MyInitData/AI_CP15-14000-33AsRuler.csv", col_names = F)
#prices <- read_csv("MyInitData/AI_CP15-14000.csv", col_names = F)
#prices <- read_csv("AI_CP15.csv", col_names = F)
prices$X1 <- ymd_hms(prices$X1)
# load macd indicator of 28 currencies
macd <- read_csv("MyInitData/AI_Macd15-14000.csv", col_names = F)
#macd <- read_csv("AI_Macd15-35000.csv", col_names = F)
macd$X1 <- ymd_hms(macd$X1)

#### Manually Selecting data... =================================================
# Market Periods
# 1. Bull normal, BUN
# 2. Bull volatile, BUV
# 3. Bear normal, BEN
# 4. Bear volatile, BEV
# 5. Sideways quiet, RAN
# 6. Sideways volatile, RAV

##########################################################################
## ---------- # 1. Bull normal, BUN ---------------
##########################################################################
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X8))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-08-22", X1 < "2018-10-01") %>% select(X1, X8)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X8))+geom_line()

# Extract corresponding piece of indicator dataframe:
macd_df <- macd %>% select(X1, X8) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X8.y, col = X8.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m <- macd_df %>% select(X8.x) %>% to_m(64)
#########################################################################
# add new column to this matrix with value BUN
macd_m_1 <- transform(macd_m, M_T = "BUN")
##########################################################################


##########################################################################
## ---------- # 2. Bull volatile, BUV ---------------
##########################################################################
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X2))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-08-16", X1 < "2018-08-29") %>% select(X1, X2)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X2))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X2) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X2.y, col = X2.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m <- macd_df %>% select(X2.x) %>% to_m(64)

#########################################################################
macd_m_2 <- transform(macd_m, M_T = "BUV") 
#########################################################################


##########################################################################
## ---------- # 3. Bear normal, BEN ---------------
##########################################################################
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X20))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-07-10", X1 < "2018-10-04") %>% select(X1, X20)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X20))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X20) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X20.y, col = X20.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m <- macd_df %>% select(X20.x) %>% to_m(64)

#########################################################################
macd_m_3 <- transform(macd_m, M_T = "BEN")
#########################################################################


##########################################################################
## ---------- # 4. Bear volatile, BEV ---------------
##########################################################################
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X11))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-07-15", X1 < "2018-08-16") %>% select(X1, X11)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X11))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X11) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X11.y, col = X11.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m <- macd_df %>% select(X11.x) %>% to_m(64)

#########################################################################
macd_m_4 <- transform(macd_m, M_T = "BEV")
#########################################################################


##########################################################################
## ---------- # 5. Sideways quiet, RAN ---------------
##########################################################################
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X7))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-08-06", X1 < "2018-08-20") %>% select(X1, X7)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X7))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X7) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X7.y, col = X7.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m <- macd_df %>% select(X7.x) %>% to_m(64)

#########################################################################
macd_m_5 <- transform(macd_m, M_T = "RAN") 
#########################################################################

##########################################################################
## ---------- # 6. Sideways volatile, RAV ---------------
##########################################################################
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X4))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-07-01", X1 < "2018-08-05") %>% select(X1, X4)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X4))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X4) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X4.y, col = X4.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m <- macd_df %>% select(X4.x) %>% to_m(64)

#########################################################################
macd_m_6 <- transform(macd_m, M_T = "RAV")
#########################################################################
#########################################################################
#########################################################################

# Combine all of that :)
macd_ML2 <- rbind(macd_m_1,macd_m_2,macd_m_3,macd_m_4,macd_m_5,macd_m_6)

### NOTE Number of rows Matrices needs to be roughly equal

# Record data into the folder
write_rds(macd_ML2, "TEST1/data_initial/macd_ML2.rds")

## Visualize new matrix in 3D
plot_ly(z = as.matrix(macd_ML2[,1:64]), type = "surface")

#### End
