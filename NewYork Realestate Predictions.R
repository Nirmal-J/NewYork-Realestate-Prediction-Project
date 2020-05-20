set.seed(2020)

if(!require(groupdata2)) install.packages("groupdata2", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorplot", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
tinytex::tlmgr_install("pdfcrop")


options(digits = 4, scipen = 999)


### NYC property sales
### https://www.kaggle.com/new-york-city/nyc-property-sales
### https://www.kaggle.com/new-york-city/nyc-property-sales/download

### reading the data from the git repository - where I have downloaded it
prices <- read_csv(url("https://raw.githubusercontent.com/AmiDavid/NYC-real-esate-prices-algorithm/master/nyc-rolling-sales.csv"))


#### golssary of terms https://www1.nyc.gov/assets/finance/downloads/pdf/07pdf/glossary_rsf071607.pdf

#### Exploring the data

head(prices)


### the different variables
colnames(prices)

### some houses are sold for 0, 1, 10 or NA.
prices %>% 
  dplyr::count(prices$`SALE PRICE` < 1000)



#### I removed the houses sold for less than 1000 and the NA's
prices <- prices %>% 
  dplyr::filter(prices$`SALE PRICE` > 1000)



### we can see the log(prices distribution)
prices %>% ggplot(aes(log(as.numeric(`SALE PRICE`)))) +
  geom_histogram(bins = 25, fill = "blue", color = "black")




#### checking the building class category

prices %>% 
  group_by(`BUILDING CLASS CATEGORY`) %>% 
  summarize(n = n())

#### grouping the neighberhood
prices %>% 
  ggplot(aes(NEIGHBORHOOD)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#### the neighberhoods with the most sales
prices %>% 
  group_by(NEIGHBORHOOD) %>% 
  summarize(n = n(), mean_sale_price = mean(as.numeric(`SALE PRICE`)/ 1000)) %>% 
  arrange(desc(n))

### the tax class 
prices %>% 
  group_by(prices$`TAX CLASS AT TIME OF SALE`) %>% 
  summarize(mean_sale_price = mean(as.numeric(`SALE PRICE`) / 1000), n = n())

### plotting the tax class
prices %>% 
  group_by(prices$`TAX CLASS AT TIME OF SALE`) %>% 
  ggplot(aes(`TAX CLASS AT TIME OF SALE`, fill = `TAX CLASS AT TIME OF SALE`)) + 
  geom_bar(color = "black")


### showing building class and price
prices %>% 
  ggplot(aes(prices$`TAX CLASS AT PRESENT`, as.numeric(prices$`SALE PRICE`))) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("Tax class at present") +
  ylab("price sold")


### showing connection between size and price

prices %>% 
  filter((prices$'GROSS SQUARE FEET') > 0) %>% 
  ggplot(aes(as.numeric(`GROSS SQUARE FEET`), as.numeric(`SALE PRICE`))) +
  geom_point()+
  scale_y_continuous(trans = "log2") +
  scale_x_continuous(trans = "log2") + 
  xlab("Gross Square FEET") +
  ylab("Price Sold")


### showing connection between size and price coloring the tax class at moment of sale

prices %>% 
  filter((prices$'GROSS SQUARE FEET') > 0) %>% 
  ggplot(aes(as.numeric(`GROSS SQUARE FEET`), as.numeric(`SALE PRICE`), color = `TAX CLASS AT TIME OF SALE`)) +
  geom_point()+
  scale_y_continuous(trans = "log2") +
  scale_x_continuous(trans = "log2") +
  xlab("Gross Square FEET") +
  ylab("Price Sold")


### in addition, some properties have changed classification since the time
prices %>% 
  mutate(changed_tax_class = str_detect(prices$`TAX CLASS AT PRESENT`,as.character(prices$`TAX CLASS AT TIME OF SALE`))) %>% 
  group_by(changed_tax_class) %>% 
  summarize(n = n())


### year built of the building
prices %>% 
  filter(prices$`YEAR BUILT` > 0) %>% 
  ggplot(aes(as.numeric(`YEAR BUILT`))) +
  geom_bar() + 
  xlim(1825, max(as.numeric(prices$`YEAR BUILT`)))+
  xlab("Year Built")


### connection between the year built and the average price
prices %>% 
  group_by(`YEAR BUILT`) %>% 
  filter(`YEAR BUILT` > 1800) %>% 
  summarize(mean_sale_price = mean(as.numeric(`SALE PRICE`) / 1000), n = n())


prices %>% 
  group_by(`YEAR BUILT`) %>% 
  filter(`YEAR BUILT` > 1800) %>% 
  summarize(mean_sale_price = mean(as.numeric(`SALE PRICE`) / 1000), n = n()) %>% 
  ggplot(aes(`YEAR BUILT`, mean_sale_price)) +
  geom_point()+
  xlab("Year Built") +
  ylab("Mean Sale Price")


### 5 boroughs of NYC: Manhattan, Brooklyn, Queens, The Bronx and Staten Island

prices %>% 
  mutate(BOROUGH = ifelse(BOROUGH == 1, "Manhattan", 
                          ifelse(BOROUGH == 2, "Brooklyn",
                                 ifelse(BOROUGH == 3, "Queens", 
                                        ifelse(BOROUGH == 4, "The Bronx", "Staten Island"))))) %>% 
  
  group_by(`BOROUGH`) %>% 
  summarize(mean_sale_price = mean(as.numeric(`SALE PRICE`) / 1000), n = n())


### bourugh info
prices %>% 
  ggplot(aes(as.factor(prices$BOROUGH), as.numeric(prices$`SALE PRICE`))) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("Borough") +
  ylab("price sold") +
  scale_x_discrete(labels = c("Manhattan", "Brooklyn", "Queens", "The Bronx", "Staten Island")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


### plotting the boroughs
prices %>% 
  mutate(BOROUGH = ifelse(BOROUGH == 1, "Manhattan", 
                          ifelse(BOROUGH == 2, "Brooklyn",
                                 ifelse(BOROUGH == 3, "Queens", 
                                        ifelse(BOROUGH == 4, "The Bronx",
                                               ifelse(BOROUGH == 5, "Staten Island", 0)))))) %>% 
  group_by(prices$BOROUGH) %>% 
  ggplot(aes(BOROUGH, fill = as.factor(`BOROUGH`))) +
  geom_bar(color = "black") +
  ylab("Number of sales")


### some weeks have more deals than others

prices %>% 
  mutate(`SALE DATE` = as_datetime(`SALE DATE`),
         `SALE DATE` = round_date(`SALE DATE`, unit = "week",
                                  week_start = getOption("lubridate.week.start", 7))) %>% 
  group_by(`SALE DATE`) %>% 
  ggplot(aes(`SALE DATE`)) +
  geom_bar() +
  ylab("Number of Sales")


### tidying the data
prices <- prices %>% 
  mutate(neighborhood = as.factor(prices$NEIGHBORHOOD), building_class_at_time_of_sale = as.factor(prices$`BUILDING CLASS AT TIME OF SALE`),
         tax_class_at_time_of_sale = as.factor(prices$`TAX CLASS AT TIME OF SALE`), year_built = as.numeric(prices$`YEAR BUILT`),
         gross_sf = as.numeric(prices$`GROSS SQUARE FEET`), land_sf = as.numeric(prices$`LAND SQUARE FEET`),
         total_units = as.numeric(prices$`TOTAL UNITS`), commercial_units = as.numeric(prices$`COMMERCIAL UNITS`),
         residential_units = as.numeric(prices$`RESIDENTIAL UNITS`),
         zip_code = as.numeric(prices$`ZIP CODE`), apartment_number = as.factor(prices$`APARTMENT NUMBER`),
         lot = as.numeric(prices$LOT), borough = as.factor(prices$BOROUGH), building_class_category = as.factor(prices$`BUILDING CLASS CATEGORY`),
         tax_class_at_present = as.factor(prices$`TAX CLASS AT PRESENT`), block = as.numeric(prices$BLOCK),
         building_class_at_present = as.factor(prices$`BUILDING CLASS AT PRESENT`), address = as.factor(prices$ADDRESS),
         sale_date = as_datetime(prices$`SALE DATE`), sale_date = round_date(prices$`SALE DATE`, unit = "week",
                                                                             week_start = getOption("lubridate.week.start", 7)),
         sale_price_in_thousands = as.numeric(`SALE PRICE`) / 1000,  
         sale_price = as.numeric(prices$`SALE PRICE`)) %>% 
  select(-`SALE PRICE`, -`BUILDING CLASS AT TIME OF SALE`, -`TAX CLASS AT TIME OF SALE`, -`YEAR BUILT`,
         -`GROSS SQUARE FEET`, -`LAND SQUARE FEET`, -`TOTAL UNITS`, -`COMMERCIAL UNITS`, -`RESIDENTIAL UNITS`,
         -`ZIP CODE`, -`APARTMENT NUMBER`, -LOT, -BOROUGH, -`BUILDING CLASS CATEGORY`,
         -`TAX CLASS AT PRESENT`, -BLOCK, -`BUILDING CLASS AT PRESENT`, -ADDRESS, 
         -`SALE DATE`, -NEIGHBORHOOD, -`EASE-MENT`, -X1)



### Dividing the prices into categories of 10000$

max(prices$sale_price)
prices$sale_price_category <- 
  as.factor(cut(prices$sale_price, breaks = c(seq(1000, 10000000, 50000), Inf), dig.lab = 50))

prices %>% 
  group_by(sale_price_category) %>% 
  filter(!is.na(sale_price_category)) %>% 
  summarize(n = n())



### We see that the prices seem to follow a normal distribution
price_category <- prices %>% 
  group_by(sale_price_category) %>% 
  filter(!is.na(sale_price_category)) %>% 
  summarize(n = n())

price_category

prices %>% 
  ggplot(aes((sale_price_category))) +
  stat_count(width = 0.5) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



### Creating a correlation matrix - by the explanations in http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
### And https://towardsdatascience.com/how-to-create-a-correlation-matrix-with-too-many-variables-309cc0c0a57



corr_simple <- function(data=prices,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
}

prices_numerical <- corr_simple(prices)
prices_numerical$sale_date <- as.numeric(prices_numerical$sale_date)


cormat <- round(cor(prices_numerical, use = "na.or.complete"), 2)
ggcorrplot(cormat)

### Reorder the correlation matrix

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat_tibble <- as.tibble(reorder_cormat(cormat))

rownames(cormat_tibble) <- rownames(cormat)


### We see that some columns have some NA's
colSums(is.na(prices))


### creating test set from the prices_set
test_index <- createDataPartition(y = prices$borough, times = 1, p = 0.1, list = FALSE)
test_set <- prices_set[test_index,]
train_set <- prices_set[-test_index,]


###RMSE function
RMSE <- function(real_sale_price, predicted_price){
  sqrt(mean((real_sale_price - predicted_price) ^ 2))
}

MSE <- function(real_sale_price, predicted_price){
  mean((real_sale_price - predicted_price) ^ 2)
}

MAE <- function(real_sale_price, predicted_price){
  mean(abs(real_sale_price - predicted_price))
}

### filtering na columns
train_set <- train_set %>%
  filter(!is.na(sale_price_category) & gross_sf > 0 & land_sf > 0 & !is.na(tax_class_at_present) & !is.na(building_class_at_present)) %>% 
  select(-apartment_number)

test_set <- test_set %>%
  filter(!is.na(sale_price_category) & gross_sf > 0 & land_sf > 0 & !is.na(tax_class_at_present) & !is.na(building_class_at_present)) %>% 
  select(-apartment_number)

### creating a random foresst using ranger

train_rf <- ranger(sale_price_category ~ ., data = train_set)

predict_rf <- predict(train_rf, data = test_set)

evaluation_results <- tibble(method = "Random Forest using ranger libary on the test set", RMSE = RMSE(as.numeric(test_set$sale_price_category), as.numeric(predict_rf$predictions)),
                             MSE = MSE(as.numeric(test_set$sale_price_category), as.numeric(predict_rf$predictions)),
                             MAE = MAE(as.numeric(test_set$sale_price_category), as.numeric(predict_rf$predictions)))
evaluation_results

### percentage of correct categories predicted
test_set$predicted_price_category <- predict_rf$predictions

#### checking if the the predicted is equal to the real
test_set <- test_set %>% 
  mutate(correct = ifelse(predicted_price_category == sale_price_category, TRUE, FALSE))


### percentage metric of succesful prediction

perc_met <- test_set %>% 
  group_by(correct) %>% 
  summarize(n = n())


perc_metric <- (perc_met$n[2]) / (perc_met$n[1] + perc_met$n[2])

evaluation_results <- cbind(evaluation_results, "Percentage of Correct Predictions on the test set" = perc_metric)

evaluation_results