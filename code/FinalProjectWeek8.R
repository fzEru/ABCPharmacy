# load necessary libraries
library(readr)
library(tibble)
library(dplyr)

# set the working directory to where the ABC Pharmacy csv files are located
setwd("C:/Users/mohammedkhan/Desktop/Data Science/ABCPharmacy/ABCPharmacy")

# import the ABC Pharmacy csvs using the read_csv() function from the readr package and specifying the column types for each tibble
MAJOR_PROD_CAT <- read_csv(file = "MAJOR_PROD_CAT.csv", col_names = TRUE, col_types = "cc")
PHRMCY_MASTER <- read_csv(file = "PHRMCY_MASTER.csv", col_names = TRUE, col_types = "cccc")
POS_TRANS <- read_csv(file = "POS_TRANS.csv", col_names = TRUE, col_types = "cccidi")
PROD_CAT <- read_csv(file = "PROD_CAT.csv", col_names = TRUE, col_types = "ccc")
PROD_MASTER <-read_csv(file = "PROD_MASTER.csv", col_names = TRUE, col_types = "ccc")
PROD_SEG <- read_csv(file = "PROD_SEG.csv", col_names = TRUE, col_types = "ccc")
PROD_SUB_CAT <- read_csv(file = "PROD_SUB_CAT.csv", col_names = TRUE, col_types = "ccc")

# check for importing problems using the problems() function from the readr package
problems(MAJOR_PROD_CAT)
problems(PHRMCY_MASTER)
problems(POS_TRANS)
problems(PROD_CAT)
problems(PROD_MASTER) 
problems(PROD_SEG)
problems(PROD_SUB_CAT)

# convert from dqtetime format to character string tyoe to make date more legible; converting back to datetime type in new format
POS_TRANS$SLS_DTE_NBR <- strptime(POS_TRANS$SLS_DTE_NBR, format = "%Y%m%d")

POS_TRANS$SLS_DTE_NBR <- strftime(POS_TRANS$SLS_DTE_NBR, format = "%Y-%m-%d")

POS_TRANS$SLS_DTE_NBR <- strptime(POS_TRANS$SLS_DTE_NBR, format = "%Y-%m-%d")



#-----------------------------------------------------------------------------------------------------------------------------------
# 1. Which products were most purchased in bundles of 2, 3, and 4 in the same cart?
#-----------------------------------------------------------------------------------------------------------------------------------
# using 'filter' to weed out extreme outlier data and to return a quantity of products between 1 and 5, exclusive
# arranged in descending order since the questions asks for the 'most; of something

q1 <- POS_TRANS %>%
  select(PROD_NBR, SLS_QTY) %>%
  filter(PROD_NBR > 1, SLS_QTY > 1, SLS_QTY < 5) %>%
  count(PROD_NBR) %>%
  arrange(desc(n)) %>%
  head(50)
q1



#-----------------------------------------------------------------------------------------------------------------------------------
# 2. What is the individual sales amount for each distinct product in ascending order?
#-----------------------------------------------------------------------------------------------------------------------------------
# using 'distinct' to isolate individual sales
# using 'mutate' to create a new column based on existing ones
# filter extreme outliers 
# arrange by ascending order absed on the question

q2 <- POS_TRANS %>%
  distinct(PROD_NBR, EXT_SLS_AMT, SLS_QTY) %>%
  mutate(IDV_SLS_AMT = EXT_SLS_AMT / SLS_QTY) %>%
  select(PROD_NBR, IDV_SLS_AMT) %>%
  filter(PROD_NBR > 1, IDV_SLS_AMT > 0, IDV_SLS_AMT != Inf) %>%
  arrange(IDV_SLS_AMT)
q2



#-----------------------------------------------------------------------------------------------------------------------------------
# 3. Which zip codes contain the most pharmacies?
#-----------------------------------------------------------------------------------------------------------------------------------
# using 'distinct' to group by two columns
# arranged in descending order due to the question asking for 'most' of something
# displays only the first 10 rows fo the query

q3 <- PHRMCY_MASTER %>%
  distinct(PHRMCY_NBR, ZIP_3_CD, .keep_all = FALSE) %>%
  count(ZIP_3_CD) %>%
  arrange(desc(n)) %>%
  head(10)
q3



#-----------------------------------------------------------------------------------------------------------------------------------
# 4. Count the number of category codes.
#-----------------------------------------------------------------------------------------------------------------------------------
# add up each individual category code as '1' to give us the total

q4 <- PROD_CAT %>%
  count(CAT_CD) %>%
  summarise(sum(n)) 
q4



#-----------------------------------------------------------------------------------------------------------------------------------
# 5. Which dates resulted in the most basket sales?
#-----------------------------------------------------------------------------------------------------------------------------------
# using 'distinct' to group by two columns
# arranged in descending order due to the question asking for 'most' of something
# displays only the first 10 rows fo the query

q5 <- POS_TRANS %>%
  distinct(SLS_DTE_NBR, BSKT_ID, .keep_all = FALSE) %>%
  count(SLS_DTE_NBR) %>%
  arrange(desc(n)) %>%
  head(10)
q5



#-----------------------------------------------------------------------------------------------------------------------------------
# 6. Which pharmacies, on average, sell the most baskets per day?
#-----------------------------------------------------------------------------------------------------------------------------------
# extract the first date recorded in the dataset, being kept as a datetime type

firstDate <- POS_TRANS[order(as.Date(POS_TRANS$SLS_DTE_NBR, format = "%Y-%m-%d")),] %>%
  select(SLS_DTE_NBR) %>%
  head(1)
firstDate

# extract the last date recorded in the dataset, being kept as a datetime type

lastDate <- POS_TRANS[order(as.Date(POS_TRANS$SLS_DTE_NBR, format = "%Y-%m-%d")),] %>%
  select(SLS_DTE_NBR) %>%
  tail(1)
lastDate

# find the difference of days between the first and last dates recorded in the dataset
# convert the value given into a numeric type for calculations

dataDays <- as.numeric(lastDate - firstDate)
dataDays

# first 'group by' is used to find the total number of baskets sold per pharmacy
# second 'group by' is used to find the number of baskets sold per day on average at each pharmacy
# arranged in descending order due to the question asking for 'most' of something
# displays only the first 10 rows fo the query

q6 <- POS_TRANS %>%
  group_by(PHRMCY_NBR) %>%
  summarise(countBSKT_SOLD = n()) %>%
  group_by(PHRMCY_NBR) %>%
  summarise(perDayBSKT = countBSKT_SOLD / dataDays) %>%
  arrange(desc(perDayBSKT)) %>%
  head(10)
q6



#-----------------------------------------------------------------------------------------------------------------------------------
# 7. Which products sold the most in terms of cumulative sales?
#-----------------------------------------------------------------------------------------------------------------------------------
# calculate the sum of sales
# filter out extreme outliers
# arranged in descending order due to the question asking for 'most' of something
# displays only the first 20 rows of the query

q7 <- POS_TRANS %>%
  group_by(PROD_NBR) %>%
  summarise(totalEXT_SLS_AMT = sum(EXT_SLS_AMT)) %>%
  filter(PROD_NBR > 0) %>%
  arrange(desc(totalEXT_SLS_AMT)) %>%
  head(20)
q7



#-----------------------------------------------------------------------------------------------------------------------------------
# 8. Which products sold the least in terms of cumulative sales?
#-----------------------------------------------------------------------------------------------------------------------------------
# calculate the sum of sales
# filter out extreme outliers
# set the calculated total to 0, since that would be the lowest amount a product bring in for revenue

q8 <- POS_TRANS %>%
  group_by(PROD_NBR) %>%
  summarise(totalEXT_SLS_AMT = sum(EXT_SLS_AMT)) %>%
  filter(PROD_NBR > 1, totalEXT_SLS_AMT == 0)
q8



#-----------------------------------------------------------------------------------------------------------------------------------
# 9. Which products sold the least number of units?
#-----------------------------------------------------------------------------------------------------------------------------------
# calculate the sum of units
# filter out extreme outliers
# set the calculated total to 0, since that would be the lowest amount of times a product can be sold

q9 <- POS_TRANS %>%
  group_by(PROD_NBR) %>%
  summarise(totalSLS_QTY = sum(SLS_QTY)) %>%
  filter(PROD_NBR > 0, totalSLS_QTY == 0)
q9



#-----------------------------------------------------------------------------------------------------------------------------------
# 10. Which products sold the most number of units?
#-----------------------------------------------------------------------------------------------------------------------------------
# calculate the sum of units
# filter out extreme outliers
# arranged in descending order due to the question asking for 'most' of something
# displays only the first 50 rows of the query

q10 <- POS_TRANS %>%
  group_by(PROD_NBR, SLS_QTY) %>%
  summarise(totalSLS_QTY = sum(SLS_QTY)) %>%
  filter(PROD_NBR > 0, SLS_QTY >= 0, totalSLS_QTY >= 0) %>%
  arrange(desc(totalSLS_QTY)) %>%
  head(50)
q10
