install.packages("RSQLite")
library(RSQLite)
install.packages("sqldf")
library(sqldf)
install.packages("RODBC")
library(RODBC)
library(dplyr)
library(ggplob)
library(stringr)
library(lubridate)
library(tidyr)


#Create three names for same dataset

a <- read.csv("cleanedData.csv")

a <- a %>% filter(Transtype == 'P')

b <- a
df<- a

df1 = df %>% filter(Transtype != 'P')

#Create expert variables with SQL with cardnum

cardnumDF = sqldf("SELECT a.Recordnum,



AVG(b.Amount) As cardnumAverage30,
AVG(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount ELSE NULL END) As cardnumAverage11,
AVG(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount ELSE NULL END) As cardnumAverage7,
AVG(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount ELSE NULL END) As cardnumAverage3,
AVG(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount ELSE NULL END) As cardnumAverage1,



MAX(b.Amount) As cardnumMax30,
MAX(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount ELSE NULL END) As cardnumMax11,
MAX(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount ELSE NULL END) As cardnumMax7,
MAX(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount ELSE NULL END) As cardnumMax3,
MAX(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount ELSE NULL END) As cardnumMax1,



SUM(b.Amount) As cardnumSum30,
SUM(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount ELSE NULL END) As cardnumSum11,
SUM(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount ELSE NULL END) As cardnumSum7,
SUM(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount ELSE NULL END) As cardnumSum3,
SUM(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount ELSE NULL END) As cardnumSum1,



AVG(b.Amount_week_diff) As cardnumDiffAvg30,
AVG(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount_week_diff ELSE NULL END) 
As cardnumDiffAvg11,
AVG(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount_week_diff ELSE NULL END) 
As cardnumDiffAvg7,
AVG(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount_week_diff ELSE NULL END) 
As cardnumDiffAvg3,
AVG(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount_week_diff ELSE NULL END) 
As cardnumDiffAvg1,



MAX(b.Amount_week_diff) As cardnumDiffMax30,
MAX(CASE WHEN a.date_index - b.date_index <= 11 THEN b.Amount_week_diff ELSE NULL END) 
As cardnumDiffMax10,
MAX(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount_week_diff ELSE NULL END) 
As cardnumDiffMax7,
MAX(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount_week_diff ELSE NULL END) 
As cardnumDiffMax3,
MAX(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount_week_diff ELSE NULL END) 
As cardnumDiffMax1,



SUM(b.Amount_week_diff) As cardnumDiffSum30,
SUM(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount_week_diff ELSE NULL END) 
As cardnumDiffSum11,
SUM(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount_week_diff ELSE NULL END) 
As cardnumDiffSum7,
SUM(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount_week_diff ELSE NULL END) 
As cardnumDiffSum3,
SUM(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount_week_diff ELSE NULL END) 
As cardnumDiffSum1,


AVG(b.Amount_month_diff) As cardnumDiffAvgMonth30,
AVG(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount_month_diff ELSE NULL END) 
As cardnumDiffAvgMonth11,
AVG(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount_month_diff ELSE NULL END) 
As cardnumDiffAvgMonth7,
AVG(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount_month_diff ELSE NULL END) 
As cardnumDiffAvgMonth3,
AVG(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount_month_diff ELSE NULL END) 
As cardnumDiffAvgMonth1,


MAX(b.Amount_month_diff) As cardnumDiffMaxMonth30,
MAX(CASE WHEN a.date_index - b.date_index <= 11 THEN b.Amount_month_diff ELSE NULL END) 
As cardnumDiffMaxMonth11,
MAX(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount_month_diff ELSE NULL END) 
As cardnumDiffMaxMonth7,
MAX(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount_month_diff ELSE NULL END) 
As cardnumDiffMaxMonth3,
MAX(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount_month_diff ELSE NULL END) 
As cardnumDiffMaxMonth1,



SUM(b.Amount_month_diff) As cardnumDiffSumMonth30,
SUM(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount_month_diff ELSE NULL END) 
As cardnumDiffSumMonth11,
SUM(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount_month_diff ELSE NULL END) 
As cardnum_amount_month_diff_sum_7,
SUM(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount_month_diff ELSE NULL END) 
As cardnum_amount_month_diff_sum_3,
SUM(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount_month_diff ELSE NULL END) 
As cardnum_amount_month_diff_sum_1,



COUNT(*) AS cardnum30,
SUM(CASE WHEN a.date_index - b.date_index <= 10 THEN 1 ELSE NULL END) AS cardnum11,
SUM(CASE WHEN a.date_index - b.date_index <= 6 THEN 1 ELSE NULL END) AS cardnum7,
SUM(CASE WHEN a.date_index - b.date_index <= 2 THEN 1 ELSE NULL END) AS cardnum3,
SUM(CASE WHEN a.date_index - b.date_index <= 0 THEN 1 ELSE NULL END) AS cardnum1,





COUNT(DISTINCT b.Merchantnum) AS cardnumMerchantnum30,
COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 10 THEN b.Merchantnum ELSE NULL END) 
AS cardnumMerchantnum11,
COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 6 THEN b.Merchantnum ELSE NULL END) 
AS cardnumMerchantnum7,
COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 2 THEN b.Merchantnum ELSE NULL END) 
AS cardnumMerchantnum3,
COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 0 THEN b.Merchantnum ELSE NULL END) 
AS cardnumMerchantnum1,



SUM(b.Merchantnum_isna) AS cardnumMerchantnumNA30,
SUM(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Merchantnum_isna ELSE NULL END) 
AS cardnumMerchantnumNA11,
SUM(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Merchantnum_isna ELSE NULL END) 
AS cardnumMerchantnumNA7,
SUM(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Merchantnum_isna ELSE NULL END) 
AS cardnumMerchantnumNA3,
SUM(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Merchantnum_isna ELSE NULL END) 
AS cardnumMerchantnumNA1,


COUNT(DISTINCT b.Merchant_Zip) AS cardnumZip30,
COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 10 THEN b.Merchant_Zip ELSE NULL END) 
AS cardnumZip11,
COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 6 THEN b.Merchant_Zip ELSE NULL END) 
AS cardnumZip7,
COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 2 THEN b.Merchant_Zip ELSE NULL END) 
AS cardnumZip3,
COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 0 THEN b.Merchant_Zip ELSE NULL END) 
AS cardnumZip1,

SUM(b.Merchant_Zip_isna) AS cardnumZipNA30,
SUM(DISTINCT CASE WHEN a.date_index - b.date_index <= 10 THEN b.Merchant_Zip_isna ELSE NULL END) 
AS cardnumZipNA11,
SUM(DISTINCT CASE WHEN a.date_index - b.date_index <= 6 THEN b.Merchant_Zip_isna ELSE NULL END) 
AS cardnumZipNA7,
SUM(DISTINCT CASE WHEN a.date_index - b.date_index <= 2 THEN b.Merchant_Zip_isna ELSE NULL END) 
AS cardnumZipNA3,
SUM(DISTINCT CASE WHEN a.date_index - b.date_index <= 0 THEN b.Merchant_Zip_isna ELSE NULL END) 
AS cardnumZipNA1



FROM a, b


ON a.Cardnum = b.Cardnum


AND a.date_index - b.date_index BETWEEN 0 AND 29


AND a.Recordnum < b.Recordnum


GROUP BY a.Recordnum 
            " )

# Create additional variables with mechantnum

MerchantnumDF = sqldf("SELECT a.Recordnum,
                  
                      
                      
                      AVG(b.Amount) As MerchantnumAverage30,
                      AVG(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount ELSE NULL END) As MerchantnumAverage11,
                      AVG(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount ELSE NULL END) As MerchantnumAverage7,
                      AVG(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount ELSE NULL END) As MerchantnumAverage3,
                      AVG(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount ELSE NULL END) As MerchantnumAverage1,
                      
                      
                      
                      MAX(b.Amount) As MerchantnumMax30,
                      MAX(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount ELSE NULL END) As MerchantnumMax11,
                      MAX(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount ELSE NULL END) As MerchantnumMax7,
                      MAX(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount ELSE NULL END) As MerchantnumMax3,
                      MAX(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount ELSE NULL END) As MerchantnumMax1,
                      
                      
                      
                      SUM(b.Amount) As MerchantnumSum30,
                      SUM(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount ELSE NULL END) As MerchantnumSum11,
                      SUM(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount ELSE NULL END) As MerchantnumSum7,
                      SUM(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount ELSE NULL END) As MerchantnumSum3,
                      SUM(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount ELSE NULL END) As MerchantnumSum1,
                      
                      
                      
                      AVG(b.Amount_week_diff) As MerchantnumDiffAvg30,
                      AVG(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount_week_diff ELSE NULL END) 
                      As MerchantnumDiffAvg11,
                      AVG(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount_week_diff ELSE NULL END) 
                      As MerchantnumDiffAvg7,
                      AVG(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount_week_diff ELSE NULL END) 
                      As MerchantnumDiffAvg3,
                      AVG(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount_week_diff ELSE NULL END) 
                      As MerchantnumDiffAvg1,
                      
                      
                      
                      MAX(b.Amount_week_diff) As MerchantnumDiffMax30,
                      MAX(CASE WHEN a.date_index - b.date_index <= 11 THEN b.Amount_week_diff ELSE NULL END) 
                      As MerchantnumDiffMax10,
                      MAX(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount_week_diff ELSE NULL END) 
                      As MerchantnumDiffMax7,
                      MAX(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount_week_diff ELSE NULL END) 
                      As MerchantnumDiffMax3,
                      MAX(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount_week_diff ELSE NULL END) 
                      As MerchantnumDiffMax1,
                      
                      
                      
                      SUM(b.Amount_week_diff) As MerchantnumDiffSum30,
                      SUM(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount_week_diff ELSE NULL END) 
                      As MerchantnumDiffSum11,
                      SUM(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount_week_diff ELSE NULL END) 
                      As MerchantnumDiffSum7,
                      SUM(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount_week_diff ELSE NULL END) 
                      As MerchantnumDiffSum3,
                      SUM(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount_week_diff ELSE NULL END) 
                      As MerchantnumDiffSum1,
                      
                      
                      AVG(b.Amount_month_diff) As MerchantnumDiffAvgMonth30,
                      AVG(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount_month_diff ELSE NULL END) 
                      As MerchantnumDiffAvgMonth11,
                      AVG(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount_month_diff ELSE NULL END) 
                      As MerchantnumDiffAvgMonth7,
                      AVG(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount_month_diff ELSE NULL END) 
                      As MerchantnumDiffAvgMonth3,
                      AVG(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount_month_diff ELSE NULL END) 
                      As MerchantnumDiffAvgMonth1,
                      
                      
                      MAX(b.Amount_month_diff) As MerchantnumDiffMaxMonth30,
                      MAX(CASE WHEN a.date_index - b.date_index <= 11 THEN b.Amount_month_diff ELSE NULL END) 
                      As MerchantnumDiffMaxMonth11,
                      MAX(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount_month_diff ELSE NULL END) 
                      As MerchantnumDiffMaxMonth7,
                      MAX(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount_month_diff ELSE NULL END) 
                      As MerchantnumDiffMaxMonth3,
                      MAX(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount_month_diff ELSE NULL END) 
                      As MerchantnumDiffMaxMonth1,
                      
                      
                      
                      SUM(b.Amount_month_diff) As MerchantnumDiffSumMonth30,
                      SUM(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Amount_month_diff ELSE NULL END) 
                      As MerchantnumDiffSumMonth11,
                      SUM(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Amount_month_diff ELSE NULL END) 
                      As Merchantnum_amount_month_diff_sum_7,
                      SUM(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Amount_month_diff ELSE NULL END) 
                      As Merchantnum_amount_month_diff_sum_3,
                      SUM(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Amount_month_diff ELSE NULL END) 
                      As Merchantnum_amount_month_diff_sum_1,
                      
                      
                      
                      COUNT(*) AS Merchantnum30,
                      SUM(CASE WHEN a.date_index - b.date_index <= 10 THEN 1 ELSE NULL END) AS Merchantnum11,
                      SUM(CASE WHEN a.date_index - b.date_index <= 6 THEN 1 ELSE NULL END) AS Merchantnum7,
                      SUM(CASE WHEN a.date_index - b.date_index <= 2 THEN 1 ELSE NULL END) AS Merchantnum3,
                      SUM(CASE WHEN a.date_index - b.date_index <= 0 THEN 1 ELSE NULL END) AS Merchantnum1,
                      
                      
                      
                      
                      
                      COUNT(DISTINCT b.Merchantnum) AS MerchantnumMerchantnum30,
                      COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 10 THEN b.Merchantnum ELSE NULL END) 
                      AS MerchantnumMerchantnum11,
                      COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 6 THEN b.Merchantnum ELSE NULL END) 
                      AS MerchantnumMerchantnum7,
                      COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 2 THEN b.Merchantnum ELSE NULL END) 
                      AS MerchantnumMerchantnum3,
                      COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 0 THEN b.Merchantnum ELSE NULL END) 
                      AS MerchantnumMerchantnum1,
                      
                      
                      
                      SUM(b.Merchantnum_isna) AS MerchantnumMerchantnumNA30,
                      SUM(CASE WHEN a.date_index - b.date_index <= 10 THEN b.Merchantnum_isna ELSE NULL END) 
                      AS MerchantnumMerchantnumNA11,
                      SUM(CASE WHEN a.date_index - b.date_index <= 6 THEN b.Merchantnum_isna ELSE NULL END) 
                      AS MerchantnumMerchantnumNA7,
                      SUM(CASE WHEN a.date_index - b.date_index <= 2 THEN b.Merchantnum_isna ELSE NULL END) 
                      AS MerchantnumMerchantnumNA3,
                      SUM(CASE WHEN a.date_index - b.date_index <= 0 THEN b.Merchantnum_isna ELSE NULL END) 
                      AS MerchantnumMerchantnumNA1,
                      
                      
                      COUNT(DISTINCT b.Merchant_Zip) AS MerchantnumZip30,
                      COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 10 THEN b.Merchant_Zip ELSE NULL END) 
                      AS MerchantnumZip11,
                      COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 6 THEN b.Merchant_Zip ELSE NULL END) 
                      AS MerchantnumZip7,
                      COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 2 THEN b.Merchant_Zip ELSE NULL END) 
                      AS MerchantnumZip3,
                      COUNT(DISTINCT CASE WHEN a.date_index - b.date_index <= 0 THEN b.Merchant_Zip ELSE NULL END) 
                      AS MerchantnumZip1,
                      
                      SUM(b.Merchant_Zip_isna) AS MerchantnumZipNA30,
                      SUM(DISTINCT CASE WHEN a.date_index - b.date_index <= 10 THEN b.Merchant_Zip_isna ELSE NULL END) 
                      AS MerchantnumZipNA11,
                      SUM(DISTINCT CASE WHEN a.date_index - b.date_index <= 6 THEN b.Merchant_Zip_isna ELSE NULL END) 
                      AS MerchantnumZipNA7,
                      SUM(DISTINCT CASE WHEN a.date_index - b.date_index <= 2 THEN b.Merchant_Zip_isna ELSE NULL END) 
                      AS MerchantnumZipNA3,
                      SUM(DISTINCT CASE WHEN a.date_index - b.date_index <= 0 THEN b.Merchant_Zip_isna ELSE NULL END) 
                      AS MerchantnumZipNA1
                      
                      
                      
                      FROM a, b
                      
                      
                      ON a.Merchantnum = b.Merchantnum
                      
                      
                      AND a.date_index - b.date_index BETWEEN 0 AND 29
                      
                      
                      AND a.Recordnum < b.Recordnum
                      
                      
                      GROUP BY a.Recordnum 
                      " )




#join datasets

df <- inner_join(df, cardnumDF, by ="Recordnum")

df <- inner_join(df, MerchantnumDF, by ="Recordnum")




saveRDS(df, file ="expert variables")

write.csv(df, file="expert varibles.csv")


