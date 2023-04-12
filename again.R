library(tidyverse)
library(lubridate)
library(Hmisc)
library(dplyr)
library(writexl)

##data loaded 
newData <- readxl::read_xls("../Documents/Rdata/csv data/Sample - EU Superstore.xls")
##step-1 data cleaning
newData |>
  group_by( `Order ID`, `Product ID`) |> 
  summarise(n = n()) |> view()
###step1.1
newData |> 
  group_by(Quantity) |>
  mutate(customer_sign = substr(`Customer ID`, 1,3)) |>
  mutate(order_code = substr(`Order ID`, 12,15)) |>
  mutate(product_code = substr(`Product ID`, 12,15)) |>
  mutate(unique_code = paste(customer_sign,order_code,"_",product_code,"_",Quantity)) |>
  mutate(duplication_exist = duplicated(unique_code)) -> df_newData
write_xlsx(df_newData, "data_set_1.xlsx")
###step-2 finding metrics over customer
df_newData |> 
  filter(duplication_exist != "TRUE") |> 
  group_by(`Customer Name`, `Order ID`, ) |>
  summarise(order_date = date(max(`Order Date`)),
            shipping_date = date(max(`Ship Date`)),
            total_spent = round(sum(Sales))) |> 
  arrange(`Customer Name`, order_date) |> 
  mutate(lag_of_days = as.integer(order_date - lag(order_date, order_by = order_date)),
         shipping_delay = as.integer(shipping_date - order_date)) |> 
  summarise(m_lag_of_days = median(lag_of_days, na.rm = TRUE),
            m_shipping_days = median(shipping_delay, na.rm = TRUE),
            first_transaction_date = min(order_date),
            last_transaction_date = max(order_date),
            total_year_withUs = as.integer(round(last_transaction_date - first_transaction_date)/365, 2),
            n_transactions = n_distinct(`Order ID`),    
            m_total_spent = median(total_spent))|>
  arrange((n_transactions))|>
  mutate(recency_intervals = cut2(m_total_spent, g = 5),
         frequency_intervals = cut2(n_transactions, g = 5),
         monetary_intervals = cut2(m_total_spent, g = 5))|> 
  mutate(rank_recency = dense_rank(recency_intervals),
         rank_frequency = dense_rank(frequency_intervals),
         rank_monetary = dense_rank(monetary_intervals)) |>
  mutate(RFM_value = rank_recency + rank_frequency + rank_monetary) |> 
  inner_join(
    df_newData|>
    filter(duplication_exist != "TRUE")|>
      group_by(`Customer Name`, Category)|>
      summarise(total_purchased = sum(Sales)) |>
      mutate(top_cat = max(total_purchased)) |>
      summarise(top_spend_Value_Category = max(top_cat))
    ) -> storeData
      
    write_xlsx(storeData, "final_dataset.xlsx")
   
 
           
  
  
 
  
 

  
  