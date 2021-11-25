library(tidyverse)
library(readxl)

# set path location and read data
#setwd("insert path here")
xfile_o <- read_excel("./Sample - Superstore[40].xlsx")

# data transformation
xfile <- xfile_o %>% 
  mutate(sd_sales = scale(Sales),
         sd_profits = scale(Profit),
         sd_prices = scale(1-Discount)) %>% 
  group_by(`Product Name`) %>% 
  summarise(sum_sales = sum(Sales),
            sum_profits = sum(Profit),
            mean_discounts = mean(Discount),
            sd_sales = sum(sd_sales),
            sd_profits = sum(sd_profits),
            sd_prices = mean(sd_prices)) %>% 
  mutate(rk_sales = rank(sum_sales),
         rk_profits = rank(sum_profits),
         rk_discounts = dense_rank(-mean_discounts))


# keep only needed variables
xfile_w <- xfile%>% 
  select(-c(sum_sales,sum_profits,mean_discounts))


# write to csv file
write_csv(xfile_w,"overall_ranking.csv")
            
