# Walmart Store Sales Forcasting
# 2021.03.01 # 2021.05.04 다시 작성 
# 코드 링크: https://www.kaggle.com/issactoast/walmart-baseline-with-tidymodels
rm(list=ls())

library(tidyverse); library(tidymodels) ; library(lubridate); library(skimr); 
library(magrittr)  # 날짜,시간 처리 

dir(path = "D:/ADP실기/KaggleR/Walmart Store Sales Forecasting/data/")
train     <- read_csv("D:/ADP실기/KaggleR/Walmart Store Sales Forecasting/data/train.csv.zip")
test      <- read_csv("D:/ADP실기/KaggleR/Walmart Store Sales Forecasting/data/test.csv.zip") 

train  %>%  dim()  # 421570      5
test   %>%  dim()  # 115064      4
submit %>%  dim()  # 115064      2

library(tibble)
bind_rows(
  train %>% add_column(key = "training"), 
  test  %>% add_column(key = "testing")
)   -> alldata

alldata$key  <- alldata$key %>%  as.factor()

train   %>%  colnames()
test    %>%  colnames()
submit  %>%  colnames()
alldata %>%  colnames()
# [1] "Store"        "Dept"         "Date"         "IsHoliday"    "key"          "Weekly_Sales"

alldata %>%  skim()

alldata %>% mutate(  year = year(Date), month = month(Date)) %>%  select(-c(Date)) %>% 
  janitor::clean_names() -> alldata

alldata %>%  select( c("weekly_sales", "year", "month", "store", "dept", "is_holiday", "key")  ) -> alldata


# recipe를 이용한 전처리 
walmart_recipe <- recipe(weekly_sales ~ ., data = alldata) %>% step_normalize( all_numeric(), -all_outcomes()) 
walmart_recipe <- prep(walmart_recipe, training = alldata  )
alldata2 <- bake(walmart_recipe, new_data = alldata )

# train / test 분할 - hold out 
train_index <-  train %>%  nrow() %>%  seq_len()
train2 <- alldata2[train_index,  ] %>%  select(-c("key"))
test2  <- alldata2[-train_index, ] %>%  select(-c("key"))

# model building and training 
lm_model <-  linear_reg() %>%  set_engine("lm")
lm_fit   <-  lm_model %>%  fit(weekly_sales ~ . , data = train2 )
lm_fit

# prediction and submission 
result <-  predict(lm_fit, new_data = test2)
submit <- read_csv("D:/ADP실기/KaggleR/Walmart Store Sales Forecasting/data/sampleSubmission.csv.zip") 
submit$Weekly_Sales  <-  result$.pred
write.csv(submit, "D:/ADP실기/KaggleR/Walmart Store Sales Forecasting/results/baseline-lm-20210504.csv")
