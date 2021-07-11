library(dplyr);library(magrittr);library(caret);library(recipes);
library(janitor);library(skimr);
rm(list=ls())
setwd("D:/RPrj/Rkaggle/Rkaggle/HousePrice")

bind_rows(read.csv("./data/train.csv", stringsAsFactors = TRUE) %>%  
            mutate(index = "train", .before = 1 )-> train, 
          read.csv("./data/test.csv", stringsAsFactors = TRUE)  %>%  
            mutate(index = "test", .before = 1  ) -> test ) -> full 

read.csv("./result/sample_submission.csv") -> submission

full %>%  skim()
full %>%  filter(index == "train") -> train
full %>%  filter(index == "test")  -> test 

full %>%  dim() # 2919   82
train %>% dim() # 1460   82
test  %>% dim() # 1459   82
submission %>% dim() # 1459    2

# 일단 기계적으로 풀어보자. 
full %>%  mutate_if(is.integer, as.numeric) %>% 
  select_if(is.numeric) %>% 
  select(-Id)  %>%  
  na.omit() %>% 
  prcomp(scale. = TRUE) %>%  
  summary() %>%  
  .$importance %>% 
  as.data.frame() %>%  
  .[3,] %>%  



# 범주형 중 결측치 존재열 확인          
factors <- which(sapply(full, is.factor))
full[,factors] %>%  head()
naniar::gg_miss_var(full[,factors], show_pct = TRUE)
full[,factors] %>% is.na() %>%  colSums()  # 각 열별 결측수 

# 결측 없는 열 중 몇 개만 선택해 보자. Street LotShape LandContour -> 일단 이걸로. 


# 수치형 중 결측치 존재열 확인          
numbers <- which(sapply(full, is.numeric))
full[,numbers] %>% head()
naniar::gg_miss_var(full[,numbers], show_pct = TRUE)
full[,numbers] %>% is.na() %>%  colSums()  # 각 열별

# 결측 없는 열 3개만 선택 해 보자. MSSubClass LotArea OverallQual

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3)

train(SalePrice ~ Street + LotShape + LandContour + MSSubClass + LotArea + OverallQual,
      data = train,  
      method = "rpart", 
      trControl = ctrl) -> rpartFit 

rpartFit %>%  plot()

rpartFit %>%  predict(test) %>%  as.data.frame() -> rpartPred

names(rpartPred) <- c("SalePrice")

rpartPred %>%  head()

rpartPred$SalePrice -> submission$SalePrice

submission %>% head()
submission %>% tail()

write.csv(submission, "./result/20210704_submission.csv", row.names = FALSE)

## 0.29075점 - 11,887등 / 13,540등 