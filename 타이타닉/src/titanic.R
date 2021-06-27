library(dplyr);library(magrittr);library(caret);library(recipes);
library(janitor);library(skimr);
rm(list=ls())
setwd("D:/ADP실기/KaggleR/titanic/")

read.csv("gender_submission.csv") -> submission
bind_rows(read.csv("train.csv") %>%  mutate(index = "train")-> train, 
          read.csv("test.csv")  %>%  mutate(index = "test") -> test ) -> full 

# train %>%  glimpse()
# test  %>%  glimpse()
# full  %>%  glimpse()
submission %>%  glimpse()

full %<>%  clean_names()

# full %>% glimpse()
# full %>% skim()

full$index    %<>%  as.factor()
full$embarked %<>%  as.factor()
full$sex      %<>%  as.factor()

full$survived <- ifelse(full$survived == "1", "live", "death")
full$survived %<>%  as.factor()

# train/test split 
full %>%  filter(index == "train") -> train
full %>%  filter(index == "test")  -> test 

# train %>%  glimpse()
# test  %>%  glimpse()

# train$survived %>%  table()

# trainControl 
ctrl_repeated <- trainControl(method = "repeatedcv",
                     repeats = 3)
# train 
set.seed(123) 
train(survived ~ . , data = train[,-6], # 결측치 있는 age 빼고 학습 
      method = "rpart", 
      trControl = ctrl_repeated) -> rpartFit_raw 
rpartFit_raw
# cp          Accuracy   Kappa    
# 0.01023392  0.7964827  0.5466863

rpartFit_raw %>%  ggplot() 
rpartFit_raw %>%  varImp()
# sexmale                                          100.000
# pclass                                            60.232
# fare                                              46.700
# embarkedS                                         13.890
# embarkedC                                         12.360
# sib_sp                                            11.207
# parch                                              4.132


# randomforest 이용해 보자. /without cv 
ctrl_none <- trainControl(method = "none")

set.seed(123) 
train(survived ~ . , data = train[,-6], # 결측치 있는 age 빼고 학습 
      method = "rf", 
      trControl = ctrl_none) -> rfFit_raw 
rfFit_raw
# cp          Accuracy   Kappa    
# 0.01023392  0.7964827  0.5466863

# rfFit_raw %>%  plot() 
rfFit_raw %>%  varImp() 

# rpart 나 rf나 큰 차이가 없네.. 일단 rpart 를 기준으로 끌고 나가보자. 

# age, fare 결측치 처리부터 해 보자. 
library(ggplot2)
full %>% ggplot() + aes(x=age) + geom_histogram()

full$age %>% is.na() %>%  which() -> tmp 


full[tmp,]  -> age_na
full[-tmp,] -> age_com

age_na   %>%  skim()
age_com  %>%  skim()

age_na$survived  %>%  table() 
age_com$survived %>%  table() 

age_na$survived  %>%  table() %>%  prop.table() 
# death      live 
# 0.7062147 0.2937853 

age_com$survived %>%  table() %>%  prop.table() 
# death      live 
# 0.5938375 0.4061625 

# age 결측치가 기존과의 생존확률이 더 낮다.. 이게 무슨 의미일까. 

# 일단 잘 모르겠으니... age, fare는 median 으로 때려 넣자. 
recipe(survived ~ . , data = train) %>%  
  step_medianimpute(age) %>% 
  step_medianimpute(fare) %>% 
  step_rm(name,ticket,cabin) %>% # 문자열 결측치 컬럼은 아예 빼 버리자. 
  prep() -> rcp

rcp %>% juice()               -> train2
rcp %>% bake(new_data = test) -> test2

train$age %>% is.na() %>%  which -> train_na_col 
train[train_na_col,] %>%  head()
train2[train_na_col,] %>%  head()


test$age %>% is.na() %>%  which -> test_na_col 
test[test_na_col,] %>%  head()
test2[test_na_col,] %>%  head()
# 모두 28로 결측 보정되네. 

train2 %>%  skim()
test2  %>%  skim()

train$fare %>%  median()
train2$fare %>%  median()

test$fare %>%  median(na.rm=TRUE)
test2$fare %>%  median()


# train 
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3)
set.seed(123) 
train(survived ~ . , data = train2, 
      method = "rpart", 
      trControl = ctrl) -> rpartFit_med 
rpartFit_med
# cp          Accuracy   Kappa    
# 0.02339181  0.8073234  0.5820216
# 쫌 올랐다. 

rpartFit_med %>%  ggplot() 
rpartFit_med %>%  varImp()

rpartFit_med %>%  predict(test2) -> rpartPred_med

# 이번에는 좀 아이디어를 내서.. age를 예측하는 선형 모델로 age를 결측해 보자. 

recipe(survived ~ . , data = train) %>%  
  step_rm(name,ticket,cabin) %>% # 문자열 결측치 컬럼은 아예 빼 버리자. 
  step_impute_linear(age, impute_with = imp_vars(pclass, sex, sib_sp, parch, embarked)) %>% 
  step_impute_linear(fare, impute_with = imp_vars(pclass, sex, sib_sp, parch,embarked)) %>%
  prep() -> rcp

rcp %>% juice()               -> train3
rcp %>% bake(new_data = test) -> test3

# train 
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3)
set.seed(123) 
train(survived ~ . , data = train3, 
      method = "rpart", 
      trControl = ctrl) -> rpartFit_lm 
rpartFit_lm
rpartFit_lm %>%  ggplot()
# cp          Accuracy   Kappa    
# 0.02339181  0.8073234  0.5820216
# 큰 차이는 없다. 

rpartFit_lm %>%  predict(test3) -> rpartPred_lm

### 최종모델은.. rpartFit_lm을 제출하자. 

rpartPred_lm %>%  as.data.frame()  -> submission$Survived

submission$Survived <- ifelse(submission$Survived == "live", "1", "0")

write.csv(submission, "gender_submission.csv", row.names = FALSE)
#### 0.77990점 12,303등 / 48,509 개 팀

vignette("broom")


