library(tidyverse, quietly=TRUE, warn.conflicts=FALSE)
library(magrittr, quietly=TRUE, warn.conflicts=FALSE)
library(skimr, quietly=TRUE, warn.conflicts=FALSE)
library(caret, quietly=TRUE, warn.conflicts=FALSE)
library(recipes, quietly=TRUE, warn.conflicts=FALSE)
library(janitor, quietly=TRUE, warn.conflicts=FALSE)
library(skimr, quietly=TRUE, warn.conflicts=FALSE)
library(naniar, quietly=TRUE, warn.conflicts=FALSE)

rm(list=ls())
setwd("D:/RPrj/Rkaggle/Rkaggle/HousePrice")

bind_rows("./data/train.csv" %>% read_csv() %>%  
            mutate(index = "train", .before = 1 ) -> train, 
          "./data/test.csv" %>%  read_csv()  %>%  
            mutate(index = "test", .before = 1  ) -> test ) -> full 

"./result/sample_submission.csv" %>%  read_csv() -> submission

full %<>% select(index, Id, SalePrice, everything()) 

full %>%  skim()
full %>%  filter(index == "train") -> train
full %>%  filter(index == "test")  -> test 

full %>%  dim() # 2919   82
train %>% dim() # 1460   82
test  %>% dim() # 1459   82
submission %>% dim() # 1459    2

# 목적변수를 탐색 해 보자. 
train %>%  ggplot() + aes(x=SalePrice) + geom_histogram()
train %>%  arrange(desc(SalePrice)) %>% head() # 755000 75만불?? 8억??? 너무 싼 거 아닌가ㅋ
train %>%  arrange(SalePrice) %>% head() # 34900 3천8백만불? 8억??? 너무 싼 거 아닌가ㅋ

# 수치형 데이터 부터 사용해 보자 
train %>%  sapply(is.numeric) %>%  
  which() %>%  train[,.] -> train_n

train_n %>%  gg_miss_var()

# 결측 
train_n %>% is.na() %>%  colSums()
train_n %>%  is.na() %>%  colSums() %>%  
  as.data.frame() %>%  
  as_tibble(rownames="col") -> tmp 

tmp %>% dim()
names(tmp) <- c("col", "na_count")
tmp %<>%  arrange(desc(na_count)) 

# na 가 없는 열만 추출 
tmp %>% filter(na_count == 0) -> tmp2

for(i in 1:nrow(tmp2)){
cor(train_n[tmp2[i,1][[1]]], train_n$SalePrice)  %>%  print()
}

train_n %>%  select(tmp2$col) -> train_n_f
  
train_n_f %>%  cor() %>%  .[,2] %>% as.data.frame() %>%  
  as_tibble(rownames="col") -> tmp2 

tmp2 %>% dim()
names(tmp2) <- c("col", "cor")
tmp2 %<>%  arrange(desc(cor)) 

# col            cor
# <chr>        <dbl>
#   1 SalePrice    1    
# 2 OverallQual  0.791
# 3 GrLivArea    0.709
# 4 GarageCars   0.640
# 5 GarageArea   0.623
# 6 TotalBsmtSF  0.614
# 7 1stFlrSF     0.606
# 8 FullBath     0.561
# 9 TotRmsAbvGrd 0.534
# 10 YearBuilt    0.523

# 따라서 결측치가 없고 상관계수가 높은 변수 순으로 추가해 보자. 

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3)

# OverallQual 만으로 예측 해 보겠습니다. 상관계수 0.791
# Rates the overall material and finish of the house
# 주택의 전체 자재 및 마감 등급을 평가합니다.

train %>% ggplot() + aes(x=OverallQual, y=SalePrice) + geom_point()

train(SalePrice ~ OverallQual,
      data = train,  
      method = "rpart", 
      trControl = ctrl) -> rpartFit1 
rpartFit1 # Rsquared 0.5843960
rpartFit1 %>%  plot()

train(SalePrice ~ OverallQual,
      data = train,  
      method = "lm", 
      trControl = ctrl) -> lmFit1 
lmFit1 # Rsquared 0.6370592
# lmFit1 %>%  plot()

# 이번에는 수치형 변수의 정규화를 해 보겠다.
library(e1071) # e1071::skewness()

train$OverallQual %>% hist()
train$OverallQual %>% skewness() # 0.2164984
train$OverallQual %>% log() %>%  hist()
train$OverallQual %>% log() %>%  skewness() # -0.9278717
train$OverallQual %>% .^2 %>%  hist()
train$OverallQual %>% .^2 %>%  skewness() # 0.8658235

train %>%
  recipe(SalePrice ~ OverallQual) %>%
  step_BoxCox(OverallQual) %>%  
  prep() %>% juice() %>%  .$OverallQual %>%  hist()

train %>%
  recipe(SalePrice ~ OverallQual) %>%
  step_BoxCox(OverallQual) %>%  
  prep() %>% juice() %>%  .$OverallQual %>%  skewness() # 0.02834792 우와 그래도 줄어드네. 

train %>%
  recipe(SalePrice ~ OverallQual) %>%
  step_YeoJohnson(OverallQual) %>%  
  prep() %>% juice() %>%  .$OverallQual %>%  hist()

train %>%
  recipe(SalePrice ~ OverallQual) %>%
  step_YeoJohnson(OverallQual) %>%  
  prep() %>% juice() %>%  .$OverallQual %>%  skewness() # 0.01818554 우와 더 줄어든다. 

train %>%
  recipe(SalePrice ~ OverallQual) %>%
  step_YeoJohnson(OverallQual) %>%  
  prep() %>% juice() -> train1


train(SalePrice ~ OverallQual,
      data = train1,  
      method = "rpart", 
      trControl = ctrl) -> rpartFit2
rpartFit2 # Rsquared 0.5878762 아주 조금 올랐다. 
rpartFit2 %>%  plot()

train(SalePrice ~ OverallQual,
      data = train1,  
      method = "lm", 
      trControl = ctrl) -> lmFit2
lmFit2 # Rsquared 0.6169727 음.. 더 떨어졌네.. 
# lmFit1 %>%  plot()

# 테스트 결론.. 모르겠다.. 다만, 직관적으로 외도가 매우 크거나/작거나 한 경우에만 적용하자. 

# 두 번째 변수 GrLivArea 적용해 보자. 
# Above grade (ground) living area square feet
# 지상 거주 구역 평방 피트 이상
train %>% ggplot() + 
  aes(x=GrLivArea, y=SalePrice) + geom_point()

train %>% ggplot() + 
  aes(x=OverallQual, y=SalePrice, size = GrLivArea, color = GrLivArea) + 
  geom_point()

train$GrLivArea %>% hist()
train$GrLivArea %>% skewness() # 1.363754

# 일단 변수를 추가해 보자. 
train(SalePrice ~ OverallQual + GrLivArea,
      data = train,  
      method = "rpart", 
      trControl = ctrl) -> rpartFit3 
rpartFit3 # Rsquared 0.5905691
rpartFit3 %>%  plot()

train(SalePrice ~ OverallQual  + GrLivArea,
      data = train,   
      method = "lm", 
      trControl = ctrl) -> lmFit3 
lmFit3 # Rsquared 0.7199711 오호.. 상당한 개선 ! 


train(SalePrice ~ OverallQual + GrLivArea,
      data = train,  
      method = "rpart",
      preProc = c("YeoJohnson"), 
      trControl = ctrl) -> rpartFit4
rpartFit4 # Rsquared 0.586836
rpartFit4 %>%  plot()

train(SalePrice ~ OverallQual  + GrLivArea,
      data = train,   
      method = "lm",
      preProc = c("YeoJohnson"),
      trControl = ctrl) -> lmFit4 
lmFit4 # Rsquared 0.7200267 오호... 아주 조금씩 좋아지고 있음. 

# 자신감이 붙었음. scale, centering 도 해 뿌리자 
train(SalePrice ~ OverallQual + GrLivArea,
      data = train,  
      method = "rpart",
      preProc = c("center","scale", "YeoJohnson"), 
      trControl = ctrl) -> rpartFit5
rpartFit5 # Rsquared 0.5937183 아주 미세하게 좋아진다. 
rpartFit5 %>%  plot()

train(SalePrice ~ OverallQual  + GrLivArea,
      data = train,   
      method = "lm",
      preProc = c("center","scale", "YeoJohnson"), 
      trControl = ctrl) -> lmFit5
lmFit5 # Rsquared 0.7188478 음..좀 더 떨어졌네..

# 세 번째 변수 GarageCars 추가 해 보자. 
# Size of garage in car capacity
# 차량 용량 내 차고 크기.. 주차장 크기 같은데..
train$GarageCars %>% hist()
train$GarageCars %>% skewness() # -0.3418454

train %>% ggplot() + 
  aes(x=GarageCars, y=SalePrice) + geom_point()

train %>% ggplot() + 
  aes(x=OverallQual, y=SalePrice, size = GrLivArea, color = GarageCars) + 
  geom_point() 

train(SalePrice ~ OverallQual + GrLivArea + GarageCars,
      data = train,  
      method = "rpart",
      preProc = c("center","scale", "YeoJohnson"), 
      trControl = ctrl) -> rpartFit6
rpartFit6 # Rsquared 0.5895149 이전 보다 더 떨어졌다.. ㅠㅠ 
rpartFit6 %>%  plot()

train(SalePrice ~ OverallQual  + GrLivArea+ GarageCars,
      data = train,   
      method = "lm",
      preProc = c("center","scale", "YeoJohnson"), 
      trControl = ctrl) -> lmFit6
lmFit6 # Rsquared 0.7438461 음..그래도 좀 올랐네..역시 선형회귀가 훨씬 좋네. 

test %>%  NROW() # 1459 인데.. 
lmFit6 %>%  predict(test) %>%  NROW() # 왜 1458개이지????

# 그 이유를 찾았음. 
test$GarageCars %>%  is.na() %>%  sum()
# train에는 결측이 없는데.. test 에는 결측이 있네.. 
# 급하게 결측치 보정합시다. 

test$GarageCars <-  ifelse(test$GarageCars  %>% is.na(), 
                           median(test$GarageCars, na.rm = TRUE), 
                           test$GarageCars)

test$GarageCars %>%  is.na() %>%  sum()

lmFit6 %>%  predict(test) %>%  as.data.frame() -> lmPred
names(lmPred) <- c("SalePrice")
lmPred %>%  head()

submission$SalePrice <- lmPred$SalePrice

submission %>% head()
submission %>% tail()

write.csv(submission, "./result/20210724_submission.csv", row.names = FALSE)

## 0.55520점  지난 번 보다 떨어짐.. 헉.. 역시 factor를 더 사용했어야..
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques/leaderboard#score
# 일단, 오늘은 여기까지.. 하려다가.. 그래도 기존에 사용했던 factor 더 넣어 보자. 


#Street + LotShape + LandContour + MSSubClass + LotArea + OverallQual

# 일단.. 이 factor 3개만 더 넣자. facotr 별 수치형 목적 변수가 큰 순서대로 넣고 싶은데..
# 어떻게 뽑을까..
train$Street %>%  class
train$LotShape %>%  class
train$LandContour %>%  class

train$Street <- as.factor(train$Street)
train$LotShape <- as.factor(train$LotShape)
train$LandContour <- as.factor(train$LandContour)

full$Street  %>%  is.na() %>%  sum()
train$LotShape %>%  is.na() %>%  sum()
train$LandContour %>%  is.na() %>%  sum()

train$Street   %>%  table()
train$LotShape %>%  table()
train$LandContour %>%  table()

train %>% ggplot() + aes(x=Street, y=SalePrice) + geom_boxplot()
train %>% ggplot() + aes(x=LotShape, y=SalePrice) + geom_boxplot()
train %>% ggplot() + aes(x=LandContour, y=SalePrice) + geom_boxplot()

# 그래도 분류 되니까 factor 넣어 보자. 

train(SalePrice ~ OverallQual + GrLivArea + GarageCars + Street + LotShape + LandContour,
      data = train,  
      method = "rpart",
      preProc = c("center","scale", "YeoJohnson"), 
      trControl = ctrl) -> rpartFit7
rpartFit7 # Rsquared 0.5973778 아주 조금 올랐네. ㅠㅠ
rpartFit7 %>%  plot()

train(SalePrice ~ OverallQual + GrLivArea + GarageCars + Street + LotShape + LandContour,
      data = train,   
      method = "lm",
      preProc = c("center","scale", "YeoJohnson"), 
      trControl = ctrl) -> lmFit7
lmFit7 # Rsquared 0.7504917 음..그래도 좀 올랐네..근데..거의 안 올랐음. 



# 지난 번에 넣었던 변수 다 
#Street + LotShape + LandContour + MSSubClass + LotArea + OverallQual

train(SalePrice ~ OverallQual + GrLivArea + GarageCars +  + MSSubClass + LotArea + 
        Street + LotShape + LandContour,
      data = train,  
      method = "rpart",
      preProc = c("center","scale", "YeoJohnson"), 
      trControl = ctrl) -> rpartFit8
rpartFit8 # Rsquared 0.5904604 오히려 떨어졌네. 
rpartFit8 %>%  plot()

train(SalePrice ~ OverallQual + GrLivArea + GarageCars +  + MSSubClass + LotArea + 
        Street + LotShape + LandContour,
      data = train,   
      method = "lm",
      preProc = c("center","scale", "YeoJohnson"), 
      trControl = ctrl) -> lmFit8
lmFit8 # Rsquared 0.7685047 음..그래도 조금 씩 올라가네..  

lmFit8 %>%  predict(test) %>%  as.data.frame() -> lmPred
names(lmPred) <- c("SalePrice")
lmPred %>%  head()

submission$SalePrice <- lmPred$SalePrice

submission %>% head()
submission %>% tail()

write.csv(submission, "./result/20210724_2_submission.csv", row.names = FALSE)

## 0.55940점 - 아.. 신기하네.. 왜 또 base 를 못 넘는 걸까?

# 지난 번 것 다시 실행 해 보자. 
train(SalePrice ~ Street + LotShape + LandContour + MSSubClass + LotArea + OverallQual,
      data = train,  
      method = "rpart", 
      trControl = ctrl) -> rpartFit9 

rpartFit9 # 0.5940964 이거면..그리 높지 않은데.. 


# 변환 하지 말자. 
train(SalePrice ~ OverallQual + GrLivArea + GarageCars +  + MSSubClass + LotArea + 
        Street + LotShape + LandContour,
      data = train,   
      method = "lm",
      trControl = ctrl) -> lmFit10
lmFit10 # Rsquared 0.7679489 음..

lmFit10 %>%  predict(test) %>%  as.data.frame() -> rpartPred
names(rpartPred) <- c("SalePrice")
rpartPred %>%  head()
rpartPred$SalePrice -> submission$SalePrice

submission %>% head()
submission %>% tail()

write.csv(submission, "./result/20210724_3_submission.csv", row.names = FALSE)

## 뭔가 귀신에 쏠린 듯..