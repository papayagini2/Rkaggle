library(dplyr);library(magrittr);library(caret);library(recipes);
library(janitor);library(skimr);
rm(list=ls())
setwd("D:/RPrj/Rkaggle/Rkaggle/타이타닉")

bind_rows(read.csv("./data/train.csv") %>%  
            mutate(index = "train", .before = 1 )-> train, 
          read.csv("./data/test.csv")  %>%  
            mutate(index = "test", .before = 1  ) -> test ) -> full 

read.csv("./result/gender_submission.csv") -> submission

full %<>%  clean_names()
full %>%  skim()
# 결측 존재열: age, fare

# 형 변환:   survived, embarked, sex, pclass
full$index    %<>%  as.factor() # 가 만든 변수니까 skip 
full$survived <- ifelse(full$survived == "1", "live", "death")
full$survived %<>%  as.factor()
# survived    : Factor w/ 2 levels "death","live": 1 2 2 2 1 1 1 1 2 2 ...
full$embarked %<>%  as.factor()
# $ embarked    : Factor w/ 4 levels "","C","Q","S": 4 2 4 4 4 3 4 4 4 2 ...
full$sex      %<>%  as.factor()
# $ sex         : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
full$pclass   %<>%  as.ordered()  
# $ pclass      : Ord.factor w/ 3 levels "1"<"2"<"3": 3 1 3 1 3 3 1 3 3 2 ...

# 컬럼을 정렬하자. index, passenger_id, 수치형, 범주형, 문자형  
full %>% names()
full[, c(1:3,7:9,11, 4,6,13, 5,10,12)] -> full 
full %>%  str()
# "index"        "passenger_id" -> 구분값 
# "survived"     -> 목적변수 
# "age"          "sib_sp"    "parch"        "fare"  -> 수치형   
# "pclass"       "sex"          "embarked"    -> 범주형 
#  "name"         "ticket"       "cabin" -> 문자형 

# train/test split 
full %>%  filter(index == "train") -> train
full %>%  filter(index == "test")  -> test 

# 목적변수 탐색 
full$survived %>%  table()
full$survived %>%  table() %>%  prop.table() %>% round(2)
# balanced data 라고 생각됨. 


# 수치형 변수: "age"          "sib_sp"    "parch"        "fare" 

# (1) age 
full$age %>%  table()  # 0.17 ~ 80세 까지 존재함 
train %>% na.omit() %>% ggplot() + aes(x=age) + geom_histogram()
# 분포는 어느정도 정규성을 보인다. 

train %>%  ggplot() + aes(x=age) + geom_histogram() + facet_grid(survived ~ . )
train %>%  ggplot() + aes(x=age) + geom_boxplot()   + facet_grid(survived ~ . )
# 생존여부와 나이대는 상관이 있어보임. 
# 사망자 평균 나이 > 생존자 평균 나이 

# 일단, 결측이 존재하고..
train$age %>%  is.na() %>%  sum()
test$age %>%  is.na() %>%  sum() 

# 단순하게 나이가 작다 크다 보다, 아주 어린 사람, 건장한 성인, 노인 간의 차이가 있을 수 있지 않을까 ?
# 나이 수치를 범주형으로 분리해 보자. 

full = within(full, {
  grade = character(0) 
  grade[ age < 10                ] = "0s" 
  grade[ age >= 10 & age < 20] = "10s" 
  grade[ age >= 20 & age < 30] = "20s" 
  grade[ age >= 30 & age < 40] = "30s" 
  grade[ age >= 40 & age < 50] = "40s" 
  grade[ age >= 50 & age < 60] = "50s" 
  grade[ age >= 60 & age < 70] = "60s" 
  grade[ age >= 70             ] = "70s" 
  grade = factor(grade, level = c("0s", "10s", "20s", "30s", "40s", "50s", "60s", "70s"))
})
## within 말고 .. recipe()로 step cut 할 수 있을 것 같은데..잘 모르겠음. 


full %>% na.omit() %>%  select(grade, survived) %>%  
  table() %>% 
  prop.table() %>%    round(2)

full %>% na.omit() %>% ggplot() + aes(x=grade, fill = survived) + geom_bar()
full %>% na.omit() %>% ggplot() + aes(x=grade, fill = survived) + geom_bar(position='dodge')

# full에 만들었으니, train, test 다시 셋팅 
full %>%  filter(index == "train") -> train
full %>%  filter(index == "test")  -> test 

# (2) sib_sp 탐색 
# of siblings / spouses aboard the Titanic 처자식수 ?? 가족수 
full$sib_sp %>%  table()
train %>%  ggplot() + aes(x=sib_sp) + geom_histogram()
train %>%  ggplot() + aes(x=sib_sp) + geom_histogram() + facet_grid(survived ~ . )
train %>%  ggplot() + aes(x=sib_sp) + geom_boxplot() + facet_grid(survived ~ . )
train %>%  ggplot() + aes(x=factor(sib_sp), fill = survived) + geom_bar(position='dodge') 
# 그렇다고 factor나 order로 변경하면 더 성능이 좋아질 지는 모르겠음. 
# 변수가 필요하다 정도만 결론 짓고 그대로 수치형으로 사용 

recipe(survived ~ . , data = train) %>%  
  # step_nzv(sib_sp) %>%
  # step_BoxCox(sib_sp) %>%
  # step_log(sib_sp) %>%
  # step_sqrt(sib_sp) %>%  
  # step_YeoJohnson(sib_sp) %>% 
  # step_scale(sib_sp) %>%
  # step_center(sib_sp) %>% 
  prep()  %>%  juice() %>%  
  ggplot() + aes(x=sib_sp) + geom_histogram()

# sip_sp 는 아무것도 하지 말자 . 


# (3) parch 탐색 
full$parch %>%  table()
# of parents / children aboard the Titanic 부모자식수 ??
train %>%  ggplot() + aes(x=parch) + geom_histogram()
train %>%  ggplot() + aes(x=factor(parch), fill = survived) + geom_bar(position='dodge') 
# 그렇다고 factor나 order로 변경하면 더 성능이 좋아질 지는 모르겠음. 
# 변수가 필요하다 정도만 결론 짓고 그대로 수치형으로 사용 

recipe(survived ~ . , data = train) %>%  
  # step_nzv(parch) %>%
  # step_BoxCox(parch) %>%
   # step_log(parch) %>%
  # step_sqrt(parch) %>%  
  # step_YeoJohnson(parch) %>% 
  # step_scale(parch) %>%
  # step_center(parch) %>% 
  prep()  %>%  juice() %>%  
  ggplot() + aes(x=parch) + geom_histogram()

# 아무것도 하지 말자. 

# (4) fare 탐색 : 요금 ??
full$fare %>%  table()
train %>%  ggplot() + aes(x=fare) + geom_histogram()
train %>%  ggplot() + aes(x=fare) + geom_boxplot() + facet_grid(survived ~ . )
# 정규 분포가 아님 -> 변환 필요 

recipe(survived ~ . , data = train) %>%  
  # step_BoxCox(fare) %>%
  # step_log(fare) %>%
  # step_sqrt(fare) %>%  
  step_YeoJohnson(fare) %>% 
  # step_scale(fare) %>%
  step_center(fare) %>% 
  prep()  %>%  juice() %>%  
  ggplot() + aes(x=fare) + geom_histogram()
# 음..훨씬 정규분포스러움. 일단 이렇게 전처리하자. 
  

##############

## EDA 
# 범주형 변수들과 survived 차이 
train %>% ggplot() + aes(x=pclass, fill = survived)   + geom_bar()
# 생존확률: 1등급 > 2등급> 3등급 

train %>% ggplot() + aes(x=sex, fill = survived)      + geom_bar()
train %>% ggplot() + aes(x=sex, fill = survived)      + geom_bar(stat='count', position='dodge') 
# 생존확률: female > male 

train %>% ggplot() + aes(x=embarked, fill = survived) + geom_bar() 
train %>% ggplot() + aes(x=embarked, fill = survived) + geom_bar(stat='count', position='dodge') 


table(train$embarked, train$survived) 
table(train$embarked, train$survived) %>%  rowSums()
93/168
30/77
217/644
# 생존확률: c > Q > S 



# 문자형(name, ticket, cabin) 으로 되어 있는 놈들 어떻게 할 거냐.
full$ticket %>%  table()
# Ticket number ??

full$cabin %>%  table()
# Cabin number ??

full$name %>%  head(100)
# Sur name(성), Mr/Miss/Mrs. name(이름) ~~ 형태임
# gsub 은 알겠으나.. 정규식 표현은 연습 해 두어야 함 !! 
gsub('(.*, )|(\\..*)', '', full$name) -> full$title 

# title과 sex를 비교해 보자. 
table(full$title, full$sex)

# 재 분류해 보자 
full$title[full$title == 'Mlle']        <- 'Miss'  # 같은 표현 
full$title[full$title == 'Ms']          <- 'Miss'  # 같은 표현 
full$title[full$title == 'Mme']         <- 'Mrs'   # 같은 표현 
full$title[full$title %in% 
             c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
               'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')]  <- 'Rare Title' # 기타 표현들은 합침 

table(full$title, full$sex)
# sex도 두고 title도 두어야 할까??

# 각 title에 따른 생존 패턴을 확인 해 보자. 
full %>%   na.omit() %>% 
  select(title, survived) %>%  
  table() %>% 
  prop.table() %>%    round(2)

full %>%   na.omit() %>% 
  ggplot() + aes(x=title, fill = survived) + geom_bar()

# 확연한 패턴 차이가 보임. 
# 결론: title이라는 범주형 파생변수 획득 

full %>%  filter(index == "train") -> train
full %>%  filter(index == "test")  -> test 


recipe(survived ~ . , data = full) %>%  
  step_impute_linear(age, 
                     impute_with = imp_vars(pclass, sex, sib_sp, parch, embarked)) %>% # age는 pclass, sex, sib_sp, parch, embarked의 선형 회귀를 돌려서//지난 번  
  step_impute_linear(fare, 
                     impute_with = imp_vars(pclass, sex, sib_sp, parch,embarked)) %>% # 논리적 근거는 없으나 그냥 이걸로 하자. 
  # step_log(sib_sp, parch) %>%
  step_YeoJohnson(fare) %>% 
  step_center(fare) %>% 
  step_rm(ticket, cabin) %>% # 이 변수는 어떻게 사용해야 할 지 모르겠음. ㅠㅠㅠ
  prep()  %>%  juice() -> full2


full2 = within(full2, {
  grade = character(0) 
  grade[ age < 10                ] = "0s" 
  grade[ age >= 10 & age < 20] = "10s" 
  grade[ age >= 20 & age < 30] = "20s" 
  grade[ age >= 30 & age < 40] = "30s" 
  grade[ age >= 40 & age < 50] = "40s" 
  grade[ age >= 50 & age < 60] = "50s" 
  grade[ age >= 60 & age < 70] = "60s" 
  grade[ age >= 70             ] = "70s" 
  grade = factor(grade, level = c("0s", "10s", "20s", "30s", "40s", "50s", "60s", "70s"))
})

full2 %>% skim() 

full2 %>%  filter(index == "train") -> train2
full2 %>%  filter(index == "test")  -> test2 


train2 %>%  str() 

set.seed(123) 
ctrl <- trainControl(method = "repeatedcv",
                      repeats = 3)
# ctrl_none <- trainControl(method = "none")

train(survived ~ . , data = train2[-c(1:2, 10),],  
       method = "rpart", tuneLength = 7, 
       trControl = ctrl) -> rpartFit

rpartFit 
rpartFit %>%  ggplot()


rpartFit %>%  predict(test2) -> rpartPred

rpartPred %>%  as.data.frame()  -> submission$Survived

submission$Survived <- ifelse(submission$Survived == "live", "1", "0")

write.csv(submission, "./result/gender_submission20210703.csv", row.names = FALSE)


#### 지난 번: 0.77990점 12,303등 / 48,509 개 팀
#### 0.77990점.. 헉.. 지난 번하고 똑깥아..ㅠㅠㅠㅠㅠㅠ
#### 좌절. OTL 


# 1. titanic 점수 높히기
# 2. titnaic 소스 올리기
# 3. github 서로 공유해서 가져오기. 
# 4. 패키지는 tidymodels 전반적인거.
# 5. 토요일 미팅 장소 공유 드릴테니, 더 좋은 장소 같이 찾기 
# 6. google meet .화면 튜닝..고민 
# 7. knit 방법 - 조시은
