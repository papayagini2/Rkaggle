library(tidyverse)  # 
library(tidymodels) # 

?tidymodels
# 공식 홈페이지: https://tidymodels.tidymodels.org/

############### parsnip ########################
library(help="parsnip")
??parsnip
# https://github.com/tidymodels/parsnip

# From randomForest 하이퍼 파라메터 
# mtry: The number of predictors that will be randomly sampled at each split when creating the tree models. X인자값보다 작어야 겠죠..
# ntree: The number of trees contained in the ensemble.
# min_n: The minimum number of data points in a node that are required for the node to be split further.

# random forest 를 돌릴 때, 3가지 방법이 있음
# from randomForest 
library(randomForest)
rf_1 <- randomForest(
  mpg ~ ., 
  data = mtcars, 
  mtry = 10, # classification (sqrt(p) where p is number of variables in x) and regression (p/3) 
  ntree = 2000, # default ntree=500
  importance = TRUE
)
rf_1

# From ranger
library(ranger)
rf_2 <- ranger(
  mpg ~ ., 
  data = mtcars, 
  mtry = 10, 
  num.trees = 2000, 
  importance = "impurity"
)
rf_2


# From sparklyr
# library(sparklyr)
# rf_3 <- ml_random_forest(
#   mtcars,
#   intercept = FALSE,
#   response = "Sepal.Length",
#   features = names(mtcars)[names(mtcars) != "mpg"],
#   col.sample.rate = 10,
#   num.trees = 2000
# )
# 에러: Unable to retrieve a Spark DataFrame from object of class data.frame

library(parsnip)

args(rand_forest)


rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("randomForest", importance = "impurity") %>%
  set_mode("regression")


rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("spark", importance = "impurity") %>%
  set_mode("regression")


set.seed(192)
rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression") %>%
  fit(mpg ~ ., data = mtcars)
# 그런데, randomforest 돌리면 에러나던데.. 뭐가 좋은 거지?

############### workflows ########################
library(help="workflows")
??workflows
# https://workflows.tidymodels.org/
# https://github.com/tidymodels/workflows

library(recipes)
library(parsnip)
library(workflows)

# without workflow
spline_cars <- recipe(mpg ~ ., data = mtcars) %>% 
  step_ns(disp, deg_free = 10) # 아래 주1) 참고

spline_cars_prepped <- prep(spline_cars, mtcars)

# install.packages("rstan")
# install.packages("rstanarm") # stan 이란 게.. 유명한 건가봐. 

bayes_lm <- linear_reg() %>% 
  set_engine("stan")

bayes_lm_fit <- fit(bayes_lm, mpg ~ ., 
                    data = juice(spline_cars_prepped))
bayes_lm_fit

# with workflow
car_wflow <- workflow() %>% 
  add_recipe(spline_cars) %>% 
  add_model(bayes_lm)

car_wflow_fit <- fit(car_wflow, data = mtcars)

# update_recipe()/ update_model()및 remove_recipe()/를 사용하여 기존 워크 플로를 변경할 수 있습니다 

# 주1) step_ns create new columns that are basis expansions of variables using natural splines. -> 정확하게는 모르겠음.
library(ggplot2)
mtcars %>%  ggplot() + aes(x=disp) + geom_histogram()

spline_cars_prepped %>% juice()  %>%  
  ggplot() + aes(x=disp_ns_01) + geom_histogram()
spline_cars_prepped %>% juice()  %>% 
  ggplot() + aes(x=disp_ns_02) + geom_histogram()

bayes_lm <- linear_reg() %>% 
  set_engine("stan")

?workflow()
set.seed(123)
library(parsnip)
library(recipes)
library(workflows)
library(modeldata)

data("Sacramento")
Sacramento %>%  str()

base_wf <- workflow() %>%
  add_formula(price ~ type + sqft + beds + baths)

Sacramento$type %>%  table()

# This first model does create dummy/indicator variables:
lm_spec <- linear_reg() %>%
  set_engine("lm")

base_wf %>%
  add_model(lm_spec) %>%
  fit(Sacramento)

# This second model does not create dummy/indicator variables:
rf_spec <- rand_forest() %>%
  set_mode("regression") %>%
  set_engine("ranger")

base_wf %>%
  add_model(rf_spec) %>%
  fit(Sacramento)

# 회귀는 factor를 더미처리하고, tree 모델은 자동 데미 처리 
library(parsnip)
library(recipes)
library(magrittr)
library(modeldata)

data("attrition")
attrition %>%  str()

model <- logistic_reg() %>%
  set_engine("glm")

base_wf <- workflow() %>%
  add_model(model)

formula_wf <- base_wf %>%
  add_formula(Attrition ~ BusinessTravel + YearsSinceLastPromotion + OverTime)
# Attrition: Factor, BusinessTravel: Factor, 
# YearsSinceLastPromotion: int, OverTime: Factor 

fit(formula_wf, attrition)

recipe <- recipe(Attrition ~ ., attrition) %>%
  step_dummy(all_nominal(), -Attrition) %>%
  step_corr(all_predictors(), threshold = 0.8)

recipe_wf <- base_wf %>%
  add_recipe(recipe)

fit(recipe_wf, attrition)

variable_wf <- base_wf %>%
  add_variables(
    Attrition,
    c(BusinessTravel, YearsSinceLastPromotion, OverTime)
  )

fit(variable_wf, attrition)

############### dials ########################
library(help="dials")
??dials
# https://dials.tidymodels.org/
# https://github.com/tidymodels/dials

library(dials)
cost_complexity() # rpart Cp 값 
cost_complexity() %>% range_get()
cost_complexity() %>% range_set(c(-5,1))
cost_complexity(range = c(-5, 1))
cost_complexity() %>% value_seq(n = 4)
cost_complexity() %>% value_seq(n = 4, original = FALSE)
set.seed(5473)
cost_complexity() %>% value_sample(n = 4)

library(rpart)
cart_mod <- rpart(mpg ~ ., data = mtcars, control = rpart.control(cp = 0.000001))
cart_mod$cptable
#>         CP nsplit rel error xerror  xstd
#> 1 0.643125      0     1.000  1.064 0.258
#> 2 0.097484      1     0.357  0.687 0.180
#> 3 0.000001      2     0.259  0.576 0.126
cp_vals <- cart_mod$cptable[, "CP"]

# We should only keep values associated with at least one split:
cp_vals <- cp_vals[ cart_mod$cptable[, "nsplit"] > 0 ]

# Here the specific Cp values, on their natural scale, are added:
mtcars_cp <- cost_complexity() %>% value_set(cp_vals)
#> Error: Some values are not valid: 0.09748...

mtcars_cp  <- cost_complexity () %>%  value_set ( log10 ( cp_vals ))
mtcars_cp 
#> Cost-Complexity Parameter (quantitative) 
#> Transformer : log-10 
#> Range (transformed scale) : [-10, -1] 
#> 값 : 2

mtcars_cp  %>%  value_seq ( 2 )
#> [1] 0.097484 0.000001 
# 샘플링 특정 값은 대체 
mtcars_cp  %>%  
  value_sample ( 20 ) %>%  
  table()
#>로 수행됩니다. 
#> 1e-06 0.0974840733898344 
#> 9 11

# 세밀한 다이얼링은 어렵다..일단 skip 

############### tune ########################
library(help="tune")
??tune
# https://tune.tidymodels.org/
# https://github.com/tidymodels/tune


library(tidymodels)

data(ames)

set.seed(4595)
data_split <- ames %>%
  mutate(Sale_Price = log10(Sale_Price)) %>%
  initial_split(strata = Sale_Price)
ames_train <- training(data_split)
ames_test  <- testing(data_split)

ames_train %>% 
  dplyr::select(Sale_Price, Longitude, Latitude) %>% 
  tidyr::pivot_longer(cols = c(Longitude, Latitude), 
                      names_to = "predictor", values_to = "value") %>% 
  ggplot(aes(x = value, Sale_Price)) + 
  geom_point(alpha = .2) + 
  geom_smooth(se = FALSE) + 
  facet_wrap(~ predictor, scales = "free_x")
#> `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

ames_rec <- 
  recipe(Sale_Price ~ Gr_Liv_Area + Longitude + Latitude, data = ames_train) %>% 
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_ns(Longitude, Latitude, deg_free = tune())

ames_rec <- 
  recipe(Sale_Price ~ Gr_Liv_Area + Longitude + Latitude, data = ames_train) %>% 
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_ns(Longitude, deg_free = tune("long df")) %>% 
  step_ns(Latitude,  deg_free = tune("lat df"))

parameters(ames_rec)
#> Collection of 2 parameters for tuning
#> 
#>  identifier     type    object
#>     long df deg_free nparam[+]
#>      lat df deg_free nparam[+]

deg_free()
#> Degrees of Freedom (quantitative)
#> Range: [1, 5]

############### rsample ########################
library(help="rsample")
??rsample
# https://github.com/tidymodels/rsample
# 모델을 평가하고 경험적으로 검증 할 수 있도록 데이터 리샘플링을 위한 인프라 가 있습니다.



############### yardstick ########################
library(help="yardstick")
??yardstick


