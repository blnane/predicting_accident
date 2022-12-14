library(readr)
Sys.setlocale(category = "LC_ALL", locale = "us")
dataset <- read_csv("dataset2.csv", col_types = cols(...17 = col_skip(), ...18 = col_skip()))
library(dplyr)
library(lubridate)
train = (year(dataset2$date) < 2017)
dataset.2017 = dataset[!train,]
dim(dataset.2017)
dataset.2017
dataset2[is.na(dataset2)] <- 0
dataset2
train = (year(dataset2$date) < 2017)
dataset2.2017 = dataset2[!train,]
dim(dataset2.2017)
dataset2.2017
training = (year(dataset2$date) > 2016)
dataset2.2013 = dataset2[!training,]
dim(dataset2.2013)
library(dplyr)
dataset2$is_occur <- ifelse(dataset2$occur_num > 0, TRUE, FALSE)
dataset2$is_death <- ifelse(dataset2$death_num > 0, TRUE, FALSE)
dataset2$is_injure <- ifelse(dataset2$injure_num > 0, TRUE, FALSE)
is_death.2017 = dataset2$is_death[!train]
is_death.2013 = dataset2$is_death[train]
is_occur.2017 = dataset2$is_occur[!train]
is_occur.2013 = dataset2$is_occur[train]
is_injure.2017 = dataset2$is_injure[!train]
is_injure.2013 = dataset2$is_injure[train]
library(ISLR)
set.seed(4)
train_num = sample(11688, 5844)
attach(dataset2.2013)
lm.fit_occur = lm(formula = occur_num~ day_rain +hi_inst_wind + hi_wind + avg_pres + total_sun + avg_cloud + dur_fog + day_dust, data = dataset2.2013, subset = train_num)
lm.fit2_occur =  lm(occur_num ~ poly(day_rain, 2) + 
                      poly(hi_inst_wind, 2) + 
                      poly(hi_wind, 2) + 
                      poly(avg_pres, 2) + 
                      poly(total_sun, 2) + 
                      poly(avg_cloud, 2) + 
                      poly(dur_fog, 2) + 
                      poly(day_dust, 2), data = dataset2.2013, subset = train_num)
lm.fit3_occur = lm(occur_num ~ poly(day_rain, 3) + poly(hi_inst_wind, 3) + poly(hi_wind, 3) + poly(avg_pres, 3) + poly(total_sun, 3) + poly(avg_cloud, 3) + poly(dur_fog,3) + poly(day_dust, 3), data = dataset2.2013, subset = train_num)
mean((occur_num - predict(lm.fit_occur))[-train_num]^2)
mean((occur_num - predict(lm.fit2_occur))[-train_num]^2)
mean((occur_num - predict(lm.fit3_occur))[-train_num]^2)
lm.fit_death = lm(formula = death_num~ avg_temp +  day_rain +hi_inst_wind + hi_wind + total_sun + dur_fog + day_dust, data = dataset2.2013, subset = train_num)
lm.fit2_death = lm(death_num ~ poly(avg_temp, 2) + poly(day_rain, 2) + 
                     poly(hi_inst_wind, 2) + 
                     poly(hi_wind, 2) + 
                     poly(total_sun, 2) + 
                     poly(dur_fog, 2) + 
                     poly(day_dust, 2), data = dataset2.2013, subset = train_num)
lm.fit3_death = lm(death_num ~ poly(avg_temp, 3) + poly(day_rain, 3) + 
                     poly(hi_inst_wind, 3) + 
                     poly(hi_wind, 3) + 
                     poly(total_sun, 3) + 
                     poly(dur_fog, 3) + 
                     poly(day_dust, 3), data = dataset2.2013, subset = train_num)
mean((death_num - predict(lm.fit_death))[-train_num]^2)
mean((death_num - predict(lm.fit2_death))[-train_num]^2)
mean((death_num - predict(lm.fit3_death))[-train_num]^2)
lm.fit_injure = lm(formula = injure_num~ day_rain +hi_inst_wind + hi_wind + avg_pres + total_sun + avg_cloud + dur_fog + day_dust, data = dataset2.2013, subset = train_num)
lm.fit2_injure =  lm(injure_num ~ poly(day_rain, 2) + 
                       poly(hi_inst_wind, 2) + 
                       poly(hi_wind, 2) + 
                       poly(avg_pres, 2) + 
                       poly(total_sun, 2) + 
                       poly(avg_cloud, 2) + 
                       poly(dur_fog, 2) + 
                       poly(day_dust, 2), data = dataset2.2013, subset = train_num)
lm.fit3_injure =  lm(injure_num ~ poly(day_rain, 3) + 
                       poly(hi_inst_wind, 3) + 
                       poly(hi_wind, 3) + 
                       poly(avg_pres, 3) + 
                       poly(total_sun, 3) + 
                       poly(avg_cloud, 3) + 
                       poly(dur_fog, 3) + 
                       poly(day_dust, 3), data = dataset2.2013, subset = train_num)
lm.fit3_injure =  lm(injure_num ~ poly(day_rain, 3) + 
                       poly(hi_inst_wind, 3) + 
                       poly(hi_wind, 3) + 
                       poly(avg_pres, 3) + 
                       poly(total_sun, 3) + 
                       poly(avg_cloud, 3) + 
                       poly(dur_fog, 3) + 
                       poly(day_dust, 3), data = dataset2.2013, subset = train_num)
mean((injure_num - predict(lm.fit_injure))[-train_num]^2)
mean((injure_num - predict(lm.fit2_injure))[-train_num]^2)
mean((injure_num - predict(lm.fit3_injure))[-train_num]^2)
glm.fit_occur = glm(occur_num~ day_rain +hi_inst_wind + hi_wind + avg_pres + total_sun + avg_cloud + dur_fog + day_dust, data = dataset2.2013)
coef(glm.fit_occur)
glm.fit_death = glm(death_num~ avg_temp +  day_rain +hi_inst_wind + hi_wind + total_sun + dur_fog + day_dust, data = dataset2.2013)
coef(glm.fit_death)
glm.fit_injure = glm(injure_num~ day_rain +hi_inst_wind + hi_wind + avg_pres + total_sun + avg_cloud + dur_fog + day_dust, data = dataset2.2013)
coef(glm.fit_injure)
lm.fit_occur = lm(occur_num~ day_rain +hi_inst_wind + hi_wind + avg_pres + total_sun + avg_cloud + dur_fog + day_dust, data = dataset2.2013)
coef(lm.fit_occur)
lm.fit_death = lm(death_num~ avg_temp +  day_rain +hi_inst_wind + hi_wind + total_sun + dur_fog + day_dust, data = dataset2.2013)
coef(lm.fit_death)
lm.fit_injure = lm(injure_num~ day_rain +hi_inst_wind + hi_wind + avg_pres + total_sun + avg_cloud + dur_fog + day_dust, data = dataset2.2013)
coef(lm.fit_injure)

//LOOCV(오래걸림)

library(boot)
cv.err_occur = cv.glm(dataset2.2013, glm.fit_occur)
cv.err_occur$delta
cv.error_occur = rep(0,5)
for (i in 1 : 5){
  glm.fit_occur = glm(occur_num ~ poly(day_rain, i) + 
                        poly(hi_inst_wind, i) + 
                        poly(hi_wind, i) + 
                        poly(avg_pres, i) + 
                        poly(total_sun, i) + 
                        poly(avg_cloud, i) + 
                        poly(dur_fog, i) + 
                        poly(day_dust, i), data = dataset2.2013)
  cv.error_occur[i] = cv.glm(dataset2.2013 , glm.fit_occur)$delta[1]
}
cv.error_death = rep(0,5)
for (i in 1 : 5) {
  glm.fit_death = glm(death_num ~ poly(avg_temp, i) + poly(day_rain, i) + 
                        poly(hi_inst_wind, i) + 
                        poly(hi_wind, i) + 
                        poly(total_sun, i) + 
                        poly(dur_fog, i) + 
                        poly(day_dust, i), data = dataset2.2013)
  cv.error_death[i] = cv.glm(dataset2.2013, glm.fit_death)$delta[1]
  
}
cv.error_injure = rep(0,5)
for (i in 1 : 5){
  glm.fit_injure = glm(occur_num ~ poly(day_rain, i) + 
                         poly(hi_inst_wind, i) + 
                         poly(hi_wind, i) + 
                         poly(avg_pres, i) + 
                         poly(total_sun, i) + 
                         poly(avg_cloud, i) + 
                         poly(dur_fog, i) + 
                         poly(day_dust, i), data = dataset2.2013)
  cv.error_injure[i] = cv.glm(dataset2.2013 , glm.fit_injure)$delta[1]
}


//k-fold

library(boot)
set.seed(17)
cv.error.100_occur = rep(0,10)
for (i in 1:10) {
  glm.fit_occur = glm(occur_num ~ poly(day_rain, i) + 
                        poly(hi_inst_wind, i) + 
                        poly(hi_wind, i) + 
                        poly(avg_pres, i) + 
                        poly(total_sun, i) + 
                        poly(avg_cloud, i) + 
                        poly(dur_fog, i) + 
                        poly(day_dust, i) ,data = dataset2.2013)
  cv.error.100_occur[i] = cv.glm(dataset2.2013, glm.fit_occur, K = 100)$delta[1]
}
cv.error.100_occur
cv.error.100_death = rep(0,10)
for (i in 1:10) {
  glm.fit_death = glm(death_num ~ poly(avg_temp, i) + poly(day_rain, i) + 
                        poly(hi_inst_wind, i) + 
                        poly(hi_wind, i) + 
                        poly(total_sun, i) + 
                        poly(dur_fog, i) +
                        poly(day_dust, i), data = dataset2.2013)
  cv.error.100_death[i] = cv.glm(dataset2.2013, glm.fit_death, K = 100)$delta[1]
}
cv.error.100_death
cv.error.100_injure = rep(0,10)
for (i in 1: 10){
  glm.fit_injure = glm(injure_num ~ poly(day_rain, i) + 
                         poly(hi_inst_wind, i) + 
                         poly(hi_wind, i) + 
                         poly(avg_pres, i) + 
                         poly(total_sun, i) + 
                         poly(avg_cloud, i) + 
                         poly(dur_fog, i) + 
                         poly(day_dust, i), data = dataset2.2013)
  cv.error.100_injure[i] = cv.glm(dataset2.2013, glm.fit_injure, K = 100)$delta[1]
}
cv.error.100_injure


//Bootstrap

boot.fn_occur = function(dataset2.2013, index) {
  return(coef(lm(occur_num ~ day_rain + 
                   hi_inst_wind + 
                   hi_wind + 
                   avg_pres + 
                   total_sun + 
                   avg_cloud + 
                   dur_fog + 
                   day_dust, 
                 data = dataset2.2013 , subset = index)))
}

boot.fn_occur(dataset2.2013, train_num)
set.seed(1)
boot.fn_occur(dataset2.2013, sample(11688, 11688, replace = T))
boot.fn_occur(dataset2.2013, sample(11688, 11688, replace = T))
boot(dataset2.2013, boot.fn_occur, 1000)

summary(lm(occur_num ~ day_rain + 
             hi_inst_wind + 
             hi_wind + 
             avg_pres + 
             total_sun + 
             avg_cloud + 
             dur_fog + 
             day_dust, 
           data = dataset2.2013))$coef

boot.fn_occur = function(dataset2.2013, index) {
  coefficients(lm(occur_num~day_rain + 
                    hi_inst_wind + 
                    hi_wind + 
                    avg_pres + 
                    total_sun + 
                    avg_cloud + 
                    dur_fog + 
                    day_dust + I((day_rain + 
                                    hi_inst_wind + 
                                    hi_wind + 
                                    avg_pres + 
                                    total_sun + 
                                    avg_cloud + 
                                    dur_fog + 
                                    day_dust)^2), data = dataset2.2013, subset = index))
}
set.seed(1)
boot(dataset2.2013, boot.fn_occur, 1000)

boot.fn_death = function(dataset2.2013, index) {
  return(coef(lm(death_num~ avg_temp +  day_rain +hi_inst_wind + hi_wind + total_sun + dur_fog + day_dust, 
                 data = dataset2.2013 , subset = index)))
}

boot.fn_death(dataset2.2013, train_num)
set.seed(1)
boot.fn_death(dataset2.2013, sample(11688, 11688, replace = T))

boot(dataset2.2013, boot.fn_death, 1000)

summary(lm(death_num ~ avg_temp +  day_rain +hi_inst_wind + hi_wind + total_sun + dur_fog + day_dust, 
           data = dataset2.2013))$coef

boot.fn_death = function(dataset2.2013, index) {
  coefficients(lm(death_num~avg_temp +  day_rain +hi_inst_wind + hi_wind + total_sun + dur_fog + day_dust + I((avg_temp +  day_rain +hi_inst_wind + hi_wind + total_sun + dur_fog + day_dust)^2), data = dataset2.2013, subset = index))
}
set.seed(1)
boot(dataset2.2013, boot.fn_death, 1000)


boot.fn_injure = function(dataset2.2013, index) {
  return(coef(lm(injure_num ~ day_rain + 
                   hi_inst_wind + 
                   hi_wind + 
                   avg_pres + 
                   total_sun + 
                   avg_cloud + 
                   dur_fog + 
                   day_dust, 
                 data = dataset2.2013 , subset = index)))
}

boot.fn_injure(dataset2.2013, train_num)
set.seed(1)
boot.fn_injure(dataset2.2013, sample(11688, 11688, replace = T))
boot.fn_injure(dataset2.2013, sample(11688, 11688, replace = T))
boot(dataset2.2013, boot.fn_injure, 1000)

summary(lm(injure_num ~ day_rain + 
             hi_inst_wind + 
             hi_wind + 
             avg_pres + 
             total_sun + 
             avg_cloud + 
             dur_fog + 
             day_dust, 
           data = dataset2.2013))$coef

boot.fn_injure = function(dataset2.2013, index) {
  coefficients(lm(injure_num~day_rain + 
                    hi_inst_wind + 
                    hi_wind + 
                    avg_pres + 
                    total_sun + 
                    avg_cloud + 
                    dur_fog + 
                    day_dust + I((day_rain + 
                                    hi_inst_wind + 
                                    hi_wind + 
                                    avg_pres + 
                                    total_sun + 
                                    avg_cloud + 
                                    dur_fog + 
                                    day_dust)^2), data = dataset2.2013, subset = index))
}
set.seed(1)
boot(dataset2.2013, boot.fn_injure, 1000)

attach(dataset2.2017)
lm.fit_death_test=lm(death_num~avg_temp +  day_rain +hi_inst_wind + hi_wind + total_sun + dur_fog + day_dust, data=dataset2.2017)
summary(lm.fit_death_test)
lm.fit_occur_test=lm(occur_num~day_rain + 
                       hi_inst_wind + 
                       hi_wind + 
                       avg_pres + 
                       total_sun + 
                       avg_cloud + 
                       dur_fog + 
                       day_dust, data=dataset2.2017)
summary(lm.fit_occur_test)
lm.fit_injure_test=lm(injure_num~day_rain + 
                        hi_inst_wind + 
                        hi_wind + 
                        avg_pres + 
                        total_sun + 
                        avg_cloud + 
                        dur_fog + 
                        day_dust, data=dataset2.2017)
summary(lm.fit_injure_test)


//Ridge Regression_occur

x.occur_num = model.matrix(occur_num~., dataset2.2013)[,-1]
y.occur_num = dataset2.2013$occur_num
library(glmnet)
grid = 10^seq(10, -2, length = 100)

ridge.mod_occur = glmnet(x.occur_num,y.occur_num,alpha = 0, lambda = grid)
set.seed(1)
train_occur = sample(1:nrow(x.occur_num), nrow(x.occur_num)/2)
test_occur = (-train_occur)
y.test_occur = y.occur_num[test_occur]
ridge.mod_occur = glmnet(x.occur_num[train_occur,], y.occur_num[train_occur], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred_occur = predict(ridge.mod_occur, s = 4, newx = x.occur_num[test_occur,])
mean((ridge.pred_occur-y.test_occur)^2)
mean((mean(y.occur_num[train_occur])-y.test_occur)^2)
ridge.pred_occur = predict(ridge.mod_occur, s=0, newx = x.occur_num[test_occur,], exact = F)
mean((ridge.pred_occur-y.test_occur)^2)
predict(ridge.mod_occur, s=0, exact = F, type ="coefficients")[1:20,]
set.seed(1)
cv.out_occur = cv.glmnet(x.occur_num[train_occur,], y.occur_num[train_occur], alpha = 0)
plot(cv.out_occur)
cv.out_occur
bestlam_occur = cv.out_occur$lambda.min
bestlam_occur
ridge.pred_occur = predict(ridge.mod_occur, s = bestlam_occur, newx = x.occur_num[test_occur,])
mean((ridge.pred_occur - y.test_occur)^2)
out_occur = glmnet(x.occur_num, y.occur_num, alpha = 0)
predict(out_occur, type = "coefficients", s = bestlam_occur)[1:20,]


//Ridge Regression_ death

x.death_num = model.matrix(death_num~., dataset2.2013)[,-1]
y.death_num = dataset2.2013$death_num
ridge.mod_death = glmnet(x.death_num,y.death_num,alpha = 0, lambda = grid)
train_death = sample(1:nrow(x.death_num), nrow(x.death_num)/2)
test_death = (-train_death)
y.test_death= y.death_num[test_death]
ridge.mod_death = glmnet(x.death_num[train_death,], y.death_num[train_death], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred_death= predict(ridge.mod_death, s = 4, newx = x.death_num[test_death,])
mean((ridge.pred_death-y.test_death)^2)
mean((mean(y.death_num[train_death])-y.test_death)^2)
cv.out_death= cv.glmnet(x.death_num[train_death,], y.death_num[train_death], alpha = 0)
plot(cv.out_death)
bestlam_death = cv.out_death$lambda.min
bestlam_death
ridge.pred_death= predict(ridge.mod_death, s = bestlam_death, newx = x.death_num[test_death,])
mean((ridge.pred_death - y.test_death)^2)
out_death = glmnet(x.death_num, y.death_num, alpha = 0)
predict(out_death, type = "coefficients", s = bestlam_death)[1:20,]

//Ridge regression_ injure

x.injure_num = model.matrix(injure_num~., dataset2.2013)[,-1]
y.injure_num = dataset2.2013$injure_num
ridge.mod_injure = glmnet(x.injure_num,y.injure_num,alpha = 0, lambda = grid)
train_injure = sample(1:nrow(x.injure_num), nrow(x.injure_num)/2)
test_injure = (-train_injure)
y.test_injure= y.injure_num[test_injure]
ridge.mod_injure = glmnet(x.injure_num[train_injure,], y.injure_num[train_injure], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred_injure= predict(ridge.mod_injure, s = 4, newx = x.injure_num[test_injure,])
mean((ridge.pred_injure-y.test_injure)^2)
mean((mean(y.injure_num[train_injure])-y.test_injure)^2)
cv.out_injure= cv.glmnet(x.injure_num[train_injure,], y.injure_num[train_injure], alpha = 0)
plot(cv.out_injure)
bestlam_injure = cv.out_injure$lambda.min
bestlam_injure
ridge.pred_injure= predict(ridge.mod_injure, s = bestlam_injure, newx = x.injure_num[test_injure,])
mean((ridge.pred_injure - y.test_injure)^2)
out_injure = glmnet(x.injure_num, y.injure_num, alpha = 0)
predict(out_injure, type = "coefficients", s = bestlam_injure)[1:20,]

//The Lasso_occur

library(glmnet)
lasso.mod_occur = glmnet(x.occur_num[train_occur,], y.occur_num[train_occur], alpha = 1, lambda = grid)
plot(lasso.mod_occur)
set.seed(1)
cv.out_occur = cv.glmnet(x.occur_num[train_occur,],y.occur_num[train_occur], alpha = 1)
plot(cv.out_occur)
bestlam_occur = cv.out_occur$lambda.min
lasso.pred_occur = predict(lasso.mod_occur, s = bestlam_occur, newx = x.occur_num[test_occur,])
mean((lasso.pred_occur-y.test_occur)^2)
out.occur = glmnet(x.occur_num, y.occur_num, alpha = 1, lambda = grid)
lasso.coef_occur = predict(out_occur, type = "coefficients", s = bestlam_occur)[1:20,]
lasso.coef_occur

//The Lasso_death


lasso.mod_death = glmnet(x.death_num[train_death,], y.death_num[train_death], alpha = 1, lambda = grid)
plot(lasso.mod_death)
set.seed(1)
cv.out_death = cv.glmnet(x.death_num[train_death,],y.death_num[train_death], alpha = 1)
plot(cv.out_death)
bestlam_death = cv.out_death$lambda.min
lasso.pred_death = predict(lasso.mod_death, s = bestlam_death, newx = x.death_num[test_death,])
mean((lasso.pred_death-y.test_death)^2)
out.death = glmnet(x.death_num, y.death_num, alpha = 1, lambda = grid)
lasso.coef_death = predict(out_death, type = "coefficients", s = bestlam_death)[1:20,]
lasso.coef_death

//The Lasso_injure

lasso.mod_injure = glmnet(x.injure_num[train_injure,], y.injure_num[train_injure], alpha = 1, lambda = grid)
plot(lasso.mod_injure)
set.seed(1)
cv.out_injure = cv.glmnet(x.injure_num[train_injure,],y.injure_num[train_injure], alpha = 1)
plot(cv.out_injure)
bestlam_injure = cv.out_injure$lambda.min
lasso.pred_injure = predict(lasso.mod_injure, s = bestlam_injure, newx = x.injure_num[test_injure,])
mean((lasso.pred_injure-y.test_injure)^2)
out.injure = glmnet(x.injure_num, y.injure_num, alpha = 1, lambda = grid)
lasso.coef_injure = predict(out_injure, type = "coefficients", s = bestlam_injure)[1:20,]
lasso.coef_injure


//svm

attach(dataset2.2013)
train_num = sample(11688, 5844)
svm_death = svm(is_death.2013~avg_temp+
                  day_rain+
                  hi_inst_wind +
                  hi_wind + 
                  avg_pres +
                  total_sun +
                  day_snow +
                  avg_cloud + 
                  dur_fog + 
                  day_dust , data = dataset2.2013, subset = train_num, type = "C-classification")
predict(svm_death, dataset2.2013[-train_num,])
table_death <- table(is_death.2013[-train_num], predict(svm_death, dataset2.2013[-train_num,]))
table_death


//Decision Trees

library(tree)
library(ISLR)
attach(dataset2.2013)
median(avg_temp)
median(day_rain)
median(hi_inst_wind)
median(hi_wind)
median(avg_pres)
median(total_sun)
median(day_snow)
median(avg_cloud)
median(dur_fog)
median(day_dust)

//Random Forest

library(randomForest)
set.seed(1)
occur_test = dataset2.2013[-train_num, "occur_num"]
death_test = dataset2.2013[-train_num, "death_num"]
injure_test = dataset2.2013[-train_num, "injure_num"]
bag.occur = randomForest(occur_num~., data = dataset2.2013, subset = train_num, mtry = 10, importance = TRUE)
bag.death = randomForest(death_num~., data = dataset2.2013, subset = train_num, mtry = 10, importance = TRUE)
bag.injure = randomForest(injure_num~., data = dataset2.2013, subset = train_num, mtry = 10, importance = TRUE)
yhat_bag.occur = predict(bag.occur, newdata = dataset2.2013[-train_num,])
mean((yhat_bag.occur-occur_test)^2)
yhat_bag.death = predict(bag.death, newdata = dataset2.2013[-train_num,])
mean((yhat_bag.death-death_test)^2)
yhat_bag.injure = predict(bag.injure, newdata = dataset2.2013[-train_num,])
mean((yhat_bag.injure - injure_test)^2)

bag.occur2 = randomForest(occur_num~day_rain +hi_inst_wind + hi_wind + avg_pres + total_sun + avg_cloud + dur_fog + day_dust., data = dataset2.2013, subset = train_num, mtry = 10, ntree = 30)
yhat_bag.occur2 = predict(bag.occur2, newdata = dataset2.2013[-train_num,])
mean((yhat_bag.occur2-occur_test)^2)
bag.death2 = randomForest(death_num~., data = dataset2.2013, subset = train_num, mtry = 10, ntree = 30)
yhat_bag.death2 = predict(bag.death2, newdata = dataset2.2013[-train_num,])
mean((yhat_bag.death2-death_test)^2)
bag.injure2 = randomForest(injure_num~., data = dataset2.2013, subset = train_num, mtry = 10, ntree = 30)
yhat_bag.injure2 = predict(bag.injure2, newdata = dataset2.2013[-train_num,])
mean((yhat_bag.injure2-injure_test)^2)


set.seed(1)
rf.occur = randomForest(occur_num~day_rain +hi_inst_wind + hi_wind + avg_pres + total_sun + avg_cloud + dur_fog + day_dust, data = dataset2.2013, subset = train_num, mtry = 5, importance = TRUE)
yhat.rf.occur = predict(rf.occur, newdata = dataset2.2013[-train_num,])
mean((yhat.rf.occur -occur_test)^2)
rf.death = randomForest(death_num~avg_temp +  day_rain +hi_inst_wind + hi_wind + total_sun + dur_fog + day_dust, data = dataset2.2013, subset = train_num, mtry = 5, importance = TRUE)
yhat.rf.death = predict(rf.death, newdata = dataset2.2013[-train_num,])
mean((yhat.rf.death - death_test)^2)
rf.injure = randomForest(injure_num~day_rain +hi_inst_wind + hi_wind + avg_pres + total_sun + avg_cloud + dur_fog + day_dust, data = dataset2.2013, subset = train_num, mtry = 5, importance = TRUE)
yhat.rf.injure = predict(rf.injure, newdata = dataset2.2013[-train_num,])
mean((yhat.rf.injure - injure_test)^2)

importance(rf.occur)
importance(rf.death)
importance(rf.injure)

varImpPlot(rf.occur)
varImpPlot(rf.death)
varImpPlot(rf.injure)