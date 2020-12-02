
#Loading Data
data_pain = read.csv("https://tinyurl.com/ha-dataset1")

#Loading packages
library(tidyverse)
library(dplyr)
library(psych)
library(lm.beta)
library(lsr)
library(car)
library(gridExtra)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(lmtest)
library(MuMIn)
library(lme4)
library(optimx)
library(r2glmm)

view(data_pain)
str(data_pain)
describe(data_pain)

data_pain <- data_pain %>% 
  mutate(sex_2=recode(sex,"female"=1.00,"male"=2.00))

#check the variables included in model 2 (age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures as predictors, and pain as an outcome) 
#for coding errors, and the model itself for influential outliers (for example using Cook's distance).

#testmodel for diagnostics
modtest = lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva, data=data_pain)
plot(x = modtest, which = 4)
plot(x = modtest, which = 5)

data_pain %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram()

data_pain %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram( bins = 50)

data_pain %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram( bins = 50)

data_pain %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram( bins = 50)

data_pain %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram( bins = 50)

data_pain %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram( bins = 50)

data_pain %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram( bins = 50)

##Unusual Data:
# Max. Age = 444
# STAI Min = 3.90 (scale 20 to 80)
# Min. IQ = 49
# Min. income = -3732

#correcting two values
data_pain_final <- data_pain %>%
  mutate(age = replace(age, age=="444", 44)) %>% 
  mutate(STAI_trait = replace(STAI_trait, STAI_trait=="3.9", 39)) %>% 
           slice(-c(109))

view(data_pain_final)
describe(data_pain_final)
str(data_pain_final)

#checking correction through plot
old_plot_age <- data_pain %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram()

new_plot_age <- data_pain_final %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram()

grid.arrange(old_plot_age, new_plot_age, ncol=2)


old_plot_STAI <- data_pain %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram( bins = 50)

new_plot_STAI <- data_pain_final %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram( bins = 50)

grid.arrange(old_plot_STAI, new_plot_STAI, ncol=2)

#testmodel 2 for diagnostics
modtest2 = lm(pain~age+sex+STAI_trait+pain_cat+mindfulness+cortisol_serum+cortisol_saliva, data=data_pain_final)
plot(x = modtest2, which = 4)
plot(x = modtest2, which = 5)

#Scatterplots Models 1 & 2
plot_Age = data_pain_final %>% 	
  ggplot() +	
  aes(x = age, y = pain) +	
  geom_point()+	
  geom_smooth(method = "lm")

plot_Sex = data_pain_final %>% 	
  ggplot() +	
  aes(x = sex, y = pain) +	
  geom_point()

plot_painCat<-data_pain_final %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain) +	
  geom_point()

plot_cortisol<-data_pain_final %>% 	
  ggplot() +	
  aes(x = cortisol_saliva, y = pain) +	
  geom_point()

plot_mindfulness<-data_pain_final %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain) +	
  geom_point()+
  geom_smooth(method = "lm")

plot_Age
plot_Sex
plot_painCat
plot_cortisol
plot_mindfulness

##Assignment 1

# Conduct a hierarchical regression
# Model 1: containing age and sex as predictors of pain
coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}
mod1 = lm(pain ~ age + sex, data = data_pain_final)

mod1
summary(mod1)
confint(mod1)
lm.beta(mod1)
standardCoefs(mod1)
coef_table(mod1)
tab_model(mod1, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

# If gender is male --> -0,04 for pain
# If one year older --> -0,08 for pain

#Cooks Distance
plot(x = mod1, which = 4)
plot(x = mod1, which = 5)

data_pain_final %>% slice(c(128, 141, 100))

#Subsetting case 128
mod1_subset = lm(pain ~ age + sex, data = data_pain_final, subset = -128)
mod1_subset

summary(mod1_subset)

#Normality of Residuals
#qqplot
plot(x = mod1, which = 2)
#histogram
residuals_mod1 = enframe(residuals(mod1))	
residuals_mod1 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()
# skew and kurtosis	
describe(residuals(mod1))

#Linearity of relationship 
yhat1 <- fitted.values( object = mod1 )
plot( x = yhat1, y = data_pain_final$pain, xlab = "Fitted Values", ylab = "Observed Values")
plot(x = mod1, which = 1)
residualPlots( model = mod1 )

#homogeneity of variance
plot(x = mod1, which = 3)
ncvTest(mod1)

#collinearity
vif(mod1)
data_pain_final %>%	
  select(age, sex, pain) %>% 	
  drop_na() %>%	
  cor()

# Model 2: age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures
mod2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_pain_final)
mod2
summary(mod2)
confint(mod2)
lm.beta(mod2)
standardCoefs(mod2)

hatvalues( model = mod2 )

#Cooks Distance
plot(x = mod2, which = 4)
plot(x = mod2, which = 5)

# Subsetting case 114
mod2_subset <-lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_pain_final, subset = -114)
summary(mod2_subset)

#Normality of Residuals
#qqplot
plot(x = mod2, which = 2)
#histogram
residuals_mod2 = enframe(residuals(mod2))	
residuals_mod2 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()
# skew and kurtosis	
describe(residuals(mod2))

#Linearity of relationship 
yhat2 <- fitted.values( object = mod2 )
plot( x = yhat2, y = data_pain_final$pain, xlab = "Fitted Values", ylab = "Observed Values")
plot(x = mod2, which = 1)
residualPlots( model = mod2 )

#homogeneity of variance
plot(x = mod2, which = 3)
ncvTest(mod2)

#collinearity --> exclude onne cortisol measurement
vif(mod2)
data_pain_final %>%	
  select(age, sex_num, pain, STAI_trait, pain_cat, mindfulness, cortisol_saliva, cortisol_serum) %>% 	
  drop_na() %>%	
  cor()
data_pain_final %>% select(age, sex_num, pain, STAI_trait, pain_cat, mindfulness, cortisol_saliva, cortisol_serum) %>%
  pairs.panels(col = "red", lm = T)

# building new model because of collinearity without cortisol_saliva. Also cortisol_serum is regarded as more reliable in research
mod2_final = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_pain_final)
mod2_final
summary(mod2_final)

confint(mod2_final)
lm.beta(mod2_final)
standardCoefs(mod2_final)
coef_table(mod2_final)
tab_model(mod2_final, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

# Running model diagnostics again
#Cooks Distance
plot(x = mod2_final, which = 4)
plot(x = mod2_final, which = 5)

# Subsetting case 114
mod2_final_subset <-lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_pain_final, subset = -113)
summary(mod2_final_subset)

#Normality of Residuals
#qqplot
plot(x = mod2_final, which = 2)
#histogram
residuals_mod2_final = enframe(residuals(mod2_final))	
residuals_mod2_final %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()
# skew and kurtosis	
describe(residuals(mod2_final))

#Linearity of relationship 
yhat_2_final <- fitted.values( object = mod2_final )
plot( x = yhat_2_final, y = data_pain_final$pain, xlab = "Fitted Values", ylab = "Observed Values")
plot(x = mod2_final, which = 1)
residualPlots( model = mod2_final )

#homogeneity of variance
plot(x = mod2_final, which = 3)
ncvTest(mod2_final)

#collinearity
vif(mod2_final)
data_pain_final %>%	
  select(age, sex_num, pain, STAI_trait, pain_cat, mindfulness, cortisol_serum) %>% 	
  drop_na() %>%	
  cor()
data_pain_final %>% select(age, sex_num, pain, STAI_trait, pain_cat, mindfulness, cortisol_serum) %>%
  pairs.panels(col = "red", lm = T)


# Notice that the predictors used in model 1 are a subset of the predictors used in model 2.
# Model comparison to assess whether substantial new information was gained about pain in model 2 compared to model 1.
AIC(mod1)
AIC(mod2_final)

summary(mod1)$adj.r.squared	
summary(mod2_final)$adj.r.squared

anova(mod1, mod2_final)
# significant: mod2 is better than mod1
# Table for report
tab_model(mod2_final, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

#Assignment 2
# -	Run a backward regression  with initial model: age, sex, STAI, pain_cat, mindfulness, serum, weight, IQ, household income (do same corrections as in assignment 1)
view(data_pain_final)

data_pain_final %>% 	
  ggplot() +	
  aes(x = IQ) +	
  geom_histogram( bins = 50)

data_pain_final %>% 	
  ggplot() +	
  aes(x = household_income) +	
  geom_histogram()

#Backward-Model --> Initial model and diagnostics

back_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight +IQ + household_income, data = data_pain_final)
#Model Diagnostics
# Cooks Distance
plot(x = back_model, which = 4)
plot(x = back_model, which = 5)
#Normality
plot(x = back_model, which = 2)
#Linearity
yhat_back <- fitted.values( object = back_model )
plot( x = yhat_back, y = data_pain_final$pain, xlab = "Fitted Values", ylab = "Observed Values")
plot(x = back_model, which = 1)
residualPlots( model = back_model )
#homogeneity of variance
plot(x = back_model, which = 3)
ncvTest(back_model)
#collinearity 
vif(back_model)

##Backwards Regression
step( object = back_model, direction = "backward")
##Final Model
backwards_model <- lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = data_pain_final)

backwards_model
summary(backwards_model)
confint(backwards_model)
lm.beta(backwards_model)
standardCoefs(backwards_model)
coef_table(backwards_model)

tab_model(backwards_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

# -	Do the model diagnostics as there are new variables
# Running model diagnostics 
# Cooks Distance
plot(x = backwards_model, which = 4)
plot(x = backwards_model, which = 5)

#Normality of Residuals
#qqplot
plot(x = backwards_model, which = 2)
#histogram
residuals_backwards_model = enframe(residuals(backwards_model))	
residuals_backwards_model %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()
# skew and kurtosis	
describe(residuals(backwards_model))

#Linearity of relationship 
yhat_backward <- fitted.values( object = backwards_model )
plot( x = yhat_backward, y = data_pain_final$pain, xlab = "Fitted Values", ylab = "Observed Values")
plot(x = backwards_model, which = 1)
residualPlots( model = backwards_model )

#homogeneity of variance
plot(x = backwards_model, which = 3)
ncvTest(backwards_model)

#collinearity 
vif(backwards_model)
data_pain_final %>%	
  select(age, sex_num, pain_cat, mindfulness, cortisol_serum, household_income) %>% 	
  drop_na() %>%	
  cor()
data_pain_final %>% select(age, sex_num, pain_cat, mindfulness, cortisol_serum, household_income) %>%
  pairs.panels(col = "red", lm = T)

# -	Save the new model as "backward model" object in R
# -	Save model from Assignment one as "theory-based-model"
theory_based_model <- mod2_final

# - Compare initial model and backwards model on AIC
AIC(back_model)
AIC(backwards_model)
anova(back_model, backwards_model)

# -	Compare both models based on AIC and anova(if appropriate)
AIC(backwards_model)
AIC(theory_based_model)

AIC(theory_based_model)-AIC(backwards_model)

summary(backwards_model)$adj.r.squared	
summary(theory_based_model)$adj.r.squared
anova(theory_based_model, backwards_model)

#Loading Data
data_pain2 = read.csv("https://tinyurl.com/ha-dataset2")
view(data_pain2)
describe(data_pain2)

data_pain2 %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram()

data_pain2 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram( bins = 50)

data_pain2 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram( bins = 50)

data_pain2 %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram( bins = 50)

data_pain2 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram( bins = 50)

data_pain2 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram( bins = 50)

data_pain2 %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram()

#Errors in data
#Max of Mindfulness = 7.17 (scales from 1 to 6)
#Excluding Case 8?
data_pain2_final <- data_pain2 %>% 
  slice(-c(8))
view(data_pain2_final)

# -	Test both models on data file 2 --> compare predicted and actual pain ratings.

# On data file 2, make predictions on pain using the regression models or equations of the
# backward model and the theory-based model which were "trained" on data file 1.

predict_test_theory = predict(theory_based_model, data_pain2_final)

predict_test_backward = predict(backwards_model, data_pain2_final)

# - Which model was able to predict the actual pain ratings better?
# - F.e. calculate sum of squared differences between predicted and actual pain,
RSS_theory2 = sum((data_pain2_final$pain - predict_test_theory)^2)
RSS_theory2

RSS_backwards = sum((data_pain2_final$pain - predict_test_backward)^2)
RSS_backwards

RSS_backwards - RSS_theory2
# - or the sum of absolute differences for each model
RAD_backwards = sum(abs(data_pain2_final$pain - predict_test_backward))
RAD_backwards

RAD_theory = sum(abs(data_pain2_final$pain - predict_test_theory))
RAD_theory

##Assignment 3

#Loading data
data_pain3<-read.csv("https://tinyurl.com/ha-dataset3")
data_pain4<-read.csv("https://tinyurl.com/ha-dataset4")
view(data_pain3)
describe(data_pain3)

#Errors in data
#Main income is minus
#Excluding Case 77
data_pain3_final <- data_pain3 %>% 
  slice(-c(77)) %>% 
  mutate(sex = replace(sex, sex=="femlae", "female"))

view(data_pain3_final)

# Building factor for hospital
data_pain3_final %>% mutate(hospital = factor(hospital))

describe(data_pain3_final)
str(data_pain3_final)
view(data_pain3_final)
summary(data_pain3_final)

# build a linear mixed model on data file 3, accounting for the clustering of the
# data at different hospital sites --> fit a random intercept model including the random 
# intercept of hospital-ID, and the fixed effect predictors you used in assignment 1.

Random_int = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_pain3_final)
Random_int

# note the model coefficients and the confidence intervals of the coefficients for all 
# fixed effect predictors, and compare them to the ones obtained in assignment 1.
summary(Random_int) ##Variance for random intercept here!
confint(Random_int)
tab_model(Random_int, theory_based_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

# compute the variance explained by the fixed effect predictors using marginal R^2
# and the variance explained by the fixed and random effect terms combined using conditional R2

# marginal R2
# If the 95% CI does not contain 0, it means that the fixed effect term(s) explain a significant portion of the variation of the
# outcome compared to the mean (the null model).
r2beta(Random_int, method = "nsj", data = data_pain3_final)

# marginal and conditional R2
# conditional R --> variance explained by the fixed and random effect terms combined 

r.squaredGLMM(Random_int)

# variance for residuals
sum(residuals(Random_int)^2)

# use the regression equation obtained on data file 3 to predict pain in data file 4
describe(data_pain4)
view(data_pain4)
data_pain4_final <- data_pain4 %>% 
  slice(-c(5, 80, 87))
view(data_pain4_final) ##slicing negative income and mindfulness 6.05
describe(data_pain4_final)


pain_pred_random4 <- predict(Random_int, data_pain4_final, allow.new.levels = TRUE)

# compute the variance explained by the model on data file 4. You can do this by using
# the formula we learned in class: 1-(RSS/TSS)
# The residual sum of squares measures the amount of error remaining between the
# regression function and the data set.

RSS = sum((data_pain4_final$pain - pain_pred_random4)^2)
RSS

# TSS defined as the sum over all squared differences between the observations
# and their overall mean 

mod_mean <- lm(pain ~ 1, data = data_pain4_final)
mod_mean
TSS=sum((data_pain4_final$pain - predict(mod_mean))^2)
TSS

R<-1-(RSS/TSS)
R

# Compare this R2 to the marginal and conditional R2 values computed for the model on data file 3
# R2m       R2c
# 0.3659089 0.4852725
# 1-(RSS_random/TSS_random)
# 0.2290998

# Build a new linear mixed effects model on dataset 3 predicting pain. However, instead
# of including all predictors, you should only include the most influential predictor from the
# previous model. Allow for both random intercept and random slope. 
Random_int_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_pain3_final)
Random_int_slope
Random_int_slope_opt = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), control = lmerControl(optimizer = "Nelder_Mead"), data = data_pain3_final)
Random_int_slope_opt

# Now visualize the fitted regression lines for each hospital separately.

predict_data_pain3 = data_pain3_final %>% 
  mutate(pred_int = predict(Random_int_cortisol),
        pred_slope = predict(Random_int_slope_opt))


neworder <- c("hospital_1", "hospital_2", "hospital_3", "hospital_4", "hospital_5", "hospital_6", "hospital_7", "hospital_8", "hospital_9", "hospital_10")
library(plyr)
predict_data_pain3_ordered <- arrange(transform(predict_data_pain3,
                           hospital=factor(hospital,levels=neworder)),hospital)

predict_data_pain3_ordered %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 1) +		
  geom_line(color='red', aes(y=pred_slope, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)

## discuss whether the random intercept or the random slope model 
## is a better fit for the data, and why.

## comparing RSS
sum(residuals(Random_int)^2)

sum(residuals(Random_int_slope_opt)^2)

## plot for random intercept
Random_int_cortisol = lmer(pain ~ cortisol_serum + (1|hospital), data = data_pain3_final)
Random_int_cortisol

predict_data_pain3_ordered %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 1) +		
  geom_line(color='red', aes(y=pred_int, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)

## cAIC 
install.packages(cAIC4)
library(cAIC4)
cAIC(Random_int)$caic
cAIC(Random_int_slope_opt)$caic

## anova
anova(Random_int_slope_opt, Random_int)

## marginal R^2
r2beta(Random_int, method = "nsj", data = data_pain3_final)
r2beta(Random_int_slope_opt, method = "nsj", data = data_pain3_final)

