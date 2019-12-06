
######################### Zoltan Kekecs - assignment 2 #########################


### author: Lisa Holzer
### date:   2nd December 2019



##load packages
library(gsheet)
library(psych)
library(lm.beta)
library(rgl)
library(gridExtra)
library(tidyverse)
library(olsrr)
library(car)
library(lmtest)
library(sandwich)
library(boot)

##set working directory
setwd("C:/Users/holze/Desktop/Lisa/Uni/Schweden/PSYP13/home assignments")
source("GraphPlot.R")



#################### data diagnostics ####################



##load data and look at it
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ")
data_sample_1 %>% summary
describe (data_sample_1)
str(data_sample_1)

##clean data and look at it again
#adjust 3.5 -> 35
data_sample_1$STAI_trait[data_sample_1$STAI_trait==3.5]=35
data_sample_1 %>% summary
#delete case ID_49 with negative income value because it is included in the calculation 
data_sample_1_cleaned <- data_sample_1[-49,]
data_sample_1_cleaned %>% summary
view(data_sample_1_cleaned)
describe(data_sample_1_cleaned)

##check data distribution with histograms
windows()
hist(data_sample_1_cleaned$pain, main="pain")
windows()
hist(data_sample_1_cleaned$age, main="age")
windows()
hist(data_sample_1_cleaned$STAI_trait, main="anxiety")
windows()
hist(data_sample_1_cleaned$pain_cat, main="pain catastophizing")
windows()
hist(data_sample_1_cleaned$cortisol_serum, main="cortisol serum")
windows()
hist(data_sample_1_cleaned$mindfulness, main="mindfulness")
windows()
hist(data_sample_1_cleaned$weight, main="weight")
windows()
hist(data_sample_1_cleaned$IQ, main="IQ")
windows()
hist(data_sample_1_cleaned$household_income, main="household income")

## check for outliers in the data
windows()
data_sample_1_cleaned %>%ggplot() +aes(x = age, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_1_cleaned %>%ggplot() +aes(x = sex, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_1_cleaned %>%ggplot() +aes(x = STAI_trait, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_1_cleaned %>%ggplot() +aes(x = pain_cat, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_1_cleaned %>%ggplot() +aes(x = cortisol_serum, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_1_cleaned %>%ggplot() +aes(x = mindfulness, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_1_cleaned %>%ggplot() +aes(x = weight, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_1_cleaned %>%ggplot() +aes(x = IQ, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_1_cleaned %>%ggplot() +aes(x = household_income, y = pain) +geom_point() +geom_smooth(method = "lm")

##create researcher's model/backwards regression model 
mod3<-lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = data_sample_1_cleaned)
mod3
summary(mod3)



#################### model diagnostics ####################



########## check for outliers, Cook's distance ##########


windows()
mod3 %>% plot(which=5)
windows()
ols_plot_cooksd_bar(mod3)


########## normality ##########


##QQ-plots
windows()
plot(x = mod3, which = 2)


##shapiro wilk test (not significant)
shapiro.test(residuals(mod3))


##histogram
windows()
residuals_mod3 = enframe(residuals(mod3))
residuals_mod3 %>% ggplot() + aes (x=value) + geom_histogram()


##skew and kurtosis 
#can be between -1;1; the closer to 0 the better
describe(residuals(mod3)) #skew: -0.15, kurtosis: -0.18


########## linearity ##########


#should be non-significant 
windows()
mod3 %>% residualPlots() 


########## homoscedasticity/homogenity of variance ##########


windows()
mod3 %>% plot(which=3)

##NCV-test
mod3 %>% ncvTest() # p=
#non-significant

##Breush-Pagan test
mod3 %>% bptest() # p=
#non-significant


########## no multicollinearity ##########


#values of 3 or higher are problematic
mod3 %>% vif()



#################### backward regression ####################



step(object = mod3, direction = "backward")
#start: AIC=62.88, -STAI_trait: AIC=60.88, -IQ: AIC=60.30, -household_income: AIC=59.61
#--> best model: mod4<-lm(pain ~ age + sex + pain_cat + cortisol_serum + mindfulness + weight, data = data_sample_1_cleaned)

##create backward model
backward_model <- lm(pain ~ age + sex + pain_cat + cortisol_serum + mindfulness + weight, data = data_sample_1_cleaned)

##load custom functions
coef_table = function(model){
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")
  mod_sum_table["(Intercept)","Std.Beta"] = "0"
  return(mod_sum_table)}

##create coefficient table for newly generated model
coef_table(backward_model)
#                 b     95%CI lb 95%CI ub Std.Beta p-value
#(Intercept)     4.25     0.93     7.58        0    .012
#age            -0.05    -0.09    -0.01    -0.17    .016
#sexmale         0.49     0.10     0.87     0.16    .014
#pain_cat        0.08     0.04     0.13     0.28   <.001
#cortisol_serum  0.40     0.20     0.61     0.27   <.001
#mindfulness    -0.30    -0.52    -0.08     -0.2    .007
#weight         -0.02    -0.04     0.00    -0.11    .074


##create coefficient table for initial model
coef_table(mod3)
#coef_table(mod3)
#                   b 95%CI lb 95%CI ub Std.Beta p-value
#(Intercept)       5.35     1.68     9.03        0    .005
#age              -0.05    -0.10    -0.01    -0.17    .019
#sexmale           0.45     0.05     0.85     0.15    .026
#STAI_trait        0.00    -0.05     0.05        0     .95
#pain_cat          0.08     0.04     0.13     0.28    .001
#cortisol_serum    0.42     0.19     0.65     0.28   <.001
#mindfulness      -0.32    -0.54    -0.09    -0.21    .005
#weight           -0.02    -0.04     0.00    -0.11    .082
#IQ               -0.01    -0.02     0.01    -0.07    .251
#household_income  0.00     0.00     0.00    -0.07    .233


##compare backward model to initial model (mod3)
AIC(backward_model) #512.83
AIC(mod3) #516.10

anova(backward_model, mod3)
#Model 1: pain ~ age + sex + pain_cat + cortisol_serum + mindfulness + weight
#Model 2: pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income
#Res.  Df    RSS Df Sum of Sq      F Pr(>F)
#1    152 211.82                           
#2    149 208.22  3    3.5999 0.8587 0.4641



#################### model comparison ####################



#model to compare backward model 
theory_based_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_1)


##Akaike information criterion
AIC(theory_based_model) #518.66
AIC(backward_model) #512.83

##look at models and adjusted R^2
summary(theory_based_model)
summary(theory_based_model)$adj.r.squared #adjusted R^2=0.41
summary(backward_model)
summary(backward_model)$adj.r.squared #adjusted R^2=0.42



#################### test models on original data ####################



##predict pain with both models (theory based and backward model)
predicted_pain_1_original = predict(theory_based_model, data_sample_1_cleaned)
predicted_pain_2_original = predict(backward_model, data_sample_1_cleaned)


##compare pain prediction with actual pain ratings
sum((data_sample_1_cleaned$pain - predicted_pain_1_original)^2) #216.27
sum((data_sample_1_cleaned$pain - predicted_pain_2_original)^2) #211.82
#->backward model predicts pain better in original data



#################### test models on new data ####################



##load new data and look at it
data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")
data_sample_2 %>% summary
describe(data_sample_2)
str(data_sample_2)


##predict pain with both models (theory based and backward model)
predicted_pain_1_new = predict(theory_based_model, data_sample_2)
predicted_pain_2_new = predict(backward_model, data_sample_2)


##compare pain prediction with actual pain ratings
sum((data_sample_2$pain - predicted_pain_1_new)^2) #227.63
sum((data_sample_2$pain - predicted_pain_2_new)^2) #233.19
#->theory based model predicts pain better in new data
