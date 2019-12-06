
######################### Zoltan Kekecs - assignment 3 #########################


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
library(lme4)
library(influence.ME)
library(lattice)
library(lmerTest)
library(stats)
library(MuMIn)
library(cAIC4)
library(r2glmm)


##set working directory
setwd("C:/Users/holze/Desktop/Lisa/Uni/Schweden/PSYP13/home assignments")
source("GraphPlot.R")



#################### data diagnostics ####################



##load data and look at it
data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv ")
data_sample_3 %>% summary
view(data_sample_3)
describe (data_sample_3)
str(data_sample_3)


##clean data and look at it again
#delete case with ID_195 because the value for mindfulness cannot be correct (6.05, max. possible 6.00)
data_sample_3_cleaned <- data_sample_3[-195,]
#change "Female" into "female"
data_sample_3_cleaned <- data_sample_3 %>% mutate(sex = droplevels(replace(sex, sex == "Female", "female")))
data_sample_3_cleaned %>% summary
view(data_sample_3_cleaned)
describe(data_sample_3_cleaned)
str(data_sample_3_cleaned)


##check data distribution with histograms
windows()
hist(data_sample_3_cleaned$pain, main="pain")
windows()
hist(data_sample_3_cleaned$age, main="age")
windows()
hist(data_sample_3_cleaned$STAI_trait, main="anxiety")
windows()
hist(data_sample_3_cleaned$pain_cat, main="pain catastophizing")
windows()
hist(data_sample_3_cleaned$cortisol_serum, main="cortisol serum")
windows()
hist(data_sample_3_cleaned$mindfulness, main="mindfulness")


## check for outliers in the data
windows()
data_sample_3_cleaned %>%ggplot() +aes(x = age, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_3_cleaned %>%ggplot() +aes(x = sex, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_3_cleaned %>%ggplot() +aes(x = STAI_trait, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_3_cleaned %>%ggplot() +aes(x = pain_cat, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_3_cleaned %>%ggplot() +aes(x = cortisol_serum, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_3_cleaned %>%ggplot() +aes(x = mindfulness, y = pain) +geom_point() +geom_smooth(method = "lm")


##exploring clustering in the data
windows()
data_sample_3_cleaned %>% ggplot() +aes(y = pain, x = sex) +geom_point(aes(color = hospital), size = 2) +geom_smooth(method = "lm",se = F)
windows()
data_sample_3_cleaned %>% ggplot() +aes(y = pain,x = sex, color = hospital) +geom_point(size = 2) +geom_smooth(method = "lm",se = F, fullrange = TRUE)

windows()
data_sample_3_cleaned %>% ggplot() +aes(y = pain, x = age) +geom_point(aes(color = hospital), size = 2) +geom_smooth(method = "lm",se = F)
windows()
data_sample_3_cleaned %>% ggplot() +aes(y = pain,x = age, color = hospital) +geom_point(size = 2) +geom_smooth(method = "lm",se = F, fullrange = TRUE)

windows()
data_sample_3_cleaned %>% ggplot() +aes(y = pain, x = STAI_trait) +geom_point(aes(color = hospital), size = 2) +geom_smooth(method = "lm",se = F)
windows()
data_sample_3_cleaned %>% ggplot() +aes(y = pain,x = STAI_trait, color = hospital) +geom_point(size = 2) +geom_smooth(method = "lm",se = F, fullrange = TRUE)

windows()
data_sample_3_cleaned %>% ggplot() +aes(y = pain, x = pain_cat) +geom_point(aes(color = hospital), size = 2) +geom_smooth(method = "lm",se = F)
windows()
data_sample_3_cleaned %>% ggplot() +aes(y = pain,x = pain_cat, color = hospital) +geom_point(size = 2) +geom_smooth(method = "lm",se = F, fullrange = TRUE)

windows()
data_sample_3_cleaned %>% ggplot() +aes(y = pain, x = cortisol_serum) +geom_point(aes(color = hospital), size = 2) +geom_smooth(method = "lm",se = F)
windows()
data_sample_3_cleaned %>% ggplot() +aes(y = pain,x = cortisol_serum, color = hospital) +geom_point(size = 2) +geom_smooth(method = "lm",se = F, fullrange = TRUE)

windows()
data_sample_3_cleaned %>% ggplot() +aes(y = pain, x = mindfulness) +geom_point(aes(color = hospital), size = 2) +geom_smooth(method = "lm",se = F)
windows()
data_sample_3_cleaned %>% ggplot() +aes(y = pain,x = mindfulness, color = hospital) +geom_point(size = 2) +geom_smooth(method = "lm",se = F, fullrange = TRUE)



#################### built linear mixed model ####################



random_int_model = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness +( 1 | hospital), data = data_sample_3_cleaned)



#################### model diagnostics ####################



########## check for outliers, Cook's distance ##########


#can take some time
influence_observation = influence(random_int_model, obs = T)$alt.fixed 
influence_group = influence(random_int_model, group = "hospital")$alt.fixed

windows()
data_plot_influence = as_tibble(influence_group) %>% gather(colnames(influence_group),value = coefficient, key = predictor)
data_plot_influence %>% ggplot() +aes(x = 1, y = coefficient,group = predictor) +geom_violin() +facet_wrap(~predictor,scales = "free")


########## normality ##########


#roughly fit on a straight line
windows()
qqmath(random_int_model, id = 0.05) 
windows()
qqmath(ranef(random_int_model))


########## linearity ##########


windows()
plot(random_int_model, arg = "pearson")

windows()
data_sample_3_cleaned %>%ggplot() +aes(x = pain, y = residuals(random_int_model)) +geom_point()
windows()
data_sample_3_cleaned %>%ggplot() +aes(x = sex, y = residuals(random_int_model)) +geom_point()
windows()
data_sample_3_cleaned %>%ggplot() +aes(x = age, y = residuals(random_int_model)) +geom_point()
windows()
data_sample_3_cleaned %>%ggplot() +aes(x = STAI_trait, y = residuals(random_int_model)) +geom_point()
windows()
data_sample_3_cleaned %>%ggplot() +aes(x = pain_cat, y = residuals(random_int_model)) +geom_point()
windows()
data_sample_3_cleaned %>%ggplot() +aes(x = cortisol_serum, y = residuals(random_int_model)) +geom_point()
windows()
data_sample_3_cleaned %>%ggplot() +aes(x = mindfulness, y = residuals(random_int_model)) +geom_point()


########## homoscedasticity/homogenity of variance ##########


#funnel shape would indicate heteroscedasticity,
windows()
plot(random_int_model, arg = "pearson")
#homoscedasticity across clusters
homosced_mod = lm(residuals(random_int_model)^2 ~ hospital, data = data_sample_3_cleaned)
summary(homosced_mod) #not significant p=0.42
#no further evaluation or checks like cyclone plots needed


########## no multicollinearity ##########


windows()
pairs.panels(data_sample_3_cleaned[,c("age", "sex", "STAI_trait", "pain_cat", "cortisol_serum", "mindfulness")], col = "pink", lm = T)



#################### model coefficients, confidence intervals of the coefficients ####################



##function to extract standardized beta coefficients from linear mixed models
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))}


##model coefficients
summary(random_int_model)


##confidence intervals for the coefficients
confint(random_int_model)


##standardized betas
stdCoef.merMod(random_int_model)



#################### marginal and coditional R2 ####################



##marginal R squared (variance explained by the fixed effect predictors)
r2beta(random_int_model, method = "nsj", data = data_sample_3_cleaned)

#output
#r2beta(random_int_model, method = "nsj", data = data_sample_3_cleaned)
#               Effect   Rsq upper.CL lower.CL
#1          Model 0.362    0.467    0.277

#6 cortisol_serum 0.077    0.158    0.022   !!!!

#5       pain_cat 0.058    0.134    0.012
#3            age 0.049    0.121    0.008
#7    mindfulness 0.017    0.070    0.000
#2        sexmale 0.015    0.065    0.000
#4     STAI_trait 0.001    0.030    0.000


##marginal and conditional R squared values (variance explained by the fixed and random effect terms combined)
r.squaredGLMM(random_int_model) 
#     R2m       R2c
#[1,] 0.3615051 0.4498447



#################### data diagnostics for data set 4 ####################



##load data and look at it
data_sample_4 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")
summary(data_sample_4)
describe (data_sample_4)
str(data_sample_4)


##check data distribution with histograms
windows()
hist(data_sample_4$pain, main="pain")
windows()
hist(data_sample_4$age, main="age")
windows()
hist(data_sample_4$STAI_trait, main="anxiety")
windows()
hist(data_sample_4$pain_cat, main="pain catastophizing")
windows()
hist(data_sample_4$cortisol_serum, main="cortisol serum")
windows()
hist(data_sample_4$mindfulness, main="mindfulness")


## check for outliers in the data
windows()
data_sample_4 %>%ggplot() +aes(x = age, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_4 %>%ggplot() +aes(x = sex, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_4 %>%ggplot() +aes(x = STAI_trait, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_4 %>%ggplot() +aes(x = pain_cat, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_4 %>%ggplot() +aes(x = cortisol_serum, y = pain) +geom_point() +geom_smooth(method = "lm")
windows()
data_sample_4 %>%ggplot() +aes(x = mindfulness, y = pain) +geom_point() +geom_smooth(method = "lm")



#################### test model ####################



##predict pain
predicted_pain_4 <- predict(random_int_model, data = data_sample_4)
predicted_pain_4


##RSS
RSS = sum((data_sample_4$pain - predict(random_int_model, data_sample_4, allow.new.levels =TRUE))^2)
RSS #321.85


##TSS
model_mean <- lmer(pain ~ 1 + (1 | hospital), data = data_sample_4)

TSS = sum((data_sample_4$pain - predict(model_mean, data_sample_4, allow.new.levels = TRUE))^2)
TSS #418.53


##variance explained by the model 
R2 = 1 - (RSS/TSS)
R2 #0.23



#################### built new model ####################



##find most influential predictor from previous linear mixed model
random_int_model 
#cortisol_serum highest with 0.47


##Build a new linear mixed effects model on dataset 3 predicting pain (allow for both random intercept and random slope)
random_slope_model = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = data_sample_3_cleaned)
random_slope_model


##prediction of model
predict_pain_slope <- predict(random_slope_model)


##how cortisol predicts pain in each hospital (visualization of the fitted regression lines for each hospital separately)
windows()
data_sample_3_cleaned %>% ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) + geom_point(aes(color = hospital), size = 2) + geom_line(color = "red", aes(y = predict_pain_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)



#################### model comparison ####################



cAIC(random_int_model)$caic # 640.03
cAIC(random_slope_model)$caic # 688.81

anova(random_int_model, random_slope_model)

r2beta(random_slope_model, method = "nsj", data = data_sample_3_cleaned)