
######################### Zoltan Kekecs - assignment 1 #########################


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
#leave negative income value because it is not included in the calculation 
data_sample_1$STAI_trait[data_sample_1$STAI_trait==3.5]=35
data_sample_1 %>% summary
view(data_sample_1)
describe(data_sample_1)

##check data distribution with histograms
windows()
hist(data_sample_1$pain, main="pain")
windows()
hist(data_sample_1$age, main="age")
windows()
hist(data_sample_1$STAI_trait, main="anxiety")
windows()
hist(data_sample_1$pain_cat, main="pain catastophizing")
windows()
hist(data_sample_1$cortisol_serum, main="cortisol serum")
windows()
hist(data_sample_1$cortisol_saliva, main="cortisol saliva")
windows()
hist(data_sample_1$mindfulness, main="mindfulness")

## check for outliers in the data
windows()
data_sample_1 %>% ggplot() +aes(x = age, y = pain) + geom_point()
windows()
data_sample_1 %>%ggplot() +aes(x = age, y = pain) +geom_point() +geom_smooth(method = "lm")

windows()
data_sample_1 %>%ggplot() +aes(x = sex, y = pain) +geom_point()
windows()
data_sample_1 %>%ggplot() +aes(x = sex, y = pain) +geom_point() +geom_smooth(method = "lm")

windows()
data_sample_1 %>%ggplot() +aes(x = STAI_trait, y = pain) +geom_point()
windows()
data_sample_1 %>%ggplot() +aes(x = STAI_trait, y = pain) +geom_point() +geom_smooth(method = "lm")

windows()
data_sample_1 %>%ggplot() +aes(x = pain_cat, y = pain) +geom_point()
windows()
data_sample_1 %>%ggplot() +aes(x = pain_cat, y = pain) +geom_point() +geom_smooth(method = "lm")

windows()
data_sample_1 %>%ggplot() +aes(x = cortisol_serum, y = pain) +geom_point()
windows()
data_sample_1 %>%ggplot() +aes(x = cortisol_serum, y = pain) +geom_point() +geom_smooth(method = "lm")

windows()
data_sample_1 %>%ggplot() +aes(x = cortisol_saliva, y = pain) +geom_point()
windows()
data_sample_1 %>%ggplot() +aes(x = cortisol_saliva, y = pain) +geom_point() +geom_smooth(method = "lm")

windows()
data_sample_1 %>%ggplot() +aes(x = mindfulness, y = pain) +geom_point()
windows()
data_sample_1 %>%ggplot() +aes(x = mindfulness, y = pain) +geom_point() +geom_smooth(method = "lm")

##create model 1 with age and sex
mod1 <-lm(pain ~ age + sex, data = data_sample_1)
mod1
sm=summary(mod1)

##create model 2 with age, sex, anxiety, pain catastrophizing, cortisol serum, cortisol salvia, mindfulness
mod2<-lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_1)
mod2
sm=summary(mod2)



#################### model diagnostics ####################



########## check for influential outliers, Cook's distance ##########


windows()
mod1 %>% plot(which=5)
windows()
mod2 %>% plot(which=5)
windows()
ols_plot_cooksd_bar(mod1)
windows()
ols_plot_cooksd_bar(mod2)


########## normality ##########


##QQ-plots
windows()
plot(x = mod1, which = 2)
windows()
plot(x = mod2, which = 2)

##shapiro wilk test 
shapiro.test(residuals(mod1)) #p=0.656
shapiro.test(residuals(mod2)) #p=0.803
#not significant

##histogram
windows()
residuals_mod1 = enframe(residuals(mod1))
residuals_mod1 %>% ggplot() + aes (x=value) + geom_histogram()
windows()
residuals_mod2 = enframe(residuals(mod2))
residuals_mod2 %>% ggplot() + aes (x=value) + geom_histogram()

##skew and kurtosis 
#can be between -1;1; the closer to 0 the better
describe(residuals(mod1)) #skew: 0.19, kurtosis: 0.29
describe(residuals(mod2)) #skew:-0.05, kurtosis: -0.36
#close to 0, no long tails, ok


########## linearity ##########


##should be non-significant
windows()
mod1 %>% residualPlots() 
windows()
mod2 %>% residualPlots()
#minor curvator visible but non significant -> linearity assumption hold true


########## homoscedasticity/homogenity of variance ##########


windows()
mod1 %>% plot(which=3)
windows()
mod2 %>% plot(which=3)

##NCV-test
mod1 %>% ncvTest() #p=0.537
mod2 %>% ncvTest() #p=0.621
#non-significant

##Breush-Pagan test
mod1 %>% bptest() #p=0.848
mod2 %>% bptest() #p=0.296
#non-significant


########## no multicollinearity ##########


#values of 3 or higher are problematic
mod1 %>% vif()
mod2 %>% vif() #problematic -> data multicollinearity -> inspect correlation matrix

windows()
data_sample_1 %>% select(pain, age, sex, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness) %>% pairs.panels(col = "pink", lm = T)
summary(mod2)
#correlation between cortisol_serum and cortisol_saliva too high .89 -> delete one of them

#built new model (exclude cortisol_saliva because it's often regarded as less reliable in research; moreover the normal distribution is well shaped for the cortisol_serum)
mod2_adj <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_1)

summary(mod2) 
#R adjusted=0.44

summary(mod2_adj)
#R adjusted=0.41
windows()
data_sample_1 %>% select(pain, age, sex, STAI_trait, pain_cat, cortisol_serum, mindfulness) %>% pairs.panels(col = "pink", lm = T)



#################### test the final model (mod2_adj) again ####################



########## outliers ##########


##Cook's distance
windows()
mod2_adj %>% plot(which=5)
windows()
ols_plot_cooksd_bar(mod2_adj)


########## normality ##########


##QQ-plots
windows()
plot(x = mod2_adj, which = 2)

##shapiro wilk test 
shapiro.test(residuals(mod2_adj)) #p=0.990

##histogram
windows()
residuals_mod2_adj = enframe(residuals(mod2_adj))
residuals_mod2_adj %>% ggplot() + aes (x=value) + geom_histogram()

##skew and kurtosis 
#can be between -1;1; the closer to 0 the better
describe(residuals(mod2_adj)) #skew: -0.12, kurtosis: -0.14


########## linearity ##########


##tukey test should be non-significant
windows()
mod2_adj %>% residualPlots() 
#tukey test p=0.215


########## homoscedasticity/homogenity of variance ##########


windows()
mod2_adj %>% plot(which=3)

##NCV-test 
mod2_adj %>% ncvTest() #p=0.970
#non-significant 

##Breush-Pagan test 
mod2_adj %>% bptest() #p=0.259
#non-significant 


########## no multicollinearity ##########


#values of 3 or higher are problematic
mod2_adj %>% vif()


#################### model comparison #################### 



########## model 1 ########## 

summary(mod1)
summary(mod1)$adj.r.squared
#adjusted R^2=0.129

AIC(mod1)
# 576.34

confint(mod1)
#             2.5 %      97.5 %
#(Intercept)  6.1313792  9.84451352
#age         -0.1342600 -0.04421579
#sexmale      0.1957662  1.10059814

lm.beta(mod1)
#(Intercept)         age     sexmale 
#0.0000000    -0.2909513   0.2103080 


########## model 2 ########## 

summary(mod2_adj)
summary(mod2_adj)$adj.r.squared
# adjusted R^2=0.41

AIC(mod2_adj)
# 518.66

confint(mod2_adj)
  #                   2.5 %       97.5 %
  #(Intercept)    -0.02951835  6.084144305
  #age            -0.08867832 -0.002829128
  #sexmale         0.09757386  0.881841005
  #STAI_trait     -0.05465731  0.040796322
  #pain_cat        0.03286560  0.126932454
  #cortisol_serum  0.21290753  0.658391478
  #mindfulness    -0.52171607 -0.076621445

lm.beta(mod2_adj)
#(Intercept)    age            sexmale        STAI_trait     pain_cat       cortisol_serum   mindfulness
#0.00000000    -0.14917551     0.15888959    -0.02210526     0.27564164     0.28868272       -0.19458573 


########## ANOVA ########## 

# models are nested therefore we can use ANOVA
anova(mod1, mod2_adj)
# F(4, 157) = 19.42, p<.001

#Analysis of Variance Table

#Model 1: pain ~ age + sex
#Model 2: pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1    157 326.83                                  
#2    153 216.79  4    110.04 19.416 6.145e-13 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


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

coef_table(mod1)
#               b    95%CI lb 95%CI ub Std.Beta p-value
#(Intercept)  7.99     6.13     9.84        0   <.001
#age         -0.09    -0.13    -0.04    -0.29   <.001
#sexmale      0.65     0.20     1.10     0.21    .005

coef_table(mod2_adj)
#                 b   95%CI lb  95%CI ub Std.Beta p-value
#(Intercept)     3.03    -0.03     6.08        0    .052
#age            -0.05    -0.09     0.00    -0.15    .037
#sexmale         0.49     0.10     0.88     0.16    .015
#STAI_trait     -0.01    -0.05     0.04    -0.02    .775
#pain_cat        0.08     0.03     0.13     0.28    .001
#cortisol_serum  0.44     0.21     0.66     0.29   <.001
#mindfulness    -0.30    -0.52    -0.08    -0.19    .009
