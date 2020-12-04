library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(tidyverse)
library(gridExtra)
library(lm.beta)
# ## Custom functions	

# We will use these custom functions to get bootstrapped confidence intervals.	


# function to obtain regression coefficients	
# source: https://www.statmethods.net/advstats/bootstrapping.html	
bs_to_boot <- function(model, data, indices) {	
  d <- data[indices,] # allows boot to select sample 	
  fit <- lm(formula(model), data=d)	
  return(coef(fit)) 	
}	

# function to obtain adjusted R^2	
# source: https://www.statmethods.net/advstats/bootstrapping.html (partially modified)	
adjR2_to_boot <- function(model, data, indices) {	
  d <- data[indices,] # allows boot to select sample 	
  fit <- lm(formula(model), data=d)	
  return(summary(fit)$adj.r.squared)	
}	


# Computing the booststrap BCa (bias-corrected and accelerated) bootstrap confidence intervals by Elfron (1987)	
# This is useful if there is bias or skew in the residuals.	

confint.boot <- function(model, data = NULL, R = 1000){	
  if(is.null(data)){	
    data = eval(parse(text = as.character(model$call[3])))	
  }	
  boot.ci_output_table = as.data.frame(matrix(NA, nrow = length(coef(model)), ncol = 2))	
  row.names(boot.ci_output_table) = names(coef(model))	
  names(boot.ci_output_table) = c("boot 2.5 %", "boot 97.5 %")	
  results.boot = results <- boot(data=data, statistic=bs_to_boot, 	
                                 R=1000, model = model)	
  
  for(i in 1:length(coef(model))){	
    boot.ci_output_table[i,] = unlist(unlist(boot.ci(results.boot, type="bca", index=i))[c("bca4", "bca5")])	
  }	
  
  return(boot.ci_output_table)	
}	

# Computing the booststrapped confidence interval for a linear model using wild bottstrapping as descibed by Wu (1986) <doi:10.1214/aos/1176350142>	
# requires the lmboot pakcage	

wild.boot.confint <- function(model, data = NULL, B = 1000){	
  if(is.null(data)){	
    data = eval(parse(text = as.character(model$call[3])))	
  }	
  
  wild_boot_estimates = wild.boot(formula(model), data = data, B = B)	
  
  result = t(apply(wild_boot_estimates[[1]], 2, function(x) quantile(x,probs=c(.025,.975))))	
  
  return(result)	
  
}	

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	


"______________________________________________________________________________________________________________________"

data_tooth = read.csv("https://tinyurl.com/ha-dataset1")

View(data_tooth)

summary(data_tooth) # Age is max 444?, Household_income -3732 debt, just one sample Check this. IQ 49 too low exclude? STAI 3.9 correct to 39 

describe(data_tooth)
"______________________________________________________________________________________________________________________"

#Histograms
plot_pain = data_tooth_changed %>%ggplot() +aes(x = pain) +geom_histogram(bins = 50)
plot_age = data_tooth_changed %>%ggplot() +aes(x = age) +geom_histogram(bins = 50)
plot_sex = data_tooth_changed %>%ggplot() +aes(x = sex) +geom_histogram(bins = 50)
plot_pain_cat = data_tooth_changed %>%ggplot() +aes(x = pain_cat) +geom_histogram(bins = 50)
plot_IQ = data_tooth_changed %>%ggplot() +aes(x = IQ) +geom_histogram(bins = 50) # one very low outlyer 
plot_STAI = data_tooth_changed %>%ggplot() +aes(x = STAI_trait) +geom_histogram(bins = 50)  #OUTLAYER 
plot_mindfulness = data_tooth_changed %>%ggplot() +aes(x = mindfulness) +geom_histogram(bins = 50) 
plot_coritsolserum = data_tooth_changed %>%ggplot() +aes(x = cortisol_serum) +geom_histogram(bins = 50) 
plot_cortisolsaliva = data_tooth_changed %>%ggplot() +aes(x = cortisol_saliva) +geom_histogram(bins = 50) 

grid.arrange(plot_pain, plot_age, plot_pain_cat, plot_IQ, plot_STAI, plot_mindfulness, plot_coritsolserum, plot_cortisolsaliva, nrow=3)
#sex does not work as it is a categorical variable
"______________________________________________________________________________________________________________________"

"______________________________________________________________________________________________________________________"
#changes to the dataset

data_tooth_changed <- data_tooth

data_tooth_changed$age[data_tooth_changed$age == 444] <- 44 # (Age datapoint codeerror corrected)

data_tooth_changed$STAI_trait[data_tooth_changed$STAI_trait == 3.9] <- 39 #DATAERROR CORRECTED 

data_tooth_changed$household_income[data_tooth_changed$household_income == -3732] <- 3732 #corrected income


data_tooth_changed <- data_tooth_changed[-c(47),]  # NUMBER ID 47 removed due to low IQ



"______________________________________________________________________________________________________________________"
#Note, if you want a better intercept use the Scale function on the dataset 
#did not do this due to lack of space in the rapport 


#MODEL 1
mod_age_sex_1_LM = lm(pain ~ age + sex, data = data_tooth_changed)
summary(mod_age_sex_1_LM) # Multiple R-squared:  0.06529,	Adjusted R-squared:  0.05331, p-value: 0.00516
                          # check is the coefficient is aligned with info from previous studies 
                            # Age has negativ corr with pain and is aligned with info about prevoius studies
                              # sex is to be seen as dependent on what surgery, but here it looks negativ aswell = female positiv? #note (you know the answer to this now)
                                  

AIC(mod_age_sex_1_LM) # 588.5887
"______________________________________________________________________________________________________________________"

# YOU HAVE TO ADD EVERY MODEL you can make a better disscussion if you do this. 
# GET A BETTER GRIP OF THAT DATA AND THE COEFFICIENTS BY DOING IT FOR EACH VARB :) 

mod_2_LM = lm(pain ~ age + sex +  STAI_trait, data = data_tooth_changed)
summary(mod_2_LM) # Multiple R-squared:  0.2612,	Adjusted R-squared:  0.2469 
AIC(mod_2_LM) # 553.1864
"______________________________________________________________________________________________________________________"

mod_3_LM = lm(pain ~ age + sex +  STAI_trait + pain_cat, data = data_tooth_changed)
summary(mod_3_LM) # Multiple R-squared:  0.3918,	Adjusted R-squared:  0.376. # p-value: 7.292e-16
AIC(mod_3_LM) # 524.2502 # AIC NOT BETTER THEN MOD_2_lm # pain cat?
"______________________________________________________________________________________________________________________"
mod_4_LM = lm(pain ~ age + sex +  STAI_trait + pain_cat + mindfulness, data = data_tooth_changed)
summary(mod_4_LM)# Multiple R-squared:  0.412,	Adjusted R-squared:  0.3927
AIC(mod_4_LM) # 520.9042
"______________________________________________________________________________________________________________________"
mod_5_LM = lm(pain ~ age + sex +  STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_tooth_changed)
summary(mod_5_LM)# Multiple R-squared:  0.4936,	Adjusted R-squared:  0.4736 #p-value: < 2.2e-16
AIC(mod_5_LM)# 499.1429
"______________________________________________________________________________________________________________________"
mod_6_LM = lm(pain ~ age + sex +  STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_tooth_changed)
summary(mod_6_LM)# Multiple R-squared:  0.5027,	Adjusted R-squared:  0.4797 
AIC(mod_6_LM)# 498.252 
"______________________________________________________________________________________________________________________"

#Modelcomparison 
mod_age_sex_1_LM = lm(pain ~ age + sex, data = data_tooth_changed)
summary(mod_age_sex_1_LM) # Multiple R-squared:  0.06529,	Adjusted R-squared:  0.05331 
AIC(mod_age_sex_1_LM) #588.5887

mod_6_LM = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_tooth_changed)
summary(mod_6_LM)# Multiple R-squared:  0.5027,	Adjusted R-squared:  0.4797  F-statistic: 21.81
AIC(mod_6_LM)# 498.252 

ANOVA_1_6 = anova(mod_age_sex_1_LM, mod_6_LM)
ANOVA_1_6 #  F = 26.563
#mod_6_LM is the better model 

AIC(mod_age_sex_1_LM)
AIC(mod_6_LM) #AIC for mod_6_LM is 90.3367 points lower the mod_1. The mod_6 is the way to go
"______________________________________________________________________________________________________________________"
#SALIVA and blod cotrisol will checked for high corr

"______________________________________________________________________________________________________________________"

#TIME FOR DIAGNOSTICS (be aware about high corr in cortisol)

#some nice plots :) 

data_tooth_changed %>% 	
  mutate(rownum = row.names(data_tooth_changed)) %>% 	
  ggplot() +	
  aes(x = age, y = pain, label = rownum) +	
  geom_label()

data_tooth_changed %>% 	
  mutate(rownum = row.names(data_tooth_changed)) %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain, label = rownum) +	
  geom_label()

"______________________________________________________________________________________________________________________"


mod_6_LM %>%plot(which = 5) # leverage is flagging 68,100, 114
mod_6_LM %>%plot(which = 4) # cooksdistance shows flagging nr 68, 100, 114. my threshold is 4/159 = 0.025 
                                  # how can i see the rest of the ID's??
data_tooth_changed %>%slice(c(68, 100, 114)) # I dont see anything in this that make me want to remove them.
"______________________________________________________________________________________________________________________"

describe(data_tooth_changed) # skew is acceptable on all lvls, but kurtosis is dif but just for sex and we have an uneven number of sex 

"______________________________________________________________________________________________________________________"
#NORMALITY 
mod_6_LM %>%plot(which = 2) #QQ PLOT TO CHECK FOR normality  think this is fine 



describe(residuals(mod_6_LM)) # residuals mod_6_LM Skew (-0.12) and kurstois (0-4) are both fine 

#THE ASSUMPTION OF NORMALITY IS FINE :)
"______________________________________________________________________________________________________________________"

#LINEARITY :)
mod_6_LM %>%residualPlots() # pain_cat and mindfulness jsut by looking at them #however noone is significant at P > 0.05 
#NO PROBLEM 
"______________________________________________________________________________________________________________________"

#Homoscedasticty  :)

mod_6_LM %>%plot(which = 3) # this will show a plot of the standarized residuals  # cant see if this okay or not so do test 

mod_6_LM %>%ncvTest() # p = 0.10454 there is no a violation since p < 0.05

"______________________________________________________________________________________________________________________"

#No multicollinearity (it will be the corisal)

mod_6_LM %>%vif()  # Cotrisal_serum at 6.67 and cortisal_saliva 7.45  
                        # You will remove the one that is deemed a less good estimate in the litteratur ()
                            #The prior litteratur shows that serum cortisol is often regarded in medical research as more reliably related to stress
                                #Hence we will re run all tests on Mod_5_LM

"______________________________________________________________________________________________________________________"


#Finall model? mod_5_LM

mod_age_sex_1_LM = lm(pain ~ age + sex, data = data_tooth_changed)
summary(mod_age_sex_1_LM) # Multiple R-squared:  0.06529,	Adjusted R-squared:  0.05331 
AIC(mod_age_sex_1_LM) #588.5887

mod_5_LM = lm(pain ~ age + sex +  STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_tooth_changed)
summary(mod_5_LM)# Multiple R-squared:  0.4936,	Adjusted R-squared:  0.4736 #p-value: < 2.2e-16
AIC(mod_5_LM)# 499.1429

Anova_mod5 = anova(mod_age_sex_1_LM, mod_5_LM)
Anova_mod5 # F = 32 change is significant 2.2e-16


AIC(mod_age_sex_1_LM)
AIC(mod_5_LM) # there is a diff on 89.4458 points, Hence Mod_5 is the way to go
 
"___________________________________________________________________________________________________________"


"___________________________________________________________________________________________________________"
#Modeldiagnostics for mod_5LM 

mod_5_LM %>%plot(which = 5) # leverage is flagging 96,100, 114
mod_5_LM %>%plot(which = 4) # cooksdistance shows flagging nr 96, 100, 114. my threshold is 4/159 = 0.025 
# how can i see the rest of the ID's??
data_tooth_changed %>%slice(c(96, 100, 114)) # I dont see anything in this thae make me want to remove them, but cooks distance 
"______________________________________________________________________________________________________________________"

describe(data_tooth_changed) # skew is acceptable on all lvls, but kurtosis is diffrent, mainly for sex 
#this is due to the dataset containing an uneven amounts of participance relating to sex. 

"______________________________________________________________________________________________________________________"
#NORMALITY 
mod_5_LM %>%plot(which = 2) #QQ PLOT TO CHECK FOR normality  think this is fine
                                #Flagging 3,20,114 
                                  #Just for fun checking thease values 

data_tooth_changed %>% slice(c(3, 20, 114)) # Nothing seems to be strange or alarming. Verifying this by checking skewness and kurtosis

describe(residuals(mod_5_LM)) # residuals mod_5_LM Skew (-0.11) and kurstois (0.36) are both fine 

#THE ASSUMPTION OF NORMALITY IS FINE :)
"______________________________________________________________________________________________________________________"

#LINEARITY :)
mod_5_LM %>%residualPlots() # STAI_trait and mindfulness diffresr slightly #however noone is significant at P > 0.05 
#NO PROBLEM ????
"______________________________________________________________________________________________________________________"

#Homoscedasticty  :)

mod_5_LM %>%plot(which = 3) # this will show a plot of the standarized residuals  #plot is not to informative. 

mod_5_LM %>%ncvTest() #  p = 0.10012. not significant and hence we are not violating this assumption 

"______________________________________________________________________________________________________________________"

#No multicollinearity 

mod_5_LM %>%vif() # No values are above 3, hence we are not violating this assumption. 


residuals_mod_5_LM = enterframe(residuals(mod_5_LM))

"_______________________________________________________________________________________________"

summary(lm.beta(lm(mod_age_sex_1_LM, data = data_tooth_changed)))
confint.lm(mod_age_sex_1_LM)                                        #confidence intervall mod_1
coef_table(mod_age_sex_1_LM)                                            # Custom function for b and std.b, CI and P 
AIC(mod_age_sex_1_LM)


summary(lm.beta(lm(mod_5_LM, data = data_tooth_changed))) 
confint.lm(mod_5_LM)          #confidence intervall mod_5
coef_table(mod_5_LM)          # Custom function for b and std.b CI and P
AIC(mod_5_LM)

summary(Anova_mod5)


Anova_mod5

data_tooth_changed %>%
  group_by(sex) %>%
  summarize(number = n())

"____________________________________________________________________________________________________"


"___________________________________________________________________________________________"
#ASSIGNMENT 2           #ASSIGNMENT 2               #ASSIGNMENT 2               #ASSIGNMENT 2         #ASSIGNMENT 2 
data_assignment2 = read.csv("https://tinyurl.com/ha-dataset2")
#Creating the new model that is the competitor 
mod_competitor_1 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_tooth_changed)
summary(mob_competitor_1) #Adjusted R-squared:  0.474
AIC(mod_competitor_1)
"_____________________________________________________________________________________________________________________________________________"
#diagnostics for mod_competitor_1
#RUN DIAGNOSTICS ON THIS NEW MODLE mob_competitor_1

mod_competitor_1 %>%plot(which = 5) #flagging 3,100,114
mod_competitor_1 %>%plot(which = 4) # flagging 3,100,114, my threshold is 4/159 = 0.025 

data_tooth_changed %>%slice(c(3, 100, 114)) # I dont see anything in this thae make me want to remove them
"______________________________________________________________________________________________________________________"

describe(data_tooth_changed) # skew is acceptable on all lvls, but kurtosis is diffrent, mainly for sex 
#this is due to the dataset containing an uneven amounts of participance relating to sex. 

"______________________________________________________________________________________________________________________"
#NORMALITY 
mod_competitor_1 %>%plot(which = 2) #QQ PLOT TO CHECK FOR normality  think this is fine
#Flagging 3,114, 149 
#Just for fun checking thease values 

data_tooth_changed %>% slice(c(3, 114, 149)) # Nothing seems to be strange or alarming. Verifying this by checking skewness and kurtosis

describe(residuals(mod_competitor_1)) # residuals mod_competitor_1 Skew (-0.08) and kurstois (-0.38) are both fine 


residuals_mod_competitor_1 =enframe(residuals(mod_competitor_1))

residuals_mod_competitor_1 %>%ggplot() +aes(x = value) +geom_histogram()
#THE ASSUMPTION OF NORMALITY IS FINE :)
"______________________________________________________________________________________________________________________"

#LINEARITY :)
mod_competitor_1 %>%residualPlots() # Noone is significant at P > 0.05 

#NO PROBLEM ????
"______________________________________________________________________________________________________________________"

#Homoscedasticty  :)

mod_competitor_1 %>%plot(which = 3) # this will show a plot of the standarized residuals  #plot is not to informative. 

mod_competitor_1 %>%ncvTest() #  p = 0.11741 not significant and hence we are not violating this assumption 

"______________________________________________________________________________________________________________________"

#No multicollinearity 

mod_competitor_1 %>%vif() # No values are above 3, hence we are not violating this assumption. 


residuals_mod_competitor_1 = enterframe(residuals(mod_competitor_1))

#No violations identified 
"____________________________"


"_________________"
#diagnostics for mod_competitor_1
#RUN DIAGNOSTICS ON THIS NEW MODLE mob_competitor_1

mod_competitor_2_retained_predictors_backward %>%plot(which = 5) #flagging 103,114,148
mod_competitor_2_retained_predictors_backward %>%plot(which = 4) # flagging 103,114,148, my threshold is 4/159 = 0.025 

data_tooth_changed %>%slice(c(103,114,148)) # I dont see anything alarming
"______________________________________________________________________________________________________________________"

describe(data_tooth_changed) # skew is acceptable on all lvls, but kurtosis is diffrent, mainly for sex 
#this is due to the dataset containing an uneven amounts of participance relating to sex. 

"______________________________________________________________________________________________________________________"
#NORMALITY 
mod_competitor_2_retained_predictors_backward %>%plot(which = 2) #QQ PLOT TO CHECK FOR normality  think this is fine
#Flagging 26,114, 148 
#Just for fun checking thease values 

data_tooth_changed %>% slice(c(26, 114, 149)) # Nothing seems to be strange or alarming. Verifying this by checking skewness and kurtosis

describe(residuals(mod_competitor_2_retained_predictors_backward)) # Skew (-0.11) and kurstois (-0.38) are both fine 


residuals_mod_competitor_2_retained_predictors_backward =enframe(residuals(mod_competitor_2_retained_predictors_backward))

residuals_mod_competitor_2_retained_predictors_backward %>%ggplot() +aes(x = value) +geom_histogram()
#THE ASSUMPTION OF NORMALITY IS FINE :)
"______________________________________________________________________________________________________________________"

#LINEARITY :)
mod_competitor_2_retained_predictors_backward %>%residualPlots() # Noone is significant at P > 0.05 

#NO PROBLEM ????
"______________________________________________________________________________________________________________________"

#Homoscedasticty  :)

mod_competitor_2_retained_predictors_backward %>%plot(which = 3) # this will show a plot of the standarized residuals  #plot is not to informative. 

mod_competitor_2_retained_predictors_backward %>%ncvTest() #  p = 0.086983 not significant and hence we are not violating this assumption 

"______________________________________________________________________________________________________________________"

#No multicollinearity 

mod_competitor_2_retained_predictors_backward %>%vif() # No values are above 3, hence we are not violating this assumption. 

#No violations identified 
"____________________________"







"___________________________________________________________________________________________________________________________________________"
#Creating the backwardsreggresion 
mod_competitor_1_backward =  step(mod_competitor_1, direction = "backward")

summary((mod_competitor_1_backward))

mod_competitor_2_retained_predictors_backward = lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = data_tooth_changed)
summary(mod_competitor_2_retained_predictors_backward) # R^2 is 0.4813 and is significatn p<0.001
#Final predictors retained pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income.
"___________________________________________________________________________________________________________________________________"
#RE-RUN DIAGNOSTICS ON THE NEW MODEL = MOD_competitor_2_retainted_predictors_backwards

mod_competitor_2_retained_predictors_backward %>%plot(which = 5) #flagging 103,114,148
mod_competitor_2_retained_predictors_backward %>%plot(which = 4) # flagging 103,114,148, my threshold is 4/159 = 0.025 

data_tooth_changed %>%slice(c(103,114,148)) # I dont see anything alarming
"______________________________________________________________________________________________________________________"

describe(data_tooth_changed) # skew is acceptable on all lvls, but kurtosis is diffrent, mainly for sex 
#this is due to the dataset containing an uneven amounts of participance relating to sex. 

"______________________________________________________________________________________________________________________"
#NORMALITY 
mod_competitor_2_retained_predictors_backward %>%plot(which = 2) #QQ PLOT TO CHECK FOR normality  think this is fine
#Flagging 26,114, 148 
#Just for fun checking thease values 

data_tooth_changed %>% slice(c(26, 114, 149)) # Nothing seems to be strange or alarming. Verifying this by checking skewness and kurtosis

describe(residuals(mod_competitor_2_retained_predictors_backward)) # Skew (-0.11) and kurstois (-0.38) are both fine 


residuals_mod_competitor_2_retained_predictors_backward =enframe(residuals(mod_competitor_2_retained_predictors_backward))

residuals_mod_competitor_2_retained_predictors_backward %>%ggplot() +aes(x = value) +geom_histogram()
#THE ASSUMPTION OF NORMALITY IS FINE :)
"______________________________________________________________________________________________________________________"

#LINEARITY :)
mod_competitor_2_retained_predictors_backward %>%residualPlots() # Noone is significant at P > 0.05 

#NO PROBLEM ????
"______________________________________________________________________________________________________________________"

#Homoscedasticty  :)

mod_competitor_2_retained_predictors_backward %>%plot(which = 3) # this will show a plot of the standarized residuals  #plot is not to informative. 

mod_competitor_2_retained_predictors_backward %>%ncvTest() #  p = 0.086983 not significant and hence we are not violating this assumption 

"______________________________________________________________________________________________________________________"

#No multicollinearity 

mod_competitor_2_retained_predictors_backward %>%vif() # No values are above 3, hence we are not violating this assumption. 

#No violations identified 
"____________________________"


"_______________________________________________________________________________________________________________________________________"
#MOD_5_LM theory-based vs mod_competitor_2_backwards
# You can not prefrom an ANOVA-analysis due to the models differ in predictors
AIC(mod_5_LM)
AIC(mod_competitor_2_retained_predictors_backward) # There is a diffrance in AIC with about 2.9 points so its significant 

"_____________________________________________"
#Time to compar mod_5LM to mod_competitor_2_retained_predictors

summary(mod_5_LM)$adj.r.squared
summary(mod_competitor_2_retained_predictors_backward)$adj.r.squared

"______________________________________________________________"
#now testing the models see predictions USING THE OLD DATASET_ data_tooth_changed 
pred_test_mod_5_LM_OLDDATA <- predict(mod_5_LM, data_tooth_changed)
pred_test_mod_competitor_2_retained_predictors_backward_OLDDATA <- predict(mod_competitor_2_retained_predictors_backward, data_tooth_changed)

RSS_test_mod_5_LM_OLDDATA = sum((data_tooth_changed[, "pain"] - pred_test_mod_5_LM_OLDDATA)^2)
RSS_test_mod_competitor_2_retained_predictors_backward_OLDDATA = sum((data_tooth_changed[, "pain"] - pred_test_mod_competitor_2_retained_predictors_backward_OLDDATA)^2)

RSS_test_mod_5_LM_OLDDATA 
RSS_test_mod_competitor_2_retained_predictors_backward_OLDDATA

"_______________________________________________________________________"
#now testing the new dataset to see predictions USING THE NEW DATASET = data_assignemnt2
pred_test_mod_5_LM <- predict(mod_5_LM, data_assignment2)
pred_test_mod_competitor_2_retained_predictors_backward <- predict(mod_competitor_2_retained_predictors_backward, data_assignment2)

RSS_test_mod_5_LM = sum((data_assignment2[, "pain"] - pred_test_mod_5_LM)^2)
RSS_test_mod_competitor_2_retained_predictors_backward = sum((data_assignment2[, "pain"] - pred_test_mod_competitor_2_retained_predictors_backward)^2)

RSS_test_mod_5_LM # error is 234.3815
RSS_test_mod_competitor_2_retained_predictors_backward # The error is higher in this model, 244,6822


#This means that the backwards regression is giving a false indication off being a better model. 
"___________________________________________________________________________________________________________________________"

#STATS
#Mod_5_LM
summary(lm.beta(lm(mod_5_LM, data = data_tooth_changed))) 
pconfint.lm(mod_5_LM)        
coef_table(mod_5_LM)          
AIC(mod_5_LM)

#mod_competitor_2_retained_predictors_backward
summary(lm.beta(lm(mod_competitor_2_retained_predictors_backward, data = data_tooth_changed))) 
confint.lm(mod_competitor_2_retained_predictors_backward)          
coef_table(mod_competitor_2_retained_predictors_backward)          
AIC(mod_competitor_2_retained_predictors_backward)

AIC(mod_competitor_1)
#SDE
1.343e+00 
1.849e-02
1.856e-01
2.293e-02
1.160e-01
1.011e-01
3.840e-06 
"_____________________________________________"
#ASSIGNMENT 3           #ASSIGNMENT 3               #ASSIGNMENT 3               #ASSIGNMENT 3         #ASSIGNMENT 3
#START THE ASSIGNMENT   #START THE ASSIGNMENT   #START THE ASSIGNMENT   #START THE ASSIGNMENT "#START THE ASSIGNMENT

library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(tidyverse)
library(gridExtra)
library(lm.beta)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)
library(optimx)
library(influence.ME) # for influence (this will also load the lme4 package)	
library(lattice) # for qqmath	
library(lme4) # for mixed models	
library(lmerTest) 
# ## Custom functions	

# ## Custom function	

# This is a function to extract standardized beta coefficients from linear mixed models.	
# This function was adapted from: https://stackoverflow.com/questions/25142901/standardized-coefficients-for-lmer-model	


stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	
# We will use these custom functions to get bootstrapped confidence intervals.	


# function to obtain regression coefficients	
# source: https://www.statmethods.net/advstats/bootstrapping.html	
bs_to_boot <- function(model, data, indices) {	
  d <- data[indices,] # allows boot to select sample 	
  fit <- lm(formula(model), data=d)	
  return(coef(fit)) 	
}	

# function to obtain adjusted R^2	
# source: https://www.statmethods.net/advstats/bootstrapping.html (partially modified)	
adjR2_to_boot <- function(model, data, indices) {	
  d <- data[indices,] # allows boot to select sample 	
  fit <- lm(formula(model), data=d)	
  return(summary(fit)$adj.r.squared)	
}	


# Computing the booststrap BCa (bias-corrected and accelerated) bootstrap confidence intervals by Elfron (1987)	
# This is useful if there is bias or skew in the residuals.	

confint.boot <- function(model, data = NULL, R = 1000){	
  if(is.null(data)){	
    data = eval(parse(text = as.character(model$call[3])))	
  }	
  boot.ci_output_table = as.data.frame(matrix(NA, nrow = length(coef(model)), ncol = 2))	
  row.names(boot.ci_output_table) = names(coef(model))	
  names(boot.ci_output_table) = c("boot 2.5 %", "boot 97.5 %")	
  results.boot = results <- boot(data=data, statistic=bs_to_boot, 	
                                 R=1000, model = model)	
  
  for(i in 1:length(coef(model))){	
    boot.ci_output_table[i,] = unlist(unlist(boot.ci(results.boot, type="bca", index=i))[c("bca4", "bca5")])	
  }	
  
  return(boot.ci_output_table)	
}	

# Computing the booststrapped confidence interval for a linear model using wild bottstrapping as descibed by Wu (1986) <doi:10.1214/aos/1176350142>	
# requires the lmboot pakcage	

wild.boot.confint <- function(model, data = NULL, B = 1000){	
  if(is.null(data)){	
    data = eval(parse(text = as.character(model$call[3])))	
  }	
  
  wild_boot_estimates = wild.boot(formula(model), data = data, B = B)	
  
  result = t(apply(wild_boot_estimates[[1]], 2, function(x) quantile(x,probs=c(.025,.975))))	
  
  return(result)	
  
}	

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

"____________________________________________________________________"

#START THE ASSIGNMENT 
#starting with dataset d_ass3
d_ass3= read.csv("https://tinyurl.com/ha-dataset3")
d_ass4= read.csv("https://tinyurl.com/ha-dataset4")

d_ass3_corr <- d_ass3
View(d_ass3_corr)

describe(d_ass3)
summary(d_ass3)

#transforming all hospitals to factor 


d_ass3_corr$hospital[d_ass3_corr$hospital == "hospital_1"] <- 1
d_ass3_corr$hospital[d_ass3_corr$hospital == "hospital_2"] <- 2
d_ass3_corr$hospital[d_ass3_corr$hospital == "hospital_3"] <- 3
d_ass3_corr$hospital[d_ass3_corr$hospital == "hospital_4"] <- 4
d_ass3_corr$hospital[d_ass3_corr$hospital == "hospital_5"] <- 5
d_ass3_corr$hospital[d_ass3_corr$hospital == "hospital_6"] <- 6
d_ass3_corr$hospital[d_ass3_corr$hospital == "hospital_7"] <- 7
d_ass3_corr$hospital[d_ass3_corr$hospital == "hospital_8"] <- 8
d_ass3_corr$hospital[d_ass3_corr$hospital == "hospital_9"] <- 9
d_ass3_corr$hospital[d_ass3_corr$hospital == "hospital_10"] <- 10
d_ass3_corr <- d_ass3_corr[-c(80),]
d_ass3_corr$sex[d_ass3_corr$sex == "femlae"] <- "female"

d_ass3_corr %>% 	
  mutate(hospital = factor(hospital))


View(d_ass4)

d_ass4_corr <- d_ass4
describe(d_ass4) #midnfulnes???
summary(d_ass4)

d_ass4_corr %>% 	
  mutate(hospital = factor(hospital))

#correcting the data in d_ass4_corr

d_ass4_corr$household_income[d_ass4_corr$household_income == -3409] <- 3409
d_ass4_corr$household_income[d_ass4_corr$household_income == -23482] <- 23482
d_ass4_corr <- d_ass4_corr[-c(80),]

d_ass4_corr$hospital[d_ass4_corr$hospital == "hospital_11"] <- 1
d_ass4_corr$hospital[d_ass4_corr$hospital == "hospital_12"] <- 2
d_ass4_corr$hospital[d_ass4_corr$hospital == "hospital_13"] <- 3
d_ass4_corr$hospital[d_ass4_corr$hospital == "hospital_14"] <- 4
d_ass4_corr$hospital[d_ass4_corr$hospital == "hospital_15"] <- 5
d_ass4_corr$hospital[d_ass4_corr$hospital == "hospital_16"] <- 6
d_ass4_corr$hospital[d_ass4_corr$hospital == "hospital_17"] <- 7
d_ass4_corr$hospital[d_ass4_corr$hospital == "hospital_18"] <- 8
d_ass4_corr$hospital[d_ass4_corr$hospital == "hospital_19"] <- 9
d_ass4_corr$hospital[d_ass4_corr$hospital == "hospital_20"] <- 10
View(d_ass4_corr)

d_ass4_corr %>% 	
  mutate(hospital = factor(hospital))

"______________________________________________________________________________________"
#histograms 
plot_pain1 = d_ass3_corr %>%ggplot() +aes(x = pain) +geom_histogram(bins = 50)
plot_age1 = d_ass3_corr %>%ggplot() +aes(x = age) +geom_histogram(bins = 50)
plot_sex1 = d_ass3_corr %>%ggplot() +aes(x = sex) +geom_histogram(bins = 50)
plot_pain_cat1 = d_ass3_corr %>%ggplot() +aes(x = pain_cat) +geom_histogram(bins = 50)
plot_IQ1 = d_ass3_corr %>%ggplot() +aes(x = IQ) +geom_histogram(bins = 50)  
plot_STAI1 = d_ass3_corr %>%ggplot() +aes(x = STAI_trait) +geom_histogram(bins = 50)  
plot_mindfulness1 = d_ass3_corr %>%ggplot() +aes(x = mindfulness) +geom_histogram(bins = 50) 
plot_coritsolserum1 = d_ass3_corr %>%ggplot() +aes(x = cortisol_serum) +geom_histogram(bins = 50) 
plot_cortisolsaliva1 = d_ass3_corr %>%ggplot() +aes(x = cortisol_saliva) +geom_histogram(bins = 50) 
plot_householdincome =d_ass3_corr %>%ggplot() +aes(x = household_income) +geom_histogram(bins = 50)


d_ass3_corr %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)		

d_ass3_corr %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)



pain_hospital_plot_paincat = d_ass3_corr %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)

pain_hospital_plot_cortisol = d_ass3_corr %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)

pain_hospital_plot_paincat # showing all regressionlines for ea hospital, very clustered 


pain_hospital_plot_paincat+		
  xlim(-1, 50)+		
  geom_hline(yintercept=0)+		
  geom_vline(xintercept=0)
"_________________________________________________________________________________________________________________"
#correcting data 
d_ass3_corr$household_income[d_ass3_corr$household_income == -6994] <- 6994 #changed codingerror 

"__________________________________________________________________________________________________________________"

#creating the mixed regression model on dataset= d_ass3_corr
#we do 3 modles, fixed effect model, random intercept model and random intercept +  slop
# i will chose the same variabels proven and grounded in theory = mod_5_LM

#FIXED MODEL
mod_fixed = lm(pain ~ age + sex +  STAI_trait + pain_cat + mindfulness + cortisol_serum, data = d_ass3_corr)
summary(mod_fixed) # Adjusted R-squared:  0.4741
AIC(mod_fixed) #653.469
coef_table(mod_fixed)

#RANDOM INTERCEPT #HERE YOU ADD HOSPITAL SINCE YOU KNOW THIS IS NOT FIXED 
mod_rnd_int = lmer(pain ~ age + sex +  STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = d_ass3_corr)	
summary(mod_rnd_int) #NOTE THE COEFFCIENTS FOR THE FIEX PREDICTORS = 
cAIC(mod_rnd_int) #653.33
"_________________________________________________________________________________________________________________________________"

#vizualization of random intercept model

data_hosp_slope = d_ass3_corr %>% 		
  mutate(pred_int = predict(mod_rnd_int))	


data_hosp_slope %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat, group = hospital)+		
  geom_point(aes(color = hospital), size = 4) +		
  geom_line(color='red', aes(y=pred_int, x=pain_cat))+		
  facet_wrap( ~ hospital, ncol = 2)	# i used pain_cat for fun 



#marginal R squared and CONFIDENCE INTERVALLS 
r2beta(mod_rnd_int, method = "nsj", data = d_ass3_corr)

r.squaredGLMM(mod_rnd_int) #Marginal and conditional r squarde values 

confint(mod_rnd_int) # CONFICENDE INTERVALL for the model  IF THE RANDOM PREDICTORE HAS NO EFFECT R2C WILL BE CLOSE TO r2M THIS IS NOT THE CASE

stdCoef.merMod(mod_rnd_int) #standardized beta

#get_variance(mod)

"______________________________________________________________________________________________________________________________"
#TIME TO PREFORM TEST predict() on new dataset = d_ass4

mod_mean <-lm(pain ~ 1, data = d_ass4_corr)
RSS_mod_rnd_intercept = sum(residuals(mod_rnd_int)^2)
TSS11 =sum((d_ass4_corr$pain- predict(mod_mean))^2)
1-RSS_mod_rnd_intercept/TSS11 # The r2 is 0.396 very close to the r2m of the traindataset
"____________________________________"



AIC(mod_fixed) 
cAIC(mod_rnd_int)$caic #


mod_mean_data3 <-lm(pain ~ 1, data = d_ass3_corr)


Anova = anova(mod_rnd_int, mod_fixed)  #When you compar regular models you get F, but when mixed you get CHISQ HERE ITS 21.2

"______________________________________________________________________________________________________________________"

#TESTARE BARA EN MOD

mod_rnd_int_slopezaza = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = d_ass3_corr) # line need to be staright 
summary(mod_rnd_int_slopezaza)

r2beta(mod_rnd_int_slopezaza, method = "nsj", data = d_ass3_corr)

r.squaredGLMM(mod_rnd_int_slopezaza) #Marginal and conditional r squarde values 

confint(mod_rnd_int) # CONFICENDE INTERVALL for the model  IF THE RANDOM PREDICTORE HAS NO EFFECT R2C WILL BE CLOSE TO r2M THIS IS NOT THE CASE

stdCoef.merMod(mod_rnd_int)


data_hosp_slope_newzaza = d_ass3_corr %>% 		
  mutate(pred_int = predict(mod_rnd_int_slopezaza))

data_hosp_slope_newzaza %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 4) +		
  geom_line(color='red', aes(y=pred_int, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)	#go to expoprt plopplotwindo
"_______________________________________________________________________________________________________________________________________"







