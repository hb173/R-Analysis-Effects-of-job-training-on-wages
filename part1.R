rm(list = ls())

library(ggplot2)
library(MASS)
library(rms)
library(xtable)
library(knitr)
library(kableExtra)


##########################################################################
############################ 0. Loading the data #########################
##########################################################################


data <- read.csv("Data/lalondedata.txt", sep=',')

head(data)
str(data)
dim(data)
summary(data)

data$treat <- as.factor(data$treat)
data$black <- as.factor(data$black)
data$hispan <- as.factor(data$hispan)
data$married <- as.factor(data$married)
data$nodegree <- as.factor(data$nodegree)
data$diff_re <- data$re78 - data$re74


##########################################################################
############################### 1. EDA ###################################
##########################################################################


# Single variables
ggplot(data, aes(x=re78)) + geom_histogram() # skewed to the right
ggplot(data, aes(x=log(re78))) + geom_histogram()

ggplot(data, aes(x=diff_re)) + geom_histogram()
ggplot(data, aes(x=age)) + geom_histogram()
ggplot(data, aes(x=educ)) + geom_histogram(bins = 15)

ggplot(data, aes(x=treat, y=re78)) + geom_boxplot()
ggplot(data, aes(x=age, y=re78)) + geom_point() + geom_jitter()
ggplot(data, aes(x=educ, y=re78)) + geom_point() + geom_jitter()
ggplot(data, aes(x=black, y=re78)) + geom_boxplot()   # ***
ggplot(data, aes(x=hispan, y=re78)) + geom_boxplot()
ggplot(data, aes(x=married, y=re78)) + geom_boxplot() # ***
ggplot(data, aes(x=nodegree, y=re78)) + geom_boxplot()

# Difference between re78 and re74
ggplot(data, aes(x=treat, y=diff_re)) + geom_boxplot() # **
ggplot(data, aes(x=age, y=diff_re)) + geom_point() + geom_jitter()
ggplot(data, aes(x=educ, y=diff_re)) + geom_point() + geom_jitter()
ggplot(data, aes(x=black, y=diff_re)) + geom_boxplot()   
ggplot(data, aes(x=hispan, y=diff_re)) + geom_boxplot() # *
ggplot(data, aes(x=married, y=diff_re)) + geom_boxplot()# * 
ggplot(data, aes(x=nodegree, y=diff_re)) + geom_boxplot()


# Interactions:
ggplot(data, aes(x=treat, y=re78)) +
  geom_boxplot() +
  facet_wrap(~black, ncol=4) # **

ggplot(data, aes(x=treat, y=re78)) +
  geom_boxplot() +
  facet_wrap(~hispan, ncol=4)

ggplot(data, aes(x=treat, y=re78)) +
  geom_boxplot() +
  facet_wrap(~married, ncol=4)

ggplot(data, aes(x=treat, y=re78)) +
  geom_boxplot() +
  facet_wrap(~nodegree, ncol=4)


# Interactions difference:
ggplot(data, aes(x=treat, y=diff_re)) +
  geom_boxplot() +
  facet_wrap(~black, ncol=4)  

ggplot(data, aes(x=treat, y=diff_re)) +
  geom_boxplot() +
  facet_wrap(~hispan, ncol=4) 

ggplot(data, aes(x=treat, y=diff_re)) +
  geom_boxplot() +
  facet_wrap(~married, ncol=4)

ggplot(data, aes(x=treat, y=diff_re)) +
  geom_boxplot() +
  facet_wrap(~nodegree, ncol=4)


##########################################################################
############################### 2.Model selection Deprecated #############
##########################################################################


# Baseline model based on the EDA
baseline_model <- lm(re78 ~ treat + married + black + black*treat, data=data)
summary(baseline_model)  
plot(baseline_model)

# Model selection
# Step AIC:
null_model <- lm(re78 ~ treat, data=data)
full_model <- lm(re78 ~ treat + age + educ + black + hispan +
                   married + nodegree + re74,
                 data=data)
step_model <- step(null_model,
                   scope=formula(full_model),
                   direction='both',
                   trace=0)

# Forward AIC:
forward_model <- step(null_model,
                      scope=formula(full_model),
                      direction='forward',
                      trace=0)
summary(forward_model) # The same as stepwise

# Backward AIC:
backward_model <- step(null_model,
                       scope=formula(full_model),
                       direction='backward',
                       trace=0)
summary(backward_model) # Constant model without variables

# Step with interaction terms:
full_model2 <- lm(re78 ~ treat + age + educ + black +
                    hispan + married + nodegree + 
                    treat*black + treat*hispan,
                  data=data)
step_model2 <- step(null_model,
                    scope=formula(full_model2),
                    direction='both',
                    trace=0)
summary(step_model2)    # Interaction term wasn't chosen

# Log age
log_age_model <- lm(re78 ~ treat + educ + married + black + log(age), data=data)
summary(log_age_model)

# Model without outliers
data_wo <- data[-c(132, 182, 611),]
model_wo <- lm(re78 ~ treat + educ + married + black + age, data=data_wo)

# Adding re75
model_w75 <- lm(re78 ~ treat + educ + married + black + age + re75, data=data)

# Model diff re78 re75
null_model_d <- lm(diff_re ~ treat, data=data)
full_model_d <- lm(diff_re ~ treat + age + educ + black + hispan +
                   married + nodegree,
                 data=data)
step_model_d <- step(null_model,
                   scope=formula(full_model_d),
                   direction='both',
                   trace=0)

# Model with race and treat
int_model <- lm(re78 ~ treat + re74 + educ + black +
                  hispan * treat, data=data)
summary(int_model)
anova(step_model, int_model) # no interaction between race and treat


##########################################################################
############################### 2.Model selection ########################
##########################################################################


# Model selection
# Step AIC:
null_model <- lm(diff_re ~ treat, data=data)
full_model <- lm(diff_re ~ treat + age + educ + black + hispan +
                   married + nodegree,
                 data=data)
step_model <- step(null_model,
                   scope=formula(full_model),
                   direction='both',
                   trace=0)
summary(step_model)

########################## PRETTY TABLE BELOW ############################

summary_step = summary(step_model)
summaryprint = data.frame(summary_step$coefficients)

stars = c("***", "**", "**", "*")
starsdf = data.frame(stars)

summarydf = data.frame(cbind(round(summaryprint,2),starsdf))
colnames(summarydf) = c("Estimate","Std. Error","t value", "Pr(>|t|)","")
knitr::kable(summarydf, format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options=c("hold_position", "scale_down"))

##########################################################################

treat_age_int <- lm(diff_re ~ treat + age + married + treat * age, data=data)
anova(step_model, treat_age_int) # ***

########################## PRETTY TABLE BELOW ############################

ftest = anova(step_model, treat_age_int)
xtable(ftest)

##########################################################################

treat_mar_int <- lm(diff_re ~ treat + age + married + treat * married, data=data)
anova(step_model, treat_mar_int)

mar_age_int <- lm(diff_re ~ treat + age + married + married * age, data=data)
anova(step_model, mar_age_int)

race <- lm(diff_re ~ treat + age + married + treat * age + black + 
             black * treat + hispan + treat * hispan, data=data)
anova(treat_age_int, race)


##########################################################################
############################### 3.Model assessment #######################
##########################################################################

par(bty='n')
summary(step_model)                   
plot(step_model, which=1)             # equal variance and independence assum problem
plot(step_model, which=2)             # normality assumption?
plot(step_model, which=3)
plot(step_model, which=4)             # no influential points
plot(step_model, which=5)             # several outliers
plot(data$educ, step_model$residuals) # linearity assumption holds
plot(data$age, step_model$residuals)
vif(step_model)                       # no multicollinearity

# In pretty:
# resid vs. fitted
tempdf = data.frame(fitted=fitted(step_model), residuals=residuals(step_model))
ggplot(data=tempdf, aes(x=fitted, y=residuals)) + 
  geom_abline(slope=0, intercept=0, color='#aaaaaa', linetype='dashed') +
  geom_point(color='#061953') + 
  theme_classic() + 
  labs(x="Fitted Values", y="Residuals") + 
  ggtitle("Residuals vs. Fitted Plot") + 
  theme(plot.title=element_text(size=16, face='bold', hjust=0.5)) +
  scale_y_continuous(labels= function(x) paste("$", as.numeric(x)/1000, "k"))
  

# QQ-plot
ggplot(data=tempdf, aes(sample=residuals)) + 
  stat_qq_line(linetype='dashed', color="#888888") + 
  stat_qq(color='#061953') + 
  theme_classic() +
  labs(x="Theoretical Quantiles", y="Empirical Quantiles") + 
  ggtitle("Residuals QQ-Plot") + 
  theme(plot.title=element_text(size=16, face='bold', hjust=0.5)) +
  scale_y_continuous(labels= function(x) paste("$", as.numeric(x)/1000, "k"))

# Cooks dist
plot(step_model, which=5, pch=16, col='#061953', cex=0.8, sub="")             

# Resid vs. age
tempdf = data.frame(age=data$age, residuals=residuals(step_model))
ggplot(data=tempdf, aes(y=residuals, x=age)) +
  geom_abline(slope=0, intercept=0, color='#aaaaaa', linetype='dashed') +
  geom_point(color='#061953') + 
  theme_classic() +
  labs(x="Age", y="Residuals") + 
  ggtitle("Residuals vs. Age") + 
  theme(plot.title=element_text(size=16, face='bold', hjust=0.5)) +
  scale_y_continuous(labels= function(x) paste("$", as.numeric(x)/1000, "k"))


# Log_age model
plot(log_age_model, which=1)             
plot(log_age_model, which=2)
plot(log_age_model, which=3)
plot(log_age_model, which=4)             
plot(log_age_model, which=5)             
plot(data$educ, log_age_model$residuals) 
plot(log(data$age), log_age_model$residuals)
vif(step_model)

# Without outliers model
plot(model_wo, which=1)             
plot(model_wo, which=2)
plot(model_wo, which=3)
plot(model_wo, which=4)            
plot(model_wo, which=5)             
plot(data_wo$educ, model_wo$residuals) 
plot(data_wo$age, model_wo$residuals)
vif(step_model)

# Model with additional predictor
plot(model_wo, which=1)             
plot(model_wo, which=2)
plot(model_wo, which=3)
plot(model_wo, which=4)            
plot(model_wo, which=5)             
plot(data_wo$educ, model_wo$residuals) 
plot(data_wo$age, model_wo$residuals)
vif(step_model)

# Diff model
plot(step_model_d, which=1)             
plot(step_model_d, which=2)
plot(step_model_d, which=3)
plot(step_model_d, which=4)             
plot(step_model_d, which=5)             
plot(data$educ, step_model_d$residuals) 
plot(log(data$age), step_model_d$residuals)
vif(step_model_d)
summary(step_model_d)

# Final model
plot(treat_age_int, which=1)             
plot(treat_age_int, which=2)
plot(treat_age_int, which=3)
plot(treat_age_int, which=4)             
plot(treat_age_int, which=5)             
plot(data$educ, treat_age_int$residuals) 
plot(data$age, treat_age_int$residuals)
vif(treat_age_int)
summary(treat_age_int)


########################## PRETTY TABLE BELOW ############################
# print final model summary

summary_final = summary(treat_age_int)
summaryprint = data.frame(summary_final$coefficients)

stars = c("***",".", "***", "*", "**")
starsdf = data.frame(stars)

summarydf = data.frame(cbind(round(summaryprint,2),starsdf))
colnames(summarydf) = c("Estimate","Std. Error","t value", "Pr(>|t|)","")
knitr::kable(summarydf, format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options="hold_position")

# print final model confidence interval

confintdf = data.frame(round(confint(treat_age_int, level = 0.95),2))
colnames(confintdf) = c("Lower Bound", "Upper Bound")
knitr::kable(confintdf, format="latex", booktabs=TRUE) %>% 
  kable_styling(latex_options=("hold_position"))

##########################################################################


##########################################################################
############################### 4. Inference #############################
##########################################################################


confint(treat_age_int)
summary(treat_age_int)
summary(step_model)


##########################################################################
############################### 5. Experiments ###########################
##########################################################################


null_model_log <- lm(log(re78) ~ treat, data=data)
full_model_log <- lm(log(re78) ~ treat + age + educ + 
                       black + hisp + married + nodegree,
                     data=data)

step_model_log <- step(null_model,
                   scope=formula(full_model),
                   direction='both',
                   trace=0)

incl75 <- lm(re78 ~ . - re74, data=data)
summary(incl75)

data$re78_p <- data$re78 + 1  # many zeros in re78
data$re78_p <- log(data$re78_p)

log_mod <- lm(re78_p ~ . - re74 - re75 - re78, data = data)
summary(log_mod)
plot(log_mod)

boxcox_trans <- boxcox(step_model, lambda = seq(1, 5, length = 50))
lambda_trans <- boxcox_trans$x[boxcox_trans$y == max(boxcox_trans$y)]
lambda_trans


plot(data$age, full_model$residuals)
plot(data$educ, full_model$residuals)

new_model1 <- lm(re78 ~ treat + married + educ + black, data=data)
