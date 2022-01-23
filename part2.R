rm(list = ls())

library(arm)
library(rms)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
library(plyr)
require(gridExtra)


##########################################################################
############################ 0. Loading the data #########################
##########################################################################


data <- read.csv("lalondedata.txt", sep=',')

head(data)
str(data)
dim(data)
summary(data)

data$X <- NULL
data$treat <- as.factor(data$treat)
data$black <- as.factor(data$black)
data$hispan <- as.factor(data$hispan)
data$married <- as.factor(data$married)
data$nodegree <- as.factor(data$nodegree)
data$pos_income <- as.integer(data$re78 > 0)
data$pos_income_f <- as.factor(data$pos_income)
data$re74_bin <- ifelse(data$re74>0, 1, 0)
data$re74_bin <- as.factor(data$re74_bin)

##########################################################################
############################### 1. EDA ###################################
##########################################################################


cond_prob <- function (df, col1, col2) {
  round(apply(table(df[, c(col1, col2)])/sum(table(df[, c(col1, col2)])),
              2,function(x) x/sum(x)), 2)
}


cond_prob(data, "pos_income_f",  "treat")
chisq.test(table(data[,c("pos_income_f", "treat")]))

cond_prob(data, "pos_income_f",  "black")
chisq.test(table(data[,c("pos_income_f", "black")]))  # **

cond_prob(data, "pos_income_f",  "hispan")
chisq.test(table(data[,c("pos_income_f", "hispan")]))

cond_prob(data, "pos_income_f",  "married")
chisq.test(table(data[,c("pos_income_f", "married")]))

cond_prob(data, "pos_income_f",  "nodegree")
chisq.test(table(data[,c("pos_income_f", "nodegree")]))

cond_prob(data, "pos_income_f",  "re74_bin")
chisq.test(table(data[,c("pos_income_f", "re74_bin")])) # **


ggplot(data,aes(x=pos_income_f, y=age, fill=pos_income_f)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  theme_classic()

ggplot(data,aes(x=pos_income_f, y=educ, fill=pos_income_f)) +  # **
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  theme_classic()

ggplot(data,aes(x=pos_income_f, y=re74, fill=pos_income_f)) +  # **
  geom_boxplot() +
  scale_fill_brewer(palette="Reds") + 
  theme_classic()

binnedplot(x=data$re74, y=data$pos_income,
           xlab="Pred. probabilities",
           col.int="red4", ylab="Avg. residuals",
           main="Binned residual plot", col.pts="navy")


# Interactions
cond_prob(data[data$black == 1,], "pos_income_f",  "treat")    # **  
cond_prob(data[data$black == 0,], "pos_income_f",  "treat")
table(data[, c("treat", "black")])


cond_prob(data[data$hispan == 1,], "pos_income_f",  "treat")  # no data points 
cond_prob(data[data$hispan == 0,], "pos_income_f",  "treat")  # treat1 pos_income0

data$sum_r <- (as.integer(data$black) - 1) + (as.integer(data$hispan) - 1)
table(data$sum_r) # unique race

cond_prob(data[data$married == 1,], "pos_income_f",  "treat")  # ** 
cond_prob(data[data$married == 0,], "pos_income_f",  "treat")

cond_prob(data[data$nodegree == 1,], "pos_income_f",  "treat")   
cond_prob(data[data$nodegree == 0,], "pos_income_f",  "treat")


##########################################################################
############################### 2. Model Selection #######################
##########################################################################


null_model <- glm(pos_income_f ~ treat, data = data, family=binomial)
full_model <- glm(pos_income_f ~ treat + black + hispan + 
                    age + educ + married + re74 + nodegree,
                  data=data, family=binomial)
step_model <- step(null_model, scope = list(lower=null_model, upper=full_model),
                   direction='both', trace=0)
summary(step_model)

# Added interaction
black_int <- glm(pos_income_f ~ treat + black + 
                   age + re74 + treat * black,
                 data=data, family=binomial)
anova(step_model, black_int, test="Chisq")

married_int <- glm(pos_income_f ~ treat + black + 
      age + re74 + treat * married,
    data=data, family=binomial)
anova(step_model, married_int, test="Chisq")

hisp_int <- glm(pos_income_f ~ treat + black + age + re74 + treat * hispan,
                data=data, family=binomial)
anova(step_model, hisp_int, test="Chisq")

without_treat <- glm(pos_income_f ~ black + 
                       age + re74,
                     data=data, family=binomial)
anova(step_model, without_treat, test="Chisq")

age_int <- glm(pos_income_f ~ treat + black + 
                       age + re74 + treat * age,
                     data=data, family=binomial)
anova(step_model, age_int, test="Chisq")

##########################################################################
############################### 3.Model assessment #######################
##########################################################################


rawresid1 <- residuals(age_int, "resp")
binnedplot(x=fitted(age_int),y=rawresid1,
           xlab="Pred. probabilities",
           col.int="red4", ylab="Avg. residuals",
           main="Binned residual plot", col.pts="navy")

binnedplot(x=data$age,y=rawresid1,
           xlab="Age",
           col.int="red4", ylab="Avg. residuals",
           main="Binned residual plot", col.pts="navy")

binnedplot(x=data$re74,y=rawresid1,
           xlab="Age",
           col.int="red4", ylab="Avg. residuals",
           main="Binned residual plot", col.pts="navy")

##########################################################################
############################### 4. Inference #############################
##########################################################################


age_int_matrix <- confusionMatrix(as.factor(ifelse(fitted(age_int) >= 0.5, "1", "0")),
                                     data$pos_income_f, positive = "1")
age_int_matrix$overall["Accuracy"]
sum(as.integer(data$pos_income)) / nrow(data) # baseline
age_int_matrix$byClass[c("Sensitivity","Specificity")]

roc(data$pos_income_f, fitted(age_int),
    plot=T, print.thres="best", legacy.axes=T,
    print.auc =T, col="red3")

age_int_matrix_best_level <- confusionMatrix(as.factor(ifelse(fitted(age_int) >= 0.737, "1", "0")),
                                     data$pos_income_f, positive = "1")
age_int_matrix_best_level$overall["Accuracy"]
age_int_matrix_best_level$byClass[c("Sensitivity","Specificity")]

summary(age_int)
confint(age_int)
exp(confint(age_int))
exp(age_int$coefficients)


##########################################################################
############################### 5. Extra #################################
##########################################################################


treat <- rep(1, 120)
black <- rep(1, 120)
age <- 1:60
re74 <- mean(data$re74)
new_data <- data.frame(treat, black, age, re74)
new_data[60:120, 1] <- 0
new_data$black <- as.factor(new_data$black)
new_data$treat <- as.factor(new_data$treat)

new_data$prediction <- predict(age_int, new_data, "response")

ggplot(new_data, aes(x = age, y = prediction)) + 
  geom_point(aes(color = treat, linetype = treat)) + 
  scale_color_manual(values = c("darkred", "steelblue")) + theme_classic()


##########################################################################
############################### 6. Extra 2 ###############################
##########################################################################


# Cubic transformation
age_int_cub <- glm(pos_income_f ~ treat + black + 
                     poly(age, 3) + re74 + treat * age,
                   data=data, family=binomial)
rawresid2 <- residuals(age_int_cub, "resp")
binnedplot(x=fitted(age_int_cub),y=rawresid2,
           xlab="Pred. probabilities",
           col.int="red4", ylab="Avg. residuals",
           main="Binned residual plot", col.pts="navy")

binnedplot(x=data$age,y=rawresid2,
           xlab="Age",
           col.int="red4", ylab="Avg. residuals",
           main="Binned residual plot", col.pts="navy") # don't think it is helpful

binnedplot(x=data$re74,y=rawresid2,
           xlab="Age",
           col.int="red4", ylab="Avg. residuals",
           main="Binned residual plot", col.pts="navy")

roc(data$pos_income_f, fitted(age_int_cub),
    plot=T, print.thres="best", legacy.axes=T,
    print.auc =T, col="red3") # not changing AUC 


# Binary re74 instead of continuous
age_int_bin <- glm(pos_income_f ~ treat + black + 
                 age + re74_bin + treat * age,
               data=data, family=binomial)

rawresid3 <- residuals(age_int_bin, "resp")
binnedplot(x=fitted(age_int_bin),y=rawresid3,
           xlab="Pred. probabilities",
           col.int="red4", ylab="Avg. residuals",
           main="Binned residual plot", col.pts="navy")

binnedplot(x=data$age,y=rawresid3,
           xlab="Age",
           col.int="red4", ylab="Avg. residuals",
           main="Binned residual plot", col.pts="navy")


roc(data$pos_income_f, fitted(age_int_bin),
    plot=T, print.thres="best", legacy.axes=T,
    print.auc =T, col="red3") # AUC is lower than our model
