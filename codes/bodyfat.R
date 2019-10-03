### Import data
rm(list = ls())
bodyfat_dat = read.csv("bodyfat.csv", header = TRUE)
str(bodyfat_dat)
colnames(bodyfat_dat) = tolower(variable.names(bodyfat_dat))

### Remove IDNO variable
bodyfat_dat = bodyfat_dat[,-1]

#######################################################################################
### Data cleaning
## first and last few observations 
head(bodyfat_dat)
tail(bodyfat_dat)

## summary of data
summary(bodyfat_dat)

# Recover bodyfat for observations with extreme value of bodyfat using siri equation
which.min(bodyfat_dat$bodyfat)    # 182
495/bodyfat_dat$density[182]-450  # bodyfat = -3.611687
which.max(bodyfat_dat$bodyfat)    # 216
495/bodyfat_dat$density[216]-450  # bodyfat = 47.48744 - no misrecord

# Recover height for observation with 29.5 inches recorded using BMI
which.min(bodyfat_dat$height) # 42
sqrt(bodyfat_dat$weight[42]/bodyfat_dat$adiposity[42]*703) # height = 69.4255 - misrecord

# Recover weight for observation with 363.1 lbs recorded using BMI
which.max(bodyfat_dat$weight) # 39
bodyfat_dat$adiposity[39]*bodyfat_dat$height[39]^2/703 # 363.1 - no misrecord 

# Recover adiposity for observation with 48.90 recorded using weight and height
which.max(bodyfat_dat$adiposity) # 39
bodyfat_dat$weight[39]/bodyfat_dat$height[39]^2*703  # 48.9 - no misrecord


######
## Plot bodyfat vs 1/density
plot( y = bodyfat_dat$bodyfat, x = 1/bodyfat_dat$density, ylab = "Bodyfat percentage",
      xlab = "1/density", main = "Bodyfat vs. 1/Density")
## Add labels to points
text(1/bodyfat_dat$density, bodyfat_dat$bodyfat, labels=rownames(bodyfat_dat),
     cex=0.9, pos=3)
## observations on plot that deviate from straight line
bodyfat_dat[48,]
bodyfat_dat[76,]
bodyfat_dat[96,]
## Observation with body fat percentage = 0
which(bodyfat_dat$bodyfat==0)
bodyfat_dat[182,]


### Remove density variable
bodyfat_1 = bodyfat_dat[,-2]


### model fitting - MLR
fit1 = lm(bodyfat~., data=bodyfat_1)
summary(fit1)
par(mfrow=c(2,2))
plot(fit1)
plot(fit1, which=4) # Cook's distance plot
abline(h = 4/(nrow(bodyfat_1)-ncol(bodyfat_1)), col='red', lty=2)

bodyfat_1[c(39,42),] # obs 42 height = 29.50 inches
# obs 39 weight = 363.15 pounds (outliers)
bodyfat_1[86,] # obs 86 age = 67


### Cleaned data set
## Replace bodyfat for obs 48, 76 with Siri equation
bodyfat_1$bodyfat[76] =  round(495/bodyfat_dat$density[76]-450,digits=1)
bodyfat_1$bodyfat[48] =  round(495/bodyfat_dat$density[48]-450,digits=1)

## Replace body density in bodyfat_dat for obs 96 with Siri equation
bodyfat_dat$density[96] =  round(495/(450+bodyfat_dat$bodyfat[96]),digits=4)

plot( y = bodyfat_1$bodyfat, x = (495/bodyfat_dat$density-450), 
      ylab = "Bodyfat percentage", xlab = "1/density", 
      main = "Bodyfat vs. 1/Density")                           # all points lie on straight line

## Replace height of observation 42 with 69.4
bodyfat_1$height[42] = 69.4
## Remove observations 39, 182 and 216
bodyfat_2 = bodyfat_1[-c(39,182,216),]
rownames(bodyfat_2) = NULL
summary(bodyfat_2)


write.csv(bodyfat_2,file="bodyfat_cleaned.csv")
###########################################################


## MLR with new data set
fit2 = lm(bodyfat~., data = bodyfat_2)
summary(fit2)
plot(fit2)
plot(fit2, which = 4)
abline(h = 4/(nrow(bodyfat_2)-ncol(bodyfat_2)), col='red', lty=2)
bodyfat_2[c(85,95,218),]

library("car")
outlierTest(fit2) # obs 219 in bodyfat_2 is a possible outlier
bodyfat_2[95,]   # data looks normal

# Check distribution of residuals
hist(fit2$residuals)
plot(fit2$fitted.values,  rstandard(fit2))

### Plotting
## Box plots for variables
par(mfrow=c(1,3))
for(i in 1:15){
  boxplot(bodyfat_2[,i], main = variable.names(bodyfat_2)[i])
  hist(bodyfat_2[,i], main = variable.names(bodyfat_2)[i])
  plot(density(bodyfat_2[,i]))
}



##########################################################################################################
### Plot bodyfat vs each variable
## variables height, age and ankle do not exhibit linear relationships
par(mfrow=c(2,2))
for(i in 1:14){
  plot(bodyfat_2$bodyfat~bodyfat_2[,(i+1)],ylab="Bodyfat percentage",
       xlab=variable.names(bodyfat_2)[i+1])
}

## Consider Boxcox Transformation
library(MASS)
boxcox_trans <- boxcox(bodyfat ~ ., data=bodyfat_2) # lambda ~ 1




##########################################################################################################
###Variable selection
## Full model
# age, abdomen and wrist are significant
full.model = lm(bodyfat~., data = bodyfat_2)
summary(full.model)


## forward and backward selection
# (BIC=705.13) model: bodyfat ~ abdomen + wrist + height
model.bic.both = step(full.model, direction = "both", k = log(nrow(bodyfat_2)))
summary(model.bic.both)  # all variables are significant

# (AIC=686.5) model: bodyfat ~ age + adiposity + neck + chest + abdomen + hip + forearm + 
#                               wrist
model.aic.both = step(full.model, direction = "both", k = 2)
summary(model.aic.both) # age, adiposity, chest, abdomen, wrist


## backward selection
# (BIC=705.81) model: bodyfat ~ age + abdomen + wrist
model.bic.backward = step(full.model, direction = "backward", k = log(nrow(bodyfat_2)))
summary(model.bic.backward)  # all are significant

# (AIC=686.5) model: bodyfat ~ age + adiposity + neck + chest + abdomen + hip + forearm + 
#                              wrist
model.aic.backward= step(full.model, direction = "backward", k = 2)
summary(model.aic.backward)  # age, adiposity, chest, abdomen, wrist


## forward selection
# (BIC=747.02) model: bodyfat ~ age + weight + height + adiposity + neck + chest +
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm +
#                     wrist
model.bic.forward = step(full.model, direction = "forward", k = log(nrow(bodyfat_2)))
summary(model.bic.forward)  # age, abdomen and wrist are significant

# (AIC=694.26) model: bodyfat ~ age + weight + height + adiposity + neck + chest +
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm +
#                     wrist
model.aic.forward = step(full.model, direction = "forward", k = 2)
summary(model.aic.forward)  # age, abdomen, wrist are significant


## best subset selection
library(leaps)
bestsubset = regsubsets(bodyfat~., data=bodyfat_2, nvmax=ncol((bodyfat_2)-1))
summary.bestsubset = summary(bestsubset)
names(summary.bestsubset)
which.min(summary.bestsubset$bic)   # 3
which.max(summary.bestsubset$adjr2) # 10
summary.bestsubset$adjr2            # 3 variables = 0.7162520
                                    # 10 variables = 0.7261865 
# best subset with 3 variables: weight, abdomen, wrist
model.bestsubset = lm(bodyfat~weight+abdomen+wrist, data=bodyfat_2)
summary(model.bestsubset) # all variables are significant


## Fit lasso
library(glmnet)
x = model.matrix(bodyfat~., data = bodyfat_2)[,-1]
y = bodyfat_2$bodyfat
model.cv.lasso = cv.glmnet(x, y, family = "gaussian", alpha = 1, type.measure = "mse")
par(mfrow=c(1,1))
plot(model.cv.lasso)
print(model.cv.lasso$lambda.min) # lambda = 0.04361805
print(model.cv.lasso$lambda.1se) # lambda = 0.3706459
print(coef(model.cv.lasso, s = "lambda.min")) # all variables except weight, adiposity, knee
print(coef(model.cv.lasso, s = "lambda.1se")) # variables = age,height,abdomen,wrist


## Using Mallow's Cp criterion
library(faraway)
mallow_Cp = leaps(x,y,nbest = 1)
Cpplot(mallow_Cp)
# best: variables = 1,3,6,7,12,14
variable.names(x)[c(1,3,6,7,12,14)] # age, height, chest, abdomen, biceps, wrist

# fit MLR with the variables
model.mallowCp = lm(bodyfat~age+height+chest+abdomen+biceps+wrist, data=bodyfat_2)
summary(model.mallowCp)   # age,height,chest,abdomen and wrist are significant


##########################################################################################################
# Model evaluation

### Normal one, withoout transfromation

### Models
### FB + BIC :  model: bodyfat ~ weight + abdomen + wrist
### FB + AIC :  model: bodyfat ~ weight + height + adiposity + abdomen + biceps + wrist
### B  + BIC :  model: bodyfat ~ weight + abdomen + wrist
### B  + AIC :  model: bodyfat ~ weight + height + adiposity + abdomen + biceps + wrist
### F  + BIC :  model: bodyfat ~ age + weight + height + adiposity + neck + chest +
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm +
#                     wrist
### F  + AIC :  model: bodyfat ~ age + weight + height + adiposity + neck + chest +
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm +
#                     wrist
### BS       :  # best subset with 3 variables: weight, abdomen, wrist
### LASSO+cv+min    :  # variables = age,height,neck,abdomen,wrist
### LASSO+cv+lse    :  # variables = age,height,abdomen,wrist
### MallowCP :   # age, height, chest, abdomen, biceps, wrist

### Select useful model with not too many variables
model1_1 = lm(bodyfat ~ weight + abdomen + wrist , data = bodyfat_train)
summary(model1_1)
model1_2 = lm(bodyfat ~ age + height + abdomen + wrist , data = bodyfat_train)
summary(model1_2)
model1_3 = lm(bodyfat ~ age + height + chest + abdomen + biceps + wrist , data = bodyfat_train)
summary(model1_3)
model1_4 = lm(bodyfat ~ weight + height + adiposity + abdomen + biceps + wrist , data = bodyfat_train)
summary(model1_4)

### Logistic transformation

### Models
### FB + BIC :  model: bodyfat ~ weight + height + log_adiposity + abdomen + wrist
### FB + AIC :  model: bodyfat ~ weight + height + log_adiposity + abdomen + wrist
### B  + BIC :  model: bodyfat ~ weight + height + log_adiposity + abdomen + wrist
### B  + AIC :  model: bodyfat ~ age + weight + height + adiposity + neck + chest +
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm +
#                     wrist
### F  + BIC : bodyfat ~ age + weight + height + adiposity + neck + chest +
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm +
#                     wrist
### F  + AIC : bodyfat ~ age + weight + height + adiposity + neck + chest +
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm +
#                     wrist
### BS       :  # best subset with 5 variables: weight, height, log_adiposity, abdomen, wrist
### LASSO+cv+min    :  # variables = age,height,neck,abdomen,wrist
### LASSO+cv+lse    :  # variables = age,height,abdomen,wrist
### MallowCP :   # age, log_adiposity, chest, abdomen,hip, wrist

### Select useful model with not too many variables
model2_1 = lm(bodyfat ~ weight + height + log_adiposity + abdomen + wrist, data = bodyfat_log_train)
summary(model2_1)
model2_2 = lm(bodyfat ~ chest + age + log_adiposity + abdomen + hip + wrist, data = bodyfat_log_train)
summary(model2_2)


model0_1 = lm(bodyfat ~ abdomen , data = bodyfat_train)
summary(model0_1)
model0_2 = lm(bodyfat ~ wrist , data = bodyfat_train)
summary(model0_2)
model1_0 = lm(bodyfat ~ abdomen + wrist, data = bodyfat_train)
summary(model1_0)
par(mfrow = c(2,2))

### Check the diagnostic plot

plot(model0_1) # bodyfat ~ abdomen
plot(model0_2) # bodyfat ~ wrist
plot(model1_0) # bodyfat ~ abdomen + wrist
plot(model1_1) # bodyfat ~ weight + abdomen + wrist
plot(model2_1) # bodyfat ~ weight + height + log_adiposity + abdomen + wrist
plot(model2_3)

### We pick the model1_0, which is bodyfat ~ abdomen + wrist

library('stringr')
modelout = function(model_in){
  colname = str_c(colnames(model_in$model),collapse = "+")
  colname = sub("bodyfat\\+","",colname)
  out = summary(model_in)
  R2 = round(out$adj.r.squared,4)
  MSE = round(out$sigma,4)
  output = c(colname,R2,MSE)
  return(output)
}

### Comparison of R2 and MSE for different models
input0_1 = modelout(model0_1)
input1_0 = modelout(model1_0)
input1_1 = modelout(model1_1)
input1_2 = modelout(model1_2)
input1_3 = modelout(model1_3)
input1_4 = modelout(model1_4)
input2_1 = modelout(model2_1)
input2_2 = modelout(model2_2)
model_data = data.frame(rbind(input0_1,input1_0,input1_1,input1_2,input1_3,input1_4,
                              input2_1,input2_2))
colnames(model_data) = c("features","R2","MSE")
model_data

## ANOVA test on the significance of features
anova(model1_0,model1_1)
anova(model1_1,model2_1)

### Estimation
#######################################################
#### Coefficients & Confidence Interval
model1_1$coefficients
confint(model1_1)
### Model Assumptions and Diagnostics
par(mfrow = c(2,2))
plot(model1_1)
## Collinearity
vif(model1_1)
vif(model1_0)

## Strength & Weakness
# P: only 3 features, easily explained
# C: VIF become large, exist collinearity