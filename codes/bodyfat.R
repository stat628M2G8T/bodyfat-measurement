### Import data
rm(list = ls())
bodyfat_original = read.csv("data/bodyfat.csv", header = TRUE)
str(bodyfat_original)
colnames(bodyfat_original) = tolower(variable.names(bodyfat_original))

### Remove IDNO variable
bodyfat_original = bodyfat_original[,-1]

#######################################################################################
### Data cleaning
## first and last few observations 
head(bodyfat_original)
tail(bodyfat_original)

## summary of data
summary(bodyfat_original)

# Recalculate bodyfat for observations with extreme value of bodyfat using siri equation
which.min(bodyfat_original$bodyfat)    # 182
495/bodyfat_original$density[182]-450  # bodyfat = -3.611687
which.max(bodyfat_original$bodyfat)    # 216
495/bodyfat_original$density[216]-450  # bodyfat = 47.48744

# Recalculate height for observation with 29.5 inches recorded using BMI
which.min(bodyfat_original$height) # 42
sqrt(bodyfat_original$weight[42]/bodyfat_original$adiposity[42]*703) # height = 69.4255 - misrecord

# Recalculate weight for observation with 363.1 lbs recorded using BMI
which.max(bodyfat_original$weight) # 39
bodyfat_original$adiposity[39]*bodyfat_original$height[39]^2/703 # 363.1 - no misrecord 

# Recalculate adiposity for observation with 48.90 recorded using weight and height
which.max(bodyfat_original$adiposity) # 39
bodyfat_original$weight[39]/bodyfat_original$height[39]^2*703  # 48.9 - no misrecord


######
## Plot bodyfat vs 1/density
plot( y = bodyfat_original$bodyfat, x = 1/bodyfat_original$density, ylab = "Body Fat Percentage",
      xlab = "1/Density", main = "Body Fat vs. 1/Density")
# Add labels to points that deviate from straight line
text(1/bodyfat_original$density[48],9,"48",col = "blue",cex=0.8)
text(1/bodyfat_original$density[76],21,"76",col = "blue",cex=0.8)
text(1/bodyfat_original$density[96],20,"96",col = "blue",cex=0.8)
## observations on plot that deviate from straight line
bodyfat_original[48,]
bodyfat_original[76,]
bodyfat_original[96,]
## Observation with body fat percentage = 0
which(bodyfat_original$bodyfat==0)
bodyfat_original[182,]


### correct the values, remove density variable and remove observations 
### 39, 182 and 216
## Replace bodyfat for obs 48, 76 with Siri equation
bodyfat_original$bodyfat[76] =  round(495/bodyfat_original$density[76]-450,digits=1)
bodyfat_original$bodyfat[48] =  round(495/bodyfat_original$density[48]-450,digits=1)

## Replace body density in bodyfat_dat for obs 96 with Siri equation
bodyfat_original$density[96] =  round(495/(450+bodyfat_original$bodyfat[96]),digits=4)

plot( y = bodyfat_original$bodyfat, x = (495/bodyfat_original$density-450), 
      ylab = "Body Fat Percentage", xlab = "1/density", 
      main = "Body Fat vs. 1/Density")                           # all points lie on straight line

## Remove density variable
bodyfat_original = bodyfat_original[,-2]

## Replace height of observation 42 with 69.4
bodyfat_original$height[42] = 69.4
## Remove observations 39, 182 and 216
bodyfat_cleaned = bodyfat_original[-c(39,182,216),]
rownames(bodyfat_cleaned) = NULL
summary(bodyfat_cleaned)
which.min(bodyfat_cleaned$bodyfat) # 171

bodyfat_cleaned = bodyfat_cleaned[-171,]
summary(bodyfat_cleaned)

write.csv(bodyfat_cleaned,file="bodyfat_cleaned.csv",row.names = FALSE)

##########################################################################################################
### Plot bodyfat vs each variable
## variables height, age and ankle do not exhibit linear relationships

par(mfrow=c(2,2))
for(i in 1:14){
  plot(bodyfat_cleaned$bodyfat~bodyfat_cleaned[,(i+1)],ylab="Bodyfat percentage",
       xlab=variable.names(bodyfat_cleaned)[i+1])
}

## Consider Boxcox Transformation
library(MASS)
par(mfrow=c(1,1))
boxcox_trans <- boxcox(bodyfat ~ ., data=bodyfat_cleaned) # lambda ~ 1


##########################################################################################################
# calculate pearson correlation coefficient between body fat percentage 
# and each predictor variable 

x = subset(bodyfat_cleaned, select = -bodyfat)
y = bodyfat_cleaned$bodyfat
correlation = cor(x, y, method = c("pearson"))
order = correlation[order(-abs(correlation[,1])),]
order

### Variable selection
## Full model
# age, abdomen and wrist are significant
full.model = lm(bodyfat~., data = bodyfat_cleaned)
summary(full.model)

## stepwise selection
# (BIC=698.57) model: bodyfat ~ abdomen + wrist + height
model.bic.both = step(full.model, direction = "both", k = log(nrow(bodyfat_cleaned)))
summary(model.bic.both)  # all variables are significant

# (AIC=681.51) model: bodyfat ~ age + adiposity + neck + chest + abdomen + hip + forearm + 
#                               wrist
model.aic.both = step(full.model, direction = "both", k = 2)
summary(model.aic.both) # age, adiposity, neck, chest, abdomen, wrist


## backward selection
# (BIC=700.87) model: bodyfat ~ age + abdomen + wrist
model.bic.backward = step(full.model, direction = "backward", k = log(nrow(bodyfat_cleaned)))
summary(model.bic.backward)  # all are significant

# (AIC=681.51) model: bodyfat ~ age + adiposity + neck + chest + abdomen + hip + forearm + 
#                               wrist
model.aic.backward= step(full.model, direction = "backward", k = 2)
summary(model.aic.backward)  # age, adiposity, neck, chest, abdomen, wrist


## forward selection
# (BIC=742.32) model: bodyfat ~ age + weight + height + adiposity + neck + chest +
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm +
#                     wrist
model.bic.forward = step(full.model, direction = "forward", k = log(nrow(bodyfat_cleaned)))
summary(model.bic.forward)  # age, abdomen and wrist are significant

# (AIC=689.62) model: bodyfat ~ age + weight + height + adiposity + neck + chest +
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm +
#                     wrist
model.aic.forward = step(full.model, direction = "forward", k = 2)
summary(model.aic.forward)  # age, abdomen, wrist are significant


## best subset selection
library(leaps)
bestsubset = regsubsets(bodyfat~., data=bodyfat_cleaned, nvmax=ncol((bodyfat_cleaned)-1))
summary.bestsubset = summary(bestsubset)
names(summary.bestsubset)
which.min(summary.bestsubset$bic)   # 3
which.max(summary.bestsubset$adjr2) # 10
summary.bestsubset$adjr2            # 3 variables = 0.7162520
                                    # 10 variables = 0.7261865 
# best subset with 3 variables: weight, abdomen, wrist
model.bestsubset = lm(bodyfat~weight+abdomen+wrist, data=bodyfat_cleaned)
summary(model.bestsubset) # all variables are significant


## Fit lasso
library(glmnet)
set.seed(100)
x = model.matrix(bodyfat~., data = bodyfat_cleaned)[,-1]
y = bodyfat_cleaned$bodyfat
model.cv.lasso = cv.glmnet(x, y, family = "gaussian", alpha = 1, type.measure = "mse")
par(mfrow=c(1,1))
plot(model.cv.lasso)
print(model.cv.lasso$lambda.1se) # lambda =  0.5314674
print(coef(model.cv.lasso, s = "lambda.1se")) # variables = age,height,abdomen,wrist


## Using Mallow's Cp criterion
library(faraway)
mallow_Cp = leaps(x,y,nbest = 1)
Cpplot(mallow_Cp)
# best: variables = 1,3,6,7,12,14
variable.names(x)[c(1,3,6,7,14)] # age, height, chest, abdomen, wrist

# fit MLR with the variables
model.mallowCp = lm(bodyfat~age+height+chest+abdomen+wrist, data=bodyfat_cleaned)
summary(model.mallowCp)   # height,abdomen and wrist are significant


##########################################################################################################
# Model evaluation

### Using predictors without transfromation

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
model1_1 = lm(bodyfat ~ weight + abdomen + wrist , data = bodyfat_cleaned)
summary(model1_1)
model1_2 = lm(bodyfat ~ age + height + abdomen + wrist , data = bodyfat_cleaned)
summary(model1_2)
model1_3 = lm(bodyfat ~ age + height + chest + abdomen + biceps + wrist , data = bodyfat_cleaned)
summary(model1_3)
model1_4 = lm(bodyfat ~ weight + height + adiposity + abdomen + biceps + wrist , data = bodyfat_cleaned)
summary(model1_4)


### Models using a combination of abdomen and wrist variables
model0_1 = lm(bodyfat ~ abdomen , data = bodyfat_cleaned)
summary(model0_1)
model0_2 = lm(bodyfat ~ wrist , data = bodyfat_cleaned)
summary(model0_2)
model1_0 = lm(bodyfat ~ abdomen + wrist, data = bodyfat_cleaned)
summary(model1_0)
par(mfrow = c(2,2))


### Logistic transformation on adiposity and ankle
bodyfat_log = as.data.frame(cbind(subset(bodyfat_cleaned,select = -c(5,12)),log(bodyfat_cleaned$adiposity),
                                  log(bodyfat_cleaned$ankle)))
colnames(bodyfat_log)[14] = "log_adiposity"
colnames(bodyfat_log)[15] = "log_ankle"

### Plot bodyfat vs log_adiposity and log_ankle
## Do not exhibit linear relationships
par(mfrow=c(2,2))
for(i in 1:2){
  plot(bodyfat_log$bodyfat~bodyfat_log[,(i+13)],ylab="Body fat percentage",
       xlab=variable.names(bodyfat_log)[i+13])
}

### Models for logistic transformation on adiposity and ankle using variables selected
### by stepwise, backward and forward selection, best subset, lasso and mallow cp's
## FB + BIC :  model: bodyfat ~ weight + height + log_adiposity + abdomen + wrist
## FB + AIC :  model: bodyfat ~ weight + height + log_adiposity + abdomen + wrist
## B  + BIC :  model: bodyfat ~ weight + height + log_adiposity + abdomen + wrist
## B  + AIC :  model: bodyfat ~ age + weight + height + adiposity + neck + chest +
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
model2_1 = lm(bodyfat ~ weight + height + log_adiposity + abdomen + wrist, data = bodyfat_log)
summary(model2_1)
model2_2 = lm(bodyfat ~ chest + age + log_adiposity + abdomen + hip + wrist, data = bodyfat_log)
summary(model2_2)


### Model diagnostics
plot(model0_1) # bodyfat ~ abdomen
plot(model0_2) # bodyfat ~ wrist
plot(model1_0) # bodyfat ~ abdomen + wrist
plot(model1_1) # bodyfat ~ weight + abdomen + wrist
plot(model2_1) # bodyfat ~ weight + height + log_adiposity + abdomen + wrist
plot(model2_2)

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
# P: model contains only 2 features, easily explained
# C: VIF become large, exist collinearity