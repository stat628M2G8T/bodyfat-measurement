### Import data
bodyfat_dat = read.csv("bodyfat.csv", header = TRUE)
str(bodyfat_dat)
colnames(bodyfat_dat) = tolower(variable.names(bodyfat_dat))

### Remove IDNO variable
bodyfat_dat = bodyfat_dat[,-1]

## Find correlation
a = cor(subset(bodyfat_dat, select = -bodyfat))
a[lower.tri(a,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
a=as.data.frame(as.table(a))  #Turn into a 3-column table
a=na.omit(a)  #Get rid of the junk we flagged above
a=a[order(-abs(a$Freq)),]    #Sort by highest correlation (whether +ve or -ve)
a


## Plot bodyfat vs 1/density
plot( y = bodyfat_dat$bodyfat, x = 1/bodyfat_dat$density, ylab = "Bodyfat percentage",
      xlab = "1/density", main = "Bodyfat vs. 1/Density")
## Add labels to points
text(1/bodyfat_dat$density, bodyfat_dat$bodyfat, labels=rownames(bodyfat_dat),
     cex=0.9, pos=3)
## Indicate observations on plot that deviate from straight line
bodyfat_dat[48,]
bodyfat_dat[76,]
bodyfat_dat[96,]
## Observation with body fat percentage = 0
which(bodyfat_dat$bodyfat==0) 
bodyfat_dat[182,]


### Remove density variable
bodyfat_1 = bodyfat_dat[,-2]

## Find correlation
b = cor(subset(bodyfat_1, select = -bodyfat))
b[lower.tri(b,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
b=as.data.frame(as.table(b))  #Turn into a 3-column table
b=na.omit(b)  #Get rid of NA
b=b[order(-abs(b$Freq)),] 
b


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
# Estimated height of obs 42 using BMI = 69.5 inches


## MLR with obs 39, 42 removed
fit2 = lm(bodyfat~., data = bodyfat_1[-c(39,42),])
summary(fit2)
plot(fit2)
plot(fit2, which = 4)
abline(h = 4/(nrow(bodyfat_1)-ncol(bodyfat_1)), col='red', lty=2)
bodyfat_1[c(41,86,221),]

### Check for outliers
library(car)
outlierTest(fit2)  # obs 224 is a possible outlier
bodyfat_1[224,]    # data looks normal

### Remove observations 39, 42, 48, 76, 96, 182
bodyfat_2 = bodyfat_1[-c(39,42,48,76,96,182),]
rownames(bodyfat_2) = NULL


## MLR with observations 39, 42, 48, 76, 96, 182 removed
fit3 = lm(bodyfat~., data = bodyfat_2)
summary(fit3)
plot(fit3)
plot(fit3, which = 4)
abline(h = 4/(nrow(bodyfat_2)-ncol(bodyfat_2)), col='red', lty=2)
outlierTest(fit3) # obs 218 in bodyfat_2 is a possible outlier 
bodyfat_2[218,]   # data looks normal

# Check distribution of residuals
hist(fit3$residuals)
plot(fit3$fitted.values,  rstandard(fit3))


### Plotting
## Box plots for variables
par(mfrow=c(1,3))
for(i in 1:15){
  boxplot(bodyfat_2[,i], main = variable.names(bodyfat_2)[i])
  hist(bodyfat_2[,i], main = variable.names(bodyfat_2)[i])
  plot(density(bodyfat_2[,i]))
}


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
## Split bodyfat_2 into train and test sets
set.seed(1)
train = sample(nrow(bodyfat_2), 0.7*nrow(bodyfat_2), replace = FALSE)
bodyfat_train = bodyfat_2[train,]
bodyfat_test = bodyfat_2[-train,]


## Full model
# weight, adiposity, abdomen and wrist are significant
full.model = lm(bodyfat~., data = bodyfat_train)
summary(full.model)


## forward and backward selection
# (BIC=490.96) model: bodyfat ~ weight + abdomen + wrist
model.bic.both = step(full.model, direction = "both", k = log(nrow(bodyfat_train)))
summary(model.bic.both)  # all variables are significant

# (AIC=476.49) model: bodyfat ~ weight + height + adiposity + abdomen + biceps + wrist
model.aic.both = step(full.model, direction = "both", k = 2)
summary(model.aic.both) # all variables except biceps are significant


## backward selection
# (BIC=490.96) model: bodyfat ~ weight + abdomen + wrist
model.bic.backward = step(full.model, direction = "backward", k = log(nrow(bodyfat_train)))
summary(model.bic.backward)  # all are significant

# (AIC=476.49) model: bodyfat ~ weight + height + adiposity + abdomen + biceps + wrist
model.aic.backward= step(full.model, direction = "backward", k = 2)
summary(model.aic.backward)  # all except biceps are significant


## forward selection
# (BIC=533.29) model: bodyfat ~ age + weight + height + adiposity + neck + chest + 
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm + 
#                     wrist
model.bic.forward = step(full.model, direction = "forward", k = log(nrow(bodyfat_train)))
summary(model.bic.forward)  # weight, adiposity, abdomen and wrist are significant

# (AIC=688.37) model: bodyfat ~ age + weight + height + adiposity + neck + chest + 
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm + 
#                     wrist
model.aic.forward = step(full.model, direction = "forward", k = 2)
summary(model.aic.forward)  # weight, adiposity, abdomen and wrist are significant


## best subset selection
library(leaps)
bestsubset = regsubsets(bodyfat~., data=bodyfat_train, nvmax=ncol((bodyfat_train)-1))
summary.bestsubset = summary(bestsubset)
names(summary.bestsubset)
which.min(summary.bestsubset$bic)   # 3
which.max(summary.bestsubset$adjr2) 
summary.bestsubset$adjr2            # 3 variables = 0.6870708  
# 8 variables = 0.6972099
# best subset with 3 variables: weight, abdomen, wrist
model.bestsubset = lm(bodyfat~weight+abdomen+wrist, data=bodyfat_train)
summary(model.bestsubset) # all variables are significant


## Fit lasso
library(glmnet)
x = model.matrix(bodyfat~., data = bodyfat_2)[,-1]
y = bodyfat_2$bodyfat
model.cv.lasso = cv.glmnet(x, y, family = "gaussian", alpha = 1, type.measure = "mse")
par(mfrow=c(1,1))
plot(model.cv.lasso)
print(model.cv.lasso$lambda.min) # lambda = 0.1509559
print(model.cv.lasso$lambda.1se) # lambda = 0.4609976
print(coef(model.cv.lasso, s = "lambda.min")) # variables = age,height,neck,abdomen,wrist
print(coef(model.cv.lasso, s = "lambda.1se")) # variables = age,height,abdomen,wrist


## Using Mallow's Cp criterion
library(faraway)
mallow_Cp = leaps(x,y,nbest = 1)
Cpplot(mallow_Cp)
which.min(mallow_Cp$Cp)# 6 variables
min(mallow_Cp$Cp)      # best: variables = 1,3,6,7,12,14
variable.names(x)[c(1,3,6,7,12,14)] # age, height, chest, abdomen, biceps, wrist 

# fit MLR with the variables
model.mallowCp = lm(bodyfat~age+height+chest+abdomen+biceps+wrist, data=bodyfat_train)
summary(model.mallowCp)



##########################################################################################################
### create dataset with log-transformed independent variables 
## the following model fitting section is for dataset with log-transformed independent variables
bodyfat_3 = bodyfat_2
bodyfat_3$adiposity = log(bodyfat_3$adiposity)
colnames(bodyfat_3)[5] = "log_adiposity"
bodyfat_3$ankle = log(bodyfat_3$ankle)
colnames(bodyfat_3)[12] = "log_ankle"


### Plot bodyfat vs each variable
## variables height, age and ankle do not exhibit linear relationships
par(mfrow=c(2,2))
for(i in 1:14){
  plot(bodyfat_3$bodyfat~bodyfat_3[,(i+1)],ylab="Bodyfat percentage",
       xlab=variable.names(bodyfat_3)[i+1])
}

## Consider Boxcox Transformation
library(MASS)
boxcox_trans <- boxcox(bodyfat ~ ., data=bodyfat_3) # lambda ~ 1


###Variable selection
## Split bodyfat_3 into train and test sets
set.seed(1)
train = sample(nrow(bodyfat_3), 0.7*nrow(bodyfat_3), replace = FALSE)
bodyfat_log_train = bodyfat_3[train,]
bodyfat_log_test = bodyfat_3[-train,]


## Full model
# weight, adiposity, abdomen and wrist are significant
full.model.trans = lm(bodyfat~., data = bodyfat_log_train)
summary(full.model.trans)


## forward and backward selection
# (BIC=490.29) model: bodyfat ~ weight + height + log_adiposity + abdomen + wrist
model.bic.both.trans = step(full.model.trans, direction = "both", k = log(nrow(bodyfat_log_train)))
summary(model.bic.both.trans)  # all variables are significant

# (AIC=471.41) model: weight + height + log_adiposity + abdomen + wrist
model.aic.both.trans = step(full.model.trans, direction = "both", k = 2)
summary(model.aic.both.trans) # all variables are significant


## backward selection
# (BIC=490.29) model: bodyfat ~ weight + height + log_adiposity + abdomen + wrist
model.bic.backward.trans = step(full.model.trans, direction = "backward", k = log(nrow(bodyfat_log_train)))
summary(model.bic.backward.trans)  # all are significant

# (AIC=471.41) model: bodyfat ~ age + weight + height + adiposity + neck + chest + 
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm + 
#                     wrist
model.aic.backward.trans = step(full.model.trans, direction = "backward", k = 2)
summary(model.aic.backward.trans)  # weight, height, log_adiposity, abdomen and wrist are significant


## forward selection
# (BIC=530.61) model: bodyfat ~ age + weight + height + adiposity + neck + chest + 
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm + 
#                     wrist
model.bic.forward.trans = step(full.model.trans, direction = "forward", k = log(nrow(bodyfat_log_train)))
summary(model.bic.forward.trans)  # weight, height, log_adiposity, abdomen and wrist are significant

# (AIC=483.39) model: bodyfat ~ age + weight + height + adiposity + neck + chest + 
#                     abdomen + hip + thigh + knee + ankle + biceps + forearm + 
#                     wrist
model.aic.forward.trans = step(full.model.trans, direction = "forward", k = 2)
summary(model.aic.forward.trans)  # weight, height, log_adiposity, abdomen and wrist are significant


## best subset selection
library(leaps)
model.bestsubset.trans = regsubsets(bodyfat~., data=bodyfat_log_train, nvmax=ncol((bodyfat_log_train)-1))
summary.bestsubset.trans = summary(model.bestsubset.trans)
names(summary.bestsubset.trans)
which.min(summary.bestsubset.trans$bic)   # 5
which.max(summary.bestsubset.trans$adjr2) # 5

# best subset with 5 variables: weight, height, log_adiposity, abdomen, wrist
bestsubset.model = lm(bodyfat~weight+height+log_adiposity+abdomen+wrist, data=bodyfat_log_train)
summary(bestsubset.model) # all variables are significant


## Fit lasso
library(glmnet)
x = model.matrix(bodyfat~., data = bodyfat_3)[,-1]
y = bodyfat_3$bodyfat
model.cv.lasso.trans = cv.glmnet(x, y, family = "gaussian", alpha = 1, type.measure = "mse")
par(mfrow=c(1,1))
plot(model.cv.lasso.trans)
print(model.cv.lasso.trans$lambda.min) # lambda = 0.1995549
print(model.cv.lasso.trans$lambda.1se) # lambda = 0.5059443
print(coef(model.cv.lasso.trans, s = "lambda.min")) # variables = age,height,neck,abdomen,wrist
print(coef(model.cv.lasso.trans, s = "lambda.1se")) # variables = age,height,abdomen,wrist


## Using Mallow's Cp criterion
library(faraway)
mallow_Cp.trans = leaps(x,y,nbest = 1)
Cpplot(mallow_Cp.trans) # best: variables = 1,4,6,7,8,14
variable.names(x)[c(1,4,6,7,8,14)] # age, log_adiposity, chest, abdomen,hip, wrist 

# fit MLR with the variables
model.mallowCp.trans = lm(bodyfat~age+log_adiposity+chest+abdomen+hip+wrist, data=bodyfat_log_train)
summary(model.mallowCp.trans)



##########################################################################################################
### Model evaluation
