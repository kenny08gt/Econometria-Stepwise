library(tidyverse)
library(caret)
library(leaps)
library(MASS)

#******************************************************************************
#*********************** FORWARD STEPWISE *************************************
#******************************************************************************
# Fit the full model 
full.model <- lm(Fertility ~., data = swiss)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "forward", 
                      trace = FALSE)
summary(step.model)

models <- regsubsets(Fertility~., data = swiss, nvmax = 5,
                     method = "forward")
summary(models)

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Fertility ~., data = swiss,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results

#nvmax: the number of variable in the model. For example nvmax = 2, specify the best 2-variables model
#RMSE and MAE are two different metrics measuring the prediction error of each model. The lower the RMSE and MAE, the better the model.
#Rsquared indicates the correlation between the observed outcome values and the values predicted by the model. The higher the R squared, the better the model.

step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, 5)


#*******************************************************************************
#*********************** BACKWARD STEPWISE *************************************
#*******************************************************************************

# Fit the full model 
full.model <- lm(Fertility ~., data = swiss)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "backward", 
                      trace = FALSE)
summary(step.model)

models <- regsubsets(Fertility~., data = swiss, nvmax = 5,
                     method = "backward")
summary(models)

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Fertility ~., data = swiss,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results

#nvmax: the number of variable in the model. For example nvmax = 2, specify the best 2-variables model
#RMSE and MAE are two different metrics measuring the prediction error of each model. The lower the RMSE and MAE, the better the model.
#Rsquared indicates the correlation between the observed outcome values and the values predicted by the model. The higher the R squared, the better the model.

step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, 5)
