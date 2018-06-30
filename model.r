library(caret)
library(randomForest)

#
# Setting up for Random Forest predictor.
#

# Initializing data 'mtcars' 
data("mtcars")

# structure in UI
dataStructure <- capture.output(str(mtcars))

set.seed(1995) 

# Defining custom training controls with cross validation.
customTrainControl <- trainControl(method = "cv", number = 10)

# Building Random Forest model function. 
# in order to regenerate the model when the user change parameters in the UI.
# The goal of this model is to predict 'mpg' (miles per gallon) using the rest
# of the variables.
carsRandomForestModelBuilder <- function() {

  return(
    train(
      mpg ~ ., 
      data = mtcars,
      method = "rf",
      trControl = customTrainControl
    )
  )

}

# Predictor function.  
randomForestPredictor <- function(model, parameters) {
 
  prediction <- predict(
     model,
     newdata = parameters
  )
  
  return(prediction)

}
