library(kernlab)

defaultGPFormula = emotion ~ .

## Creates classifier based on Gaussian Processes.
## Includes methods for prediction, cross validation, etc.
## @formula parameter is used to specify labels & features 

initGPClassifier <- function(trainingSet, formula = defaultGPFormula) {
    if (is.null(formula)) {
        formula <- defaultGPFormula
    }
    
    # Training phase
    gp.classifier <- gausspr(defaultGPFormula, data = trainingSet, type = "classification", fit = TRUE, cross = 5, tol = 0.0001)
    
    gp.predict <- function(data) {
        predict(gp.classifier, data)
    }
    
    gp.hitsNum <- function(inputs, trueLabels) {
        predictions <- predict(inputs)
        hits <- predictions == trueLabels
        hitsNum <- sum(hits)
        hitsNum
    }
    
    gp.crossValidation <- function(dataSet = trainingSet, K = 10) {
        crossValidationSVM(dataSet, "gp", K)
    }
    
    ## Returns a list representation of the object with methods and properties accessed through indexed keys
    list(classifier = gp.classifier, predict = gp.predict, hitsNum = gp.hitsNum, crossValidation = gp.crossValidation)
}  