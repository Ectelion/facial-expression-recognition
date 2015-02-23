library(nnet)
library(neuralnet)

defaultNNFormula = emotion ~ .

## Creates classifier based on Artificial Neural Networks.
## Includes methods for prediction, cross validation, etc.
## @formula parameter is used to specify labels & features.

initNNClassifier <- function(trainingSet, formula = NULL, params = list(type = NULL)) {
    # Training phase
    if (is.null(formula)) {
        formula <- defaultNNFormula
    }
    nn.classifier <- nnet(data = trainingSet,
                          formula = emotion ~ X33+X55+X65+X89+X91+X111+X117+X121+X127+X128+X130, 
                          size = 10,
                          maxit = 700,
                          skip = TRUE,  # Allowing layer skips actually boosts the performance in our case 
                          MaxNWts = 1000000)  
    
    # nn.classifier <- neuralnet(data = trainingSet, emotion ~ X33+X55+X65+X89+X91+X111+X117+X121+X127+X128+X130, hidden = 5)
   
    nn.predict <- function(data) {
        predict(nn.classifier, data)
    }
    
    nn.hitsNum <- function(inputs, trueLabels) {
        predictions <- predict(inputs)
        hits <- predictions == trueLabels
        hitsNum <- sum(hits)
        hitsNum
    }
    
    nn.crossValidation <- function(dataSet = trainingSet, K = 10) {
        crossValidationSVM(dataSet, "neural_network", K)
    }
    
    ## Returns a list representation of the object with methods and properties accessed through indexed keys
    list(classifier = nn.classifier, predict = nn.predict, hitsNum = nn.hitsNum, crossValidation = nn.crossValidation)
}