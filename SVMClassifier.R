# Dependencies
library(kernlab)
library(e1071)

## Optimal features for multi-class SVM classification (landmarks displacement approach)
overallOptimalFormulaSVMDisplacement <- emotion ~ X33+X55+X65+X89+X91+X111+X117+X121+X127+X128+X130 

## Optimal features for multi-class SVM classification using Kernlab ksvm implementation (landmarks displacement approach)
overallOptimalFormulaSVMDisplacementKernlab <- emotion ~ X18+X33+X55+X63+X89+X91+X117+X121+X128+X120+X123+X124+X133+X135

## Optimal features for multi-class SVM classification (single frame approach)
overallOptimalFormulaSVMSingleFrame <- emotion ~ X8+X21+X27+X49+X53+X65+X76+X88+X89+X90+X93+X97+X102+X104+X110+X115+X117+X118+X121+X122+X124+X125+X128+X130+X135+X136

## Optimal features for multi-class SVM classification Kernlab ksvm implementation (single frame approach)
overallOptimalFormulaSVMSingleFrameKernlab <- emotion ~ X55+X62+X65+X91+X99+X116+X117+X118+X121+X122+X127+X130+X131+X134+X136+X93+X100+X49+X68+X98


## Optimal features for a specific emotion (for binary SVM classification)
optimalFormulasSVM <- list(
    emotion ~ X40+X43+X48+X92+X111+X127,
    emotion ~ X87+X93+X124+X126,
    emotion ~ X23+X25+X49+X54+X57+X67+X91+X104+X106+X118+X125+X135+X136,
    emotion ~ X37+X104+X129+X33+X55+X89+X91+X111+X117+X121+X127+X128+X130,  # overallOptimalFormulaSVM, #X40 || X122 || X130(?) 
    emotion ~ X26+X66+X101+X115+X117+X120+X132,
    emotion ~ X48+X50+X90+X117+X136,
    emotion ~ X19+X76+X87+X102+X125+X133
)

## Creates SVM based classifier object.
## Includes methods for prediction, cross validation, etc.
## @formula parameter is used to specify labels & features. 
## @params accepts a list of optional parameters to be passed to the classifier. 
## @params$type specifies SVM implementation to be used. Available options: {"kernlab", "e1071"}

initSVMClassifier <- function(trainingSet, formula = NULL, params = list(type = NULL)) {
    # Training phase
    if (is.null(params$type) || (!is.null(params$type) && params$type == "kernlab")) {
        if (is.null(formula)) {
            formula <- overallOptimalFormulaSVMSingleFrameKernlab  
        }
        svm.classifier <- ksvm(formula, data = trainingSet, type = "C-svc", kernel = 'rbfdot', C = 10, tol=0.001) # cross = 10)  
        # "cross = 10" parameter results in minor improvement (0.1-0.2%) by the cost of training time        
    } else {
        if (is.null(formula)) {
            formula <- overallOptimalFormulaSVMDisplacement
        }
        svm.classifier <- svm(formula, data = trainingSet, kernel = "linear", type = "C-classification")
    }
    
    svm.predict <- function(data) {
        predict(svm.classifier, data)
    }
        
    svm.hitsNum <- function(inputs, trueLabels) {
        predictions <- svm.predict(inputs)
        hits <- predictions == trueLabels
        print(predictions)
        hitsNum <- sum(hits)
        hitsNum
    }
    
    svm.crossValidation <- function(dataSet = trainingSet, K = 10) {
        crossValidation(dataSet, classifierType = "svm", K)
    }
    
    ## Returns a list representation of the object with methods and properties accessed through indexed keys
    list(classifier = svm.classifier, predict = svm.predict, hitsNum = svm.hitsNum, crossValidation = svm.crossValidation)
}  
