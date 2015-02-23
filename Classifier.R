source("FeatureExtraction.R")
source("RandomForestClassifier.R")
source("BinaryClassifier.R")
source("DecisionTreeClassifier.R")
source("SVMClassifier.R")
source("NeuralNetworkClassifier.R")

## Specifies a common interface for a variety of classifier objects, containing 
## functionality for training, prediction, cross validation, etc.
## @type of classifier: {"decision_tree", "random_forest", binary", "svm", "neural_network", "gp", "naive_bayes"} 
## @formula parameter is used to specify labels & features 
## @params accepts a list of optional parameters to be passed to the classifier.

classifier <- function(data, type = "decision_tree", formula = NULL, params = list()) {	
    classifier <- NA
    
    if (type == "decision_tree") {
        classifier <- initTree(data, formula)
    } 
    else if (type == "random_forest") {
        classifier <- initForest(data, formula)
    }
    else if (type == "random_forest2") {
        classifier <- initForest2(data, formula)
    }
    else if (type == "binary") {
        classifier <- initBinClassifier(data)
    }
    else if (type == "svm") {
        classifier <- initSVMClassifier(data, formula, params)
    } 
    else if (type == "svm2") {
        params <- list(params, type = "e1071")
        classifier <- initSVMClassifier(data, formula, params)
    } 
    else if (type == "neural_network") {
        classifier <- initNNClassifier(data, formula, params)
    } 
    else if (type == "gp") {
        classifier <- initGPClassifier(data, formula)
    }
    
    predict <- function(entry) {
        classifier$predict(entry)
    }
    
    test <- function(testSet) {
        classifier$test(testSet)
    }
    
    hitsNum <- function(testSet, trueLabels) {
        classifier$hitsNum(testSet, trueLabels)
    }
    
    # Static method, performing a K-foldes cross-validation  
    crossValidation <- function(dataSet = data, K = 10) {
        crossValidation(dataSet, classifierType = type, K)
    }
    
    ## Returns a list representation of the object with methods and properties accessed through indexed keys
    list(classifier = classifier, predict = predict, crossValidation = crossValidation, test = test, hitsNum = hitsNum)
}  

## Estimates performance of a classifier object of certain @classifierType for the
## specified @dataset based on cross validation, performing splits into @K subsets. 
## Returns: mean accuracy of the cross validation
## @params accepts a list of optional parameters to be passed to the classifier

crossValidation <- function(dataSet, classifierType = "decision_tree", formula = NULL, K = 10, params = list()) {
    trueLabelsColumn <- ncol(dataSet)
    uniqueClasses    <- unique(dataSet[, trueLabelsColumn])
    classesNum <- length(uniqueClasses)  
    accuracy   <- numeric(0)
    folds      <- cvFolds(nrow(dataSet), K = K)
    totalCm    <- matrix(0, classesNum, classesNum)
    
    for(i in 1:K) {
        train <- dataSet[folds$subsets[folds$which != i], ]
        validation <- dataSet[folds$subsets[folds$which == i], ]
        classifier <- classifier(train, type = classifierType, formula = formula, params)
        pred <- classifier$predict(validation)
        pred <- round(as.numeric(pred))
        validation <- c(validation[, ncol(dataSet)]) 
        ## Adding one dumb prediction per class. They will be extracted from the final result 
        validation <- c(validation, uniqueClasses)
        pred <-  c(pred, uniqueClasses)
        cm   <- table(pred, validation)
        acc  <- (sum(diag(cm)) - classesNum) / (sum(cm) - classesNum)
        totalCm <- totalCm + cm
        accuracy <- rbind(accuracy, acc)
    }
    print (totalCm)
    accuracy <- mean(accuracy)
    print(accuracy)
    accuracy
}
