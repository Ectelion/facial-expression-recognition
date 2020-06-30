# Dependencies - concrete classifiers
source("FeatureExtraction.R")
source("RandomForestClassifier.R")
source("BinaryClassifier.R")
source("DecisionTreeClassifier.R")
source("SVMClassifier.R")
source("NeuralNetworkClassifier.R")

library(cvTools)

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
crossValidationWithDetails <- function(dataSet, classifierType = "decision_tree", formula = NULL, K = 10, probResp = F, params = list(), ...) {
  trueLabelsColumn <- ncol(dataSet)
  uniqueClasses    <- unique(dataSet[, trueLabelsColumn])
  classesNum <- length(uniqueClasses)  
  accuracy   <- numeric(0)
  folds      <- cvFolds(nrow(dataSet), K = K)
  totalCm    <- matrix(0, classesNum, classesNum)
  foldPreds  <- list()
  foldLabels <- list()
  foldMaps   <- list()
  
  for(i in 1:K) {
    train <- dataSet[folds$subsets[folds$which != i], ]
    validationSet <- folds$subsets[folds$which == i]
    validation <- dataSet[validationSet, ]
    classifier <- classifier(train, type = classifierType, formula = formula, probResp = probResp, params, ...)
    pred <- classifier$predict(validation)
    validation    <- c(validation[, ncol(dataSet)]) 
    foldPreds[i]  <- list(pred)
    foldLabels[i] <- list(validation)
    foldMaps[i]   <- list(validationSet)
    if (!is.factor(pred)) pred <- round(c(pred))
    cm <- table(factor(pred, levels = min(validation):max(validation)),
                factor(validation, levels = min(validation):max(validation)))
    acc <- sum(pred == validation) / length(validation)
    print(paste("Fold ", i, " accuracy: ", acc * 100, "%", sep = ""))
    totalCm <- totalCm + cm
    accuracy <- rbind(accuracy, acc)
  }
  accuracy <- mean(accuracy)
  
  print(totalCm)
  print(paste("Mean accuracy: ", accuracy * 100, "%"))
  
  list(accuracy = accuracy, foldPreds = foldPreds, foldLabels = foldLabels, foldMaps = foldMaps, confusionMatrix = totalCm) 
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

## Evaluates a classifier using metrics such as accuracy, precision, recall, AUROC and AUPRC.
## The metrics are computed for N cross-validation experiments. 
## The results are summarized and returned in a list. 
evaluateClassifier <- function(formula, data, classifierType, selectFeatures = T, N = 10, cvFoldsNum = 10) {
    cm <- matrix(rep(0, 4), 2, 2)
    auprcs <- c()
    aurocs <- c()
    accs   <- rep(0, N)
    precisions <- rep(0, N)
    recalls    <- rep(0, N)
    for (i in 1:N) {
        ## Perform K-folds cross validation
        cvRes <- crossValidationWithDetails(formula = formula, dataSet = data, K = cvFoldsNum,
                                  classifierType = classifierType, selectFeatures = selectFeatures, probResp = T)
        ## Compute accuracy, precision and recall
        accs[i] <- cvRes$accuracy
        precisions[i] <- cvRes$confusionMatrix[2, 2] / (cvRes$confusionMatrix[2, 2] + cvRes$confusionMatrix[2, 1])
        recalls[i]    <- cvRes$confusionMatrix[2, 2] / (cvRes$confusionMatrix[2, 2] + cvRes$confusionMatrix[1, 2])
        cm <- cm + cvRes$confusionMatrix
        ## Compute AUROC
        preds = prediction(cvRes$foldPreds, cvRes$foldLabels)
        aucPerf = performance(preds, measure = "auc")
        auroc <- as.numeric(aucPerf@y.values) 
        aurocs <- c(aurocs, auroc)
        ## Compute AUCPR
        aucs <- c()
        for (j in 1:cvFoldsNum) {
            auc <- pr.curve(cvRes$foldPreds[[j]][cvRes$foldLabels[[j]] == 1], cvRes$foldPreds[[j]][cvRes$foldLabels[[j]]==0])
            aucs <- c(aucs, auc$auc.integral)
        }
        auprcs <- c(auprcs, aucs)
    }

    ## Return the metrics summaried in a list
    list(confusionMatrix = cm, accuracies = accs, auprcs = auprcs, aurocs = aurocs, precisions = precisions, recalls = recalls)
}