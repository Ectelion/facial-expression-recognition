source("RandomForestClassifier.R")
source("BinaryClassifier.R")
source("DecisionTreeClassifier.R")
source("SVMClassifier.R")

## Specifies a common interface for a variety of classificator objects, containing 
## functionality for training, prediction, cross validation, etc.
## @parameter "type": {"decision_tree", "random_forest", binary", "svm", "neural_network", "naive_bayes"} 

classifier <- function(data, type="decision_tree") {	
	classifier <- NA
	
	if (type=="decision_tree") {
		classifier <- initTree(data)
	} 
	else if (type=="random_forest") {
		classifier <- initForest(data)
	}
    else if (type=="random_forest2") {
		classifier <- initForest2(data)
	}
	else if (type=="binary") {
		classifier <- initBinClassifier(data)
	}
	else if (type=="svm") {
		classifier <- initSVMClassifier(data)
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
	crossValidation <- function(dataSet=data, K=10) {
		crossValidation(dataSet, classifierType=type, K)
	}
	
	## Returns a list representation of the object with methods and properties accessed through indexed keys
	list(classifier=classifier, predict=predict, crossValidation=crossValidation, test=test, hitsNum=hitsNum)
}  

crossValidation <- function(dataSet, classifierType="decision_tree", K = 10) {
	trueLabelsColumn <- ncol(dataSet)
	classesNum <- length(unique(dataSet[, trueLabelsColumn]))  
	accuracy <- numeric(0)
	folds    <- cvFolds(nrow(dataSet), K=K)
	totalCm  <- matrix(0, classesNum, classesNum)
	
	for(i in 1:K) {
		train <- dataSet[folds$subsets[folds$which != i], ]
		validation <- dataSet[folds$subsets[folds$which == i], ]
		classifier <- classifier(train, type=classifierType)
		pred <- classifier$predict(validation)
		pred <- round(as.numeric(pred))
		validation <- c(validation[, ncol(dataSet)]) 
		## Adding one dumb prediction per class. They will be extracted from the final result 
		validation <- c(validation, c(1:classesNum))
		pred <-  c(pred, c(1:classesNum))
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
