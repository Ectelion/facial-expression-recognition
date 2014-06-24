library(e1071)

## Creates SVM based classificator object, containing 
## methods for prediction, cross validation, etc.

initSVMClassifier <- function(trainingSet) {
	# Training phase
	formula <- emotion ~ X33+X55+X65+X89+X91+X111+X117+X121+X127+X128+X130 #optimalForwardSearchGLOB #X66+X76+X91+X117+X119+X121+X128
	svm.classifier <- fitDTClassifierclassifier <- svm(formula, data = trainingSet, kernel="linear", type="C-classification")
	
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
	
	svm.crossValidation <- function(K=10) {
		crossValidationSVM(trainingSet, K)
	}
	
	## Returns a list representation of the object with methods and properties accessed through indexed keys
	list(classifier=svm.classifier, predict=svm.predict, hitsNum = svm.hitsNum, crossValidation=svm.crossValidation)
}  


## Estimates cross validation of the SVM based classifier for 
## the specified data set, performing splits into K subsets. 
## Returns mean accuracy cross validation  

crossValidationSVM <- function(dataSet, K = 10) {
	accuracy <- numeric(0)
	folds    <- cvFolds(nrow(dataSet), K=K)
	totalCm  <- matrix(rep(0, 49), 7, 7)
	for(i in 1:K) {
		train <- dataSet[folds$subsets[folds$which != i], ]
		validation <- dataSet[folds$subsets[folds$which == i], ]
		classifier <- initSVMClassifier(train)
		pred <- classifier$predict(validation)
		pred <- round(as.numeric(pred))
		validation <- c(validation[, ncol(dataSet)]) 
		validation <- c(validation, c(1, 2, 3, 4, 5, 6, 7))
		pred <-  c(pred, c(1, 2, 3, 4, 5, 6, 7))
		cm   <- table(pred, validation)
		acc  <- (cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5]+cm[6,6]+cm[7,7]-7)/(sum(cm)-7)
		totalCm <- totalCm + cm
		accuracy <- rbind(accuracy, acc)
	}
	print (totalCm)
	accuracy <- mean(accuracy)
	print(accuracy)
	accuracy
}