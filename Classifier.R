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
		print("fwe")
		classifier$crossValidation(dataSet, K)
	}
	
	## Returns a list representation of the object with methods and properties accessed through indexed keys
	list(classifier=classifier, predict=predict, crossValidation=crossValidation, test=test, hitsNum = hitsNum)
}  
