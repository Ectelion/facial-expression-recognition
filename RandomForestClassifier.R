library(randomForest)

## Custom implementation of a simple Random Forest, based on 
## the rpart package's decision tree

initForest <- function(trainingSet, treesNum=10) {
	# Training phase
	folds  <- cvFolds(nrow(trainingSet), K=treesNum)
	forest <- list()
	
	for(i in 1:treesNum) {
		train <- trainingSet[folds$subsets[folds$which != i], ]
		tree <- fitDTClassifier(train) #rpart(emotion ~ ., method="class", data=train, control=rpart.control(minsplit=1, cp=0.006147)) 
		if (i==1) {
			forest <- list(tree)
		} else {
			forest <- c(forest, list(tree))
		}
	}
	
	forest.predict <- function(entry) {
		votes <- c(0, 0, 0, 0, 0, 0, 0)
		
		for (treeId in 1:length(forest)) {
			assumptions <- predict(forest[[treeId]], entry)
			votes <- votes + assumptions
		}
		
		# Identifying the elected winner
		max        <- -1
		winnerId   <-  1
		voteId     <-  1  
		
		for (vote in votes) {
			if (vote > max) {
				max   <- vote
				winnerId <- voteId
			}
			voteId <- voteId + 1
		}
		winnerId
	}
	
	forest.test <- function(testSet) {
		predictions <- c()
		for (testId in 1:nrow(testSet)) {
			class <- forest.predict(testSet[testId, ])
			predictions <- c(predictions, class)
		}
		predictions
	}
	
	forest.hitsNum <- function(testSet, trueLabels) {
		predictions <- test(testSet)
		hits <- predictions == trueLabels
		hitsNum <- sum(hits)
		hitsNum
	}
	
	forest.crossValidationRF <- function(dataSet, K=10) {
		crossValidationRF(dataSet, K)
	}
	
	## Returns a list representation of the object with methods and properties accessed through indexed keys
	list(classifier=forest, predict=forest.predict, hitsNum = forest.hitsNum, crossValidation=forest.crossValidation)
}  

## Random Forest implementation based on the randomForest package

initForest2 <- function(trainingSet, treesNum=500) {
	# Training phase
	formula <- emotion ~ . #X1+X11+X18+X43+X52+X87+X89+X91+X92+X101+X102+X117+X118+X120+X123+X125
	forest2 <- randomForest(formula, data=dataSet, importance=TRUE, proximity=TRUE, ntree=treesNum) # maxnodes = 15, nodesize = 10)
	
	forest2.predict <- function(entry) {
		predict(forest2, entry)
	}
	
	forest2.hitsNum <- function(inputs, trueLabels) {
		hitsNum  <- 0
		labels <- c(8)
		pred <-  predict(forest2, inputs[, 1:136])
		for (resultInd in 1: length(pred)) {
			result <- pred[resultInd]
			labels <- c(labels, result)
			
			if (round(result) == trueLabels[resultInd]) {
				hitsNum <- hitsNum + 1
			} 
		}
		hitsNum
	}
	
	forest2.crossValidation <- function(dataSet, treesNum = 50, K=10) {
		crossValidationRF2(dataSet, K)
	}
	
	## Returns a list representation of the object with methods and properties accessed through indexed keys
	list(classifier=forest2, predict=forest2.predict, hitsNum = forest2.hitsNum, crossValidation=forest2.crossValidation)
}

## Estimates cross validation of the custom Random Forest classifier for the 
## specified data set, performing splits into K subsets. 
## Returns mean accuracy cross validation

crossValidationRF <- function(dataSet, K = 10) {
	accuracy <- numeric(0)
	folds    <- cvFolds(nrow(dataSet), K=K)
	totalCm  <- matrix(rep(0, 49), 7, 7)
	for(i in 1:K) {
		train <- dataSet[folds$subsets[folds$which != i], ]
		validation <- dataSet[folds$subsets[folds$which == i], ]
		rf <- initForest(train, treesNum=10)
		pred <- rf$test(validation)
		validation <- c(validation[, ncol(dataSet)]) 
		validation <- c(validation, c(1, 2, 3, 4, 5, 6, 7))
		pred <-  c(pred, c(1, 2, 3, 4, 5, 6, 7))
		cm   <- table(pred, validation)
		acc  <- (cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5]+cm[6,6]+cm[7,7]-7)/(sum(cm)-7)
		totalCm <- totalCm + cm
		accuracy <- rbind(accuracy, acc)
	}
	print (totalCm)
	mean(accuracy)
}

## Estimates cross validation of the Random Forest (from randomForest package) classifier 
## for the specified data set, performing splits into K subsets. 
## Returns mean accuracy cross validation

crossValidationRF2 <- function(dataSet, K = 10) {
	accuracy <- numeric(0)
	folds    <- cvFolds(nrow(dataSet), K=K)
	totalCm  <- matrix(rep(0, 49), 7, 7)
	for(i in 1:K) {
		train <- dataSet[folds$subsets[folds$which != i], ]
		validation <- dataSet[folds$subsets[folds$which == i], ]
		classifier <- fitRFClassifier(train, treesNum=50)
		hitsNum <- testRFClassifier(classifier, validation, validation[, 137])
		acc <- hitsNum / nrow(validation)
		accuracy <- rbind(accuracy, acc)
	}
	#print (cm)
	mean(accuracy)
}

