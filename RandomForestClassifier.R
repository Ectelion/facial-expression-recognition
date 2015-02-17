library(randomForest)

## Custom implementation of a simple Random Forest, based on 
## the rpart package's decision tree

initForest2 <- function(trainingSet, treesNum=10) {
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
	
	forest.predictOne <- function(entry) {
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
	
	forest.predict <- function(testSet) {
		predictions <- c()
		for (testId in 1:nrow(testSet)) {
			class <- forest.predictOne(testSet[testId, ])
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
	
	forest.crossValidation <- function(dataSet=trainingSet, K=10) {
		crossValidation(dataSet, classifierType="random_forest2", K)
	}
	
	## Returns a list representation of the object with methods and properties accessed through indexed keys
	list(classifier=forest, predict=forest.predict, hitsNum=forest.hitsNum, crossValidation=forest.crossValidation)
}  

## Random Forest implementation based on the randomForest package

initForest <- function(trainingSet, treesNum=100) {
	# Training phase
	formula <- emotion ~ . #X1+X11+X18+X43+X52+X87+X89+X91+X92+X101+X102+X117+X118+X120+X123+X125
	forest <- randomForest(formula, data=dataSet, importance=TRUE, proximity=TRUE, ntree=treesNum) # maxnodes = 15, nodesize = 10)
	
	forest.predict <- function(entry) {
		predict(forest, entry)
	}
	
	forest.hitsNum <- function(inputs, trueLabels) {
		hitsNum  <- 0
		labels <- c(8)
		pred <-  predict(forest, inputs[, 1:136])
		for (resultInd in 1: length(pred)) {
			result <- pred[resultInd]
			labels <- c(labels, result)
			
			if (round(result) == trueLabels[resultInd]) {
				hitsNum <- hitsNum + 1
			} 
		}
		hitsNum
	}
	
	forest.crossValidation <- function(dataSet, K=10) {
		crossValidation(dataSet, classifierType="random_forest", K)
	}
	
	## Returns a list representation of the object with methods and properties accessed through indexed keys
	list(classifier=forest, predict=forest.predict, hitsNum = forest.hitsNum, crossValidation=forest.crossValidation)
}
