## Dependencies
library(rpart)
source("DecisionTreeClassifier.R")

## Define

optimalBinFormulas <- list(
	  emotion ~ X41+X54+X93+X119+X124+X127
	, emotion ~ X54+X103+X124
	, emotion ~ X37+X49+X55+X57+X99+X104+X117+X121+X127
	, emotion ~ X19+X55+X128
	, emotion ~ X89+X106+X117+X135
	, emotion ~ X90+X100+X106+X121+X134
	, emotion ~ X46+X57+X59+X87+X103
) 

## Creates binary classificator object, consisted of binary classifitarors for 
## each emotion and methods for prediction, cross validation, etc.

initBinClassifier <- function(trainingSet=loadData()) {
	# Training phase
	binTrees <- list()
	diffClass <- initDifferentiatingClassifier(trainingSet)
	
	# Train binary trees
	for(em in 1:7) {
		train <- oneVsAll(trainingSet, em)
		formula <- optimalBinFormulas[[em]]
		tree <- rpart(formula, data=train) # control=rpart.control(cp=0.006147541)) 
		
		if (em==1) {
			binTrees <- list(tree)
		} else {
			binTrees <- c(binTrees, list(tree))
		}
	}
	
	## Prediction approach based on polling
	
	binTrees.predictOne <- function(entry) {
		classes <- c(0, 0, 0, 0, 0, 0, 0)
		
		for (treeId in 1:length(binTrees)) {
			classProb <- predict(binTrees[[treeId]], entry)
			classes[treeId] <- classProb
		}
		
		# Identifying the elected winner
		max        <- -1
		winnerId   <-  0
		voteId     <-  1  
		
		for (prob in classes) {
			if (prob > max) {
				max <- prob
				winnerId <- voteId
			}
			voteId <- voteId + 1
		}
		print(classes)
		winnerId
	}
	
	# A hybrid prediction approach based on binary and differentiating classifiers
	
	binTrees.predictOne2 <- function(entry) {
		classProbs <- c(0, 0, 0, 0, 0, 0, 0)
		
		for (treeId in 1:length(binTrees)) {
			classProb <- predict(binTrees[[treeId]], entry)
			classProbs[treeId] <- classProb
		}
		#print(classProbs)
		
		# Identifying the most probable classes
          n <- length(classProbs)
		sortedProbs <- sort(classProbs, partial=n)
		max1    <- sortedProbs[n]
		max2    <- sortedProbs[n-1]
		
		# Handling the case when the two top indices are a tie
		# or when the second top element has a tie
		max1Ind <- max(which(classProbs==max1))
		max2Ind <- min(which(classProbs==max2))
		
		if (max1 >= 0.6) {
			# If the top probability is high enough, return the result
			res <- max1Ind
		} else {
			# Launching differentiating classifiers for dealing with the confusion 
			fitDiff <- diffClass$fit(max1Ind, max2Ind)
			pred <- predict(fitDiff, entry) #, type="class")
			#print(pred)
			maxId <- which.max(pred)
			res <- as.numeric(dimnames(pred)[[2]][maxId])
		}
		res
	}
	
	## Applies predicting function to each instance of the provided testSet 
	
	binTrees.predict <- function(testSet) {
		predictions <- c()
		for (testId in 1:nrow(testSet)) {
			winnerId <- binTrees.predictOne2(testSet[testId, ])
			predictions <- c(predictions, winnerId)
		}
		predictions
	}
	
	## Counts positive predictions of the testSet
	
	binTrees.hitsNum <- function(testSet, trueLabels) {
		predictions <- binTrees.test(testSet)
		hits <- predictions == trueLabels
		sum(hits)
	}
	
	## Performs cross validation on the specified testSet
	
	binTrees.crossValidation <- function(testSet, K=10) {
		crossValidationBin(testSet, K)
	}
	
	## Returns a list representation of the object with methods and properties accessed through indexed keys
	list(classifier=binTrees, predict=binTrees.predict, test=binTrees.predictOne, hitsNum = binTrees.hitsNum, crossValidation=binTrees.crossValidation)
}

## Differentiating classifier object. Caches the results across calls

initDifferentiatingClassifier <- function(dataSet=loadData()) {
	differentiatingClassifiers <- list() 
	
	fit <- function(emotionIdX, emotionIdY) {
		if (emotionIdX == emotionIdY) {
			stop("The same emotion codes are passed")
		}
		if (is.na(emotionIdX) || is.na(emotionIdY)) {
			stop("NA value passed")
		}
		emotionIdX <- as.numeric(emotionIdX)
		emotionIdY <- as.numeric(emotionIdY)
		
		if (emotionIdX > emotionIdY) {
			tmp <- emotionIdY
			emotionIdY <- emotionIdX
			emotionIdX <- tmp
		}
		key <- paste(emotionIdX, emotionIdY, sep="_")
		#if (key %in% names(differentiatingClassifiers) ) {
			# Retrieve and return cached entry
		#	differentiatingClassifiers[key]	
		#} else {
			emX <- dataSet[dataSet[,"emotion"]==emotionIdX, ]
			emY <- dataSet[dataSet[,"emotion"]==emotionIdY, ]
			differentiatorDt <- rbind(emX, emY) 
			formula <- emotion ~ X66+X76+X91+X117+X119+X121+X128 
			fitDiff <- rpart(formula, data=data.frame(differentiatorDt), method="class", control=rpart.control(minsplit=1, cp= 0.006147541))
			#differentiatingClassifiers[key] <- fitDiff
			fitDiff
		#}
	}
	
	list(fit=fit)
}

## Transforms the dataset to a binary for the specified class 

oneVsAll <- function(dataSet, binClass) {
	oneVsAll <- dataSet
	oneVsAll[oneVsAll[,"emotion"]!=binClass, "emotion"] <- FALSE
	oneVsAll[oneVsAll[,"emotion"]!=FALSE, "emotion"] <- TRUE
	oneVsAll
}		

## Creates a binary classifier for a specified emotion. Based on rpart 
## Decision Tree classificator

fitBinaryClassifier <- function(dataSet=loadData(), emotionId) {
	labels <- dataSet[, "emotion"]
	labelsBin <- labels == emotionId
	dataSet[, "emotion"] <- labelsBin
	fit <- rpart(emotion ~ ., dataSet)
	fit			
}

## Estimates cross validation of the Binary classifier for the specified data set, performing splits into
## K subsets. Returns mean accuracy of cross validations  

crossValidationBin <- function(dataSet, K = 10) {
	accuracy <- numeric(0)
	folds    <- cvFolds(nrow(dataSet), K=K)
	totalCm  <- matrix(rep(0, 4), 2, 2)
	for(i in 1:K) {
		train <- dataSet[folds$subsets[folds$which != i], ]
		validation <- dataSet[folds$subsets[folds$which == i], ]
		classifier <- initBinClassifier(train)
		pred <- classifier$predict(validation)
		validation <- c(validation[, ncol(dataSet)]) 
		eq <- as.vector(pred) == as.vector(validation)
		acc <- sum(eq)/length(eq)
		accuracy <- rbind(accuracy, acc)
	}
	accuracy <- mean(accuracy)
	print(accuracy)
	accuracy
}

## Estimates cross validation of the Binary classifier (based on Decision Trees) for the specified data set, performing splits into
## K subsets. Returns mean accuracy of cross validations  

crossValidationBin2 <- function(dataSet, K = 10) {
	accuracy <- numeric(0)
	folds    <- cvFolds(nrow(dataSet), K=K)
	totalCm  <- matrix(rep(0, 4), 2, 2)
	for(i in 1:K) {
		train <- dataSet[folds$subsets[folds$which != i], ]
		validation <- dataSet[folds$subsets[folds$which == i], ]
		classifier <- fitDTClassifier(train)
		pred <- predict(classifier, validation, type="class")
		validation <- c(validation[, ncol(dataSet)]) 
		eq <- as.vector(pred) == as.vector(validation)
		acc <- sum(eq)/length(eq)
		accuracy <- rbind(accuracy, acc)
	}
	accuracy <- mean(accuracy)
	print(accuracy)
	accuracy
}