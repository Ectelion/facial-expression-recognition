## Custom implementation of a rule based classificator with rules extracted
## by analyzing one of the optimal Decision Trees

predictByRule <- function(features) {
	em <- rep(FALSE, 7)
	c1  <- features$X87  < -0.58
	c2  <- features$X117 < -1.7157 # Original: -1.7
	c3  <- features$X102 < -0.049
	c4  <- features$X118 < -0.85
	c5  <- features$X120 < -0.81
	c6  <- features$X125 <  0.69
	c7  <- features$X92  <  0.86
	c8  <- features$X89  <  0.86
	c9  <- features$X101 >= -0.014
	c10 <- features$X123 >= -1.4
	c11 <- features$X52  >= -0.61
	
	# Surprise ~ 0% confusion
	em[7] <- c1 && c3
	# Disgust ~ 5,77% confusion
	em[3] <- !c1 && !c2 && c4 && !c8
	# Happiness ~ 5,88% confusion
	em[5] <- !c1 && c2 && c5
	# Sadness ~ 8% confusion
	em[6] <- (c1 && !c3 && c6) || (!c1 && !c2 && !c4 && c7 && c9 && c10 && c11)
	# Fear ~ 9,09% confusion
	em[4] <- (c1 && !c3 && !c6) || (!c1 && !c2 && c4 && c8) || (!c1 && !c2 && !c4 && c7 && !c9)
	# Anger ~ 11,63% confusion 
	em[1] <- (!c1 && !c2 && !c4 && !c7) || (!c1 && !c2 && !c4 && c7 && c9 && c10 && !c11)
	# Contempt ~ 17,65% confusion
	em[2] <- (!c1 && c2 && !c5) || (!c1 && !c2 && !c4 && c7 && c9 && !c10)
	em
}

## Tests performance of the custom rule based classifier, implemented within the predictByRule() function

testRuleClassifier <- function(inputs, trueLabels) {
	hitsNum  <- 0
	falses <- c()
	
	for (inputInd in 1: nrow(inputs)) {
		emotions <- predictByRule(inputs[inputInd, ])
		if (emotions[trueLabels[inputInd]] == TRUE) {
			hitsNum <- hitsNum + 1	
		} else {
			falses <- c(falses, inputInd)
		}
	}
	print(falses)
	hitsNum
}

## [not_finalized] 
## Prediction based on Decision Tree and custom rules. The function
## wraps the tree based predictor, applying manually specified rules when applicable 

predictByTreeRule <- function(DTClassifier, data) {
	classesProbs <- predict(DTClassifier, data)
	maxProb <- max(classesProbs)
	maxInd <- which(classesProbs == maxProb, arr.ind = TRUE)
	
	if (maxInd == 3) {
		for (i in length(classesProbs)) {
			classesProbs[i]
		}
	}
}