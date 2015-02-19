Facial Expression Recognition
=============================

The main purpose of the project is recognition of emotions based on facial expressions. Cohn-Kanade data set 
(http://www.pitt.edu/~emotion/ck-spread.htm) is used for explorations and training.

The Cohn-Kanade is a data set of face images labeled for 7 expressions/emotions :
* Anger
* Contempt
* Disgust
* Fear
* Happiness
* Sadness
* Surprise

The solution consists of versatile classifiers including Decision Tree, Random Forests, Support Vector Machines, DT based Binary Classifier, simple hybrid classifier.

Performance results, estimated by cross validation tests: 

* Multi-class Support Vector Machines (radial kernel, kernlab): 97.3% (st. deviation = 0.4%)
* Multi-class Support Vector Machines (linear kernel, e1071): 96.5%
* Decision Tree based classifier: 89.63% 
