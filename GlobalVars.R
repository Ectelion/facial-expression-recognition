## Directory, where the project is located
WORK_DIR     = "{PATH_TO_PROJECT}/FER Dataset/facial-expression-recognition"
## Directory for the data set, containing "CK" and "CK+" folders from the orinigal Cohn-Kanade data set 
DATA_SET_DIR = "{PATH_TO_PROJECT}/FER Dataset/Data provided"

## File system configuration variables
imagesFolder          <- paste(DATA_SET_DIR, "CK+/Landmarks/cohn-kanade-images" , sep = "/")
emotionLabelsFolder   <- paste(DATA_SET_DIR, "CK+/Emotion_labels/Emotion"       , sep = "/")
landmarksFolder       <- paste(DATA_SET_DIR, "CK+/Landmarks/Landmarks"          , sep = "/")
AULabelsFolder        <- paste(DATA_SET_DIR, "CK+/FACS_labels/FACS"             , sep = "/")
faceExpressionsFolder <- paste(WORK_DIR,     "Facial Expressions Sorted"        , sep = "/")
outputFolder          <- "Observations"

# Training and test folders
trainingDataFolder <- paste(WORK_DIR, "Training", sep = "/")
testDataFolder     <- paste(WORK_DIR, "Training/Test", sep = "/")

emotionTitles <- c(
    'Emotion 1. Anger'    , 
    'Emotion 2. Contempt' , 
    'Emotion 3. Disgust'  ,
    'Emotion 4. Fear'     ,
    'Emotion 5. Happiness',
    'Emotion 6. Sadness'  ,
    'Emotion 7. Surprise' 
)

## Folders by types of emotions
angerFolder     = paste(faceExpressionsFolder, emotionTitles[1], sep = "/")
contemptFolder  = paste(faceExpressionsFolder, emotionTitles[2], sep = "/")
disgustFolder   = paste(faceExpressionsFolder, emotionTitles[3], sep = "/")
fearFolder      = paste(faceExpressionsFolder, emotionTitles[4], sep = "/")
happinessFolder = paste(faceExpressionsFolder, emotionTitles[5], sep = "/")
sadnessFolder   = paste(faceExpressionsFolder, emotionTitles[6], sep = "/")
surpriseFolder  = paste(faceExpressionsFolder, emotionTitles[7], sep = "/")

## Emotional labels sorted in ascending emotional code order
## Usage example: emotionFolders[emotionLabels$happy]
emotionFolders <- c(angerFolder, contemptFolder, disgustFolder, fearFolder, happinessFolder, sadnessFolder, surpriseFolder)

## Emotional labels with codes
emotionLabels <- list(
    neutral  = 0, 
    anger    = 1, 
    contempt = 2, 
    disgust  = 3, 
    fear     = 4, 
    happy    = 5, 
    sadness  = 6, 
    surprise = 7
)

## Abbreviated names for packages
pFE <- "FeatureExtraction.R"
pC  <- "Classification.R"

