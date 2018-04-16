library(leaps)
library(neuralnet)
library(hydroGOF)
library(arules)

# Change this to your file name
FILE <- "C:\\Users\\joaop\\Documents\\GitHub\\srcr_assignment\\parte3\\original_data\\exaustao.csv"
dataset <- read.csv(FILE)

## DATA ADAPTATION ##
dataset$Performance.Task <- as.numeric(dataset$Performance.Task)

## USEFUL VALUES ##
formulaFL = FatigueLevel ~ Performance.KDTMean + Performance.MAMean + Performance.MVMean + Performance.TBCMean + Performance.DDCMean + Performance.DMSMean + Performance.AEDMean + Performance.ADMSLMean
formulaT = Performance.Task ~ Performance.KDTMean + Performance.MAMean + Performance.MVMean + Performance.TBCMean + Performance.DDCMean + Performance.DMSMean + Performance.AEDMean + Performance.ADMSLMean
formulaT_FL = Performance.Task + FatigueLevel ~ Performance.KDTMean + Performance.MAMean + Performance.MVMean + Performance.TBCMean + Performance.DDCMean + Performance.DMSMean + Performance.AEDMean + Performance.ADMSLMean

# DATA DISCRETIZATION #
# after using weka to see the distribution of each
# variable, we discretize each one accordingly
x1 <- as.numeric(discretize(dataset$Performance.KDTMean,method="frequency",categories=1,labels=1:1))
x2 <- as.numeric(discretize(dataset$Performance.MAMean,method="frequency",categories=3,labels=1:3))
x3 <- as.numeric(discretize(dataset$Performance.MVMean,method="frequency",categories=3,labels=1:3))
x4 <- as.numeric(discretize(dataset$Performance.TBCMean,method="frequency",categories=2,labels=1:2))
x5 <- as.numeric(discretize(dataset$Performance.DDCMean,method="frequency",categories=3,labels=1:3))
x6 <- as.numeric(discretize(dataset$Performance.DMSMean,method="frequency",categories=2,labels=1:2))
x7 <- as.numeric(discretize(dataset$Performance.AEDMean,method="frequency",categories=3,labels=1:3))
x8 <- as.numeric(discretize(dataset$Performance.ADMSLMean,method="frequency",categories=4,labels=1:4))

dataset$Performance.KDTMean <- x1
dataset$Performance.MAMean <- x2
dataset$Performance.MVMean <- x3
dataset$Performance.TBCMean <- x4
dataset$Performance.DDCMean <- x5
dataset$Performance.DMSMean <- x6
dataset$Performance.AEDMean <- x7
dataset$Performance.ADMSLMean <- x8


# set a seed so that sample always gives the same train set
set.seed(11)
TRAIN_PERCENTAGE = 0.75
DATASET_SIZE = nrow(dataset)

sample <- sample.int(n=DATASET_SIZE, size=floor(TRAIN_PERCENTAGE*DATASET_SIZE),replace=F)
train <- dataset[sample, ]
test  <- dataset[-sample, ]

# cases used to test neural network accuracy
testCases <- subset(test, select=c("Performance.MAMean", "Performance.AEDMean", "Performance.DDCMean", "Performance.DMSMean", "Performance.KDTMean", "Performance.MVMean", "Performance.TBCMean", "Performance.ADMSLMean"))



## STATISTICAL ANALISYS ##
rsubsFL <- regsubsets(x = formulaFL, data = dataset, nvmax = 8)
rsubsT <- regsubsets(x = formulaT, data = dataset, nvmax = 8)
rsubsT_FL <- regsubsets(x = formulaT_FL, data = dataset, nvmax = 8)

## NEURAL NETWORK DEFINITION AND TRAINING ##
# RNA for analysis of the Fatigue Level #
rnaFL <- neuralnet(formulaFL, train, hidden=c(7,4,4,3), lifesign = "FULL",algorithm="rprop+",linear.output = FALSE,threshold = 0.01)
rnaFL.results <- compute(rnaFL, testCases)
resultsFL <- data.frame(real=test$FatigueLevel, calculated=rnaFL.results$net.result)
resultsFL$calculated <- round(resultsFL$calculated, digits=0)
rmse(c(test$FatigueLevel), c(resultsFL$calculated))

# RNA for analysis of the Task #
rnaT <- neuralnet(formulaT, train, hidden=c(7,4,3), lifesign = "FULL",algorithm="rprop+",linear.output = FALSE,threshold = 0.01)
rnaT.results <- compute(rnaT, testCases)
resultsT <- data.frame(real=test$Performance.Task, calculated=rnaT.results$net.result)
resultsT$calculated <- round(resultsT$calculated, digits=0)
rmse(c(test$FatigueLevel), c(resultsT$calculated))

rnaT_FL <- neuralnet(formulaT_FL, train, hidden=c(7,4,3), lifesign = "FULL",algorithm="rprop+",linear.output = FALSE,threshold = 0.01)
