library(arules)
library(leaps)

###
#
# SHOULD WE RUN regsubsets (for selecting the most meaningful variables) BEFORE OR AFTER RUNNING 
# THIS SCRIPT???? I TRIED BOTH WAYS, THERE WAS NO RELEVANT DIFFERENCE
#
###

# change this to your path
dataset <- read.csv("~/Desktop/6th_semester/srcr/project/fst_project/parte3/exaustao.csv")

# after using weka or running plot(density(dataset$<column>)) to see the distribution of each
# variable, we discretize each one accordingly
x1 <- as.numeric(discretize(dataset$Performance.KDTMean,method="frequency",categories=2,labels=1:2))
x2 <- as.numeric(discretize(dataset$Performance.MAMean,method="frequency",categories=4,labels=1:4))
x3 <- as.numeric(discretize(dataset$Performance.MVMean,method="frequency",categories=3,labels=1:3))
x4 <- as.numeric(discretize(dataset$Performance.TBCMean,method="frequency",categories=3,labels=1:3))
x5 <- as.numeric(discretize(dataset$Performance.DDCMean,method="frequency",categories=3,labels=1:3))
x6 <- as.numeric(discretize(dataset$Performance.DMSMean,method="frequency",categories=3,labels=1:3))
x7 <- as.numeric(discretize(dataset$Performance.AEDMean,method="frequency",categories=3,labels=1:3))
x8 <- as.numeric(discretize(dataset$Performance.ADMSLMean,method="frequency",categories=5,labels=1:5))

dataset$Performance.KDTMean <- x1 * (1/2)
dataset$Performance.MAMean <- x2 * (1/4)
dataset$Performance.MVMean <- x3 * (1/3)
dataset$Performance.TBCMean <- x4 * (1/3)
dataset$Performance.DDCMean <- x5 * (1/3)
dataset$Performance.DMSMean <- x6 * (1/3)
dataset$Performance.AEDMean <- x7 * (1/3)
dataset$Performance.ADMSLMean <- x8 * (1/5)

#dataset$FatigueLevel <- as.numeric(discretize(dataset$FatigueLevel,method="frequency",categories=3,labels=1:3))
dataset$FatigueLevel[which(dataset$FatigueLevel > 3)] <- 4
set.seed(11) # set a seed so that sample always gives the same train set
sample <- sample.int(n=nrow(dataset), size=floor(.75*nrow(dataset)),replace=F)
train <- dataset[sample, ]
test  <- dataset[-sample, ]

# uncomment the next lines and change category and labels to use other exhaustion scales
train$FatigueLevel = (train$FatigueLevel - 1) / 3

# this just maps work, office and programming to 1,2 and 3 (not sure about the order here)

#train$Performance.Task <- as.numeric(train$Performance.Task) * (1/3)
#train$Performance.Task <- as.numeric(train$Performance.Task)
#train$Performance.Task[train$Performance.Task == 1] <- 0
#train$Performance.Task[train$Performance.Task == 2] <- 0.5
#train$Performance.Task[train$Performance.Task == 3] <- 1
#test$Performance.Task <- as.numeric(test$Performance.Task)

# formula to use when using regsubsets
formFL <- FatigueLevel ~ Performance.KDTMean + Performance.MAMean + Performance.MVMean + Performance.DDCMean + Performance.AEDMean + Performance.DMSMean + Performance.ADMSLMean + Performance.TBCMean
formT <- Performance.Task ~ Performance.KDTMean + Performance.MAMean + Performance.MVMean + Performance.DDCMean + Performance.AEDMean + Performance.DMSMean + Performance.ADMSLMean + Performance.TBCMean
## write converted file. add your path
write.csv(dataset, file ="~/Desktop/6th_semester/srcr/project/projecto_rna/discrExaustao.csv", row.names=FALSE)

resultsFL <- regsubsets(formFL, dataset, nvmax=8)
resultsT <- regsubsets(formT, dataset, nvmax=8)