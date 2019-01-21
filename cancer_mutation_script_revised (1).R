library(readr)

can_final <- read_csv("~/cancermutation.csv")
attach(can_final)

str(can_final)
names(can_final)
sapply(can_final,class)

can_final$X1 <-NULL
utils::View(can_final)


can_final<-na.omit(can_final)
utils::View(can_final)
can_final$label<-as.factor(can_final$label)
class(can_final$label)

# check for class bias 
table(can_final$label)
prop.table(table(can_final$label))

## result :0 0.5419262 1. 0.4580738 

can_final<-na.omit(can_final)
utils::View(can_final)


# Creating Training data because of unbalanced label class

input_ones<-can_final[which(can_final$label== 1),]
input_zeros<-can_final[which(can_final$label == 0),]

set.seed(100)

sample(1:nrow(input_ones),0.8*nrow(input_ones))
input_ones_training_rows<-sample(1:nrow(input_zeros),0.8*nrow(input_ones))
input_zeros_training_rows<-sample(1:nrow(input_zeros),0.8*nrow(input_ones))


training_ones <- input_ones[input_ones_training_rows,]
training_zeros <- input_zeros[input_zeros_training_rows,]

# row bind the training set of 0s and 1s

trainingData<- rbind(training_ones, training_zeros)
utils::View(trainingData)
sapply(train)

### substituting NAs with the Mean
na2mean <-function(x) replace(x,is.na(x),mean(x,na.rm = T))
trainingData[]<-lapply(trainingData,na2mean)
utils::View(trainingData)

class(trainingData$label)

## test data 
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
# row bind the test data sets
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

# Model building 

logitModel<- glm(label~.,data = trainingData, 
    family = 'binomial')

summary(logitModel)


# predicted 

predicted <-predict(logitModel, testData, type = 'response')
predicted

# predicted probability scores use plogis

predicted_log<- plogis(predict(logitModel,testData))


# checking for multicollinearity in the model use VarianceInflation Factor (VIF)
install.packages("InformationValue")


library(InformationValue)
optCutOff <- optimalCutoff(testData$label, predicted)[1] 
optCutOff

############ Model Diagnostics #########################3

summary(logitModel)
#summary(logitMod) gives the 
#beta coefficients, Standard error, z Value and p Value.
# If your model had categorical variables 
#with multiple levels, you will find a row-entry for 
#each category of that variable. 
#That is because, each individual category is considered 
#as an independent binary variable by the glm(). 
#In this case it is ok if few of the categories in 
#a multi-category variable don't turn out to 
#be significant in the model 
#(i.e. p Value turns out greater than significance level of 0.5).
#Misclassification Error



#########################VIF################################33
library(car) # for vif
round(vif(logitModel),3)
#Like in case of linear regression, we should check for multicollinearity in the model. As seen below, 
#all X variables in the model have VIF well below 4.
##### Misclassification Errors#########################
#Misclassification error is the percentage 
#mismatch of predcited vs actuals, 
#irrespective of 1's or 0's. 
#The lower the misclassification error, 
#the better is your model.

misClassError(testData$label, predicted, 
              threshold = optCutOff )
# Result: 0.1243

################### ROC ##############################
#Receiver Operating Characteristics Curve traces 
#the percentage of true positives accurately predicted 
#by a given logit model as the prediction probability 
#cutoff is lowered from 1 to 0. 
#For a good model, as the cutoff is lowered, 
#it should mark more of actual 1's as positives and 
#lesser of actual 0's as 1's. 
#So for a good model, the curve should rise steeply, 
#indicating that the TPR (Y-Axis) increases faster 
#than the FPR (X-Axis) as the cutoff score decreases. 
#Greater the area under the ROC curve, 
#better the predictive ability of the model.

plotROC(testData$label, predicted)
## result : 0.9349

## concordance 
#Ideally, the model-calculated-probability-scores 
#of all actual Positive's, (aka Ones) should be greater 
#than the model-calculated-probability-scores of ALL 
#the Negatives (aka Zeroes). 
#Such a model is said to be perfectly concordant and 
#a highly reliable one. 
#This phenomenon can be measured by Concordance and Discordance.

#In simpler words, of all combinations of 1-0 pairs (actuals), 
#Concordance is the percentage of pairs, 
#whose scores of actual positive's are greater than the scores of actual negative's. For a perfect model, this will be 100%. So, the higher the concordance, the better is the quality of model.

Concordance(testData$label, predicted)
# result : 
#$Concordance
#[1] 0.9354377 ### great result

#$Discordance
#[1] 0.06456227

#$Tied
#[1] 1.387779e-17

#$Pairs
#[1] 110467

#################sensitivity#################
#Sensitivity (or True Positive Rate) is the percentage 
#of 1's (actuals) correctly predicted by the model, 
#while, specificity is the percentage of 0's (actuals) correctly predicted. Specificity can also be calculated as 1 ??? False Positive Rate.


sensitivity(testData$label, predicted, threshold = optCutOff)
#####################SPECIFICITY###############################


specificity(testData$label,predicted, threshold = optCutOff)


confusionMatrix(testData$label, predicted, threshold = optCutOff)