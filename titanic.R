library(ggplot2)

rm(list = ls()) # remove all the environment
getwd()
#setwd("~/MEGA/Uni/Intro Data Science/Data") #Andreswd
setwd("C:/Users/alejo/OneDrive/Documentos/UC3M/Data Science Introduction/data") #AlejoWd

load("titanic_train.Rdata")
head(titanic.train)

# We prepair the data for its analysis -----------------------------------------------------------------------------------------------
prepairData = function(titanic.train){
  #Change binary survived to true or false
  titanic.train$Survived=titanic.train$Survived==1
  
  #Change embarked ports to their proper name
  indexEmbarked = which(colnames(titanic.train) == "Embarked")
  titanic.train$Embarked = as.character(titanic.train$Embarked)
  
  embarkedFactors = c("C", "Q", "S")
  embarkedNames = c("Cherbourg", "Queenstown", "Southampton")
  
  for (i in 1:length(embarkedFactors)) {
    indexPort = which(titanic.train[, indexEmbarked] == embarkedFactors[i])
    for (a in indexPort) {
      titanic.train[a, indexEmbarked] = as.character(embarkedNames[i])
    }
  }
  
  titanic.train$Embarked = as.factor(titanic.train$Embarked)
  titanic.train$Survived = as.factor(titanic.train$Survived)
  
  return(titanic.train)
}
titanic.train = prepairData(titanic.train)
#-----------------------------------------------------------------------------------------------------------------------------------




#Main characteristics of each variable. --------------------------------------------------------------------------------------------
#AGE
class(titanic.train$Age)
summary(titanic.train$Age)

ggplot(titanic.train)+aes(x=Age, fill = Survived)+geom_density(alpha = 0.8)+
  scale_fill_manual(values = c("#00E5FF", "#8BC34A"))

ggplot(titanic.train)+aes(x = Age, fill = Sex)+
  geom_boxplot()+theme(legend.position = "top")+ scale_fill_manual(values = c("#66BB6A", "#E7B800"))
#Fare
class(titanic.train$Fare)
summary(titanic.train$Fare)
#Pclass
class(titanic.train$Pclass)
table(titanic.train$Pclass)
ggplot(titanic.train)+aes(x=Pclass, fill = Survived)+geom_bar()+
  scale_fill_manual(values = c("#00E5FF", "#8BC34A"))
#Sex
class(titanic.train$Sex)
table(titanic.train$Sex)
#SibSp
class(titanic.train$SibSp)
table(titanic.train$SibSp)
summary(titanic.train$SibSp)
#Parch
class(titanic.train$Parch)
table(titanic.train$Parch)
summary(titanic.train$Parch)
#ticket
class(titanic.train$Ticket)
table(titanic.train$Ticket)
#Cabin
class(titanic.train$Cabin)
table(titanic.train$Cabin)
#Embarked
class(titanic.train$Embarked)
table(titanic.train$Embarked)


#Question 1: Study the ports. How rich they are, what class they are in according to the port they embarked in. ----------------------
for (i in levels(titanic.train$Embarked)) {
  print(i)
  aux = which(titanic.train$Embarked == i)
  data = titanic.train[aux, ]
  print(summary(data$Fare))
  print(paste("The standard deviation is", sd(data$Fare)))
}

ggplot(titanic.train, aes(fill = Pclass, y = Fare)) + geom_boxplot() + facet_wrap(~Embarked)

prop.table(table(titanic.train$Embarked, titanic.train$Pclass), 1)
ggplot(titanic.train, aes(x = Embarked, fill = Pclass)) + geom_bar(position = position_fill())
#--------------------------------------------------------------------------------------------------------------------------------



#Question 2: If you belong to one class is more probable to survive? -------------------------------------------------------------

#Survival with Pclass
ggplot(titanic.train)+aes(x = Pclass, fill = Survived,)+
  geom_bar(position = position_dodge())+ scale_fill_manual(values = c("#00E5FF", "#8BC34A"))

#First Class Trial
Survived_Fclass = sum((titanic.train$Pclass == "1") & titanic.train$Survived)
Dead_Fclass = sum((titanic.train$Pclass == "1") & (titanic.train$Survived == FALSE))
FClass = as.data.frame(prop.table(c(Survived_Fclass,Dead_Fclass)))
First = cbind(Survived_Fclass, Dead_Fclass)

#Class and Survival Ratio
tab = prop.table(table(titanic.train$Survived,titanic.train$Pclass), margin=2)
tab = as.data.frame(tab)
ggplot(as.data.frame(tab), aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_manual(values = c("#00E5FF", "#8BC34A"))+ xlab("Passenger Class") +labs(fill = "Survived")+ylim(0,1)
#--------------------------------------------------------------------------------------------------------------------------------------


  
#Question 3: Is one class paying more? Is the age relevant to be in one class or another?----------------------------------------------
#Relation between Fare and PClass

cor(titanic.train$Fare,titanic.train$Pclass)

#Age and Survival in density
class(titanic.train$Pclass)
titanic.train$Pclass = as.factor(titanic.train$Pclass)
ggplot(titanic.train)+aes(x=Age, fill = Survived)+geom_density(alpha = 0.8)+
  scale_fill_manual(values = c("#00E5FF", "#8BC34A"))

#Summary of the Age in each class
for (i in 1:3){
  aux = which(titanic.train$Pclass == i)
  mean_age = summary(titanic.train$Age[aux])
  print(mean_age)
}

#Fare,Age and Pclass
ggplot(titanic.train) + aes(x = Age, y = Fare, color = Pclass)+
  geom_point(size = 2) + ylim(0,300) +scale_color_manual(values = c("#00E5FF", "#8BC34A","#E65100"))+
  theme(text = element_text(size = 10))+ facet_wrap(~ Pclass) + theme(legend.position = "none")
#--------------------------------------------------------------------------------------------------------------------------------------

  
  
#Question 4: How is related the Sex, the Survival Ratio and the Age?-------------------------------------------------------------------

#Sex proportion
females = length(which(titanic.train$Sex == "female"))
males = length(which(titanic.train$Sex == "male"))
cbind(females,males)

#Female/Survived
Fsurvived = which((titanic.train$Survived == TRUE) & (titanic.train$Sex == "female"))
FDead = which((titanic.train$Survived == FALSE) & (titanic.train$Sex == "female"))
FSurvived_per = (length(Fsurvived)/females)*100 
FDead_per = (length(FDead)/females)*100 

#Male/Survived
Msurvived = which((titanic.train$Survived == TRUE) & (titanic.train$Sex == "male"))
length(Msurvived)
MDead = which((titanic.train$Survived == FALSE) & (titanic.train$Sex == "male"))
length(MDead)
MSurvived_per = (length(Msurvived)/males)*100 
MDead_per = (length(MDead)/males)*100 

#Survived Ratio
Survived_Sex_Ratio = rbind(MSurvived_per,MDead_per,FSurvived_per,FDead_per)
#Plot Sex&Survived
tab = prop.table(table(titanic.train$Survived,titanic.train$Sex), margin=2)
tab = as.data.frame(tab)

#Histogram with Age and Survived
ggplot(as.data.frame(tab), aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_manual(values = c("#00E5FF", "#8BC34A"))+ xlab("Sex") +labs(fill = "Survived")+ylim(0,1)

#Density Plot with Age, Sex and Survied
ggplot(titanic.train, aes(fill = Survived, x = Age)) + geom_density(alpha= 0.8) + facet_wrap(~Sex)+
  scale_fill_manual(values = c("#00E5FF", "#8BC34A"))

#Correlation Coefficient between Age and Survived
cor(titanic.train$Age,titanic.train$Survived)
#--------------------------------------------------------------------------------------------------------------------------------------


  
# Question 5: What person had the highest probability to survive according to age, sex and family? -------------------------------------
#Visualize de data
summary(titanic.train$Age)
summary(titanic.train$SibSp)
summary(titanic.train$Parch)

ggplot(titanic.train, aes(x = Age)) + geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = 0.1, fill = "red")

ggplot(titanic.train, aes(x = Sex)) + geom_bar(aes(y = (..count..)/sum(..count..)))

ggplot(titanic.train, aes(x = SibSp)) + geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = 0.1, fill = "red")

ggplot(titanic.train, aes(x = Parch)) + geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = 0.1, fill = "red")

#Functions to make sequences according to data and a vector sequence
makeSequences = function(intervalLength, data) {
  SEQUENCES = list(0, 0, 0, 0)
  intervalStart = 0
  
  for (i in 1:length(intervalLength)) {
    intervalNumber = ceiling(max(data[, i]) / intervalLength[i])
    sequence = seq(from = intervalStart, to = (intervalLength[i] * intervalNumber) - 1, by = intervalLength[i])
    SEQUENCES[[i]] = c(sequence)
  }
  
  return(SEQUENCES)
}

#Function to make a sequence as a vector of strings (intervals)
makeSequenceAsIntervalVectors = function(sequence, intervalLength, indexSequence) {
  intervals = c()
  
  for (i in 1:length(sequence)) {
    if (indexSequence == 2) intervals = c(intervals, (sequence[i] + intervalLength[indexSequence]))
    else intervals = c(intervals, paste(sequence[i], "to", (sequence[i] + intervalLength[indexSequence])))
  }
  
  return(intervals)
}
    
#Function to get the intervals from a string (almost the opposite of the previous functions)
getIntervalFromString = function(string, splitCharacters) {
  list = strsplit(string, splitCharacters)
  interval = c()
  
  for (i in 1:length(list)) {
    interval = c(interval, as.integer(list[[i]]))
  }
  return(interval)
}

INTERVAL_LENGTH = c(15, 1, 2, 2)  #Length of the intervals (age, sex, sibsp, parch)
DATA = data.frame(Age = titanic.train$Age, 
                  Sex = as.integer(titanic.train$Sex), 
                  SibSp = titanic.train$SibSp, 
                  Parch = titanic.train$Parch, 
                  Survived = titanic.train$Survived)

SEQUENCES = makeSequences(intervalLength = INTERVAL_LENGTH, data = DATA) #get sequence of all the intervals (previous function explained)

# Returns a data.frame with all the possible combinations between age, sex, sibsp, parch intervals
DATA_PROBABILITY = expand.grid(AgeInterval = makeSequenceAsIntervalVectors(SEQUENCES[[1]], INTERVAL_LENGTH, 1), 
                               Sex = makeSequenceAsIntervalVectors(SEQUENCES[[2]], INTERVAL_LENGTH, 2), 
                               SibSpInterval = makeSequenceAsIntervalVectors(SEQUENCES[[3]], INTERVAL_LENGTH, 3), 
                               ParchInterval = makeSequenceAsIntervalVectors(SEQUENCES[[4]], INTERVAL_LENGTH, 4),
                               total = 0, 
                               totalSurvived = 0, 
                               probabilitySurvived = 0,
                               stringsAsFactors = FALSE)

#For every row in the previous data.frame we get the total people in that combination of intervals, the people that survived within
#it, and the probability that that group had to survive. We save it in the columns of the data.frame.
for (i in 1:nrow(DATA_PROBABILITY)) {
  #We use the which() expression to get all the people in the combination of intervals and the ones that survived, but we make it as
  #a string first in order to be able to automate the process, so we use eval(parse(text = "which expression")) to run a string as a R expression.
  SPLIT_CHARACTERS = " to "
  whichString = "which("
  
  for (a in 1:(ncol(DATA_PROBABILITY) - 3)) {
    if (a == 2) interval = DATA_PROBABILITY[i, a]
    else interval = getIntervalFromString(DATA_PROBABILITY[i, a], SPLIT_CHARACTERS)
    
    if (a != 1) whichString = paste(whichString, " & ", sep ="")
    
    if (a == 2) {
      whichString = paste(whichString, "(DATA[, ", a, "] == ", interval, ")", sep = "")
    } else {
      whichString = paste(whichString, "(DATA[, ", a, "] >= ", interval[1], ") & (DATA[, ", a, "] < ", interval[2], ")", sep = "")
    }
  }
  
  whichStringSurvived = paste(whichString, "& (DATA[, 5] == TRUE))")
  whichString = paste(whichString, ")")
  
  total = length(eval(parse(text = whichString)))
  totalSurvived = length(eval(parse(text = whichStringSurvived)))
  probabilitySurvived = round(totalSurvived / total, 3)
  if (is.nan(probabilitySurvived)) probabilitySurvived = 0
  
  DATA_PROBABILITY$total[i] = total
  DATA_PROBABILITY$totalSurvived[i] = totalSurvived
  DATA_PROBABILITY$probabilitySurvived[i] = probabilitySurvived
}

#We view the data
head(DATA_PROBABILITY)
View(DATA_PROBABILITY)

#Set up the threshold and get all the ones equal or above that threshold.
PROBABILITY_SURVIVED_LIMIT_TOP = 0.75
indexesHighSurvivedProbabiltyTop = which(DATA_PROBABILITY$probabilitySurvived >= PROBABILITY_SURVIVED_LIMIT_TOP)

#Create a new data.frame and organize it for better understanding
DATA_PROBABILITY_HIGH = DATA_PROBABILITY[indexesHighSurvivedProbabiltyTop, ]
DATA_PROBABILITY_HIGH$Sex = ifelse(DATA_PROBABILITY_HIGH$Sex == 1, yes = "Female", no = "Male")
DATA_PROBABILITY_HIGH = DATA_PROBABILITY_HIGH[order(-DATA_PROBABILITY_HIGH$probabilitySurvived), ]

View(DATA_PROBABILITY_HIGH)









