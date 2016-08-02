directory = getwd()

library(languageR)
library(stringr)
library(lme4)
library(multcomp)
library(binom)
mean.na.rm <- function(x) { mean(x,na.rm=T) }
stderr <- function(x) sqrt(var(x)/length(x))



#Read in the cleaned data file (alldata), extracted from JSON output of the psiturk script. 
#See GlyphGridCleaning.R for details

mydata <- read.csv('dflong_full.csv', header=T)
mydata <- subset(mydata, select=-c(X))
mydata <- mydata[mydata$isTestTrial==1,]
mydata <- mydata[mydata$IsTransitive==1,]
mydata <- mydata[mydata$cheated==0,]
mydata <- mydata[mydata$PassesPractice==1,]
mydata <- mydata[mydata$WithAlmost!='BadResponse',]
#handData <- read.csv('CaseInstructionData.csv', header=T)
#freeData$Instructions <- "FreeInstructions"
#handData$Instructions <- "HandInstructions"
#freeData[is.na(freeData$Spatial.Cue),]$Spatial.Cue <- 0
#handData$Spatial.Cue <- as.numeric(as.character(handData$Spatial.Cue))
#handData[is.na(handData$Spatial.Cue),]$Spatial.Cue <- 0
#mydata <- rbind(freeData,handData)

#Get rid of the speech columns, we don't need them!
#mydata <- mydata[mydata$Trial.Type == "Gesture",]

#Handle problem people and items!!

#mydata <- mydata[(mydata$Trial.Number != 2) & (mydata$Trial.Number != 3),]
#mydata <- mydata[mydata$Subject != 28,]

#Important columns!
#Instructions <- None or Case marking
#Type.of.Action <- Transitive/Intransitive sentence
#Object.Type <- Person, Object, or None type patient
#Word.Order <- SOV, SVO <- Participants raw response! Sequence of gestures
#Spatial.Cue <- Did the participants use a spatial cue for the subject and object?
#Exclude <- Individual trials to be excluded
#P_Before_A <- Hand-coded interpretation of the sequences (1 means SOV) - Errorful!

#################################################################
## CATEGORIZE RESPONSES

#We want to know whether S and O are separated by the V or not. Since people sometimes gesture extra, we'll keep 
#the first instance of each item.  So a person who gestures SVSVVO would be scored as SVO, and a person who 
#gestured SOOVS would be coded as SOV.  

#Find the first instance of each item
# mydata$C_S.pos <- str_locate(mydata$CleanOrder,"S")[,1]
# mydata$C_V.pos <- str_locate(mydata$CleanOrder,"V")[,1]
# mydata$C_O.pos <- str_locate(mydata$CleanOrder,"O")[,1]

mydata$S.pos <- str_locate(mydata$WithAlmost,"S")[,1]
mydata$V.pos <- str_locate(mydata$WithAlmost,"V")[,1]
mydata$O.pos <- str_locate(mydata$WithAlmost,"O")[,1]
mydata$X.pos <- str_locate(mydata$WithAlmost,"X")[,1]

#And code that for the measure of interest!
#mydata$Participants.Clustered <- ((mydata$S.pos < mydata$V.pos) & (mydata$O.pos < mydata$V.pos)) | ((mydata$S.pos > mydata$V.pos) & (mydata$O.pos > mydata$V.pos))
patient.before.action = function(S, O, V, X) {
  if (!is.na(X)) {
    if (!is.na(S)) {
      if (!is.na(O)) {
        check.PA.order = X > O
      } else {
        check.PA.order = V > X
      }
    } else {
      check.PA.order = V > O
    }
  } else {
    check.PA.order = V > O
  }
  
  if (!is.na(S)) {
    if (check.PA.order) {
      if (S==1) {
        fixed.order = 'SOV'
        order.type = 'VFin'
        subject.first = 1
      } else if (S==2) {
        fixed.order = 'OSV'
        order.type = 'VFin'
        subject.first = 0
      } else {
        fixed.order = 'OVS'
        order.type = 'VMed'
        subject.first = 0
      }
    } else {
      if (S==1) {
        fixed.order = 'SVO'
        order.type = 'VMed'
        subject.first = 1
      } else if (S==2) {
        fixed.order = 'VSO'
        order.type = 'VInt'
        subject.first = 0
      } else {
        fixed.order = 'VOS'
        order.type = 'VInt'
        subject.first = 0
      }
    } 
  } else {
    if (check.PA.order) {
      if (X==1) {
        fixed.order = 'SOV'
        order.type = 'VFin'
        subject.first = 1
      } else if (X==2) {
        fixed.order = 'OSV'
        order.type = 'VFin'
        subject.first = 0
      } else {
        fixed.order = 'OVS'
        order.type = 'VMed'
        subject.first = 0
      }
    } else {
      if (X==1) {
        fixed.order = 'SVO'
        order.type = 'VMed'
        subject.first = 1
      } else if (X==2) {
        fixed.order = 'VSO'
        order.type = 'VInt'
        subject.first = 0
      } else {
        fixed.order = 'VOS'
        order.type = 'VInt'
        subject.first = 0
      }
    }
  }
  
  return(c(check.PA.order, subject.first, fixed.order, order.type))
}


mydata$Usable.Fixed <- mapply(patient.before.action, mydata$S.pos, mydata$O.pos, mydata$V.pos, mydata$X.pos)[3,]
mydata$Patient.Before.Action <- as.logical(mapply(patient.before.action, mydata$S.pos, mydata$O.pos, mydata$V.pos, mydata$X.pos)[1,])
mydata$Subject.First <- as.integer(mapply(patient.before.action, mydata$S.pos, mydata$O.pos, mydata$V.pos, mydata$X.pos)[2,])
mydata$Order.Type <- mapply(patient.before.action, mydata$S.pos, mydata$O.pos, mydata$V.pos, mydata$X.pos)[4,]

percent.subject.first = mean(mydata$Subject.First)


#Keep only data that has a codeable response!
mydata <- mydata[!is.na(mydata$Patient.Before.Action),]

#################################################################
## REPORT DESCRIPTIVES
sum.na.rm <- function(x) { sum(x,na.rm=T) }
my.sd <- function(x) {sd(x)/sqrt(length(x))}

#Report S counts
subj.count = length(unique(mydata$participant))

#Report counts of Word Order instances
word.order.counts = table(mydata$Usable.Fixed, mydata$Animacy, exclude = c('NoObject', 'PracticeImage'))

#Report counts of Word Order Type
order.type.counts = table(mydata$Order.Type, mydata$Animacy, exclude = c('NoObject', 'PracticeImage'))


#Make scores for each participant
mydata$ChoseVLat <- 0
mydata[mydata$Order.Type!="VMed",]$ChoseVLat <- 1

ParticipantScores <- aggregate(mydata$ChoseVLat, by=list(mydata$participant, mydata$Animacy), sum.na.rm)
names(ParticipantScores) <- c("Participant", "ObjectType", "ChoseVLat")
ParticipantScores$ObjectType <- factor(ParticipantScores$ObjectType)

#Table for mean VLat scores
with(ParticipantScores, tapply(ChoseVLat, list(ObjectType), mean, na.rm=TRUE), drop=TRUE)

#Time for bootstrapped confidence intervals around the means of the 4 conditions!
library(bootstrap)
animate.boot.mean = bootstrap(ParticipantScores[ParticipantScores$ObjectType=="animate",]$ChoseVLat, 1000, mean)
quantile(animate.boot.mean$thetastar, c(0.025, 0.975))
inanimate.boot.mean = bootstrap(ParticipantScores[ParticipantScores$ObjectType=="inanimate",]$ChoseVLat, 1000, mean)
quantile(inanimate.boot.mean$thetastar, c(0.025, 0.975))

# ObjectFree.boot.mean = bootstrap(ParticipantScores[ParticipantScores$ObjectType=="Object" & ParticipantScores$Instructions=="FreeInstructions",]$ChoseSOV, 1000, mean)
# quantile(ObjectFree.boot.mean$thetastar, c(0.025, 0.975))
# ObjectHand.boot.mean = bootstrap(ParticipantScores[ParticipantScores$ObjectType=="Object" & ParticipantScores$Instructions=="HandInstructions",]$ChoseSOV, 1000, mean)
# quantile(ObjectHand.boot.mean$thetastar, c(0.025, 0.975))

#########################################
##STATISTICAL TESTS!!
#########################################

mydata$Usable.Fixed <- as.factor(mydata$Usable.Fixed)
mydata$Animacy <- as.factor(mydata$Animacy)

AnimateData <- mydata[mydata$Animacy == "animate",]
InanimateData <- mydata[mydata$Animacy == "inanimate",]
AnimateData$Usable.Fixed <- as.factor(AnimateData$Usable.Fixed)
InanimateData$Usable.Fixed <- as.factor(InanimateData$Usable.Fixed)
AnimateData$Animacy <- as.factor(AnimateData$Animacy)
InanimateData$Animacy <- as.factor(InanimateData$Animacy)

AnimateVLat = mean(AnimateData$ChoseVLat)
InanimateVLat = mean(InanimateData$ChoseVLat)

#CHECK 1
# stimulus_model <- lmer(ChoseVLat ~ stimulus  + (1+stimulus|participant) + (1|Animacy), data=mydata, family="binomial")
# stimulus_nofix <- lmer(ChoseVLat ~ (1+stimulus|participant) + (1|Animacy), data=mydata, family="binomial")
# anova(stimulus_model, stimulus_nofix)

#CHECK 2
Animacy_OrdType_model <- lmer(ChoseVLat ~ Animacy  + (1+Animacy|participant) + (1|stimulus), data=mydata, family="binomial")
Animacy_OrdType_nofix <- lmer(ChoseVLat ~ (1+Animacy|participant) + (1|stimulus), data=mydata, family="binomial")
VLat.anova = anova(Animacy_OrdType_model, Animacy_OrdType_nofix)


#### 


