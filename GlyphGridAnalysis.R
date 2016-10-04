
#This produces all analyses for the glyphgrid study. It starts with the alldata.csv
#which is cleaned/extracted from the raw JSON produced by the experiment; 
#see GlyphGridCleaning.R for those details.

rm(list = ls())
directory = getwd()
library(languageR)
library(stringr)
library(lme4)
library(multcomp)
library(binom)
library(dplyr)
library(tidyr)
library(ggplot2)
mean.na.rm <- function(x) { mean(x,na.rm=T) }
stderr <- function(x) sqrt(var(x)/length(x))


#Read in the cleaned data file (alldata), extracted from JSON output of the psiturk script. 
#At this point, we have the *sequence* of cards people moved, S, V, O or X for other.
#We need to condense them for analysis, but we'll keep raw orders around for comparison. 

# total of 59 addtl ppl are excluded from analysis because their data 
#couldn't be parsed from the json string (indicating missed clicks/screen refresh type issues)

mydata <- read.csv('alldata.csv', header=T)

mydata = mydata %>%
  filter(browser != "EXCLUDED") %>%
  filter(isTestTrial == 1) %>%
  arrange(participant)


# Re-add stimulus info, since it didn't print from the exp
mydata$stimnum <- as.numeric(mydata$stimulus)

animate_stim <- c('girl-elbowing-oldlady.mp4','oldlady-rubbing-fireman.mp4','fireman-pushing-boy.mp4','girl-kissing-boy.mp4','girl-throwing-oldlady.mp4','fireman-kicking-girl.mp4','boy-lifting-girl.mp4')
inanimate_stim <- c('girl-pushing-car.mp4','boy-kicking-ball.mp4','fireman-lifting-car.mp4','oldlady-kissing-ball.mp4','fireman-throwing-ball.mp4','girl-pushing-ball.mp4','oldlady-elbowing-heart.mp4','girl-rubbing-heart.mp4')
intransitive_stim <-c('ball-rolling-none.mp4','girl-tumbling-none.mp4','car-tumbling-none.mp4','boy-rolling-none.mp4','car-rolling-none.mp4')

labelcat <- function(astring){
  cata = "NOT FOUND"
  if (astring %in% animate_stim){
    cata = "Animate"
  } else if (astring %in% inanimate_stim){
    cata = "Inanimate"
  }else if (astring %in% intransitive_stim){
    cata = "Intransitive"
  }
  return(cata)
}

mydata$StimCategory <- mapply(labelcat, mydata$stimulus)


###
# Word order simplification
###

#Now programmatically turn the raw sequence of card moves to a three-letter description of what
#they did.  We are interested in the 1st 3 symbols moved, either S, V, or O. When participants
#moved 2 categories plus a mistake, we will analyzed a 'relaxed' version as well, assuming they
#meant that X to be the missing element. 

##Get the first instance of each symbol people gave during each trial, this is the order we'll use
#NOTE: see isPerfect calc below if this bothers you. 

mydata$RawOrder <- as.character(mydata$RawOrder)

long_split = strsplit(mydata$RawOrder,character(0))
short_order = list()
for (i in 1:length(long_split)){
  if(mydata$RawOrder[i]!="NONE"){
    short_order[[i]] = paste(unique(long_split[[i]]), collapse='')
  } else {short_order[[i]] = "NONE"}
}

mydata$UniqueRawOrder = as.character(as.factor(unlist(short_order)))

##Now save up to 3 (transitive) or 2 (intransitive) glyphs, preferring SVO, allowing X if 
#all reasonable glyphs weren't passed. 

simpleOrderRelaxed <- function(acat, astring){
  simpleStr = ""
  astring = as.character(astring)
  if (acat %in% c("Inanimate","Animate")){
    maxlen = 3
  } else if (acat %in% c("Intransitive")){
    maxlen = 2
  }
  if (nchar(astring) > maxlen){ #we have an X (plus maybe more?) & don't need it!
    simpleStr = gsub("X","",astring)
    simpleStr = gsub("Y","",astring)
    simpleStr = gsub("Z","",astring)
    simpleStr = substr(simpleStr,1,maxlen)
  } else { #We have either an X we want to keep, or no Xs
    simpleStr = astring
  }
  
  return(simpleStr)
}

mydata$simpleOrderRelaxed <- mapply(simpleOrderRelaxed, mydata$StimCategory, mydata$UniqueRawOrder)

#Now make 2 versions that we'll analyze: simpleOrderStrict, which include only correct glyphs, and
#simpleOrderGenerous which replaces a single X with the missing glyph

toStrict <- function(astring){
  astring <- gsub("X", "", astring)
  astring <- gsub("Y", "", astring)
  astring <- gsub("Z", "", astring)
}

toGenerous <- function(acat, astring){
  if (acat %in% c("Inanimate","Animate")){
    needed <- c("S","V","O")
  } else if (acat %in% c("Intransitive")){
    needed <- c("S","V")
  }
  #remove items until we have just ones that the string doesn't have
  isMissing <- c()
  for (n in needed){
    if (!(str_detect(astring, n))){
      isMissing <- append(isMissing, n)
    }   
  }
  #If there is exactly 1 missing, replace the X with the missing thing (if it's there)
  if(length(isMissing) == 1){
    astring <- gsub("X", isMissing[1],astring)
    
  } else if(length(isMissing) > 1){ #couldn't do this one!
    astring = "UNFIXABLE"
  }
  
    #(otherwise leave it as-is)
  return(astring)
}
mydata$simpleOrderStrict <- mapply(toStrict, mydata$simpleOrderRelaxed)
mydata$simpleOrderGenerous <- mapply(toGenerous, mydata$StimCategory, mydata$simpleOrderRelaxed)


#Okay, so at this point we have 3 possible groups we could analyze: people whose strict simple 
#order is identical to their raw order, people whose first-mention order is a valid word
#order, and people whose 'generous'/corrected first-mention order is a valid word

passesMuster <- function(acat, astring){ #"please be the correct length! Above filters ensure that there is only 1 copy of each glyph listed in sequences
  if (acat %in% c("Inanimate","Animate")){
    corrlen = 3
  } else if (acat %in% c("Intransitive")){
    corrlen = 2
  }
  #Make sure we don't get trimmed "unfixable" entries
  if(astring == "UNFIXABLE"){
    astring = ""
  }
  return(corrlen == nchar(astring))
}

mydata$includeStrict <- mapply(passesMuster, mydata$StimCategory, mydata$simpleOrderStrict)
mydata$includeGenerous <- mapply(passesMuster, mydata$StimCategory, mydata$simpleOrderGenerous)
mydata$includePerfect <- mydata$includeStrict & (mydata$simpleOrderStrict == mydata$RawOrder)

#The isPerfect column allowed us to check what was up with people who had a usable Strict
#condensation (i.e. we got 3/2 'real' glyphs taking the first instance of each) but who did
#not match their ORIGINAL long order.  
#There are 64 such instances out of the 4,000ish total responses; some are esoteric word 
#orders like SVOV, but many are things like SSOV which indicate they 'dropped' a symbol on the way


###
# Analysis plan!  We have 2 sets of responses: 'readable' responses allowing for people
# to make 1 screwup (allows us to include the most data, which we'll see is a problem),
# and then a stricter one.  We focus on the 'readable' one since it required throwing away
# less data
###

#find & filter out participants who should be excluded because they reported cheating
mydata <- mydata %>%
  filter(cheated == 0) %>%
  filter(StimCategory != "Intransitive") #take just transitives!

#save the main & 'strict' rows, and narrow down to the columns relevant for each of those analyses

vars <- c('participant','trial.number', 'stimnum', 'StimCategory', 'simpleOrderStrict', 'RawOrder')
strictdata <- mydata %>%
  filter(includeStrict) %>%
  dplyr::select(one_of(vars))

vars <- c('participant','trial.number', 'stimnum', 'StimCategory', 'simpleOrderGenerous','RawOrder')
mydata <- mydata %>%
  filter(includeGenerous) %>%
  dplyr::select(one_of(vars))



#################################################################
## REPORT DESCRIPTIVES
sum.na.rm <- function(x) { sum(x,na.rm=T) }
my.sd <- function(x) {sd(x)/sqrt(length(x))}

#Report S counts (out of original 292)
subj.count = length(unique(mydata$participant)) #230

#Report counts of Word Order instances
word.order.counts = table(mydata$simpleOrderGenerous, mydata$StimCategory)

#And categorize them!
mydata$Order.Type <- ""
mydata[mydata$simpleOrderGenerous == "SOV",]$Order.Type <- "VerbLateral"
mydata[mydata$simpleOrderGenerous == "OSV",]$Order.Type <- "VerbLateral"
mydata[mydata$simpleOrderGenerous == "VSO",]$Order.Type <- "VerbLateral"
mydata[mydata$simpleOrderGenerous == "VOS",]$Order.Type <- "VerbLateral"
mydata[mydata$simpleOrderGenerous == "SVO",]$Order.Type <- "VerbMedial"
mydata[mydata$simpleOrderGenerous == "OVS",]$Order.Type <- "VerbMedial"

#Report counts of Word Order Type
order.type.counts = table(mydata$Order.Type, mydata$StimCategory)

#Make scores for each participant (we'll use these for graphing confidence intervals...)
mydata$ChoseVLat <- 0
mydata[mydata$Order.Type!="VerbMedial",]$ChoseVLat <- 1

ParticipantScores <- aggregate(mydata$ChoseVLat, by=list(mydata$participant, mydata$StimCategory), mean.na.rm)
names(ParticipantScores) <- c("participant", "ObjectType", "ChoseVLat")
ParticipantScores$ObjectType <- factor(ParticipantScores$ObjectType)

#Table for mean VLat scores, just taking a peak.
with(ParticipantScores, tapply(ChoseVLat, list(ObjectType), mean, na.rm=TRUE), drop=TRUE)

#Repeating the word order counts, let's look at people who produced a non-SVO order at some
#point in the proceedings. 
mydata$participant <- as.factor(mydata$participant)
nonSVO <- aggregate(ParticipantScores$ChoseVLat, by=list(ParticipantScores$participant), sum)
names(nonSVO)<- c("participant","nonSVO")
mydata <- merge(mydata, nonSVO)

mixers <- filter(mydata, nonSVO>0)
mixer.order.counts = table(mixers$simpleOrderGenerous, mixers$StimCategory)

word.order.counts #from above, all participants
mixer.order.counts

length(unique(mydata$participant))
length(unique(mixers$participant))
#########
# GRAPHS
#########


#Bootstrapped confidence intervals 
library(bootstrap)
animate.boot.mean = bootstrap(ParticipantScores[ParticipantScores$ObjectType=="Animate",]$ChoseVLat, 1000, mean)
quantile(animate.boot.mean$thetastar, c(0.025, 0.975))
inanimate.boot.mean = bootstrap(ParticipantScores[ParticipantScores$ObjectType=="Inanimate",]$ChoseVLat, 1000, mean)
quantile(inanimate.boot.mean$thetastar, c(0.025, 0.975))


GraphScores <- aggregate(ParticipantScores$ChoseVLat, by=list(ParticipantScores$ObjectType), mean.na.rm)
names(GraphScores) <- c("ObjectType", "ChoseVLat")
GraphScores <- arrange(GraphScores, desc(ObjectType))


GraphScores$errorLow = 0
GraphScores$errorHigh = 0
GraphScores[GraphScores$ObjectType == "Animate",]$errorLow = quantile(animate.boot.mean$thetastar, 0.025)
GraphScores[GraphScores$ObjectType == "Inanimate",]$errorLow = quantile(inanimate.boot.mean$thetastar, 0.025)
GraphScores[GraphScores$ObjectType == "Animate",]$errorHigh = quantile(animate.boot.mean$thetastar, 0.975)
GraphScores[GraphScores$ObjectType == "Inanimate",]$errorHigh = quantile(inanimate.boot.mean$thetastar, 0.975)

GraphScores$ObLabel <- ""
GraphScores[GraphScores$ObjectType == "Inanimate",]$ObLabel <- "Inanimate patient"
GraphScores[GraphScores$ObjectType == "Animate",]$ObLabel <- "Animate patient"

library(RColorBrewer)
my.cols <- brewer.pal(9, "Purples")
my.cols <- c(my.cols[6], my.cols[3])


ggplot(data=GraphScores, aes(x=ObLabel, y=ChoseVLat, fill=ObLabel)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=errorLow, ymax=errorHigh), colour="black", width=.1, position=position_dodge(.9)) +
  scale_fill_manual(values=my.cols) +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1))+
  xlab('') +
  ylab('proportion VNM orders (inc. SOV)') +
  theme_bw() +
  theme(legend.position='none')

ggsave('glyphgrid.jpg')
  

#########################################
##STATISTICAL TESTS!!
#########################################

#they are definitely not different from one another (numerically in the OPPOSITE direction)

mydata$StimCategory <- as.factor(mydata$StimCategory)

#m1 <- glmer(ChoseVLat ~ StimCategory + (StimCategory|participant) + (StimCategory|stimnum), data=mydata, family="binomial")
#doesn't converge, start dropping slopes...
m1 <- glmer(ChoseVLat ~ StimCategory + (StimCategory|participant) + (1|stimnum), data=mydata, family="binomial")
m0 <- glmer(ChoseVLat ~ 1 + (StimCategory|participant) + (1|stimnum), data=mydata, family="binomial")
anova(m1,m0)
