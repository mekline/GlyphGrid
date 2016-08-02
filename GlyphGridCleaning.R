## Lines 1-405 were used to clean the big hairy JSON output of the experiment script into a csv
## with raw word orders of cards that participants passed. 
##
## MIGUEL SALINAS (masm@mit.edu)
## CODE SKELETON PROVIDED BY MELISSA KLEIN (mekline@mit.edu)
## LOAD R-PACKAGES ----------------------------------------------------------------
rm(list=ls())
library(lsr)
library(dplyr)
library(rjson)
library(RSQLite)
library(stringr)
library(ggplot2)
library(Hmisc)

## CREATE HELPER FUNCTIONS FOR ANALYSIS
mean.na.rm <- function(x) { mean(x,na.rm=T) }
sum.na.rm <- function(x) { sum(x,na.rm=T) }
stderr <- function(x) sqrt(var(x)/length(x))


# READ DATA  ---------------------------------------------------------------
# DATA TAKEN FROM particpants.db WITHIN THE DIRECTORY #
con = dbConnect(SQLite(),dbname = "experiment script/participants.db");
df.complete = dbReadTable(con,"glyphs") 
dbDisconnect(con)

# FILTER OUT MTURK HITS THAT WERE NOT FULLY COMPLETED (USING DPLYR METHODS)
df.complete = subset(df.complete, status %in% c(3,4)) 

#nrow(df.complete) includes all subjects ever plus all debug attempts!
#filter to a particular day (if I haven't set codeversions).
##HERE ARE ALL THE PILOT RUNS -- SOME RUNS WERE DONE IN CHUNKS ON DIFFERENT DAYS##
##PILOTS includes: developing basic script, getting ppl to use clicker, getting them to
##do so quickly, calibrating the timer. 
df.complete$currentVersion.pilot1.1 = str_detect(df.complete$beginhit, "2015-03-24")
df.complete$currentVersion.pilot1.2 = str_detect(df.complete$beginhit, "2015-03-25")
df.complete$currentVersion.pilot2 = str_detect(df.complete$beginhit, "2015-05-27")
df.complete$currentVersion.pilot3 = str_detect(df.complete$beginhit, "2015-06-21")
df.complete$currentVersion.pilot4 = str_detect(df.complete$beginhit, "2015-06-24 15:32:10.316064")
df.complete$currentVersion.pilot5.1 = str_detect(df.complete$beginhit, "2015-07-27")
df.complete$currentVersion.pilot5.2 = str_detect(df.complete$beginhit, "2015-07-29")
df.complete$currentVersion.pilot5.3 = str_detect(df.complete$beginhit, "2015-07-30")
df.complete$currentVersion.pilot6 = str_detect(df.complete$beginhit, "2015-09-23 22:33:42.206700")
df.complete$currentVersion.pilot7.1 = str_detect(df.complete$beginhit, "2015-09-23")
df.complete$currentVersion.pilot7.2 = str_detect(df.complete$beginhit, "2015-09-24")
df.complete$currentVersion.pilot8 = str_detect(df.complete$beginhit, "2015-10-07")

##HERE ARE ALL THE LARGER SAMPLES 
### RUN#1 - THERE WERE 175 PARTICIPANTS RUN THROUGHOUT 5 DAYS
df.complete$currentVersion.Run1.1 = str_detect(df.complete$beginhit, "2015-07-31")
df.complete$currentVersion.Run1.2 = str_detect(df.complete$beginhit, "2015-08-01")
df.complete$currentVersion.Run1.3 = str_detect(df.complete$beginhit, "2015-08-02")
df.complete$currentVersion.Run1.4 = str_detect(df.complete$beginhit, "2015-08-11")
df.complete$currentVersion.Run1.5 = str_detect(df.complete$beginhit, "2015-08-12")

### RUN#2 - THERE WERE 130 PARTICIPANTS RUN THROUGHOUT 5 DAYS
df.complete$currentVersion.Run2.1 = str_detect(df.complete$beginhit, "2015-10-08")
df.complete$currentVersion.Run2.2 = str_detect(df.complete$beginhit, "2015-10-09")
df.complete$currentVersion.Run2.3 = str_detect(df.complete$beginhit, "2015-10-10")
df.complete$currentVersion.Run2.4 = str_detect(df.complete$beginhit, "2015-10-13")
df.complete$currentVersion.Run2.5 = str_detect(df.complete$beginhit, "2015-10-14")
df.complete$currentVersion.Run2.6 = str_detect(df.complete$beginhit, "2015-10-15")


#PILOT 1, 03/24/2015 - 03/25/2015
#df.complete = df.complete[df.complete$currentVersion.pilot1.1 == TRUE | df.complete$currentVersion.pilot1.2 == TRUE,]

#PILOT 2, 05/27/2015
#df.complete = df.complete[df.complete$currentVersion.pilot2 == TRUE,]

#PILOT 3, 06/21/2015
#df.complete = df.complete[df.complete$currentVersion.pilot3 == TRUE,]

#To get Trial Times 06/24/2015 CLICK -- ONLY ONE PARTICIPANT RUN FOR GETTING TRIAL TIME ESTIMATES (MELANIE)
#df.complete = df.complete[df.complete$currentVersion.pilot4 == TRUE,]

#PILOT 4, 07/27/2015 - TRIALS RUN WITH WORKING TIMER. ALSO, THE DRAG OPTION WAS REPLACED BY THE CLICK OPTION.
#df.complete = df.complete[df.complete$currentVersion.pilot5.1 == TRUE|df.complete$currentVersion.pilot5.2 == TRUE|df.complete$currentVersion.pilot5.3 == TRUE,]

#To get Trial Times 09/23/2015 CLICK -- ONLY ONE PARTICIPANT RUN FOR GETTING TRIAL TIME ESTIMATES (LAURA)
#df.complete = df.complete[df.complete$currentVersion.pilot6 == TRUE,]

#PILOT 7, 09/23/2015 - TRIALS RUN WITH WORKING TIMER. ALSO, THE DRAG OPTION WAS REPLACED BY THE CLICK OPTION.
#df.complete = df.complete[df.complete$currentVersion.pilot7.1 == TRUE|df.complete$currentVersion.pilot7.2 == TRUE,]

#PILOT 8, 10/07/2015 - PARTICIPANTS MUST TAKE THE QUIZ TWO TIMES
#df.complete = df.complete[df.complete$currentVersion.pilot8 == TRUE,]

#FIRST LARGE SAMPLE! WILL INCLUDE PARTICIPANTS RUN FROM 1.1 TO 1.5
#df.complete = df.complete[df.complete$currentVersion.Run1.1 == TRUE|df.complete$currentVersion.Run1.2 == TRUE|df.complete$currentVersion.Run1.3 == TRUE|df.complete$currentVersion.Run1.4 == TRUE|df.complete$currentVersion.Run1.5 == TRUE,]

#SECOND LARGE SAMPLE! WILL INCLUDE PARTICIPANTS RUN FROM 2.1 TO 2.6
#df.complete = df.complete[df.complete$currentVersion.Run2.1 == TRUE|df.complete$currentVersion.Run2.2 == TRUE|df.complete$currentVersion.Run2.3 == TRUE|df.complete$currentVersion.Run2.4 == TRUE|df.complete$currentVersion.Run2.5 == TRUE|df.complete$currentVersion.Run2.6 == TRUE,]

#BOTH LARGE SAMPLES, 1.1-2.6
df.complete = df.complete[df.complete$currentVersion.Run1.1 == TRUE|df.complete$currentVersion.Run1.2 == TRUE|df.complete$currentVersion.Run1.3 == TRUE|df.complete$currentVersion.Run1.4 == TRUE|df.complete$currentVersion.Run1.5 == TRUE|df.complete$currentVersion.Run2.1 == TRUE|df.complete$currentVersion.Run2.2 == TRUE|df.complete$currentVersion.Run2.3 == TRUE|df.complete$currentVersion.Run2.4 == TRUE|df.complete$currentVersion.Run2.5 == TRUE|df.complete$currentVersion.Run2.6 == TRUE,]


nrow(df.complete)

#FILTER OUT any remaining 'debug' PARTICIPANTS!
df.complete = filter(df.complete, !str_detect(df.complete$workerid,"debug"))
nrow(df.complete)

# STRUCTURE DATA ----------------------------------------------------------
#NOTE: COMPILE IN WIDE FORM: 1 ROW/PARTICIPANT; EACH TRIAL GETS A SERIES OF COLUMN NAMES, FORMATTED XYFIELD_#
#ALSO, NO EXTRA UNDERSCORES IN THE COLUMN NAMES, THIS BREAKS wideToLong
df.wide = data.frame(NULL)
df.wide = data.frame(matrix(nrow=nrow(df.complete),ncol=4))
colnames(df.wide) = c("participant","workerId","browser","beginhit") #DYNAMICALLY ADDS COLUMNS FROM THE DATASTRING BELOW


#ORGANIZE DATA -------------------------------------------------------------
#GLOBAL INDECES ARE NUMBERS CORRESPONDING TO THE PAGE NUMBER THAT THE PARTICIPANT SAW
global_indeces = c()
#free_sorts WILL INCLUDE TRIALS OF TYPE 'free-sort' ONLY - THERE SHOULD BE 22 PER PARTICIPANT
#18 OF THESE ARE ACTUAL TEST TRIALS
free_sorts = list()
#categorized WILL INCLUDE TRIALS OF TYPE 'categorized' WHICH ARE USED TO QUIZ THE PARTICIPANT'S MEMORIZATION OF THE GLYPHS
#THESE TRIALS ARE RUN IN GROUPS OF 16. PARTICIPANT CAN TAKE THE QUIZ AS MANY TIMES AS NEEDED
categorized = list()


for (i in 1:nrow(df.wide)){
  partic_free = list()
  partic_catg = list()
  if (!is.na(df.complete$datastring[i])){
    a = fromJSON(df.complete$datastring[i])
    mylength = length(a$data)
  } else{
    a = data.frame(NULL)
    mylength = 0
  }
  print(mylength)
  if (mylength>=51){
    df.wide$participant[i] = i
    df.wide$workerId[i] = a$workerId
    df.wide$browser[i] = df.complete$browser[i]
    df.wide$beginhit[i] = df.complete$beginhit[i]
    #cycle through all the trials, but only record where isTestTrial = 1
    for (j in 1:mylength){
      if(a$data[[j]]$trialdata$trial_type == "free-sort"){
        partic_free = c(partic_free, list(a$data[[j]]$trialdata))
      } else if (a$data[[j]]$trialdata$trial_type == "categorize") {
        partic_catg = c(partic_catg, list(a$data[[j]]$trialdata))
      }
    }
    free_sorts[a$workerId] = list(partic_free)
    categorized[a$workerId] = list(partic_catg)
  } else {
      df.wide[i,] = 'EXCLUDED'
      df.wide$workerId[i] = a$workerId
    }
  #And grab the info we need from the last 'trial' (feedback)
  if (is.null(a$data[[mylength-1]]$trialdata$responses)){df.wide$feedback[i] = "none"
  } else {
    df.wide$feedback[i] = a$data[[mylength-1]]$trialdata$responses
  }
}


#Check we didn't lose anyone there
nrow(df.wide)

#### CHECK TO SEE IF PARTICIPANT CHEATED (IE did they say they cheated when we asked) ####
df.wide$cheated = as.numeric(grepl('Q1\":\"y', df.wide$feedback, ignore.case=TRUE) | grepl('Q2\":\"y', df.wide$feedback, ignore.case=TRUE))


##INSERT QUIZ DATA TO WIDE DATA FRAME##
df.wide$NumQuizTries = 'NoInputYet'
df.wide$LastQuizScore = 'NoInputYet'

for (i in 1:length(names(categorized))) {
  myquizlength = length(categorized[[names(categorized)[i]]])
  df.wide$NumQuizTries[which(df.wide$workerId %in% names(categorized)[i])] = as.double(myquizlength/16)
  tot_score = numeric()
  for (j in (myquizlength-15):myquizlength) {
    tot_score = c(tot_score, categorized[[names(categorized)[i]]][[j]]$correct)
  }
  df.wide$LastQuizScore[which(df.wide$workerId %in% names(categorized)[i])] = as.double(mean(tot_score))
}


##LABEL PARTICIPANTS THAT HAD TOO MANY OR TOO FEW TRIALS, AND EXCLUDE##
##OTHERWISE GRAB EACH TRIALS DATA##
worker_ids = names(free_sorts)
for (i in 1:length(worker_ids)) {
  if (length(free_sorts[[worker_ids[i]]]) != 22) {
   grabbed_row = which(df.wide$workerId %in% worker_ids[i])
   df.wide[grabbed_row,] = 'EXCLUDED' 
   df.wide$participant[grabbed_row] = as.numeric(grabbed_row)
   df.wide$workerId[grabbed_row] = worker_ids[i]
   df.wide$beginhit[grabbed_row] = 'TOO MANY TRIALS'
  }
}

for (i in 1:nrow(df.wide)){
  counter = 1
  if (!str_detect(df.wide$browser[i], 'EXCLUDED')){
    a = free_sorts[[df.wide$workerId[i]]]
    mylength = length(free_sorts[[df.wide$workerId[i]]])
    
    
    for (j in mylength:1) {
      if(j == mylength) {max_g_i = free_sorts[[df.wide$workerId[i]]][[j]]$trial_index_global}
      if(!is.null(free_sorts[[df.wide$workerId[i]]][[j]]$glyph)){
        trial_num = (28 - (max_g_i - free_sorts[[df.wide$workerId[i]]][[j]]$trial_index_global))
        df.wide[[paste("glyph_",trial_num, sep="")]][i] = free_sorts[[df.wide$workerId[i]]][[j]]$glyph
        df.wide[[paste("Stimulus_",trial_num, sep="")]][i] = "PracticeImage"
      }
      if(!is.null(free_sorts[[df.wide$workerId[i]]][[j]]$moviefile)){
        trial_num = (26 - (max_g_i - free_sorts[[df.wide$workerId[i]]][[j]]$trial_index_global))
        df.wide[[paste("Stimulus_",trial_num, sep="")]][i] = free_sorts[[df.wide$workerId[i]]][[j]]$moviefile
      }
      if(!is.null(free_sorts[[df.wide$workerId[i]]][[j]]$isTestTrial)){
        if(free_sorts[[df.wide$workerId[i]]][[j]]$isTestTrial == 1) {
          df.wide[[paste("isTestTrial_",trial_num, sep="")]][i] = free_sorts[[df.wide$workerId[i]]][[j]]$isTestTrial
        }}
      if(!is.null(free_sorts[[df.wide$workerId[i]]][[j]]$moves)){
        df.wide[[paste("moves_",trial_num, sep="")]][i] = free_sorts[[df.wide$workerId[i]]][[j]]$moves
      }
      if(!is.null(free_sorts[[df.wide$workerId[i]]][[j]]$Sub)){
        df.wide[[paste("S_",trial_num, sep="")]][i] = free_sorts[[df.wide$workerId[i]]][[j]]$Sub
      }
      if(!is.null(free_sorts[[df.wide$workerId[i]]][[j]]$Vrb)){
        df.wide[[paste("V_",trial_num, sep="")]][i] = free_sorts[[df.wide$workerId[i]]][[j]]$Vrb
      }
      if(!is.null(free_sorts[[df.wide$workerId[i]]][[j]]$Obj)){
        df.wide[[paste("O_",trial_num, sep="")]][i] = free_sorts[[df.wide$workerId[i]]][[j]]$Obj
      }
      if(!is.null(free_sorts[[df.wide$workerId[i]]][[j]]$rt)){
        df.wide[[paste("rt_",trial_num, sep="")]][i] = free_sorts[[df.wide$workerId[i]]][[j]]$rt
      }
    }
    
  } else{
    a = data.frame(NULL)
    mylength = 0
    grabbed_row = which(df.wide$workerId %in% worker_ids[i])
    df.wide[grabbed_row,] = 'EXCLUDED' 
    df.wide$participant[grabbed_row] = as.numeric(grabbed_row)
    df.wide$workerId[grabbed_row] = worker_ids[i]
    df.wide$beginhit[grabbed_row] = 'TOO MANY TRIALS'
  }
  
} #End of this participant


##PREP FOR GETTING RAW WORD ORDER##
names = c('moves_', "S_", "O_", "V_", "glyph_")
col.names = names(df.wide)
col.nums = c()

where_moves = which(str_detect(col.names, 'moves'))
for (w in 1:length(where_moves)) {
  col.nums = unique(c(col.nums, unlist(strsplit(col.names[where_moves][w], split='moves_'))))
}

col.nums = as.numeric(col.nums[-which(col.nums %in% "")])
col.nums = sort(col.nums)

df.wide[paste('RawOrd_', col.nums, sep='')] = 'NoOrderYet'

for (i in 1:length(col.nums)){
  if (!exists(paste("isTestTrial_",col.nums[i], sep=""), where=df.wide)){
    df.wide[paste("isTestTrial_",col.nums[i], sep="")] = 0
  }
}



###GET RAW WORD ORDER###
for (j in 1:nrow(df.wide)){
  if (df.wide$browser[j] != "EXCLUDED"){  
      for (k in 1:length(col.nums)) {
        if (df.wide[paste('Stimulus_', col.nums[k], sep = "")][j,] == "PracticeImage") {
          check_m = df.wide[paste(names[1], col.nums[k], sep = "")][j,]
          check_m = unlist(strsplit(check_m, split='{\"src\":\"', fixed=TRUE))
          check_m = unlist(strsplit(check_m, split='.png\",', fixed=TRUE))
          check_m = check_m[which(str_detect(check_m, 'g'))]
          check_g = df.wide[paste(names[5], col.nums[k], sep = "")][j,]
          where_g = ''
          check_m[which(unlist(gregexpr(check_g, check_m)) == 1)] = 'G'
          choices = c('X', 'Y', 'Z', 'Q', 'H', 'W', 'R')
          choice_num = 0
          while (mean(str_detect(check_m, 'g')) > 0) {
            choice_num = choice_num + 1
            which_g = check_m[which(grepl('g', check_m))][1]
            check_m[which(str_detect(check_m, which_g))] = choices[choice_num]
          }
          if (length(check_m) > 0) {
            word_order = paste0(check_m, collapse='')
          } else {word_order = 'NONE'}
        } else if (df.wide[paste('Stimulus_', col.nums[k], sep = "")][j,] != "") {
          check_m = df.wide[paste(names[1], col.nums[k], sep = "")][j,]
          check_m = unlist(strsplit(check_m, split='{\"src\":\"', fixed=TRUE))
          check_m = unlist(strsplit(check_m, split='.png\",', fixed=TRUE))
          check_m = check_m[which(str_detect(check_m, 'g'))]
          check_s = df.wide[paste(names[2], col.nums[k], sep = "")][j,]
          check_o = df.wide[paste(names[3], col.nums[k], sep = "")][j,]
          check_v = df.wide[paste(names[4], col.nums[k], sep = "")][j,]
          where_s = where_o = where_v = ''
          check_m[which(unlist(gregexpr(check_s, check_m)) == 1)] = 'S'
          check_m[which(unlist(gregexpr(check_o, check_m)) == 1)] = 'O'
          check_m[which(unlist(gregexpr(check_v, check_m)) == 1)] = 'V'
          choices = c('X', 'Y', 'Z', 'Q', 'H', 'W', 'R')
          choice_num = 0
          if (length(check_m) != 0) {
          while (mean(str_detect(check_m, 'g')) > 0) {
            choice_num = choice_num + 1
            which_g = check_m[which(grepl('g', check_m))][1]
            check_m[which(str_detect(check_m, which_g))] = choices[choice_num]
          }}
          if (length(check_m) > 0) {
            word_order = paste0(check_m, collapse='')
          } else {word_order = 'NONE'}
      } else {word_order = 'NONE'}
    df.wide[paste('RawOrd_', col.nums[k], sep = "")][j,] = word_order  
    }
  }  
}


###CLEAN UP EXCLUSION ROWS
for (i in 1:nrow(df.wide)) {
  if (df.wide$browser[i] == 'EXCLUDED') {
    grabbed_row = i
    df.wide[grabbed_row,] = 'EXCLUDED' 
    df.wide$participant[grabbed_row] = as.numeric(grabbed_row)
    df.wide$workerId[grabbed_row] = worker_ids[i]
    df.wide$beginhit[grabbed_row] = 'TOO MANY TRIALS'
  }
}


##GROUP INDIVIDUAL GLYPH COLUMNS INTO ONE COLUMN##
nec.glyphs = function(S, O, V) {
  final_group = ''
  final_group = paste("S:", S, sep='')
  final_group = paste(final_group, ' O:', sep='')
  final_group = paste(final_group, O, sep='')
  final_group = paste(final_group, ' V:', sep='')
  final_group = paste(final_group, V, sep='')
}

glyphs.group = ''
for (i in 1:length(col.nums)) {
  if (df.wide[paste('Stimulus_', col.nums[i], sep = "")][1,] != "PracticeImage") {
    df.wide[paste("Glyphs_",col.nums[i], sep="")] = mapply(nec.glyphs, S=df.wide[[paste("S_",col.nums[i], sep="")]], O=df.wide[[paste("O_",col.nums[i], sep="")]], V=df.wide[[paste("V_",col.nums[i], sep="")]])
  } else {df.wide[paste("Glyphs_",col.nums[i], sep="")] = df.wide[paste("glyph_",col.nums[i], sep="")]}
}

#df.wide <- data.frame(matrix(unlist(df.wide), nrow=nrow(df.wide), byrow=T))

#Make sure nobody got lost! should be 292 still
nrow(df.wide)

###REFORMAT FROM WIDE TO LONG###
moves_list = c()
stimulus_list = c()
rt_list = c()
S_list = c()
O_list = c()
V_list = c()
order_list = c()
glyphs_list = c()
rem.glyph = c()
isTestTrial_list = c()

for (i in 1:length(col.nums)) {
  if (exists(paste('S_', col.nums[i], sep=''), df.wide)) {
      moves_list = c(moves_list, paste("moves_",col.nums[i], sep=""))
      stimulus_list = c(stimulus_list, paste("Stimulus_",col.nums[i], sep=""))
      rt_list = c(rt_list, paste("rt_",col.nums[i], sep=""))
      S_list = c(S_list, paste("S_",col.nums[i], sep=""))
      O_list = c(O_list, paste("O_",col.nums[i], sep=""))
      V_list = c(V_list, paste("V_",col.nums[i], sep=""))
      order_list = c(order_list, paste("RawOrd_",col.nums[i], sep=""))
      glyphs_list = c(glyphs_list, paste("Glyphs_",col.nums[i], sep=""))
      isTestTrial_list = c(isTestTrial_list, paste("isTestTrial_",col.nums[i], sep=""))
  } else {
      moves_list = c(moves_list, paste("moves_",col.nums[i], sep=""))
      stimulus_list = c(stimulus_list, paste("Stimulus_",col.nums[i], sep=""))
      rt_list = c(rt_list, paste("rt_",col.nums[i], sep=""))
      order_list = c(order_list, paste("RawOrd_",col.nums[i], sep=""))
      glyphs_list = c(glyphs_list, paste("Glyphs_",col.nums[i], sep=""))
      rem.glyph = c(rem.glyph, paste("glyph_",col.nums[i], sep=""))
      isTestTrial_list = c(isTestTrial_list, paste("isTestTrial_",col.nums[i], sep=""))
  }
}

list_of_lists = list(stimulus_list, rt_list, glyphs_list, moves_list, isTestTrial_list, order_list) #literal_list, keypress_list, match_list)

df.long <- reshape(df.wide, 
                   varying = list_of_lists, 
                   v.names = c('stimulus', 't.time', 'glyphs', 'moves', 'isTestTrial', 'RawOrder'),
                   timevar = "trial.number", 
                   times = 1:length(moves_list), 
                   drop = c(S_list, O_list, V_list, rem.glyph),
                   direction = "long")


#Check!
length(unique(df.long$participant))

write.csv(df.long, file="alldata.csv", row.names = FALSE)

###############Below analyses conducted by masm.  For final analysis, we are printing the
###############clean-formatted dataset out at this point and then moving the rest to GlyphGridAnalysis

#####################
##SORT AND CLEAN DF.LONG##
long.names = names(df.long)
long.names = long.names[-which(long.names %in% "id")]
df.long = df.long[long.names]
df.long = df.long[df.long$browser !="EXCLUDED",]
df.long <- df.long[order(df.long$participant),]



##GET RAW SHORT ORDER WITH MISTAKES##
long_split = strsplit(df.long$RawOrder, '')
short_order = list()
for (i in 1:length(long_split)){
  if(df.long$RawOrder[i]!="NONE"){
    short_order[[i]] = paste(unique(long_split[[i]]), collapse='')
  } else {short_order[[i]] = "NONE"}
}

df.long$ShortRawOrder = as.character(as.factor(unlist(short_order)))



##LABEL STIMULUS AS TRANSITIVE OR NOT##
df.long$IsTransitive = as.numeric(!str_detect(df.long$glyphs, 'none') & !str_detect(df.long$stimulus, 'PracticeImage'))


## LABEL WHETHER PARTICIPANT GAVE A COMPLETE RESPONSE FOR THAT STIMULUS RAW RAW RAW RAW ####
complete_answer = function(ShortOrder, IsTransitive) {
  complete = numeric()
  if (ShortOrder != 'NONE') {
  if (IsTransitive == 1 & nchar(ShortOrder) >= 3) {
    complete = 1
  } else if (IsTransitive == 0 & nchar(ShortOrder) == 2) {
    complete = 1
  } else if (ShortOrder == 'G') {
    complete = 1 
  } else {complete = 0}
  } else {complete = 0}
  return(complete)
}

df.long$RawComplete = mapply(complete_answer, ShortOrder = df.long$ShortRawOrder, IsTransitive = df.long$IsTransitive, USE.NAMES = FALSE)


### PRODUCE A CLEAN WORD ORDER WITH ONLY SOV's ###
raw_to_clean = function(raworder) {
  rawsplit = unlist(strsplit(raworder, split=''))
  cleanorder = character()
  if (mean(str_detect(rawsplit, 'N')) > 0) {
    cleanorder = "NONE"
  } else if (mean(str_detect(rawsplit, 'G')) > 0) {
    cleanorder = raworder
  } else {
    cleanorder = paste0(rawsplit[which(rawsplit %in% "S" | rawsplit %in% "O" | rawsplit %in% "V")], collapse='')
  }
  return(cleanorder)
}  

df.long$CleanOrder = mapply(raw_to_clean, df.long$ShortRawOrder)


### GET WHETHER A COMPLETE RESPONSE WAS PRODUCED WITH THIS NEW ORDER ###
df.long$CleanComplete = mapply(complete_answer, ShortOrder = df.long$CleanOrder, IsTransitive = df.long$IsTransitive, USE.NAMES = FALSE)

### PRODUCE A WORD ORDER INCLUDING RESPONSES WITH ONLY 1 MISTAKE###
with_one_mistake = function(raworder, cleanorder, ccomplete, transitive) {
  ##rawsplit = unlist(strsplit(raworder, split=''))
  to_return = ''
  if (ccomplete == 1) {
    to_return = cleanorder
  } else if (str_detect(cleanorder, 'G')) {
    to_return = cleanorder
  } else if (str_detect(raworder, 'Y')) {
    to_return = 'BadResponse'
  } else if (transitive == 1 && nchar(raworder) != 3) {
      to_return = 'BadResponse'
  } else if (transitive == 0 && nchar(raworder) != 2) {
      to_return = 'BadResponse'
  } else {
      to_return = raworder
  }
  
  return(to_return)
}  

df.long$WithAlmost = mapply(with_one_mistake, raworder = df.long$ShortRawOrder, cleanorder = df.long$CleanOrder, ccomplete = df.long$CleanComplete, transitive = df.long$IsTransitive)


### TRANSLATE WithAlmost INTO A LOGICAL COLUMN. 'BadResponse' IS 0, AND EVERYTING ELSE IS 1 ###
df.long$UsableOneMistk = as.numeric(!str_detect(df.long$WithAlmost, "BadResponse"))



### CHECK WHETHER PARTICIPANT WAS EXPOSED TO A WORD ORDER ###
### AND GOT BOTH PRACTICE IMAGE TRIALS CORRECT ###
df.long$PassesPractice = 0

for (i in 1:length(worker_ids)) {
  if (worker_ids[i] %in% df.long$workerId) {
    pract_temp = df.long[df.long$workerId == worker_ids[i] & df.long$isTestTrial == 0,]
    pract_score = numeric()
    for (j in 1:nrow(pract_temp)) {
      if (!str_detect(pract_temp$CleanOrder[j], "NONE")) {
        if (pract_temp$stimulus[j] == "PracticeImage") {
          pract_score = c(pract_score, str_detect(pract_temp$CleanOrder[j], "G"))
        } else {
            if (as.logical(pract_temp$CleanComplete[j])) {
              pract_score = c(pract_score, 1)
            } else if (as.logical((nchar(pract_temp$CleanOrder[j]) >= 1)) & !as.logical(pract_temp$IsTransitive[j])) {
              pract_score = c(pract_score, 1)
            } else if (as.logical((nchar(pract_temp$CleanOrder[j]) > 1))) {
              pract_score = c(pract_score, 1)
            } else {pract_score = c(pract_score, 0)}
          }
        } else {pract_score = c(pract_score, 0)}
    }
    if (mean(pract_score) == 1) {
      df.long$PassesPractice[which(df.long$workerId == worker_ids[i])] = 1
    }
  }
}




### LABEL ANIMACY OF STIMULI ###
#it_events = c("girl-tumbling-none", "boy-rolling-none", "car-rolling-none", "ball-rolling-none") 
animates = c("fireman-pushing-boy", "fireman-kicking-girl", "girl-elbowing-oldlady", "girl-kissing-boy", "girl-throwing-oldlady", "boy-lifting-girl",  "oldlady-rubbing-fireman")
#inanimates = c("fireman-lifting-car", "fireman-throwing-ball", "oldlady-kissing-ball", "oldlady-elbowing-heart", "girl-rubbing-heart", "boy-kicking-ball", "girl-pushing-car")
#all_events = c(it_events, animates, inanimates)

animacy = function (video, animate_events) {
  if (mean(str_detect(video, 'none')) > 0) {
    tr.anim = 'NoObject'
  } else if (mean(str_detect(video, 'PracticeImage')) > 0) {
      tr.anim = 'PracticeImage'
    } else if (mean(str_detect(video, animate_events)) > 0){
      tr.anim = 'animate'
  } else {tr.anim = 'inanimate'}
  return(tr.anim)
}

df.long$Animacy = sapply(df.long$stimulus, FUN=animacy, animate_events = animates, USE.NAMES = FALSE)


### CHECK IS RESPONSE IS V-lat or V-med ###
isVLat = function (shortclean, iscomplete, istrans) {
  if (as.logical(istrans) && as.logical(iscomplete)) { 
    if (str_detect(shortclean, 'V')) {
      if (unlist(strsplit(shortclean, ''))[2]=="V"){
        return(0)
      } else {return(1)}
    } else if (unlist(strsplit(shortclean, ''))[2]=="X") {
      return(0)
    } else {return(1)}
  } else {return(0)}
}

### CHECK FOR VERB MEDIAL, AND IF TRUE, RETURN 0 FOR THE SHORT CLEAN ORDER ###
df.long$CleanVLat = mapply(isVLat, shortclean = df.long$CleanOrder, iscomplete = df.long$CleanComplete, istrans = df.long$IsTransitive, USE.NAMES = FALSE)


### CHECK FOR VERB MEDIAL, AND IF TRUE, RETURN 0 FOR THE SHORT CLEAN ORDER ###
df.long$UsableVLat = mapply(isVLat, shortclean = df.long$WithAlmost, iscomplete = df.long$UsableOneMistk, istrans = df.long$IsTransitive, USE.NAMES = FALSE)





### GRAB ROWS FROM PARTICIPANTS THAT DID NOT CHEAT ###
df.long.no.cheat = df.long[df.long$cheated == 0,]

### GRAB ROWS FROM PARTICPANTS THAT WERE NOT EXPOSED TO A WORD ORDER###
df.long.pass.practice = df.long.no.cheat[df.long.no.cheat$PassesPractice==1,]

### MAKE TABLE OF PARTICIPANTS THAT DID CHEAT ###
df.long.cheaters = df.long[df.long$cheated == 1,]

### GRAB ROWS THAT ARE TEST TRIALS ###
df.long.testtrials = df.long.pass.practice[df.long.pass.practice$isTestTrial == 1,]

### OF THOSE, NOW GRAB TRANSITIVE STIMULI ###
df.long.transitives = df.long.testtrials[df.long.testtrials$IsTransitive == 1,]

### OF THOSE, NOW GRAB COMPLETE RESPONSES ###
df.long.usables = df.long.transitives[df.long.transitives$UsableOneMistk == 1,]
df.long.absolute = df.long.transitives[df.long.transitives$CleanComplete == 1,]





###GET SOME BASIC %'s HERE ABOUT RESPONSES###
raw.complete = mean(df.long$RawComplete)
clean.complete = mean(df.long$CleanComplete)
usable.complete = mean(df.long$UsableOneMistk)
trans.complete.clean = mean(df.long.transitives$CleanComplete)
trans.complete.raw = mean(df.long.transitives$RawComplete)
trans.complete.usable = mean(df.long.transitives$UsableOneMistk)
percent.Vlat.Clean = mean(df.long.usables$CleanVLat)
percent.Vlat.Usable = mean(df.long.usables$UsableVLat)
percent.passes.practice = mean(df.long$PassesPractice)
    df.long$cheated = as.numeric(df.long$cheated)
percent_cheaters = mean(df.long$cheated)


#GET OVERALL PERCENTAGES FROM DIFFERENT TABLES#
summary1 = df.long.usables %>% group_by(Animacy) %>% summarise(CleanVlat = mean(CleanVLat), UsableVlat = mean(UsableVLat), how_many=sum(IsTransitive))
summary2 = df.long.transitives %>% group_by(Animacy) %>% summarise(CleanVlat = mean(CleanVLat), UsableVlat = mean(UsableVLat), how_many=sum(IsTransitive))
summary3 = df.long %>% group_by(Animacy) %>% summarise(CleanVlat = mean(CleanVLat), UsableVlat = mean(UsableVLat), how_many=sum(IsTransitive))
summary4 = df.long.absolute %>% group_by(Animacy) %>% summarise(CleanVlat = mean(CleanVLat), UsableVlat = mean(UsableVLat), how_many=sum(IsTransitive))

#PROVIDES TIME AVERAGES FOR ALL TRIALS, AND ONLY-TRANSITIVE TRIALS#
time.summary1 = df.long %>% group_by(participant) %>% summarise(AvgTime = mean(as.numeric(t.time)), how_many=sum(IsTransitive))
time.summary2 = df.long.transitives %>% group_by(participant) %>% summarise(AvgTime = mean(as.numeric(t.time)), how_many=sum(IsTransitive))

#PROVIDES NUMBERS FOR EACH PARTICIPANT - USED BY 
indv.summary1 = df.long.transitives %>% group_by(participant, Animacy) %>% summarise(CleanVLat = mean(CleanVLat), UsableVLat = mean(UsableVLat), how_many=sum(IsTransitive))
indv.summary2 = df.long.transitives %>% group_by(participant, Animacy) %>% summarise(CleanVLat = sum(CleanVLat), UsableVLat = sum(UsableVLat), how_many=sum(IsTransitive))
indv.summary3 = df.long.usables %>% group_by(participant, Animacy) %>% summarise(CleanVLat = mean(CleanVLat), UsableVLat = mean(UsableVLat), how_many=sum(IsTransitive))
indv.summary4 = df.long.usables %>% group_by(participant, Animacy) %>% summarise(CleanVLat = sum(CleanVLat), UsableVLat = sum(UsableVLat), how_many=sum(IsTransitive))
indv.summary5 = df.long %>% group_by(participant, Animacy) %>% summarise(CleanVLat = mean(CleanVLat), UsableVLat = mean(UsableVLat), how_many=sum(IsTransitive))
indv.summary6 = df.long %>% group_by(participant, Animacy) %>% summarise(CleanVLat = sum(CleanVLat), UsableVLat = sum(UsableVLat), how_many=sum(IsTransitive))
indv.summary7 = df.long.cheaters %>% group_by(participant, Animacy) %>% summarise(CleanVLat = mean(CleanVLat), UsableVLat = mean(UsableVLat), how_many=sum(IsTransitive))
indv.summary8 = df.long.cheaters %>% group_by(participant, Animacy) %>% summarise(CleanVLat = sum(CleanVLat), UsableVLat = sum(UsableVLat), how_many=sum(IsTransitive))

## THIS PRODUCES 'EFFECT.TABLE' WHICH TAKES ONE OF THE BEFORE 'INDV.SUMMARY' TABLES AND
## DETERMINES WHICH PARTICIPANTS PRODUCED THE EFFECT ##
## EFFECT.SUMMARY PROVIDES SUMS OF EFFECT.TABLE
check.effect = function(check.table) {
    #check.table = indv.summary4
    participant_nums = sort(as.numeric(unique(check.table$participant)))
    Effect.Table = data.frame(matrix(nrow=length(participant_nums)))
    colnames(Effect.Table) = 'participant'
    Effect.Table$participant = participant_nums
    Effect.Table$U.SVO.Only = Effect.Table$U.Effect = Effect.Table$U.Anti.Effect = Effect.Table$U.No.Direction = Effect.Table$C.SVO.Only = Effect.Table$C.Effect = Effect.Table$C.Anti.Effect = Effect.Table$C.No.Direction = NA
    for (i in 1:length(participant_nums)) {
      temp_tab = check.table[check.table$participant==participant_nums[i],]
      where.inanimate = which(temp_tab$Animacy %in% 'inanimate')
      if (sum(temp_tab$CleanVLat)==0 & sum(temp_tab$CleanVLat)==0) {
        Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$U.No.Direction = 1
        Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$U.SVO.Only = 1
        Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$C.No.Direction = 1
        Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$C.SVO.Only = 1
      } else {
        if (sum(temp_tab$CleanVLat)!=0) {
          if (temp_tab$CleanVLat[where.inanimate] > sum(temp_tab$CleanVLat)-temp_tab$CleanVLat[where.inanimate]) {
            Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$C.Effect = 1
          } else if (temp_tab$CleanVLat[where.inanimate] < sum(temp_tab$CleanVLat)-temp_tab$CleanVLat[where.inanimate]) {
            Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$C.Anti.Effect = 1
          } else {Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$C.No.Direction = 1}
        } else {
          Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$C.No.Direction = 1
          Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$C.SVO.Only = 1
        }
        if (sum(temp_tab$UsableVLat)!=0) {
          if (temp_tab$UsableVLat[where.inanimate] > sum(temp_tab$UsableVLat)-temp_tab$UsableVLat[where.inanimate]) {
            Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$U.Effect = 1
          } else if (temp_tab$UsableVLat[where.inanimate] < sum(temp_tab$UsableVLat)-temp_tab$UsableVLat[where.inanimate]) {
            Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$U.Anti.Effect = 1
          } else {Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$U.No.Direction = 1}
        } else {
          Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$U.No.Direction = 1
          Effect.Table[which(Effect.Table$participant==participant_nums[i]),]$U.SVO.Only = 1
        }
      }
    }
    Effect.Table[is.na(Effect.Table)] <- 0
    return(Effect.Table)
}


create.summary = function(input.table){
    Effect.Summary = data.frame(NULL)
    Effect.Summary = data.frame(matrix(nrow=1,ncol=9))
    colnames(Effect.Summary) = c("participants","C.NoDirection","C.AntiEffect","C.Effect","C.AllSVO","U.NoDirection","U.AntiEffect","U.Effect","U.AllSVO")
    Effect.Summary$participants[1] = nrow(input.table)
    Effect.Summary$C.NoDirection[1] = sum(input.table$C.No.Direction)
    Effect.Summary$C.AntiEffect[1] = sum(input.table$C.Anti.Effect)
    Effect.Summary$C.Effect[1] = sum(input.table$C.Effect)
    Effect.Summary$C.AllSVO[1] = sum(input.table$C.SVO.Only)
    Effect.Summary$U.NoDirection[1] = sum(input.table$U.No.Direction)
    Effect.Summary$U.AntiEffect[1] = sum(input.table$U.Anti.Effect)
    Effect.Summary$U.Effect[1] = sum(input.table$U.Effect)
    Effect.Summary$U.AllSVO[1] = sum(input.table$U.SVO.Only)
    return(Effect.Summary)
}

Effect.Table1 = check.effect(indv.summary4)
Summary.Table1 = create.summary(Effect.Table1)



## PRODUCES CSV FILES WITHIN THE DIRECTORY FOR VIEWING ##
directory = getwd()
write.csv(df.long, file = paste0(directory, "/dflong_full.csv"))
write.csv(df.long.transitives, file = paste0(directory, "/dflong_transitives.csv"))
write.csv(df.long.usables, file = paste0(directory, "/dflong_usables.csv"))
write.csv(Summary.Table1, file = paste0(directory, "/ParticipantsSummary.csv"))


#DF.WIDE HAS RAW DATA AND SHOWS WHICH PARTICIPANTS WERE EXCLUDED AS WELL
write.csv(df.wide, file = paste0(directory, "/dfwide_full.csv"))








##### NEW DATA WORKS WITH CODE UP TO HERE SO FAR #####
##### NEW DATA WORKS WITH CODE UP TO HERE SO FAR #####
##### NEW DATA WORKS WITH CODE UP TO HERE SO FAR #####
##### NEW DATA WORKS WITH CODE UP TO HERE SO FAR #####






























# ####
# ####
# #### HERE IS OLD CODE FROM MELISSA'S ANALYSIS FOR MANY-DAX
# ####
# ####
# 
# 
# 
# #Weird behavior! I got those wrong-lenght participants to be assigned a participant no of NA, which is something, anyway.
# #Lost 6 people to this.
# nrow(df.wide)
# df.wide = df.wide[!is.na(df.wide$participant),]
# nrow(df.wide)
# 
# #OOPS from 1/15/15: I didn't have the first trial (set to show the prototype movie) save the right variables, so record them
# #here
# 
# df.wide$exposurePath_5 =df.wide$exposurePath_6
# df.wide$exposureManner_5 =df.wide$exposureManner_6
# df.wide$condition_5 =df.wide$condition_6
#   
# 
# #Reformat into long form!
# df.long = wideToLong(subset(df.wide,select=-feedback),within="trial")
# 
# #create factors
# df.long = mutate(df.long, participant = as.numeric(participant),
#           trial = as.numeric(as.character(trial)),
#           rt = as.numeric(as.character(rt)),
#           keypress = as.numeric(as.character(keypress))-48, #transform keycodes to numerals!
#           stimCondition = factor(stimCondition,levels=c("NoChange","BothChange", "PathChange","MannerChange")),
#           condition = factor(condition, levels=c("Noun","Verb")))
# 
# df.long = df.long[order(df.long$participant,df.long$trial),]
# 
# #Analyze data!--------------------------------------------------
# 
# #For each participant, make a score, which is abs(mean(mannerchange)-mean(pathchange))
# #(And add some extra descriptive stats for the paper)
# 
# Scores = ""
# 
# mannerScores = aggregate(df.long[df.long$stimCondition=="MannerChange",]$keypress, by=list(df.long[df.long$stimCondition=="MannerChange",]$participant, df.long[df.long$stimCondition=="MannerChange",]$condition), mean.na.rm)
# names(mannerScores) = c("participant", "condition", "mannerscore")
# pathScores = aggregate(df.long[df.long$stimCondition=="PathChange",]$keypress, by=list(df.long[df.long$stimCondition=="PathChange",]$participant, df.long[df.long$stimCondition=="PathChange",]$condition), mean.na.rm)
# names(pathScores) = c("participant", "condition", "pathscore")
# sameScores = aggregate(df.long[df.long$stimCondition=="NoChange",]$keypress, by=list(df.long[df.long$stimCondition=="NoChange",]$participant, df.long[df.long$stimCondition=="NoChange",]$condition), mean.na.rm)
# names(sameScores) = c("participant","condition","samescore")
# bothScores = aggregate(df.long[df.long$stimCondition=="BothChange",]$keypress, by=list(df.long[df.long$stimCondition=="BothChange",]$participant, df.long[df.long$stimCondition=="BothChange",]$condition), mean.na.rm)
# names(bothScores) = c("participant","condition","bothscore")
#       
# Scores = merge(mannerScores, pathScores, by=c("participant", "condition"))
# Scores = merge(Scores, sameScores, by=c("participant", "condition"))
# Scores = merge(Scores, bothScores, by=c("participant", "condition"))
# 
# 
# 
# 
# 
# 
# 
# 
# #Basic descriptives
# 
# mean(Scores$samescore)
# mean(Scores$mannerscore)
# mean(Scores$pathscore)
# mean(Scores$bothscore)
# 
# with(Scores, tapply(samescore, list(condition), mean, na.rm=TRUE), drop=TRUE)
# t.test(Scores[Scores$condition == "Noun",]$samescore, Scores[Scores$condition == "Verb",]$samescore)
# 
# with(Scores, tapply(bothscore, list(condition), mean, na.rm=TRUE), drop=TRUE)
# t.test(Scores[Scores$condition == "Noun",]$bothscore, Scores[Scores$condition == "Verb",]$bothscore)
# 
# with(Scores, tapply(mannerscore, list(condition), mean, na.rm=TRUE), drop=TRUE)
# t.test(Scores[Scores$condition == "Noun",]$mannerscore, Scores[Scores$condition == "Verb",]$mannerscore)
# 
# with(Scores, tapply(pathscore, list(condition), mean, na.rm=TRUE), drop=TRUE)
# t.test(Scores[Scores$condition == "Noun",]$pathscore, Scores[Scores$condition == "Verb",]$pathscore)
# 
# #More interesting measures
# Scores$diffscore = abs(Scores$mannerscore - Scores$pathscore)
# Scores$ILikeMannerscore = Scores$pathscore - Scores$mannerscore
# 
# with(Scores, tapply(diffscore, list(condition), mean, na.rm=TRUE), drop=TRUE)
# with(Scores, tapply(diffscore, list(condition), stderr), drop=TRUE)
# 
# with(Scores, tapply(ILikeMannerscore, list(condition), mean, na.rm=TRUE), drop=TRUE)
# with(Scores, tapply(ILikeMannerscore, list(condition),stderr), drop=TRUE)
# 
# #And let's do a dead simple t test on that
# 
# t.test(Scores[Scores$condition == "Noun",]$diffscore, Scores[Scores$condition == "Verb",]$diffscore)
# cohensD(Scores[Scores$condition == "Noun",]$diffscore, Scores[Scores$condition == "Verb",]$diffscore)
# 
# 
# #Time for some regressions
# nounScores <- Scores[Scores$condition == 'Noun',]
# verbScores <- Scores[Scores$condition == 'Verb',]
# 
# noun.lm <- lm(mannerscore ~ pathscore, data=nounScores)
# summary(noun.lm)
# summary(noun.lm)$r.squared
# 
# verb.lm <- lm(mannerscore ~ pathscore, data=verbScores)
# summary(verb.lm)
# summary(verb.lm)$r.squared
# 
# #A post-hoc analysis: do verb or noun people say yes more often?
# allScores = aggregate(df.long$keypress, by=list(df.long$participant, df.long$condition), mean.na.rm)
# names(allScores) = c("participant", "condition", "allscore")
# 
# with(allScores, tapply(allscore, list(condition), mean, na.rm=TRUE), drop=TRUE)
# with(allScores, tapply(allscore, list(condition), stderr), drop=TRUE)
# 
# t.test(allScores[allScores$condition == "Noun",]$allscore, allScores[allScores$condition == "Verb",]$allscore)
# cohensD(allScores[allScores$condition == "Noun",]$allscore, allScores[allScores$condition == "Verb",]$allscore)
# 
# #Against the prediction I might have made, Noun categories are slightly SMALLER!  
# #So this isn't just verb people making smaller categories - they say yes to about the same # of things, but distribute differently
# 
# #Graph data------------------------------------------------
# 
# #Bar graph of means----------------
# 
# ##Summarize the data for graphing
# data.summary.diffscores <- data.frame(
#   condition=levels(Scores$condition),
#   mean=with(Scores, tapply(diffscore, list(condition), mean, na.rm=TRUE), drop=TRUE),
#   n=with(Scores, tapply(diffscore, list(condition), length)),
#   se=with(Scores, tapply(ILikeMannerscore, list(condition), stderr), drop=TRUE)
# )
# 
# # Precalculate margin of error for confidence interval
# data.summary.diffscores$me <- qt(1-0.05/2, df=data.summary.diffscores$n)*data.summary.diffscores$se
# 
# # Use ggplot to draw the bar plot!
# png('manydax-barplot-se.png') # Write to PNG
# ggplot(data.summary.diffscores, aes(x = condition, y = mean)) +  
#   geom_bar(position = position_dodge(), stat="identity", fill="brown") + 
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.25) +
#   ylim(0,6) +
#   ylab("Abs(mean(manner) - mean(path))")+
#   xlab("")+
#   ggtitle("Rating of manner vs. path changes") + # plot title
#   theme_bw() + # remove grey background (because Tufte said so)
#   theme(panel.grid.major = element_blank()) # remove x and y major grid lines (because Tufte said so)
# dev.off() # Close PNG
# 
# #Scatterplot of manner vs. path scores of each partic ---------
# 
# 
# png('manydax-noun-scatterplot-95ci.png')
# ggplot(nounScores, aes(x=mannerscore, y=pathscore)) +
#   geom_point(shape=1) +    # Use hollow circles
#   geom_smooth(method=lm)   # Add linear regression line 
# #  (by default includes 95% confidence region)
# dev.off()
# 
# png('manydax-verb-scatterplot-95ci.png')
# ggplot(verbScores, aes(x=mannerscore, y=pathscore)) +
#   geom_point(shape=1) +    # Use hollow circles
#   geom_smooth(method=lm)   # Add linear regression line 
# #  (by default includes 95% confidence region)
# dev.off()
# 
