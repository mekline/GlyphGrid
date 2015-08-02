# Packages ----------------------------------------------------------------

rm(list=ls())
library(lsr)
library(dplyr)
library(rjson)
library(RSQLite)
library(stringr)
library(ggplot2)
library(Hmisc)

mean.na.rm <- function(x) { mean(x,na.rm=T) }
sum.na.rm <- function(x) { sum(x,na.rm=T) }
stderr <- function(x) sqrt(var(x)/length(x))

# Read data ---------------------------------------------------------------

con = dbConnect(SQLite(),dbname = "/Users/masm/Desktop/GlyphGrid/participants.db");
df.complete = dbReadTable(con,"glyphs") #change the name of the database here (mine was called "almost")
dbDisconnect(con)

#filter out incompletes (using dplyr methods)
df.complete = subset(df.complete, status %in% c(3,4)) 

#nrow(df.complete) includes alll subjects ever plus all debug attempts!
#filter to a particular day (if I haven't set codeversions). OR together multiple days if needed
df.complete$currentVersion.pilot1 = str_detect(df.complete$beginhit, "2015-03-24")
df.complete$currentVersion.pilot2 = str_detect(df.complete$beginhit, "2015-03-25")
df.complete$currentVersion.pilot3 = str_detect(df.complete$beginhit, "2015-05-27")
df.complete$currentVersion.pilot4 = str_detect(df.complete$beginhit, "2015-06-21")
df.complete$currentVersion.pilot5 = str_detect(df.complete$beginhit, "2015-06-24 15:32:10.316064")
df.complete$currentVersion.pilot6 = str_detect(df.complete$beginhit, "2015-07-27")|str_detect(df.complete$beginhit, "2015-07-29")|str_detect(df.complete$beginhit, "2015-07-30")
df.complete$currentVersion.Run1.1 = str_detect(df.complete$beginhit, "2015-07-31")
df.complete$currentVersion.Run1.2 = str_detect(df.complete$beginhit, "2015-08-01")

#Run 1, 03/24/2015 - 03/25/2015
#df.complete = df.complete[df.complete$currentVersion.pilot1 == TRUE | df.complete$currentVersion.pilot2 == TRUE,]

#Run 2, 05/27/15
#df.complete = df.complete[df.complete$currentVersion.pilot3 == TRUE,]

#To get Trial Times 06/21/2015
#df.complete = df.complete[df.complete$currentVersion.pilot4 == TRUE,]

#To get Trial Times 06/24/2015 CLICK -- Melanie
#df.complete = df.complete[df.complete$currentVersion.pilot5 == TRUE,]

#Data with timer and CLICK option. Not DRAG option.
#df.complete = df.complete[df.complete$currentVersion.pilot6 == TRUE,]

#FIRST RUN! (yay) Will include particpants ran on different days
df.complete = df.complete[df.complete$currentVersion.Run1.1 == TRUE|df.complete$currentVersion.Run1.2 == TRUE,]

nrow(df.complete)

#filter out 'debug' participants!
df.complete = filter(df.complete, !str_detect(df.complete$workerid,"debug"))
nrow(df.complete)

# Structure data ----------------------------------------------------------
#Note: Compile in wide form: 1 row/participant; each trial gets a series of column names, formatted XYFIELD_#
#Also, no extra underscores in the column names, this breaks wideToLong
df.wide = data.frame(NULL)
df.wide = data.frame(matrix(nrow=nrow(df.complete),ncol=4))
colnames(df.wide) = c("participant","workerId","browser","beginhit") #will dynamically add columns from datastring below


#PLACA DATA OF SPECIFIC TRIAL TYPES INTO LISTS#
global_indeces = c()
free_sorts = list()
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
  if (mylength==51){
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
  } else {df.wide[i,] = 'EXCLUDED' }
  #And grab the info we need from the last 'trial' (feedback)
  if (is.null(a$data[[mylength-1]]$trialdata$responses)){df.wide$feedback[i] = "none"
  } else {
    df.wide$feedback[i] = a$data[[mylength-1]]$trialdata$responses
  }
}

#### CHECK TO SEE IF PARTICIPANT CHEATED ####
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


##LABEL PARTICIPANTS THAT HAD TOO MANY TRIALS, AND EXCLUDE##
##OTHERWISE GRAB EACH TRIALS DATA##
worker_ids = names(free_sorts)
for (i in 1:length(free_sorts)) {
  if (length(free_sorts[[worker_ids[i]]]) != 22) {
   df.wide[i,] = 'EXCLUDED' 
   df.wide$workerId[i] = 'TOO MANY TRIALS'
  }
}

for (i in 1:nrow(df.wide)){
  counter = 1
  if (!str_detect(df.wide$participant[i], 'EXCLUDED')){
    a = free_sorts[[df.wide$workerId[i]]]
    mylength = length(free_sorts[[df.wide$workerId[i]]])
  } else{
    a = data.frame(NULL)
    mylength = 0
    df.wide[i,] = 'EXCLUDED' 
    df.wide$workerId[i] = 'TOO MANY TRIALS'
  }
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
  if (df.wide$participant[j] != "EXCLUDED"){  
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





##SORT AND CLEAN DF.LONG##
long.names = names(df.long)
long.names = long.names[-which(long.names %in% "id")]
df.long = df.long[long.names]
df.long = df.long[df.long$participant !="EXCLUDED",]
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


### GET WHETHER A COMPLETE RESPONSES WAS PRODUCED WITH THIS NEW ORDER ###
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

### GRAB ROWS THAT ARE TEST TRIALS ###
df.long.testtrials = df.long.no.cheat[df.long.no.cheat$isTestTrial == 1,]

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


summary1 = df.long.usables %>% group_by(Animacy) %>% summarise(CleanVlat = mean(CleanVLat), UsableVlat = mean(UsableVLat), how_many=sum(IsTransitive))
summary2 = df.long.transitives %>% group_by(Animacy) %>% summarise(CleanVlat = mean(CleanVLat), UsableVlat = mean(UsableVLat), how_many=sum(IsTransitive))
summary3 = df.long %>% group_by(Animacy) %>% summarise(CleanVlat = mean(CleanVLat), UsableVlat = mean(UsableVLat), how_many=sum(IsTransitive))
summary4 = df.long.absolute %>% group_by(Animacy) %>% summarise(CleanVlat = mean(CleanVLat), UsableVlat = mean(UsableVLat), how_many=sum(IsTransitive))

time.summary1 = df.long %>% group_by(participant) %>% summarise(AvgTime = mean(as.numeric(t.time)), how_many=sum(IsTransitive))
time.summary2 = df.long.transitives %>% group_by(participant) %>% summarise(AvgTime = mean(as.numeric(t.time)), how_many=sum(IsTransitive))

some.summary1 = df.long.transitives %>% group_by(participant, Animacy) %>% summarise(CleanVLat = mean(CleanVLat), UsableVLat = mean(UsableVLat), how_many=sum(IsTransitive))
some.summary2 = df.long.usables %>% group_by(participant, Animacy) %>% summarise(CleanVLat = mean(CleanVLat), UsableVLat = mean(UsableVLat), how_many=sum(IsTransitive))

directory = getwd()
write.csv(df.long, file = paste0(directory, "/dflong_full.csv"))
write.csv(df.long.transitives, file = paste0(directory, "/dflong_transitives.csv"))
write.csv(df.long.usables, file = paste0(directory, "/dflong_usables.csv"))


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
