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

#Run 1, 03/24/2015 - 03/25/2015
#df.complete = df.complete[df.complete$currentVersion.pilot1 == TRUE | df.complete$currentVersion.pilot2 == TRUE,]

#Run 2, 1/16/15
df.complete = df.complete[df.complete$currentVersion.pilot3 == TRUE,]

nrow(df.complete)

#filter out 'debug' participants!
df.complete = filter(df.complete, !str_detect(df.complete$workerid,"debug"))
nrow(df.complete)

# Structure data ----------------------------------------------------------
#Note: Compile in wide form: 1 row/participant; each trial gets a series of column names, formatted XYFIELD_#
#Also, no extra underscores in the column names, this breaks wideToLong
#df.wide = data.frame(NULL)
df.wide = data.frame(matrix(nrow=nrow(df.complete),ncol=4))
colnames(df.wide) = c("participant","workerId","browser","beginhit") #will dynamically add columns from datastring below

global_indeces = c()
free_sorts = list()


for (i in 1:nrow(df.wide)){
  partic_free = list()
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
      } #Else just don't make any columns right now!!!
    }
    free_sorts[a$workerId] = list(partic_free)
  }
  #And grab the info we need from the last 'trial' (feedback)
  if (is.null(a$data[[mylength-1]]$trialdata$responses)){df.wide$feedback[i] = "none"
  }else{
    df.wide$feedback[i] = a$data[[mylength-1]]$trialdata$responses
  }
}

worker_ids = names(free_sorts)
for (i in 1:length(free_sorts)) {
  if (length(free_sorts[[worker_ids[i]]]) != 22) {
   df.wide[i,] = 'EXCLUDED' 
   df.wide$workerId[i] = 'TOO MANY TRIALS'
  }
}

for (i in 1:nrow(df.wide)){
  counter = 1
  if (df.wide$participant[i] != 'EXCLUDED'){
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
  }
} #End of this participant



names = c('moves_', "S_", "O_", "V_", "glyph_")
col.names = names(df.wide)
col.nums = c()

where_moves = which(str_detect(col.names, 'moves'))
for (w in 1:length(where_moves)) {
  col.nums = unique(c(col.nums, unlist(strsplit(col.names[where_moves][w], split='moves_'))))
}

col.nums = as.numeric(col.nums[-which(col.nums %in% "")])
col.nums = sort(col.nums)

df.wide[paste('WordOrd_', col.nums, sep='')] = 'NoOrderYet'

for (i in 1:length(col.nums)){
  if (!exists(paste("isTestTrial_",col.nums[i], sep=""), where=df.wide)){
    df.wide[paste("isTestTrial_",col.nums[i], sep="")] = 0
  }
}




for (j in 1:nrow(df.wide)){
  if (df.wide$participant[j] != "EXCLUDED"){  
      for (k in 1:length(col.nums)) {
        if (paste(df.wide[paste('Stimulus_', col.nums[k], sep = "")][j,] != "PracticeImage")) {
          check_m = df.wide[paste(names[1], col.nums[k], sep = "")][j,]
          check_s = df.wide[paste(names[2], col.nums[k], sep = "")][j,]
          check_o = df.wide[paste(names[3], col.nums[k], sep = "")][j,]
          check_v = df.wide[paste(names[4], col.nums[k], sep = "")][j,]
          where_s = where_o = where_v = ''
          if (unlist(gregexpr(check_s, check_m))[1] != -1) {
            where_s = unlist(gregexpr(check_s, check_m))}
          if (unlist(gregexpr(check_o, check_m))[1] != -1) {
            where_o = unlist(gregexpr(check_o, check_m))}
          if (unlist(gregexpr(check_v, check_m))[1] != -1) {
            where_v = unlist(gregexpr(check_v, check_m))}
          where_svo = as.numeric(c(where_s, where_v, where_o))
          where_svo = sort(where_svo)
          word_order = ''
          if (length(where_svo) > 0) {
          for (l in 1:length(where_svo)) {
            if (where_svo[l] %in% where_s) {
              word_order = paste(word_order, 'S', sep='')
            } else if (where_svo[l] %in% where_o) {
              word_order = paste(word_order, 'O', sep='')
            } else if (where_svo[l] %in% where_v) {
              word_order = paste(word_order, 'V', sep='')
            } 
          }} else {word_order = 'NONE'}
          df.wide[paste('WordOrd_', col.nums[k], sep = "")][j,] = word_order
      } else {
        check_m = df.wide[paste(names[1], col.nums[k], sep = "")][j,]
        check_g = df.wide[paste(names[5], col.nums[k], sep = "")][j,]
        where_g = ''
        if (unlist(gregexpr(check_g, check_m))[1] != -1) {
          where_g = unlist(gregexpr(check_g, check_m))
        }
        where_moved = as.numeric(where_g)
        where_moved = sort(where_moved)
        word_order = ''
        if (length(where_moved) > 0) {
          for (l in 1:length(where_moved)) {
            if (where_moved[l] %in% where_g) {
              word_order = paste(word_order, 'G', sep='')
            } 
          }} else {word_order = 'NONE'}
        df.wide[paste('WordOrd_', col.nums[k], sep = "")][j,] = word_order
      }
    }
  }
}

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



#REFORMAT FROM WIDE TO LONG
moves_list = c()
stimulus_list = c()
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
      S_list = c(S_list, paste("S_",col.nums[i], sep=""))
      O_list = c(O_list, paste("O_",col.nums[i], sep=""))
      V_list = c(V_list, paste("V_",col.nums[i], sep=""))
      order_list = c(order_list, paste("WordOrd_",col.nums[i], sep=""))
      glyphs_list = c(glyphs_list, paste("Glyphs_",col.nums[i], sep=""))
      isTestTrial_list = c(isTestTrial_list, paste("isTestTrial_",col.nums[i], sep=""))
  } else {
      moves_list = c(moves_list, paste("moves_",col.nums[i], sep=""))
      stimulus_list = c(stimulus_list, paste("Stimulus_",col.nums[i], sep=""))
      order_list = c(order_list, paste("WordOrd_",col.nums[i], sep=""))
      glyphs_list = c(glyphs_list, paste("Glyphs_",col.nums[i], sep=""))
      rem.glyph = c(rem.glyph, paste("glyph_",col.nums[i], sep=""))
      isTestTrial_list = c(isTestTrial_list, paste("isTestTrial_",col.nums[i], sep=""))
  }
}

list_of_lists = list(stimulus_list, glyphs_list, moves_list, isTestTrial_list, order_list) #literal_list, keypress_list, match_list)

df.long <- reshape(df.wide, 
                   varying = list_of_lists, 
                   v.names = c('stimulus', 'glyphs', 'moves', 'isTestTrial', 'LongOrder'),
                   timevar = "trial.number", 
                   times = 1:length(moves_list), 
                   drop = c(S_list, O_list, V_list, rem.glyph),
                   direction = "long")

## Sort and clean df.long
df.long <- df.long[order(df.long$workerId),]
long.names = names(df.long)
long.names = long.names[-which(long.names %in% "id")]
df.long = df.long[long.names]


long_split = strsplit(df.long$LongOrder, '')
short_order = list()
for (i in 1:length(long_split)){
  if(df.long$LongOrder[i]!="NONE"){
    short_order[[i]] = paste(unique(long_split[[i]]), collapse='')
  } else {short_order[[i]] = "NONE"}
}

df.long$ShortOrder = as.character(as.factor(unlist(short_order)))

df.long$IsTransitive = as.numeric(!str_detect(df.long$glyphs, 'none') & !str_detect(df.long$stimulus, 'PracticeImage'))


complete_answer = function(ShortOrder, IsTransitive) {
  if (IsTransitive == 1 & nchar(ShortOrder) == 3) {
    complete = 1
  } else if (IsTransitive == 0 & nchar(ShortOrder) == 2) {
    complete = 1
  } else if (ShortOrder == 'G') {
    complete = 1 
  } else {complete = 0}
  return(complete)
}

df.long$IsComplete = mapply(complete_answer, ShortOrder = df.long$ShortOrder, IsTransitive = df.long$IsTransitive, USE.NAMES = FALSE)

# df.long$PassesPractice = 0
# 
# for (i in length(worker_ids)) {
#   pract_temp = df.long[df.long$workerId == worker_ids[i] & df.long$isTestTrial == 0,]
#   pract_score = numeric()
#   for (j in 1:nrow(pract_temp)) {
#     if (!str_detect(pract_temp$LongOrder[j], "NONE")) {
#       if (pract_temp$stimulus[j] == "PracticeImage") {
#         pract_score = c(pract_score, str_detect(pract_temp$LongOrder[j], "G"))
#       } else {
#           if (pract_temp$IsComplete[j]) {
#             pract_score = c(pract_score, 1)
#           } else if (nchar(df.long$ShortOrder[j]) > 1) {
#             pract_score = c(pract_score, 1)
#           } else {pract_score = c(pract_score, 0)}
#         }
#       } else {pract_score = c(pract_score, 0)}
#   }
#   if (mean(pract_score) == 1) {
#     df.long$PassesPractice[which(df.long$workerId == worker_ids[i])] = 1
#   }
# }



it_events = c("girl-tumbling-none", "boy-rolling-none", "car-rolling-none", "ball-rolling-none") 
animates = c("fireman-pushing-boy", "fireman-kicking-girl", "girl-elbowing-oldlady", "girl-kissing-boy", "girl-throwing-oldlady", "boy-lifting-girl",  "oldlady-rubbing-fireman")
inanimates = c("fireman-lifting-car", "fireman-throwing-ball", "oldlady-kissing-ball", "oldlady-elbowing-heart", "girl-rubbing-heart", "boy-kicking-ball", "girl-pushing-car")
all_events = c(it_events, animates, inanimates)

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



directory = getwd()
write.csv(df.long, file = paste0(directory, "/dflong_full.csv"))

df.long.testtrials = df.long[df.long$isTestTrial == 1,]
df.long.completes = df.long[df.long$IsComplete == 1,]
df.long.transitives = df.long[df.long$IsTransitive == 1,]
df.long.usables = df.long.transitives[df.long$IsComplete == 1,]



isVLat = function (shortorder, iscomplete, istrans) {
  if(istrans) {
    if (unlist(strsplit(shortorder, ''))[2]=="V"){
      return(0)
    } else {return(1)}
  }
}

df.long.usables$isVLat = mapply(isVLat, shortorder = df.long.usables$ShortOrder, USE.NAMES = FALSE)



###GET SOME BASIC STATS HERE ABOUT RESPONSES###
percent.complete = nrow(df.long.completes)/nrow(df.long)
trans.complete = mean(df.long.transitives$IsComplete)
percent.Vlat = mean(df.long.usables$isVLat)



directory = getwd()
write.csv(df.long, file = paste0(directory, "/dflong_full.csv"))
write.csv(df.long.completes, file = paste0(directory, "/dflong_completes.csv"))
write.csv(df.long.usables, file = paste0(directory, "/dflong_usables.csv"))











##### NEW DATA WORKS WITH CODE UP TO HERE SO FAR #####















for (k in 1:nrow(df.wide)) {
  if (df.wide$participant[k] != "EXCLUDED") {
    for (i in (1:length(all_events)+2)) {
      for (j in 1:length(all_events)) {
        if (isTRUE(unlist(gregexpr(all_events[j], df.wide[paste("MovieFile_", i, sep = "")][k,])) == 1)) {
        if (df.wide[paste("WordOrd_", i, sep = "")][k,] != 'NONE') {
          long_order = df.wide[paste("WordOrd_", i, sep = "")][k,]
          long_order = unlist(strsplit(long_order, ''))
          how_long = length(long_order)
          short_order = ''
          for (l in 1:how_long) {
            short_order = unique(c(short_order, long_order[l]))
          }
          short_order = paste(short_order, collapse='')
          df.wide[order_col_names[j]][k,] = short_order
        } else {df.wide[order_col_names[j]][k,] = 'NONE'}
        }
      }
    }
  }
}

cln.table = df.wide[1:4]
cln.table[order_col_names[5:length(order_col_names)]] = df.wide[order_col_names[5:length(order_col_names)]]
#cln.table <- data.frame(matrix(unlist(cln.table), nrow=nrow(cln.table)))

cln.table$Per.Vlat = cln.table$Per.Vmed = cln.table$Per.Othe = 0

for (i in 1:nrow(cln.table)) {
  if (cln.table$participant[i] != 'EXCLUDED') {
    p_lat = p_med = p_none = 0
    for (j in 5:length(order_col_names)) {
      if ((cln.table[order_col_names[j]])[i,] != 'NONE') {
        if (length(unlist(strsplit((cln.table[order_col_names[j]])[i,], ''))) != 3) {
          p_none = p_none + 1
        } else {
          where = unlist(gregexpr('V', (cln.table[order_col_names[j]])[i,]))
          if (where == 2) {
            p_med = p_med + 1
          } else {
            p_lat = p_lat + 1
          }
        }
      }
    }
    tot = (p_none+p_med+p_lat)
    p_none = p_none/tot
    p_med = p_med/tot
    p_lat = p_lat/tot
    cln.table$Per.Vlat[i] = p_lat
    cln.table$Per.Vmed[i] = p_med
    cln.table$Per.Othe[i] = p_none
  }
}

cln.table[(nrow(cln.table)+1),] = "%Vlat"
cln.table[(nrow(cln.table)+1),] = "#Incomplete"
cln.table[(nrow(cln.table)+1),] = "%Incomplete"

for (j in 5:length(order_col_names)) {
  p_lat = p_med = p_none = 0
  for (i in 1:(nrow(cln.table)-3)) {
      if (length(unlist(strsplit((cln.table[order_col_names[j]])[i,], ''))) != 3) {
        p_none = p_none + 1
      } else {
        where = unlist(gregexpr('V', (cln.table[order_col_names[j]])[i,]))
        if (where == 2) {
          p_med = p_med + 1
        } else {
          p_lat = p_lat + 1
        }
      } 
    }
  p_lat = p_lat/(p_lat+p_med)
  cln.table[order_col_names[j]][(nrow(cln.table)-2),] = p_lat*100
  cln.table[order_col_names[j]][(nrow(cln.table)-1),] = p_none
  p_none = p_none/(p_lat+p_med+p_none)
  cln.table[order_col_names[j]][(nrow(cln.table)),] = p_none*100
}

directory = getwd()
write.csv(cln.table, file = paste0(directory, "/dfwide.csv"))
  
  
#   tot = (p_med+p_lat)
#   df.wide[order_col_names[j]][k,]
#   (cln.table[cow])[(nrow(cln.table)),] <- p_none
  }


#   #And grab the info we need from the last 'trial' (feedback)
#   if (is.null(a$data[[mylength-1]]$trialdata$responses)){df.wide$feedback[i] = "none"
#   }else{
#     df.wide$feedback[i] = a$data[[mylength-1]]$trialdata$responses
#   }
} #End of this participant

# Run this again for participants who did not pass the quiz the first time.
# for (i in 1:nrow(df.wide)){
#   if (!is.na(df.complete$datastring[i])){
#     a = fromJSON(df.complete$datastring[i])
#     mylength = length(a$data)
#   } else{
#     a = data.frame(NULL)
#     mylength = 0
#   }
#   print(mylength)
#   if (mylength>86){
#     df.wide$participant[i] = i
#     df.wide$workerId[i] = a$workerId
#     df.wide$browser[i] = df.complete$browser[i]
#     df.wide$beginhit[i] = df.complete$beginhit[i]
#     for (j in 1:mylength){
#       try(
#       if(a$data[[j]]$trialdata$trial_index_global %in% global_indeces){
#         if(!is.null(a$data[[j]]$trialdata$rt)){
#           df.wide[[paste("rt_",a$data[[j]]$trialdata$trial_index_global, sep="")]][i] = a$data[[j]]$trialdata$rt
#         }
#         if(!is.null(a$data[[j]]$trialdata$moves)){
#           df.wide[[paste("moves_",a$data[[j]]$trialdata$trial_index_global, sep="")]][i] = a$data[[j]]$trialdata$moves
#           global_indeces <- unique(c(global_indeces,a$data[[j]]$trialdata$trial_index_global))
#         }
#         if(!is.null(a$data[[j]]$trialdata$glyph)){
#           df.wide[[paste("PracGlyph_",a$data[[j]]$trialdata$trial_index_global, sep="")]][i] = a$data[[j]]$trialdata$glyph
#         }
#         if(!is.null(a$data[[j]]$trialdata$Sub)){
#           df.wide[[paste("Sub_",a$data[[j]]$trialdata$trial_index_global, sep="")]][i] = a$data[[j]]$trialdata$Sub
#         }
#         if(!is.null(a$data[[j]]$trialdata$Vrb)){
#           df.wide[[paste("Vrb_",a$data[[j]]$trialdata$trial_index_global, sep="")]][i] = a$data[[j]]$trialdata$Vrb
#         }
#         if(!is.null(a$data[[j]]$trialdata$Obj)){
#           df.wide[[paste("Obj_",a$data[[j]]$trialdata$trial_index_global, sep="")]][i] = a$data[[j]]$trialdata$Obj
#         }
#       } #Else just don't make any columns right now!!!
#     )}
#   }
#   
#   #And grab the info we need from the last 'trial' (feedback)
#   if (is.null(a$data[[mylength-1]]$trialdata$responses)){df.wide$feedback[i] = "none"
#   }else{
#     df.wide$feedback[i] = a$data[[mylength-1]]$trialdata$responses
#   }
# } 

    
    
#Notes: Something up with 1/16/15 subj 71's datastring:it's missing - recorded at end, so presumably lost before that?  Add a check for null datastrings

#Weird behavior! I got those wrong-lenght participants to be assigned a participant no of NA, which is something, anyway.
#Lost 6 people to this.
nrow(df.wide)
df.wide = df.wide[!is.na(df.wide$participant),]
nrow(df.wide)

#OOPS from 1/15/15: I didn't have the first trial (set to show the prototype movie) save the right variables, so record them
#here

df.wide$exposurePath_5 =df.wide$exposurePath_6
df.wide$exposureManner_5 =df.wide$exposureManner_6
df.wide$condition_5 =df.wide$condition_6
  

#Reformat into long form!
df.long = wideToLong(subset(df.wide,select=-feedback),within="trial")

#create factors
df.long = mutate(df.long, participant = as.numeric(participant),
          trial = as.numeric(as.character(trial)),
          rt = as.numeric(as.character(rt)),
          keypress = as.numeric(as.character(keypress))-48, #transform keycodes to numerals!
          stimCondition = factor(stimCondition,levels=c("NoChange","BothChange", "PathChange","MannerChange")),
          condition = factor(condition, levels=c("Noun","Verb")))

df.long = df.long[order(df.long$participant,df.long$trial),]

#Analyze data!--------------------------------------------------

#For each participant, make a score, which is abs(mean(mannerchange)-mean(pathchange))
#(And add some extra descriptive stats for the paper)

Scores = ""

mannerScores = aggregate(df.long[df.long$stimCondition=="MannerChange",]$keypress, by=list(df.long[df.long$stimCondition=="MannerChange",]$participant, df.long[df.long$stimCondition=="MannerChange",]$condition), mean.na.rm)
names(mannerScores) = c("participant", "condition", "mannerscore")
pathScores = aggregate(df.long[df.long$stimCondition=="PathChange",]$keypress, by=list(df.long[df.long$stimCondition=="PathChange",]$participant, df.long[df.long$stimCondition=="PathChange",]$condition), mean.na.rm)
names(pathScores) = c("participant", "condition", "pathscore")
sameScores = aggregate(df.long[df.long$stimCondition=="NoChange",]$keypress, by=list(df.long[df.long$stimCondition=="NoChange",]$participant, df.long[df.long$stimCondition=="NoChange",]$condition), mean.na.rm)
names(sameScores) = c("participant","condition","samescore")
bothScores = aggregate(df.long[df.long$stimCondition=="BothChange",]$keypress, by=list(df.long[df.long$stimCondition=="BothChange",]$participant, df.long[df.long$stimCondition=="BothChange",]$condition), mean.na.rm)
names(bothScores) = c("participant","condition","bothscore")
      
Scores = merge(mannerScores, pathScores, by=c("participant", "condition"))
Scores = merge(Scores, sameScores, by=c("participant", "condition"))
Scores = merge(Scores, bothScores, by=c("participant", "condition"))

#Basic descriptives

mean(Scores$samescore)
mean(Scores$mannerscore)
mean(Scores$pathscore)
mean(Scores$bothscore)

with(Scores, tapply(samescore, list(condition), mean, na.rm=TRUE), drop=TRUE)
t.test(Scores[Scores$condition == "Noun",]$samescore, Scores[Scores$condition == "Verb",]$samescore)

with(Scores, tapply(bothscore, list(condition), mean, na.rm=TRUE), drop=TRUE)
t.test(Scores[Scores$condition == "Noun",]$bothscore, Scores[Scores$condition == "Verb",]$bothscore)

with(Scores, tapply(mannerscore, list(condition), mean, na.rm=TRUE), drop=TRUE)
t.test(Scores[Scores$condition == "Noun",]$mannerscore, Scores[Scores$condition == "Verb",]$mannerscore)

with(Scores, tapply(pathscore, list(condition), mean, na.rm=TRUE), drop=TRUE)
t.test(Scores[Scores$condition == "Noun",]$pathscore, Scores[Scores$condition == "Verb",]$pathscore)

#More interesting measures
Scores$diffscore = abs(Scores$mannerscore - Scores$pathscore)
Scores$ILikeMannerscore = Scores$pathscore - Scores$mannerscore

with(Scores, tapply(diffscore, list(condition), mean, na.rm=TRUE), drop=TRUE)
with(Scores, tapply(diffscore, list(condition), stderr), drop=TRUE)

with(Scores, tapply(ILikeMannerscore, list(condition), mean, na.rm=TRUE), drop=TRUE)
with(Scores, tapply(ILikeMannerscore, list(condition),stderr), drop=TRUE)

#And let's do a dead simple t test on that

t.test(Scores[Scores$condition == "Noun",]$diffscore, Scores[Scores$condition == "Verb",]$diffscore)
cohensD(Scores[Scores$condition == "Noun",]$diffscore, Scores[Scores$condition == "Verb",]$diffscore)


#Time for some regressions
nounScores <- Scores[Scores$condition == 'Noun',]
verbScores <- Scores[Scores$condition == 'Verb',]

noun.lm <- lm(mannerscore ~ pathscore, data=nounScores)
summary(noun.lm)
summary(noun.lm)$r.squared

verb.lm <- lm(mannerscore ~ pathscore, data=verbScores)
summary(verb.lm)
summary(verb.lm)$r.squared

#A post-hoc analysis: do verb or noun people say yes more often?
allScores = aggregate(df.long$keypress, by=list(df.long$participant, df.long$condition), mean.na.rm)
names(allScores) = c("participant", "condition", "allscore")

with(allScores, tapply(allscore, list(condition), mean, na.rm=TRUE), drop=TRUE)
with(allScores, tapply(allscore, list(condition), stderr), drop=TRUE)

t.test(allScores[allScores$condition == "Noun",]$allscore, allScores[allScores$condition == "Verb",]$allscore)
cohensD(allScores[allScores$condition == "Noun",]$allscore, allScores[allScores$condition == "Verb",]$allscore)

#Against the prediction I might have made, Noun categories are slightly SMALLER!  
#So this isn't just verb people making smaller categories - they say yes to about the same # of things, but distribute differently

#Graph data------------------------------------------------

#Bar graph of means----------------

##Summarize the data for graphing
data.summary.diffscores <- data.frame(
  condition=levels(Scores$condition),
  mean=with(Scores, tapply(diffscore, list(condition), mean, na.rm=TRUE), drop=TRUE),
  n=with(Scores, tapply(diffscore, list(condition), length)),
  se=with(Scores, tapply(ILikeMannerscore, list(condition), stderr), drop=TRUE)
)

# Precalculate margin of error for confidence interval
data.summary.diffscores$me <- qt(1-0.05/2, df=data.summary.diffscores$n)*data.summary.diffscores$se

# Use ggplot to draw the bar plot!
png('manydax-barplot-se.png') # Write to PNG
ggplot(data.summary.diffscores, aes(x = condition, y = mean)) +  
  geom_bar(position = position_dodge(), stat="identity", fill="brown") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.25) +
  ylim(0,6) +
  ylab("Abs(mean(manner) - mean(path))")+
  xlab("")+
  ggtitle("Rating of manner vs. path changes") + # plot title
  theme_bw() + # remove grey background (because Tufte said so)
  theme(panel.grid.major = element_blank()) # remove x and y major grid lines (because Tufte said so)
dev.off() # Close PNG

#Scatterplot of manner vs. path scores of each partic ---------


png('manydax-noun-scatterplot-95ci.png')
ggplot(nounScores, aes(x=mannerscore, y=pathscore)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)
dev.off()

png('manydax-verb-scatterplot-95ci.png')
ggplot(verbScores, aes(x=mannerscore, y=pathscore)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)
dev.off()

