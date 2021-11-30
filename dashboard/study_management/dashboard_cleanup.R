# break up the rdata files into dataframes that are appropriately formatted for front-end
# save these dataframes as csvs
# create the graphs for the front-end
# copy the csvs, json, ect to the site's data folder
# copy the graphs to the site's static folder
# Have a directory called "cleanup" where the cleanup scripts can be placed, found, and executed

#library(plyr)

# little helper function to create named list from objects
name_list <- function(...) {
  vnames <- as.character(match.call())[-1]
  return(setNames(list(...), vnames))
}

# This script should only be run after sourcing data_management_functions.R

# function to convert lists in a dataframe to character vectors
list_to_cv <- function(x) {
  if(typeof(x) == "list") {
    return (toString(unlist(x)))
  }
  else {
    return(x)
  }
}

# load the current subject's data
perf <- filter(output$subj_performance,ID==subj) %>% filter(!is.na(date))
proc_sched <- output$proc_data[[subj]]
info <- output$subj_info[[subj]]
# 2021-11-30 AndyP changed to individual subject
path_to_eeg <- paste0(dataPath,'/Subjects/',subj,'/physio')
eeg_raw <- list.files(path_to_eeg,pattern=paste0(subj,'_physio_proc.rdata'))
if (length(eeg_raw)==1){
  #answers <- output$proc_data[[subj]]$raw_data$answers
  all_output <- output
  load(paste0(path_to_eeg,'/',eeg_raw))
  eeg <- output$eeg_summary
  eeg_rawsum <- output$eeg_rawsum
  hr <- output$ecg_summary
  output <- all_output
} else {
  warning('could not find subject physio_proc.rdata, reverting to output_physio.rdata')
  warning('2021-10-06 AndyP changed eeg$summary[[subj]] to eeg$rawsum[[subj]], delete warning when fixed')
  eeg_rawsum <- output_physio$eeg$rawsum[[subj]]
  hr <- output_physio$ecg$summary[[subj]]
  eeg <- output_physio$eeg$summary[[subj]]
  #answers <- output$proc_data[[subj]]$raw_data$answers
}
#if(exists("redcap_data")) {
#  if(redcap_data != "The RL-EMA QA checklist on RedCAP is blank for all participants in the active participants list.") {
#    checklist <- redcap_data %>% filter(ID==subj) %>% select(`Checklist Complete?`,Date)
#    checklist$Date <- as.character(checklist$Date)
#  }
#}
perf$date <- as.character(perf$date)
#get block and date columns
blk_dt <- perf %>% select(block,date) #%>% dplyr::rename("Date"=date)
blk_dt$date <- sapply(blk_dt$date,date_format) #lapply(blk_dt$date,date_format) #reformat dates
#blk_dt <- dplyr::rename(blk_dt,"Block"=block)

#if(exists("redcap_data")) {
#  if(redcap_data != "The RL-EMA QA checklist on RedCAP is blank for all participants in the active participants list.") {
#    blk_dt <- left_join(blk_dt, checklist, by="Date")
    #blk_dt <- transmute(blk_dt,"Date"=date,"Block"=block, checklist=`Checklist Complete?`)
#  }
#}

print(paste0("Running cleanup for ", subj, "..."))

# function to source the cleanup scripts in order to run them
source_cleanup_scripts <- function(subdir='') {
  curr_subdir <- subdir
  if (curr_subdir != '') {
    curr_subdir = paste0('/cleanup/', subdir)
  }
  else {
    curr_subdir = '/cleanup'
  }
  # create the path for the images if it does not exist

  # get the list of cleanup scripts from the subdir
  cleanup_scripts = list.files(path=paste0(getwd(), curr_subdir), pattern=".R")
  #cleanup_scripts = list('eeg.R')
  #print(paste0("Running this dir: ",getwd(), curr_subdir))
  for(script in cleanup_scripts) {
    # Running of data cleanup needed before generating graphs
    stimuli <<- output$proc_data[[subj]]$raw_data$stimuli
    trials_1 <<- output$proc_data[[subj]]$raw_data$trials
    ## remove blocks that have not been played yet
    if (length(which(is.na(trials_1$choice)))!=0){
      trials_1<<-trials_1[-c(which(is.na(trials_1$choice))),]}
    #remove fMRI blocks:
    trials_1<<-trials_1[trials_1$block<1000, ]

    ##accuracy analysis according to designated probabilities##
    #add objective expected value (EV) for each stimulus and objective accuracy for each trial

    for (i in 1:length(trials_1$block)){
      trials_1$rank1[i]<<-stimuli$rank[trials_1$stim1[i]+1]
      trials_1$rank2[i]<<-stimuli$rank[trials_1$stim2[i]+1]
      trials_1$accuracy[i]<<-((trials_1$rank1[i]>trials_1$rank2[i])&&(trials_1$choice[i]==0)||(trials_1$rank1[i]<trials_1$rank2[i])&&(trials_1$choice[i]==1))
      if (trials_1$rank1[i]==trials_1$rank2[i])
        trials_1$accuracy[i]=NA
    }

    #By block
    objective_accuracy_by_block<<-plyr::ddply(trials_1, plyr::.(block, feedback), summarize, mean=mean(accuracy, na.rm = T))
    #Overall
    overall_with_feedback_mean<<-mean(objective_accuracy_by_block$mean[objective_accuracy_by_block$feedback==1], na.rm = T)*100
    overall_no_feedback_mean<<-mean(objective_accuracy_by_block$mean[objective_accuracy_by_block$feedback==0], na.rm = T)*100
    objective_accuracy_by_block$feedback<<-as.factor(objective_accuracy_by_block$feedback)

    ##Accuracy relative to probabilities that were experienced (constantly updating until an image switches to its no-feedback phase)##

    trials_1$relative_stim1<<-rep(NaN, nrow(trials_1))
    trials_1$relative_stim2<<-rep(NaN, nrow(trials_1))
    for (i in 2:nrow(trials_1)){
      trials_1$relative_stim1[i]<<-mean(trials_1$outcome[which(trials_1$stim1==trials_1$stim1[i]&trials_1$choice==0&trials_1$feedback==1&((trials_1$trial<trials_1$trial[i]&trials_1$block==trials_1$block[i])|trials_1$block<trials_1$block[i])|trials_1$stim2==trials_1$stim1[i]&trials_1$choice==1&trials_1$feedback==1&((trials_1$trial<trials_1$trial[i]&trials_1$block==trials_1$block[i])|trials_1$block<trials_1$block[i]))])
      trials_1$relative_stim2[i]<<-mean(trials_1$outcome[which(trials_1$stim1==trials_1$stim2[i]&trials_1$choice==0&trials_1$feedback==1&((trials_1$trial<trials_1$trial[i]&trials_1$block==trials_1$block[i])|trials_1$block<trials_1$block[i])|trials_1$stim2==trials_1$stim2[i]&trials_1$choice==1&trials_1$feedback==1&((trials_1$trial<trials_1$trial[i]&trials_1$block==trials_1$block[i])|trials_1$block<trials_1$block[i]))])
    }
    trials_1$relative_accuracy<<-NA
    index<-which(!is.nan(trials_1$relative_stim1)&!is.nan(trials_1$relative_stim2))

    #accuracy according to experienced probabilities (differences of less than 10% between probabilties are omitted)
    for (i in index){
      trials_1$relative_accuracy[i]<<-((trials_1$relative_stim1[i]>trials_1$relative_stim2[i]+0.1)&&(trials_1$choice[i]==0)||(trials_1$relative_stim1[i]+0.1<trials_1$relative_stim2[i])&&(trials_1$choice[i]==1))
      if (((abs(trials_1$relative_stim1[i]-trials_1$relative_stim2[i])<0.1)&&(abs(trials_1$relative_stim2[i]-trials_1$relative_stim1[i]))<0.1))
        trials_1$relative_accuracy[i]<<-NA
    }

    relative_accuracy_by_block<<-plyr::ddply(trials_1, plyr::.(block, feedback), summarize, mean=mean(relative_accuracy, na.rm = T))
    overall_relative_with_feedback_mean<<-mean(relative_accuracy_by_block$mean[relative_accuracy_by_block$feedback==1], na.rm = T)*100
    overall_relative_no_feedback_mean<<-mean(relative_accuracy_by_block$mean[relative_accuracy_by_block$feedback==0], na.rm = T)*100
    relative_accuracy_by_block$feedback<<-as.factor(relative_accuracy_by_block$feedback)

    #test reaction time
    trials_1$RT<<-as.numeric(((trials_1$choice_time)-(trials_1$stim_time)))
    RT_by_block<<-plyr::ddply(trials_1, plyr::.(block, feedback), dplyr::summarize, mean=mean(rt, na.rm = T))
    RT_by_block$feedback<<-as.factor(RT_by_block$feedback)
    RT_by_block$mean <<- (RT_by_block$mean)*1000

    #test side bias
    side_bias_by_block<<-plyr::ddply(trials_1, plyr::.(block), plyr::summarize, mean=mean(choice, na.rm = T))

    #test what percentage in the last ten blocks have relative accuracy of less than 70% (with feedback)
    #this will only run after 5 sessions were played (in addition to the 6 practice blocks)
    if (max(trials_1$block)>14){
      blocks_with_poor_performance<<-data.frame(poor_blocks=relative_accuracy_by_block$block[which(relative_accuracy_by_block$block>5&relative_accuracy_by_block$feedback==1&relative_accuracy_by_block$mean<0.7)])
      percentage_last_ten<<-as.numeric(sum(blocks_with_poor_performance$poor_blocks>(max(relative_accuracy_by_block$block)-10)))*10
      if (percentage_last_ten>20){
        mean_RT_low_performance<<-mean(RT_by_block$mean[which(RT_by_block$block %in% relative_accuracy_by_block$block[which(relative_accuracy_by_block$block>5&relative_accuracy_by_block$feedback==1&relative_accuracy_by_block$mean<0.7)]&RT_by_block$feedback==1&RT_by_block$block>max(relative_accuracy_by_block$block)-10)])
        mean_side_bias_low_performance<<-mean(side_bias_by_block$mean[which(side_bias_by_block$block %in% relative_accuracy_by_block$block[which(relative_accuracy_by_block$block>5&relative_accuracy_by_block$feedback==1&relative_accuracy_by_block$mean<0.7)]&side_bias_by_block$block>max(relative_accuracy_by_block$block)-10)])}}

    answers <<- output$proc_data[[subj]]$raw_data$answers
    #test valance-arousal/VAS correlation
    valence_arousal<<-answers$answer[answers$questionnaire_type==0&answers$question==0&answers$questionnaire_number>0]
    if (length(valence_arousal)!=0){
      VAS<<-answers$answer[answers$questionnaire_type==0&answers$question==1&answers$questionnaire_number>0]
      valence <<- as.numeric(str_match(valence_arousal, "Valence=(.*?),")[,2])
      arousal<<-as.numeric(sub("^.+Arousal=", "", valence_arousal))
      elated <<- as.numeric(str_match(VAS, "Elated=(.*?),")[,2])
      sad <<- as.numeric(str_match(VAS, "Sad=(.*?),")[,2])
      energetic<<-as.numeric(sub("^.+Energetic=", "", VAS))
      elated_sad<<-elated-sad
    }
    arousal_energetic<<-data.frame(arousal,energetic)
    valence_elated_sad<<-data.frame(valence, elated_sad)

    print(paste0("Running: ", getwd(), curr_subdir, '/', script))
    source(paste0(getwd(), curr_subdir, '/', script))
  }
}

# run cleanup
source_cleanup_scripts(subdir="reports")
