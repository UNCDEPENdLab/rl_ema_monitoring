Overall_Compliance <- round(0, digits= 3) # output$sample_info_df$compliance
#print(output$sample_info_df$compliance)
current_date <- Sys.Date()

#Games % Stats
Avg_Objective_Percent_Correct_No_Feedback <- output$proc_data[[subj]]$performance_overall$abs_accurate_nofeed
Avg_Objective_Percent_Correct_No_Feedback <- round(Avg_Objective_Percent_Correct_No_Feedback, digits = 3)
Avg_Objective_Percent_Correct_Feedback <- output$proc_data[[subj]]$performance_overall$abs_accurate_feed
Avg_Objective_Percent_Correct_Feedback <- round(Avg_Objective_Percent_Correct_Feedback, digits = 3)
Avg_Experienced_Percent_Correct_No_Feedback <- output$proc_data[[subj]]$performance_overall$relative_accuracy_nofeed
Avg_Experienced_Percent_Correct_No_Feedback <- round(Avg_Experienced_Percent_Correct_No_Feedback, digits = 3)
Avg_Experienced_Percent_Correct_Feedback <- output$proc_data[[subj]]$performance_overall$relative_accuracy_feed
Avg_Experienced_Percent_Correct_Feedback <- round(Avg_Experienced_Percent_Correct_Feedback, digits = 3)
#Add EEG Avg Here (named EEG_Average)
# EEG_Average <- mean(mean(output_physio$eeg$summary[[subj]]$per_Ch_1),
#                     mean(output_physio$eeg$summary[[subj]]$per_Ch_2),
#                     mean(output_physio$eeg$summary[[subj]]$per_Ch_3),
#                     mean(output_physio$eeg$summary[[subj]]$per_Ch_4),)*100 #[match(output$sample_info_df[[subj]], output_physio$eeg$sample_summary[[subj]])]
#EEG_Average <- round(EEG_Average, digits = 3)
EEG_Average <- mean(eeg_rawsum$goodTrials)
EEG_Average <- round(EEG_Average, digits = 3)
#Add HR Avg Here (named HR_Average)
# 2021-12-14 AndyP using individual subject's processed physio
path_to_physio <- paste0(dataPath,'/Subjects/',subj,'/physio')
physio_proc <- list.files(path_to_physio,pattern=paste0(subj,'_physio_proc.rdata'))
if (length(physio_proc)==1){
  all_output <- output # preserve global variable output (which is the processed schedule file) into a temporary variable
  load(paste0(path_to_physio,'/',physio_proc)) # loads a variable called output into global environment
  hr_gen <- output$ecg_ov$per_Good
  output <- all_output # revert output to processed schedule file
}
#hr_gen <- output_physio$ecg$summary[[subj]]$per_Good
HR_Average <- hr_gen*100 #[match(subj, hr_gen[[subj]])]*100
HR_Average <- round(HR_Average, digits = 3)
qxn <- output$proc_data[[subj]]$form_summary
#print(qxn)
ID_Overview = data.frame(
  subj,
  #"Needs_RC",
  #"Needs_RC",
  #"Needs_RC",
  #"Needs_RC",
  #"Needs_RC",
  #"Needs_RC",
  #"Needs_RC",
  Overall_Compliance,
  Avg_Objective_Percent_Correct_No_Feedback,
  Avg_Objective_Percent_Correct_Feedback,
  Avg_Experienced_Percent_Correct_No_Feedback,
  Avg_Experienced_Percent_Correct_Feedback,
  EEG_Average,
  HR_Average,
  round(output$proc_data[[subj]]$performance_overall$IDe_bias, digits = 3),
  round(qxn$val_arr_dis_avg, digits = 3),
  round(qxn$emo_rate_avg, digits = 3),
  round(qxn$val_emo_cor, digits = 3)
)
col_names <- c("ID",
               #"Site", "Group", "RA", "RL-EMA Start Date", "Current Day in Protocol", "fMRI date", "fMRI completed",
               "Overall Compliance",
               "Avg Obj Correct (no feedback)", "Avg Obj Correct (w/ feedback)", "Avg Rel Correct (no feedback)", "Avg Rel Correct (w/ feedback)",
               "EEG Average", "HR Average", "Left %", "Valence/Arousal Distance from Origin", "Emotion Distance from 0", "Emotion/Valence Correlation")
colnames(ID_Overview) <- col_names
Individual_Overview <- filter(ID_Overview, ID==subj) #change for copying for other participants
#Individual_Overview<- head(Individual_Overview,1) #remove once REDCap is figured out

saveRDS(ID_Overview, paste0(dataPath, "/Subjects/", subj, "/reports/overall.rds"))
