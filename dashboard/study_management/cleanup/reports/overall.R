Overall_Compliance <- round(output$sample_info_df$compliance, digits= 3)
current_date <- Sys.Date()

#Games % Stats
Avg_Objective_Percent_Correct_No_Feedback <- output$sample_performance$abs_accurate_nofeed
Avg_Objective_Percent_Correct_No_Feedback <- round(Avg_Objective_Percent_Correct_No_Feedback, digits = 3)
Avg_Objective_Percent_Correct_Feedback <- output$sample_performance$abs_accurate_feed
Avg_Objective_Percent_Correct_Feedback <- round(Avg_Objective_Percent_Correct_Feedback, digits = 3)
Avg_Experienced_Percent_Correct_No_Feedback <- output$sample_performance$relative_accuracy_nofeed
Avg_Experienced_Percent_Correct_No_Feedback <- round(Avg_Experienced_Percent_Correct_No_Feedback, digits = 3)
Avg_Experienced_Percent_Correct_Feedback <- output$sample_performance$relative_accuracy_feed
Avg_Experienced_Percent_Correct_Feedback <- round(Avg_Experienced_Percent_Correct_Feedback, digits = 3)
#Add EEG Avg Here (named EEG_Average)
EEG_Average <- output_physio$eeg$sample_summary$avg_allCh*100 #[match(output$sample_info_df[[subj]], output_physio$eeg$sample_summary[[subj]])]
EEG_Average <- round(EEG_Average, digits = 3)
#Add HR Avg Here (named HR_Average)
hr_gen <- output_physio$ecg$sample_summary
HR_Average <- hr_gen$per_Good*100 #[match(subj, hr_gen[[subj]])]*100
HR_Average <- round(HR_Average, digits = 3)
qxn <- output$sample_form_summary
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
  round(output$sample_performance$IDe_bias, digits = 3),
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
