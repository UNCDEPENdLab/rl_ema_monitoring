---
title: "{{ replace .Name "-" " " | title }}"
output: html_document
categories: ["subjects"]
tags: ["{{ replace .Name "-" " " | title }}"]
params:
  id: {{ replace .Name "-" " " | title }}
---

```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = '../../../..') # makes root dir rl_ema_monitoring
library(kableExtra)
library(dplyr)
```

```{r source, include=FALSE, }
#knitr::opts_chunk$set(root.dir = '../../..')
source("EEG_Dashboard.R")
source("ECG_Dashboard.R")
source("dashboard/study_management/data_management_functions.R")
source("dashboard/study_management/dashboard_aggregate.R")

```

```{r load, include=FALSE}
#Not sure how subject-specific processed schedule file data and processed physio file data will be passed to this Rmd, so just loading in local RData objects and manually accessing a single subject's data for now
load("site/data/abb_proc_sched.Rdata") 
load("site/data/emp_physiooutput.Rdata")
perf <- filter(output$subj_performance,ID==params$id)
proc_sched <- output$proc_data[[params$id]]
info <- output$subj_info[[params$id]]
eeg <- output_physio$eeg$summary[[params$id]]
hr <- output_physio$ecg$summary[[params$id]]
#get block and date columns
blk_dt <- perf %>% select(block,date)
blk_dt$date <- lapply(blk_dt$date,date_format) #reformat dates
blk_dt <- transmute(blk_dt,"Date"=date,"Block"=block)
 
```

```{r sleep, include=FALSE}
#Sleep diary table wrangling
diary <- proc_sched$form_dfs$`Sleep Diary`
diary <- diary[,-c(1:3,9)]
diary <- rename(diary,"Block"=session_number) 
diary <- left_join(diary,blk_dt,by="Block") #add date column
diary <- transmute(diary, "Date"=Date, "Block"=Block, "Sleep latency"=sleep_latency, "Woke many time"=woke_many_times, "Woke early"=woke_early, "Overall"=overall)
diary <- arrange(diary, -row_number()) #make the most recent block come first
```

```{r mood, include=FALSE}
#Mood report table wrangling
mood <- proc_sched$form_dfs$`Mood Questionnaire`
mood <- mood[,-c(1:3,9,12,14,15)]
mood$`RA Check` <- c(1,1,1,0,1,1,1,0,1,0,0,0,1,1,1,1,1,1,1,1,0,1,1,1,1,1) #adding RA check column, which will eventually be filled by REDCap integration
mood <- rename(mood,"Block"=session_number)
mood <- left_join(mood,blk_dt,by="Block") #add date column
mood <- arrange(mood, -row_number()) #make the most recent block come first
mood <- transmute(mood, "Date"=Date, "Block"=Block, "RA Check"=`RA Check`,"Number of events"=number_of_events, "Valence"=Valence, "Arousal"=Arousal, "Anxious"=Anxious, "Elated"=Elated, "Irritable"=Irritable,"Energetic"=Energetic)
```

```{r performance, include=FALSE}
#task performance table wrangling
task <- filter(perf,!is.na(date)) #get rid of blocks not played
task$date <- lapply(task$date,date_format) #reformat dates (these dates don't come from blk_dt)
task <- arrange(task, -row_number()) #flip df so that most recent dates are first
task <- transmute(task, "Date"=date, "Block"=block, "Objective % correct (feed.)"=abs_accurate_feed, "Experienced % correct (feed.)"=relative_accuracy_feed, "Objective % correct (no feed.)"=abs_accurate_nofeed, "Experienced % correct (no feed.)"=relative_accuracy_nofeed, "RT(ms)"=mean_rt, "Left %"=side_bias)
#convert decimals to percentages
task$"Objective % correct (feed.)" <- task$"Objective % correct (feed.)"*100
task$"Experienced % correct (feed.)" <- task$"Experienced % correct (feed.)"*100
task$"Experienced % correct (no feed.)" <- task$"Experienced % correct (no feed.)"*100
task$"Objective % correct (no feed.)" <- task$"Objective % correct (no feed.)"*100
task$"Left %" <- task$"Left %"*100
task$"RT(ms)" <- task$"RT(ms)"*1000
task[,-c(1,2)] <- round(task[,-c(1,2)]) #round percentages to nearest whole percent
rownames(task) <- NULL
```

```{r eeg, include=FALSE}
#EEG table wrangling
eeg <- rename(eeg,"Block"=block)
eeg_dates <- left_join(eeg,blk_dt,by="Block") #add date column
eeg_avg <- mutate(eeg_dates,"avg"=(per_Ch_1 + per_Ch_2 + per_Ch_3 +per_Ch_4)/4) #create colum for avg EEG signal 
eeg_row_ordered <- arrange(eeg_avg, -row_number()) #flip df so most recent blocks are first
#convert all to percentages, rounded
eeg_row_ordered$per_Ch_1 <- eeg_row_ordered$per_Ch_1*100
eeg_row_ordered$per_Ch_1 <- round(eeg_row_ordered$per_Ch_1)
eeg_row_ordered$per_Ch_2 <- eeg_row_ordered$per_Ch_2*100 
eeg_row_ordered$per_Ch_2 <- round(eeg_row_ordered$per_Ch_2)
eeg_row_ordered$per_Ch_3 <- eeg_row_ordered$per_Ch_3*100 
eeg_row_ordered$per_Ch_3 <- round(eeg_row_ordered$per_Ch_3)
eeg_row_ordered$per_Ch_4 <- eeg_row_ordered$per_Ch_4*100
eeg_row_ordered$per_Ch_4 <- round(eeg_row_ordered$per_Ch_4)
eeg_row_ordered$avg <- eeg_row_ordered$avg*100
eeg_row_ordered$avg <- round(eeg_row_ordered$avg)
eeg_col_named <- transmute(eeg_row_ordered, "Date"=Date, "Block"=Block, "Overall % good"=avg, "Ch. 1 % good"=per_Ch_1, "Ch. 2 % good"=per_Ch_2, "Ch. 3 % good"=per_Ch_3, "Ch. 4 % good"=per_Ch_4) 
```
 
```{r hr, include=FALSE}
#HR table wrangling
hr <- rename(hr,"Block"=block)
hr_dates <- left_join(hr,blk_dt,by="Block") #add date column
hr_ordered <- arrange(hr_dates, -row_number()) #most recent blocks first
#convert all to percentages, rounded
hr_ordered$per_Good <- hr_ordered$per_Good*100
hr_ordered$per_Good <- round(hr_ordered$per_Good)
hr_ordered <- transmute(hr_ordered, "Date"=Date, "Block"=Block, "Good signal %"=per_Good) 
```

```{r compliance, include=FALSE}
compliance <-info[c("scheduled_time","delay","type", "session_number")]
compliance$scheduled_time <- as.Date(compliance$scheduled_time)
compliance$delay <- round(as.numeric(compliance$delay), digits = 0)
compliance$s_type[compliance$type=="trials"] <- "Behavioral Game"
compliance$s_type[compliance$type=="questionnaires"] <- sapply(info$spec[compliance$type=="questionnaires"],`[[`,3)
compliance$session_number <- as.character(compliance$session_number)
compliance$delay <- round(compliance$delay)
#filters out NA/incomplete/upcoming data
compliance_filtered <- compliance %>% filter(!is.na(s_type))
compliance_filtered <- compliance_filtered %>% filter(!is.na(delay))
print(compliance_filtered)
#pivot table
library("pivottabler")
compliance_pf <- PivotTable$new()
compliance_pf$addData(compliance_filtered)
compliance_pf$addColumnDataGroups("s_type", addTotal=FALSE)
compliance_pf$addRowDataGroups("scheduled_time", addTotal=FALSE)
compliance_pf$addRowDataGroups("session_number", addTotal=FALSE)
compliance_pf$defineCalculation(calculationName = "delay", summariseExpression = "mean(as.numeric(delay), na.rm=TRUE)")
compliance_pf$evaluatePivot()
# apply the green style for an average delay of between 0 and 2 hours
cells <- compliance_pf$findCells(minValue=0, maxValue=120, includeNull=FALSE, includeNA=FALSE)
compliance_pf$setStyling(cells=cells, declarations=list("background-color"="#C6EFCE", "color"="#006100"))
# apply the yellow style for an average delay of between 2 and 4 hours
cells <- compliance_pf$findCells(minValue=121, maxValue=240, includeNull=FALSE, includeNA=FALSE)
compliance_pf$setStyling(cells=cells, declarations=list("background-color"="#FFEB9C", "color"="#9C5700"))
# apply the red style for an average delay of 4 hours or greater
cells <- compliance_pf$findCells(minValue=241, includeNull=FALSE, includeNA=FALSE)
compliance_pf$setStyling(cells=cells, declarations=list("background-color"="#FFC7CE", "color"="#9C0006"))
```


\newline
# Task performance

```{r perf table, echo=FALSE}
#Task performance table
kbl(task) %>%
  kable_styling(fixed_thead = T) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, bold = T) %>%
  column_spec(3, background=case_when(task[,3]>80 ~ "#C6EFCE",task[,3]>70 & task[,3]<=80 ~ "#FFEB9C", task[,3]<70 ~ "#ffc7ce")) %>%
  column_spec(4, background=case_when(task[,4]>80 ~ "#C6EFCE",task[,4]>70 & task[,4]<=80 ~ "#FFEB9C", task[,4]<70 ~ "#ffc7ce")) %>%
  column_spec(5, color=ifelse(task[,5] > 55 | is.na(task[,5]), "#D8D8D8", "black")) %>%
  column_spec(6, color=ifelse(task[,6] > 55 | is.na(task[,6]), "#D8D8D8","black")) %>%
  column_spec(7, color=ifelse(task[,7] > 1400, "#E6E6E6","black")) %>%
  column_spec(8, color=ifelse(task[,8] >= 67 | task[,8] < 67, "#D8D8D8","black"))
```

\newline
# Task compliance
```{r compliance table, echo=FALSE}
#Compliance table
compliance_pf$renderPivot()
```

\newline
# [EEG](/`r paste0(tolower(params$id),"-eeg")`/)
```{r two, echo=FALSE}
#EEG table
kbl(eeg_col_named) %>%
  kable_styling(fixed_thead = T) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, bold = T) %>%
  column_spec(3, background=if_else(eeg_col_named$`Overall % good`>80, "#C6EFCE", "#ffc7ce")) %>%
  column_spec(4, color=if_else(eeg_col_named[4]>70, "#D8D8D8", "black")) %>%
  column_spec(5, color=if_else(eeg_col_named[5]>70, "#D8D8D8", "black")) %>%
  column_spec(6, color=if_else(eeg_col_named[6]>70, "#D8D8D8", "black")) %>%
  column_spec(7, color=if_else(eeg_col_named[7]>70, "#D8D8D8", "black"))
```

\newline
# [HR](/`r paste0(tolower(params$id),"-ecg")`/)
```{r hr perf, echo=FALSE}
#HR table
kbl(hr_ordered) %>%
  kable_styling(fixed_thead = T) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, bold = T) %>%
  column_spec(3, background=if_else(hr_ordered[3]>90, "#C6EFCE", "#ffc7ce"))
```

\newline
# Sleep diary
```{r diary table, echo=FALSE}
#Sleep diary table
#Will need to change the figure for sleep_di_avg when we figure out that threshold
kbl(diary) %>%
  kable_styling(fixed_thead = T) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, bold = T) %>%
  column_spec(3, color=if_else(output$sample_form_summary$sleep_di_avg > 50, "#D8D8D8", "black")) %>%
  column_spec(4, color=if_else(output$sample_form_summary$sleep_di_avg > 50, "#D8D8D8", "black")) %>%
  column_spec(5, color=if_else(output$sample_form_summary$sleep_di_avg > 50, "#D8D8D8", "black")) %>%
  column_spec(6, color=if_else(output$sample_form_summary$sleep_di_avg > 50, "#D8D8D8", "black"))
```

\newline
# Mood questionnaire
```{r mood table, echo=FALSE}
#Mood table
#Will need to change the figure for val_arr_dis_avg when we figure out that threshold
kbl(mood) %>%
  kable_styling(fixed_thead = T) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, bold = T) %>%
  column_spec(3, background=if_else(mood[3]==1, "#C6EFCE", "#ffc7ce")) %>%
  column_spec(4, color="#D8D8D8") %>%
  column_spec(5, color=if_else(output$sample_form_summary$val_arr_dis_avg > 50, "#D8D8D8", "black")) %>%
  column_spec(6, color=if_else(output$sample_form_summary$val_arr_dis_avg > 50, "#D8D8D8", "black")) %>%
  column_spec(7, color=if_else(output$sample_form_summary$val_arr_dis_avg > 50, "#D8D8D8", "black")) %>%
  column_spec(8, color=if_else(output$sample_form_summary$val_arr_dis_avg > 50, "#D8D8D8", "black")) %>%
  column_spec(9, color=if_else(output$sample_form_summary$val_arr_dis_avg > 50, "#D8D8D8", "black")) %>%
  column_spec(10, color=if_else(output$sample_form_summary$val_arr_dis_avg > 50, "#D8D8D8", "black"))
```