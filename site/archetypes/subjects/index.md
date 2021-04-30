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
wd <- getwd()
source("EEG_Dashboard.R")
source("ECG_Dashboard.R")
source("dashboard/study_management/data_management_functions.R")
source("dashboard/study_management/dashboard_aggregate.R")
```

```{r one, include=FALSE}
#commented out for now, since get_ema is acting weird (the 123 issue)
#path_info <- get_ema_subject_metadata(subject_list = "Abbegail") 

#Commenting this out too since it takes forever to do output_physio, and reading in Jiazhou's outputs instead
#using this instead:
 root_dir <- "./data/Subjects"
   path_info<-lapply(c("schedule","physio","video"),function(dj){
     list_id <- list.dirs(path = root_dir,recursive = F,full.names = F)
     if(dj == "video") {
       pat = ".*.mp4"
     } else {
       pat = ".db"
     }
     do.call(rbind,lapply(list_id,function(idx){
       db_files<-list.files(file.path(root_dir,idx,dj),pattern = pat,full.names = T,recursive = F,include.dirs = F)
       if(length(db_files)>0){
         dx<-data.frame(subject_id=idx,file_path=db_files,stringsAsFactors = F)
         return(dx)
       } else {
         return(NULL)
       }
     }))
   })
   names(path_info) <- c("schedule","physio","video")
 
 output <- proc_schedule(schedule_df = path_info$schedule)
```

```{r performance, include=FALSE}

#performance breakdown

perf <- output$subj_performance 
df1 <- filter(perf,!is.na(date)) #get rid of blocks not played
df1$date <- lapply(df1$date,function(dt){
  year <- str_extract(dt,"\\d{4}")
  month <- str_extract(dt,"-\\d{2}-") %>% str_extract("\\d{2}")
  day <- str_extract(dt,"\\d{2}$")
  return(paste0(month,"/",day,"/",year))
})

#Prob want to add this to the proc_schedule function
#A little reformatting...

df2 <- arrange(df1, -row_number()) #flip df so that most recent dates are first
df3 <- transmute(df2, "Date"=date, "Block"=block, "Objective % correct (feed.)"=abs_accurate_feed, "Experienced % correct (feed.)"=relative_accuracy_feed, "Objective % correct (no feed.)"=abs_accurate_nofeed, "Experienced % correct (no feed.)"=relative_accuracy_nofeed, "RT(ms)"=mean_rt, "Left %"=side_bias) #rename columns to be more descriptive
#convert decimals to percentages
df3$"Objective % correct (feed.)" <- df3$"Objective % correct (feed.)"*100
df3$"Experienced % correct (feed.)" <- df3$"Experienced % correct (feed.)"*100
df3$"Experienced % correct (no feed.)" <- df3$"Experienced % correct (no feed.)"*100
df3$"Objective % correct (no feed.)" <- df3$"Objective % correct (no feed.)"*100
df3$"Left %" <- df3$"Left %"*100
df3$"RT(ms)" <- df3$"RT(ms)"*1000
df3[,-c(1,2)] <- round(df3[,-c(1,2)]) #round percentages to nearest whole percent

rownames(df3) <- NULL
```



```{r eeg, include=FALSE}
#Commenting this out too since it takes forever to do output_physio, and reading in Jiazhou's outputs instead
# output_physio <- proc_physio(physio_df = path_info$physio,sch_pro_output=output, tz="EST",
#                                eeg_sample_rate=256.03, sd_times=10, eeg_pre=500,eeg_post=1500, #EEG options
#                                ecg_sample_rate = 100, HRstep = 10, ecg_pre=1000,ecg_post=10000 #ECG options
#                                )

load("site/data/emp_physiooutput.RData")
eeg <- output_physio$eeg$summary[[params$id]]
abb_info <- output$subj_performance %>% select(block,date)

eeg_dates <- left_join(eeg,abb_info,by="block")

eeg_avg <- mutate(eeg_dates,"avg"=(per_Ch_1 + per_Ch_2 + per_Ch_3 +per_Ch_4)/4)
eeg_row_ordered <- arrange(eeg_avg, -row_number())

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

#convert date to more readable format
eeg_row_ordered$date <- lapply(eeg_row_ordered$date,function(dt){
  year <- str_extract(dt,"\\d{4}")
  month <- str_extract(dt,"-\\d{2}-") %>% str_extract("\\d{2}")
  day <- str_extract(dt,"\\d{2}$")
  return(paste0(month,"/",day,"/",year))
})

eeg_col_named <- transmute(eeg_row_ordered, "Date"=date, "Block"=block, "Overall % good"=avg, "Ch. 1 % good"=per_Ch_1, "Ch. 2 % good"=per_Ch_2, "Ch. 3 % good"=per_Ch_3, "Ch. 4 % good"=per_Ch_4) 
```
 
```{r hr, include=FALSE}
hr <- output_physio$ecg$summary[[params$id]]

hr_dates <- left_join(hr,abb_info,by="block")


hr_ordered <- arrange(hr_dates, -row_number())

#convert all to percentages, rounded
hr_ordered$per_Good <- hr_ordered$per_Good*100
hr_ordered$per_Good <- round(hr_ordered$per_Good)

#convert date to more readable format
hr_ordered$date <- lapply(hr_ordered$date,function(dt){
  year <- str_extract(dt,"\\d{4}")
  month <- str_extract(dt,"-\\d{2}-") %>% str_extract("\\d{2}")
  day <- str_extract(dt,"\\d{2}$")
  return(paste0(month,"/",day,"/",year))
})

hr_ordered <- transmute(hr_ordered, "Date"=date, "Block"=block, "Good signal %"=per_Good) 
```

```{r compliance, include=FALSE}
compliance <-output$subj_info$`params$id`[c("scheduled_time","delay","type", "session_number")]
compliance$scheduled_time <- as.Date(compliance$scheduled_time)
compliance$delay <- round(as.numeric(compliance$delay), digits = 0)
compliance$s_type[compliance$type=="trials"] <- "Behavioral Game"
compliance$s_type[compliance$type=="questionnaires"] <- sapply(output$subj_info$`Abbegail`$spec[compliance$type=="questionnaires"],`[[`,3)
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
\newline
# Task performance

```{r perf table, echo=FALSE}
kbl(df3) %>%
  kable_styling(fixed_thead = T) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, bold = T) %>%
  column_spec(3, background=case_when(df3[,3]>80 ~ "green",df3[,3]>70 & df3[,3]<=80 ~ "yellow", df3[,3]<70 ~ "red")) %>%
  column_spec(4, background=case_when(df3[,4]>80 ~ "green",df3[,4]>70 & df3[,4]<=80 ~ "yellow", df3[,4]<70 ~ "red")) %>%
  column_spec(5, color=ifelse(df3[,5] > 55 | is.na(df3[,5]), "#D8D8D8", "black")) %>%
  column_spec(6, color=ifelse(df3[,6] > 55 | is.na(df3[,6]), "#D8D8D8","black")) %>%
  column_spec(7, color=ifelse(df3[,7] > 1400, "#E6E6E6","black")) %>%
  column_spec(8, color=ifelse(df3[,8] >= 67 | df3[,8] < 67, "#D8D8D8","black"))
```

\newline
\newline
# Task compliance
```{r compliance table, echo=FALSE}
compliance_pf$renderPivot()
```

\newline
\newline
# EEG
```{r two, echo=FALSE}
kbl(eeg_col_named) %>%
  kable_styling(fixed_thead = T) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, bold = T) %>%
  column_spec(3, background=if_else(eeg_col_named$`Overall % good`>80, "green", "red")) %>%
  column_spec(4, color=if_else(eeg_col_named[4]>70, "#D8D8D8", "black")) %>%
  column_spec(5, color=if_else(eeg_col_named[5]>70, "#D8D8D8", "black")) %>%
  column_spec(6, color=if_else(eeg_col_named[6]>70, "#D8D8D8", "black")) %>%
  column_spec(7, color=if_else(eeg_col_named[7]>70, "#D8D8D8", "black"))
```

\newline
\newline
# HR
```{r hr perf, echo=FALSE}
kbl(hr_ordered) %>%
  kable_styling(fixed_thead = T) %>%
  column_spec(1, bold = T) %>%
  column_spec(2, bold = T) %>%
  column_spec(3, background=if_else(hr_ordered[3]>90, "green", "red"))
```