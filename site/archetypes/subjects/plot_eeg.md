---
title: "{{ replace .Name "-" " " | title }} EEG"
output: html_document
aliases:
  - /{{ lower ( replace .Name "-" " " | title ) }}-eeg/
  - /{{ lower ( replace .Name "-" " " | title ) }}/eeg/
categories: ["graphs", "{{ replace .Name "-" " " | title }}", "EEG"]
tags: ["{{ replace .Name "-" " " | title }}"]
params:
  block: 1
  id: "{{ replace .Name "-" " " | title }}"
---

```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = '../../../..') # makes root dir rl_ema_monitoring
library(kableExtra)
library(dplyr)
library(plotly)
```

```{r load_physio, include=FALSE}
# need to change path to location of Rdata objects in active directory
load("site/data/abb_proc_sched.Rdata") 
load("site/data/emp_physiooutput.Rdata")
```

```{r EEG, include=FALSE}
library(ggplot2)
library(tidyverse)
library(viridis)

# plot for individual subject iS out of a cached physio file
df <- output_physio$eeg$fb
df <- df[[params$id]]
a2f <- round(ncol(df$ch1)/4) # default times are -500 to 1500 ms for EEG
y <- rep(seq(1,nrow(df$ch1),length.out=nrow(df$ch1)),4)
name <- names(df)
dq <- rbind(df[[name[1]]],df[[name[[2]]]],df[[name[3]]],df[[name[4]]]) # concatenate channels 1-4
##temporary code for truncating the schedule output so that it matches the size of the physio output
output$proc_data[[params$id]]$raw_data$trials <- output$proc_data[[params$id]]$raw_data$trials[1:756,]
##
dq <- dq %>% mutate(trial=y, block=rep(output$proc_data[[params$id]]$raw_data$trials$block,4)) %>% filter(block==params$block) %>% select(-block) #add a trial column, then select only trials corresponding to the block designated by params$block
nT = nrow(dq)/4
dq <- dq %>% mutate(channel=case_when(
  as.numeric(rownames(dq)) <= nT ~ 1,
  as.numeric(rownames(dq)) <= nT*2 & as.numeric(rownames(dq))  > nT ~ 2,
  as.numeric(rownames(dq)) <= nT*3 & as.numeric(rownames(dq)) > nT*2  ~ 3,
  as.numeric(rownames(dq)) > nT*3  ~ 4
))
dq <- dq %>% pivot_longer(cols=starts_with("V"),names_to="t", values_to="V")
dq <- dq %>% mutate(t0=as.numeric(substr(t,2,nchar(t))))
dq <- dq %>% mutate(zscore=(V-mean(V,na.rm=TRUE))/sd(V,na.rm=TRUE))
dq <- dq %>% filter(abs(zscore) <= 5)
```

```{r plot, echo=FALSE}
eeg_plot <- ggplot(dq, aes(t0,trial,fill=zscore)) + geom_tile() + facet_wrap(~channel) + scale_x_continuous(breaks=c(0,a2f,max(dq$t0)),labels=c(-500,0,1500),name='time [ms]') +
  geom_vline(xintercept = a2f, lty = "dashed", color = "#FF0000", size = 2) + 
  scale_fill_viridis(option = "plasma",begin=0,end=1)
```

```{r show, echo=FALSE}
ggplotly(eeg_plot)
```