---
title: "Abbegail ECG"
output: html_document
type: "single"
categories: ["graphs", "Abbegail", "ECG"]
tags: ["Abbegail"]
params:
  block: 1
  id: "Abbegail"
---

```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = '../../../../..') # makes root dir rl_ema_monitoring
library(kableExtra)
library(dplyr)

```

## R Markdown

```{r load_physio, include=FALSE}
# need to change path to location of output_physio.Rdata in active directory
load("site/data/abb_proc_sched.Rdata") 
load("site/data/emp_physiooutput.Rdata")
```

```{r ECG, include=FALSE}
library(ggplot2)
library(tidyverse)
library(viridis)
# plot for individual subject iS out of a cached physio file
df <- output_physio$ecg$fb
df <- df[[params$id]]

a2f <- round(ncol(df)/6) # default times are -1000 to 10000 ms for ECG
y <- rep(seq(1,nrow(df),length.out=nrow(df)),1)

##temporary measure
output$proc_data[[params$id]]$raw_data$trials <- output$proc_data[[params$id]]$raw_data$trials[1:756,]
##
df <- df %>% mutate(trial=y, block=output$proc_data[[params$id]]$raw_data$trials$block) %>% filter(block==params$block) %>% select(-block) #add a trial column, then select only trials corresponding to the block designated by params$block

df <- df %>% pivot_longer(cols=starts_with("V"),names_to="t", values_to="V")
df <- df %>% mutate(t0=as.numeric(substr(t,2,nchar(t))))
df <- df %>% group_by(t0) %>% summarize(mean = mean(V, na.rm = TRUE), st_err=var(V,na.rm=TRUE)/sqrt(length(V)-sum(is.na(V))))
```


```{r plot, echo=FALSE}
ggplot(df, aes(x=t0,y=mean)) + geom_line() + geom_point() + geom_ribbon(aes(ymin = mean-st_err, ymax = mean+st_err), linetype=2, alpha=0.1) + scale_x_continuous(breaks=c(0,a2f,max(df$t0)),labels=c(-1000,0,100000),name='time [ms]') + geom_vline(xintercept = a2f, lty = "dashed", color = "#FF0000", size = 2)
```
