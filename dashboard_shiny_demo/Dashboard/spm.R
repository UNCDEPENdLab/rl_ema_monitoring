rm(list = ls())
graphics.off()
library("plyr")
library("dplyr")
library("ggplot2")
library("zoo")

#By block
objective_accuracy_by_block=ddply(trials_1, .(block, feedback), summarize, mean=mean(accuracy, na.rm = T))
#Overall
overall_with_feedback_mean=mean(objective_accuracy_by_block$mean[objective_accuracy_by_block$feedback==1], na.rm = T)*100
overall_no_feedback_mean=mean(objective_accuracy_by_block$mean[objective_accuracy_by_block$feedback==0], na.rm = T)*100
objective_accuracy_by_block$feedback=as.factor(objective_accuracy_by_block$feedback)
figure_1 <- ggplot(data=objective_accuracy_by_block, aes(x=block, y=mean*100, group=feedback, colour=feedback)) +
            geom_line() +
            geom_point(aes(shape = feedback), size=2)+
            geom_hline(yintercept=overall_with_feedback_mean, linetype="dashed", color = "cyan4")+
            geom_hline(yintercept=overall_no_feedback_mean, linetype="dashed", color = "firebrick2")+
            ylab("% correct choice")+
            ggtitle("Accuracy according to designated probabilities")+
            theme(plot.title = element_text(hjust = 0.5))+
            scale_x_continuous(breaks = seq(0, max(objective_accuracy_by_block$block),by = 1))+
            geom_text(aes(0,overall_with_feedback_mean,label =round(overall_with_feedback_mean, digits = 1), vjust = -0.4), size = 2.8, color="cyan4")+
            geom_text(aes(0,overall_no_feedback_mean,label =round(overall_no_feedback_mean, digits = 1), vjust = -0.4), size = 2.8, color="firebrick2")

##Accuracy relative to probabilities that were experienced (constantly updating until an image switches to its no-feedback phase)##

trials_1$relative_stim1=rep(NaN, nrow(trials_1))
trials_1$relative_stim2=rep(NaN, nrow(trials_1))
for (i in 2:nrow(trials_1)){
   trials_1$relative_stim1[i]=mean(trials_1$outcome[which(trials_1$stim1==trials_1$stim1[i]&trials_1$choice==0&trials_1$feedback==1&(trials_1$trial<trials_1$trial[i]|trials_1$block<trials_1$block[i])|trials_1$stim2==trials_1$stim1[i]&trials_1$choice==1&trials_1$feedback==1&(trials_1$trial<trials_1$trial[i]|trials_1$block<trials_1$block[i]))]) 
   trials_1$relative_stim2[i]=mean(trials_1$outcome[which(trials_1$stim1==trials_1$stim2[i]&trials_1$choice==0&trials_1$feedback==1&(trials_1$trial<trials_1$trial[i]|trials_1$block<trials_1$block[i])|trials_1$stim2==trials_1$stim2[i]&trials_1$choice==1&trials_1$feedback==1&(trials_1$trial<trials_1$trial[i]|trials_1$block<trials_1$block[i]))])  
}
trials_1$relative_accuracy=NA
index=which(!is.nan(trials_1$relative_stim1)&!is.nan(trials_1$relative_stim2))

#accuracy according to experienced probabilities (differences of less than 10% between probabilties are omitted)
for (i in index){
    trials_1$relative_accuracy[i]=((trials_1$relative_stim1[i]>trials_1$relative_stim2[i]+0.1)&&(trials_1$choice[i]==0)||(trials_1$relative_stim1[i]+0.1<trials_1$relative_stim2[i])&&(trials_1$choice[i]==1))
  if (((abs(trials_1$relative_stim1[i]-trials_1$relative_stim2[i])<0.1)&&(abs(trials_1$relative_stim2[i]-trials_1$relative_stim1[i]))<0.1))
    trials_1$relative_accuracy[i]=NA
}

relative_accuracy_by_block=ddply(trials_1, .(block, feedback), summarize, mean=mean(relative_accuracy, na.rm = T))
overall_relative_with_feedback_mean=mean(relative_accuracy_by_block$mean[relative_accuracy_by_block$feedback==1], na.rm = T)*100
overall_relative_no_feedback_mean=mean(relative_accuracy_by_block$mean[relative_accuracy_by_block$feedback==0], na.rm = T)*100
relative_accuracy_by_block$feedback=as.factor(relative_accuracy_by_block$feedback)

figure_2 <- ggplot(data=relative_accuracy_by_block, aes(x=block, y=mean*100, group=feedback, colour=feedback)) +
  geom_line() +
  geom_point(aes(shape = feedback), size=2)+
  geom_hline(yintercept=overall_relative_with_feedback_mean, linetype="dashed", color = "cyan4")+
  geom_hline(yintercept=overall_relative_no_feedback_mean, linetype="dashed", color = "firebrick2")+
  ylab("% correct choice")+
  ggtitle("Accuracy according to experienced probabilities")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(0, max(relative_accuracy_by_block$block),by = 1))+
  geom_text(aes(0,overall_relative_with_feedback_mean,label =round(overall_relative_with_feedback_mean, digits = 1), vjust = -0.4), size = 2.8, color="cyan4")+
  geom_text(aes(0,overall_relative_no_feedback_mean,label =round(overall_relative_no_feedback_mean, digits = 1), vjust = -0.4), size = 2.8, color="firebrick2")

#learning trend: a sliding window in a size of 10 trials which calculates subject's accuracy according to designated probabilities (starts being calculated only from block 6, after the practice session)
test_learning=subset(trials_1, (((trials_1$block>5)&trials_1$feedback==1)))
test_learning=test_learning[-c(3:13)]
test_learning=test_learning[-c(4:8)]
if (nrow(test_learning)!=0){
learning_matrix=data.frame(block=rep(c(6:max(test_learning$block)), each=39), window=rep(c(1:39), max(test_learning$block)-5), accuracy=rep(NA, 39*(max(test_learning$block)-5)))
t=1
for (i in seq(from=1, to=which(learning_matrix==max(learning_matrix$block))[1], by=39)){
  learning_matrix[i:(i+38),3]=rollapply(test_learning$accuracy[t:(t+47)], width = 10, by = 1, FUN = mean, align = "left")
  t=t+48
  }

accuracy_by_window=ddply(learning_matrix, c("window"),summarise,mean=mean(accuracy, na.rm = T))

figure_3 <- ggplot(data=accuracy_by_window, aes(x=window, y=mean*100)) +
  geom_line(color="skyblue3") +
  ylab("% correct choice")+
  xlab("window")+
  ggtitle("learning process")

figure_3
}

figure_1
figure_2

