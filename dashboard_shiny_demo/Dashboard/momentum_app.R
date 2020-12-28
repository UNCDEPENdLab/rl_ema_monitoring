library("plyr")
library("dplyr")
library(ggplot2)
library(shiny)
library(shinydashboard)
library(reticulate)
library(rjson)
library(RSQLite)
library("zoo")

ui <- dashboardPage(
  dashboardHeader(title = "Momentum"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Data", tabName = "Data", icon = icon("calculator")),
      menuItem("Plots", tabName = "Plots", icon = icon("chart-line")),
      menuItem("Subjects", tabName = "Subjects", icon = icon("address-card")),
      menuItem("Admin", tabName = "Admin", icon = icon("user-tie")),
      menuItem("Financial", tabName = "Financial", icon = icon("comment-dollar")),
      menuItem("Help", tabName = "Help", icon = icon("lightbulb"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Home"),
      tabItem("Data"),
      tabItem("Plots",
        mainPanel(
          fluidRow(
            uiOutput("active")
          ),
          #("subjectMean1"),
          #uiOutput(),
          fluidRow(
            tabBox(id = "tabcharts1",
              tabPanel("Plot1", 
                #textOutput("overall_no_feedback_mean"),
                #textOutput("overall_with_feedback_mean"),
                #textOutput("id"),
                #dataTableOutput("table1"),
                #plotOutput("plot1")
                uiOutput("tab1out")
              ),
              tabPanel("Plot2", 
                #textOutput("overall_relative_no_feedback_mean"),
                #textOutput("overall_relative_with_feedback_mean"),
                #textOutput("id"),
                #dataTableOutput("table2"),
                #plotOutput("plot2")
                uiOutput("tab2out")
              ),
              tabPanel("Plot3", uiOutput("tab3out"))
            )
          )
        )
      ),
      tabItem("Subjects",
        mainPanel(
          fluidRow(
            tabBox(id="tabsubjs1",
              tabPanel("Add",
                uiOutput("tab4out")
              ),
              tabPanel("Status",
                uiOutput("tab5out")
              ),
              tabPanel("Pull",
                uiOutput("tab6out"),
                actionButton("pullBtn", "Pull Subject Data")
              )
            )
          )
        )
      ),
      tabItem("Admin"),
      tabItem("Financial"),
      tabItem("Help")
    )
  )
)

server <- function(input, output) {
  
  #output$subjectList <- renderTable({
  #  subjectList <- fromJSON(file = paste0(dirname(getwd()), "/subject_status.json"))$subjects$active
  #})
  
  activeSubjs <- reactive({
    subjects <- fromJSON(file = paste0(dirname(getwd()), "/subject_status.json"))
    subjects <- subjects$subjects
    active <- subjects$active
    return(active)
  })
  
  output$active = renderUI({
    sidebarPanel({
      selectInput("pid", "Select Participant ID:", choices = activeSubjs())
    })
  })
  
  output$tab1out <- renderUI({
    fluidRow(
      column(8, align="left",
        textOutput("overall_no_feedback_mean"),
        textOutput("overall_with_feedback_mean")
      ),
      column(8, align="center",
        plotOutput("plot1")
      )
    )
  })
  
  output$tab2out <- renderUI({
    fluidRow(
      column(8, align="left",
        textOutput("overall_relative_no_feedback_mean"),
        textOutput("overall_relative_with_feedback_mean")
      ),
      column(8, align="center",
        plotOutput("plot2")
      )
    )
  })
  
  output$tab3out <- renderUI({
    fluidRow(
      column(8, align="center",
        plotOutput("plot3")
      )
    )
  })
  
  output$tab4out <- renderUI({
    fluidRow(
      column(8, align="center",
        textInput("newID", "Enter the Subject ID"),
        textInput("newGmail", "Enter the Subject Gmail"),
        actionButton("addBtn", "Add Subject")
      )
    )
  })
  
  output$tab5out <- renderUI({
    fluidRow(
      column(8, align="center",
        hello2
      )
    )
  })
  
  output$tab6out <- renderUI({
    fluidRow(
      sidebarPanel({
        selectInput("pid2pull", "Select Participant ID:", choices = activeSubjs())
      })
    )
  })
  
  scheduleFile <- reactive({
    projdir <- dirname(getwd())
    schedPath <- paste0(projdir, "/Subjects/", input$pid, "/schedule/")
    currSched <- list.files(path=schedPath, pattern=".db")
    scheduleFile <- paste0(schedPath, currSched)
    scheduleFile
  })
  
  observeEvent(input$addBtn, {
    dashdir <- getwd()
    projdir <- dirname(dashdir)
    use_virtualenv(virtualenv = dashdir, required = TRUE)
    setwd(projdir)
    add_subject <- import("add_subject")$add_subject
    add_subject(id=input$newID, gmail=input$newGmail)
    #print(input$newID)
    #print(input$newGmail)
    setwd(dashdir)
  })
  
  observeEvent(input$pullBtn, {
    dashdir <- getwd()
    projdir <- dirname(dashdir)
    use_virtualenv(virtualenv = dashdir, required = TRUE)
    setwd(projdir)
    pull_files <- import("momentum_pull")$pull_files
    pull_files(id=input$pid2pull, path=projdir)
    #print(input$newID)
    #print(input$newGmail)
    setwd(dashdir)
  })
  
  trials_1 <- reactive({
    #setwd("C:/Users/tshan/Desktop/Momentum_R") #directory where the schedule file is located
    #"C:/Users/tshan/Desktop/Drive_wrkdir/Subjects/Shane/schedule/1605215538.635642_Shane_schedule.db"
    sched <- scheduleFile()
    data_1 = dbConnect(SQLite(), sched)
    stimuli_1 = dbGetQuery(data_1, "SELECT * FROM stimuli")
    trials_1 = dbGetQuery(data_1, "SELECT * FROM trials")
    dbDisconnect(data_1)
    ## remove blocks that have not been played yet 
    if (length(which(is.na(trials_1$choice)))!=0){
      trials_1=trials_1[-c(which(is.na(trials_1$choice))),]}
    ##accuracy analysis according to designated probabilities##
    #add objective expected value (EV) for each stimulus and objective accuracy for each trial
    for (i in 1:length(trials_1$block)){
      trials_1$EVStim1[i]=stimuli_1$reward[trials_1$stim1[i]+1]-stimuli_1$punishment[trials_1$stim1[i]+1]
      trials_1$EVStim2[i]=stimuli_1$reward[trials_1$stim2[i]+1]-stimuli_1$punishment[trials_1$stim2[i]+1]
      trials_1$accuracy[i]=((trials_1$EVStim1[i]>trials_1$EVStim2[i])&&(trials_1$choice[i]==0)||(trials_1$EVStim1[i]<trials_1$EVStim2[i])&&(trials_1$choice[i]==1))
      if (trials_1$EVStim1[i]==trials_1$EVStim2[i])
        trials_1$accuracy[i]=NA
    }
    trials_1
  })
  
  trials_2 <- reactive({
    # "C:/Users/tshan/Desktop/Drive_wrkdir/Subjects/Shane/schedule/1605215538.635642_Shane_schedule.db"
    sched <- scheduleFile()
    data_1 = dbConnect(SQLite(), sched)
    stimuli_1 = dbGetQuery(data_1, "SELECT * FROM stimuli")
    trials_1 = dbGetQuery(data_1, "SELECT * FROM trials")
    dbDisconnect(data_1)
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
    trials_1
  })
  
  objective_accuracy_by_block <- reactive({
    trials_1 <- trials_1()
    objective_accuracy_by_block=ddply(trials_1, .(block, feedback), summarize, mean=mean(accuracy, na.rm = T))
    objective_accuracy_by_block
  })
  
  overall_no_feedback_mean <- reactive({
    #source(paste0(wrkdir, "/spm.R"))
    objective_accuracy_by_block <- objective_accuracy_by_block()
    overall_no_feedback_mean=mean(objective_accuracy_by_block$mean[objective_accuracy_by_block$feedback==0], na.rm = T)*100
    overall_no_feedback_mean
  })
  
  overall_with_feedback_mean <- reactive({
    #source(paste0(wrkdir, "/Subject_performance_monitoring_modular.R"))
    objective_accuracy_by_block <- objective_accuracy_by_block()
    overall_with_feedback_mean=mean(objective_accuracy_by_block$mean[objective_accuracy_by_block$feedback==1], na.rm = T)*100
    overall_with_feedback_mean
  })
  
  relative_accuracy_by_block <- reactive({
    trials_1 <- trials_2()
    relative_accuracy_by_block=ddply(trials_1, .(block, feedback), summarize, mean=mean(relative_accuracy, na.rm = T))
    relative_accuracy_by_block
  })
  
  overall_relative_no_feedback_mean <- reactive({
    #source(paste0(wrkdir, "/spm.R"))
    relative_accuracy_by_block <- relative_accuracy_by_block()
    overall_relative_no_feedback_mean=mean(relative_accuracy_by_block$mean[relative_accuracy_by_block$feedback==0], na.rm = T)*100
    overall_relative_no_feedback_mean
  })
  
  overall_relative_with_feedback_mean <- reactive({
    #source(paste0(wrkdir, "/Subject_performance_monitoring_modular.R"))
    relative_accuracy_by_block <- relative_accuracy_by_block()
    overall_relative_with_feedback_mean=mean(relative_accuracy_by_block$mean[relative_accuracy_by_block$feedback==1], na.rm = T)*100
    overall_relative_with_feedback_mean
  })
  
  output$overall_no_feedback_mean <- renderText({
    paste("Mean Accuracy (without feedback):", overall_no_feedback_mean())
  })
  
  output$overall_with_feedback_mean <- renderText({
    paste("Mean Accuracy (with feedback):", overall_with_feedback_mean())
  })
  
  output$table1 <- renderTable({
    req(objective_accuracy_by_block())
    objective_accuracy_by_block()
  })
  
  output$overall_relative_no_feedback_mean <- renderText({
    paste("Mean Accuracy (without feedback):", overall_relative_no_feedback_mean())
  })
  
  output$overall_relative_with_feedback_mean <- renderText({
    paste("Mean Accuracy (with feedback):", overall_relative_with_feedback_mean())
  })
  
  output$table2 <- renderTable({
    req(relative_accuracy_by_block())
    relative_accuracy_by_block()
  })
  
  output$plot1 <- renderPlot({
    objective_accuracy_by_block <- objective_accuracy_by_block()
    objective_accuracy_by_block$feedback <- factor(objective_accuracy_by_block$feedback)
    overall_with_feedback_mean <- overall_with_feedback_mean()
    overall_no_feedback_mean <- overall_no_feedback_mean()
    ggplot(data=objective_accuracy_by_block, aes(x=block, y=mean*100, group=feedback, colour=feedback)) +
      geom_line() +
      geom_point(aes(shape = feedback), size=2)+ 
      geom_hline(yintercept=overall_with_feedback_mean(), linetype="dashed", color = "cyan4")+
      geom_hline(yintercept=overall_no_feedback_mean(), linetype="dashed", color = "firebrick2")+
      ylab("% correct choice")+
      ggtitle("Accuracy according to designated probabilities")+
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(0,overall_with_feedback_mean,label =round(overall_with_feedback_mean, digits = 1), vjust = -0.4), size = 2.8, color="cyan4")+
      geom_text(aes(0,overall_no_feedback_mean,label =round(overall_no_feedback_mean, digits = 1), vjust = -0.4), size = 2.8, color="firebrick2")
  } , height=400, width=500)

  output$plot2 <- renderPlot({
    relative_accuracy_by_block <- relative_accuracy_by_block()
    relative_accuracy_by_block$feedback <- factor(relative_accuracy_by_block$feedback)
    overall_relative_with_feedback_mean <- overall_relative_with_feedback_mean()
    overall_relative_no_feedback_mean <- overall_relative_no_feedback_mean()
    ggplot(data=relative_accuracy_by_block, aes(x=block, y=mean*100, group=feedback, colour=feedback)) +
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
  } , height=400, width=500)
  
  output$plot3 <- renderPlot({
    trials_1 <- trials_1()
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
  } , height=400, width=500)
  
}

shinyApp(ui, server)