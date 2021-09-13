library(shiny)
library(shinythemes)
library(rjson)
library(RSQLite)

wrkdir <- getwd()
projdir <- dirname(wrkdir)
subjects <- fromJSON(file = paste0(projdir, "/subject_status.json"))
active <- subjects$subjects$active
inactive <- subjects$subjects$inactive

ui <- fluidPage(theme = shinytheme("darkly"),
    titlePanel("Momentum Dashboard"),
    sidebarLayout(
        sidebarPanel(
            selectInput("pid", "Select Participant ID:", choices = active)
        ),
        mainPanel(
           textOutput("mean1"),
           textOutput("mean2"),
           textOutput("id"),
           dataTableOutput("table"),
           plotOutput("plot")
        )
    )
)

server <- function(input, output, session) {
    trial_1 <- reactive({
        subjects <- fromJSON(file = paste0(projdir, "/subject_status.json"))
        active <- subjects$subjects$active
        inactive <- subjects$subjects$inactive
        #setwd("C:/Users/tshan/Desktop/Momentum_R") #directory where the schedule file is located
        #paste0(projdir, "/Subjects/", input$pid, "/schedule/", "1605215538.635642", "_", input$pid, "_schedule.db")
        data_1 = dbConnect(SQLite(), "C:/Users/tshan/Desktop/Drive_wrkdir/Subjects/Shane/schedule/1605215538.635642_Shane_schedule.db")
        stimuli_1 = dbGetQuery(data_1, "SELECT * FROM stimuli")
        trials_1 = dbGetQuery(data_1, "SELECT * FROM trials")
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
        #assign(trials_1, trials_1, envir=globalenv())
        #source(paste0(wrkdir, "/spm.R"))
    })
    
    output$id <- renderText({
        paste("Participant ID:", input$pid)
        #return (input$pid)
    })
    
    mean1 <- reactive({
        #source(paste0(wrkdir, "/spm.R"))
        overall_no_feedback_mean
    })
    
    mean2 <- reactive({
        #source(paste0(wrkdir, "/Subject_performance_monitoring_modular.R"))
        overall_with_feedback_mean
    })
    
    objacc <- reactive({
        #source(paste0(wrkdir, "/Subject_performance_monitoring_modular.R"))
        objective_accuracy_by_block
    })
    
    output$mean1 <- renderText({
        paste("Mean Accuracy (without feedback):", mean1())
    })
    
    output$mean2 <- renderText({
        paste("Mean Accuracy (with feedback):", mean2())
    })
    
    output$table <- renderTable({
        req(objacc())
        objacc()
        
    })
    
    output$plot <- renderPlot({
        req(objacc())
        ggplot(data=objacc(), aes(x=block, y=mean*100, group=feedback, colour=feedback)) +
            geom_line() +
            geom_point(aes(shape = feedback), size=2)+
            geom_hline(yintercept=overall_with_feedback_mean, linetype="dashed", color = "cyan4")+
            geom_hline(yintercept=overall_no_feedback_mean, linetype="dashed", color = "firebrick2")+
            ylab("% correct choice")+
            ggtitle("Accuracy according to designated probabilities")+
            theme(plot.title = element_text(hjust = 0.5))+
            geom_text(aes(0,overall_with_feedback_mean,label =round(overall_with_feedback_mean, digits = 1), vjust = -0.4), size = 2.8, color="cyan4")+
            geom_text(aes(0,overall_no_feedback_mean,label =round(overall_no_feedback_mean, digits = 1), vjust = -0.4), size = 2.8, color="firebrick2")
    }, height=400, width=500)
    
}

shinyApp(ui = ui, server = server)
