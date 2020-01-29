################## version2
library(shiny)
library(dplyr)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(lubridate)
library(tidyr)
library(shinyjs)
library(magrittr)
library(data.table)

#Define UI for data upload app
ui <- dashboardPage(
    
    #App title
    dashboardHeader(title = "Treatment and Care Cascade",
                    titleWidth = 300),
    
    #Sidebar contents (tabs)
    dashboardSidebar(width = 170,
        sidebarMenu(
            menuItem("Intro",tabName = "intro",icon=icon("book-open")),
            menuItem("Code", tabName = "code", icon = icon("code")),
            menuItem("HIV Cascade", tabName = "hiv", icon=icon("layer-group")),
            menuItem("HCV Cascade", tabName = "hcv", icon=icon("signal"))
        )),
    
    
    
    #Main panel for displaying outputs
    dashboardBody(
        # Also add some custom CSS to make the title background area the same
        # color as the rest of the header.
        tags$head(tags$style(HTML('.skin-blue .main-header .logo {
          background-color: #3c8dbc;}
        .skin-blue .main-header .logo:hover {background-color: #3c8dbc;
        }
      '))),
        #organizing what will go in the different tabs
        tabItems(
          #First tab "intro" and connecting it to the readME file in the directory
            tabItem(tabName = "intro", includeMarkdown("readMe.Rmd")),
            #Second tab "code" and connecting it to this code file
            tabItem(tabName = "code", pre(includeText("app.R"))),
            #third tab content "HIV" and organizing the different sections that will go in
            tabItem(tabName = "hiv",
                    h2("HIV Treatment and Care Cascade"),
                    fluidRow(
                        #making a box for all the inputs to divide the tab page into two sections one for inputs
                        #and one for the outputs 
                        box(fluidRow(
                          #Input: Select a file for hcv data
                            box(fileInput("dt_hiv","Choose CSV File",
                                          multiple = TRUE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),width = 12,solidHeader = TRUE, height = 75),
                            
                            
                            #Input: Checkbox if file has header and if user is uploading multiple files
                            box(checkboxInput("hivheader","Header",TRUE),
                                checkboxInput("multiplehiv", "Uploading multiple files",FALSE),width = 3,solidHeader = TRUE, height = 100),
                            
                            
                            #Input: Select separator
                            box(radioButtons("hivsep","Separator",
                                             choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                                             selected = ","),width = 3,solidHeader = TRUE, height = 120),
                            
                            #Input: Select quotes
                            box(radioButtons("hivquote","Quote",
                                             choices = c(None = "","Double Quote" = '"',"Single Quote" = "'"),
                                             selected = '"'), width = 3 ,solidHeader = TRUE, height = 120),
                            
                            #Input: PLHIV - checkbox and numeric input
                            #using shinyjs to make the numeric input visible or not, depending on the checkboxinput plhivb
                            #of whether PLHIV is known or not
                            useShinyjs(),
                            box(checkboxInput("plhivb", "PLHIV",FALSE), width = 3, solidHeader = TRUE, height = 40),
                            #uiOutput is for numeric input which will be defined in the server, since it depends on the 
                            #input from the checkboxInput plhivb
                            box(uiOutput(outputId = "numinput"), width = 3, solidHeader = TRUE, height = 75)
                            ), width = 12, height = 255),
                        
                        #for the Cascade table output (it is a uiOutput because it will depend on the multiplefile input, if 
                        #multiple files are uploaded then mulptile tabs according to the number of files will be created in addition to
                        #the main one with the compiled data, otherwise it will only be one main tab)
                        uiOutput("tabshivcascade"),
                        #for the Cascade-duration table output
                        uiOutput("tabshivcascadedur"),
                        #for the Cascade plot output
                        uiOutput("tabshivcascadeplot")
                    )),
            ##fourth tab content "HCV" and organizing the different sections that will go in
            tabItem(tabName = "hcv",
                    h2("HCV Treatment and Care Cascade"),
                    fluidRow(
                      #making a box for all the inputs to divide the tab page into two sections one for inputs
                      #and one for the outputs
                        box(fluidRow(
                          #Input: Select a file for hcv data
                          box(fileInput("dt_hcv","Choose CSV File",
                                                   multiple = TRUE,
                                                   accept = c("text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),width = 12,solidHeader = TRUE, height = 75),
                                     
                                     
                                     #Input: Checkbox if file has header and if user is uploading multiple files 
                                     box(checkboxInput("hcvheader","Header",TRUE),
                                         checkboxInput("multiplehcv", "Uploading multiple files",FALSE), width = 4,solidHeader=TRUE, height = 50),
                                     
                                     #Input: Select separator
                                     box(radioButtons("hcvsep","Separator",
                                                      choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                                                      selected = ","), width = 4,solidHeader=TRUE, height = 120),
                                     
                                     #Input: Select quotes
                                     box(radioButtons("hcvquote","Quote",
                                                      choices = c(None = "","Double Quote" = '"',"Single Quote" = "'"),
                                                      selected = '"'), width = 4,solidHeader=TRUE, height = 120),
                                     
                                     #Input: Ab test, Ab positive, RNA test
                                     useShinyjs(),
                                     box(checkboxInput("abtestsb","AB tests",FALSE),width = 4,solidHeader = TRUE,height = 40),
                                     box(checkboxInput("abposb","AB positive tests",FALSE),width = 4,solidHeader = TRUE,height = 40),
                                     box(checkboxInput("rnatestsb","RNA test",FALSE),width = 4,solidHeader = TRUE,height = 40),
                                     box(uiOutput(outputId = "abtestinput"),width = 4,solidHeader = TRUE, height = 75),
                                     box(uiOutput(outputId = "abposinput"),width = 4,solidHeader = TRUE, height = 75),
                                     box(uiOutput(outputId = "rnatestinput"),width = 4,solidHeader = TRUE, height = 75)), width = 12, height = 390),
                        
                        
                        
                        #for the Cascade table 
                        uiOutput("tabshcvcascade"),
                        #for the Cascade-duration table
                        uiOutput("tabshcvcascadedur"),
                        #for the Cascade plot - retaintion in care
                        uiOutput("tabshcvcascaderetained"),
                        #for the Cascade plot - re-infection
                        uiOutput("tabshcvcascadereinf")
                        ))  
        )))

####################################
#a function to calculate duration between 2 dates in weeks

durfunct <- function(date1, date2, units = "weeks", floor = TRUE) 
{
    calc.dur = interval(date1, date2) / duration(num = 1, units = units)
    if (floor) return(as.integer(floor(calc.dur)))
    return(calc.dur)
}

#########################
#Defining the server
server <- function(input, output){
    
  
  #to make the numeric input for the plhiv visible when plhiv is checked and invisible when it is unchecked
    plhivstatus <- c(1,2)
    numinput2 <- paste0("input", plhivstatus)
    
    
    output$numinput <- renderUI({
        
        num_inputs <- lapply(1:2, function(i){
            numericInput(inputId = numinput2[i], label = 'Number of PLHIV', 80, min = 1, max = 10000)})
        
        #this creates two numeric input one with the id: input1 and one with the id: input2
        #both are hidden
        shinyjs::hidden(num_inputs)
    })
    
    observe({
        for (i in plhivstatus) { #since input$plhivb is either 0 (unchecked) or 1 (checked), if plhiv is checked that means
          #that if statement is true and it will show input1 numeric input, but if plhiv is unchecked then the if statement
          #is false and it won't show any numeric input
            if (i %in% input$plhivb) {
                shinyjs::show(id = paste0("input", i))
            } else {
                shinyjs::hide(id = paste0("input", i))
            }  
        }
    })
    
    
    # reading the files,if multiple files are uploaded the rbindlist compiles them in one dataset
    dthiv <- reactive({req(input$dt_hiv)
        rbindlist(lapply(input$dt_hiv$datapath, fread, header = input$hivheader, quote = input$hivquote, sep = input$hivsep),
                  use.names = TRUE, fill = TRUE)
    })
    
   
    n_files_hiv <- reactive({length(input$dt_hiv$datapath)})
    #results in a list of the different datasets uploaded
    dt_files_hiv <- reactive({lapply(input$dt_hiv$datapath[1:n_files_hiv()],read.csv)})
    
   
     ####HIV Cascade Table
    
    #the output for the box which will have the cascade table
    #so if multiple files are uploaded (ie multiplehiv is checked ==1), tabs will be created for each file 
    #in additional to the main one tab1, otherwise it will only be tab1
    output$tabshivcascade <- renderUI({
        
        if (input$multiplehiv == 1) {
            str <- "tabBox(id = 'hivcasbox',
                   tabPanel(id = 'taball', title = 'HIV Cascade' ,tableOutput('hivcascade')),"
            for (i in 1:n_files_hiv()) {str <- paste0(str, "tabPanel(id = paste('tab', ",i,") , title = paste('Data', ",i,") , tableOutput('hivcascader_",i,"')),")}
            str <- gsub(",$",")",str)
            eval(parse(text = str)) 
        }
        else {
            tabBox(id = "hivcasbox", 
                   tabPanel(id = "tab1", title = "HIV Cascade",tableOutput("hivcascade")))
        }
    })
    
    
    ###the code chunk for the analysis - aggregating the individual-level data for tab1 - this is going to show 
    #whether multiple files are uploaded or only 1. It will use the compiled dataset dthiv
    cascade_hiv <- reactive({dthiv() %>% summarize("Diagnosed" = sum(hiv_posresult,na.rm = T),
                                                   "Linkage to care" = sum(linkagetocare_hiv,na.rm = T),
                                                   "ART initiation" = sum(art_ini,na.rm = T),
                                                   "VL suppression" = sum(vl_sup,na.rm = T),
                                                   "Retained in care" = sum(retained_care_hiv,na.rm = T))})
    #transposing it and making sure it is a dataframe
    cascade_hiv1 <- reactive({as.data.frame(t(cascade_hiv()))})
    #these objects will be used if plhiv is known, setting the plhiv as 100%
    cascade_hiv2p <- reactive({rbind(input$input1, cascade_hiv1())})
    Percentagep <- reactive({(round((cascade_hiv2p()$V1*100/cascade_hiv2p()$V1[1]),1))})
    cascade_hiv3p <- reactive({cbind(cascade_hiv2p(),Percentagep())})
    cascade_hiv4p <- reactive({cascade_hiv3p() %>% rename(Total = V1, Percentage = "Percentagep()")})
    
    #these objects will be used if plhiv is unknown, setting the diagnosed (V1) as 100%
    Percentage <- reactive({(round((cascade_hiv1()$V1*100/cascade_hiv1()$V1[1]),1))}) 
    cascade_hiv3 <- reactive({cbind(cascade_hiv1(),Percentage())})
    cascade_hiv4 <- reactive({cascade_hiv3() %>% rename(Total = V1, Percentage = "Percentage()")})
    
    
    #the table output for main overall tab1 - using the compiled dataset/ the only uploaded dataset
    output$hivcascade <- renderTable({
        if (input$plhivb == 1) {
            cascade_hiv5 <- as.data.frame(cascade_hiv4p())
            rownames(cascade_hiv5) <- c("PLHIV","Diagnosed","Linkage to care","ART initiation","VL suppression","Retained in care")
        } else { 
            cascade_hiv5 <- as.data.frame(cascade_hiv4())
            rownames(cascade_hiv5) <- c("Diagnosed","Linkage to care","ART initiation","VL suppression","Retained in care")        
        }
        
        cascade_hiv5},include.rownames = TRUE)
    
    #the code for the table for each tab if multiple files are uploaded
    #I used str and eval(parse) because due to  the dependence on the unknown n_files I couldn't loop it and due to the reactivity
    #each object  need to have a separate name  
    observe({
        for (i in 1:n_files_hiv())
        {str <- paste0("dthiv_r_",i,"<- reactive({dt_files_hiv()[[",i,"]] %>% summarize('Diagnosed' = sum(hiv_posresult,na.rm = T),
                                                   'Linkage to care' = sum(linkagetocare_hiv,na.rm = T),
                                                   'ART initiation' = sum(art_ini,na.rm = T),
                                                   'VL suppression' = sum(vl_sup,na.rm = T),
                                                   'Retained in care' = sum(retained_care_hiv,na.rm = T))})
                    cascade_hiv1_r_",i," <- reactive({as.data.frame(t(dthiv_r_",i,"()))})
            
                    cascade_hiv2p_r_",i," <- reactive({rbind(input$input1, cascade_hiv1_r_",i,"())})
                    Percentagep_r_",i," <- reactive({(round((cascade_hiv2p_r_",i,"()$V1*100/cascade_hiv2p_r_",i,"()$V1[1]),1))})
                    cascade_hiv3p_r_",i," <- reactive({cbind(cascade_hiv2p_r_",i, "(),Percentagep_r_",i,"())})
                    cascade_hiv4p_r_",i," <- reactive({cascade_hiv3p_r_",i,"() %>% rename(Total = V1, Percentage = 'Percentagep_r_",i,"()')})

                    Percentage_r_",i,"<- reactive({round((cascade_hiv1_r_",i,"()$V1*100/cascade_hiv1_r_",i,"()$V1[1]),1)})
                    cascade_hiv3_r_",i," <- reactive({cbind(cascade_hiv1_r_",i,"(),Percentage_r_",i,"())})
                    cascade_hiv4_r_",i,"<- reactive({cascade_hiv3_r_",i,"() %>% rename(Total = V1, Percentage = 'Percentage_r_",i,"()')})")
        eval(parse(text = str))}
        
        for (i in 1:n_files_hiv()) {
            str <- paste0("output$hivcascader_",i," <- renderTable({
            if (input$plhivb == 1) {
                    cascade_hiv5_r_",i," <- as.data.frame(cascade_hiv4p_r_",i,"())
                    rownames(cascade_hiv5_r_",i,") <- c('PLHIV','Diagnosed','Linkage to care','ART initiation','VL suppression','Retained in care')
            } else {
                    cascade_hiv5_r_",i," <- as.data.frame(cascade_hiv4_r_",i,"())
                    rownames(cascade_hiv5_r_",i,") <- c('Diagnosed','Linkage to care','ART initiation','VL suppression','Retained in care')
            }
                           cascade_hiv5_r_",i,"},include.rownames = TRUE)")
            eval(parse(text = str))}
    })
    
    #####HIV Cascade Plot
    
    output$tabshivcascadeplot <- renderUI({
        
        if (input$multiplehiv == 1) {
            str <- "tabBox(id = 'hivcasplot', width = 7,
                   tabPanel(id = 'plotall', title = 'HIV Cascade' ,plotlyOutput('hivcascadeplot')),"
            for (i in 1:n_files_hiv()) {str <- paste0(str, "tabPanel(id = paste('tab', ",i,") , title = paste('Data', ",i,") , plotlyOutput('hivcasplotr_",i,"')),")}
            str <- gsub(",$",")",str)
            eval(parse(text = str)) 
        }
        else {
            tabBox(id = "hivcasplot", width = 7,
                   tabPanel(id = "tab1", title = "HIV Cascade",plotlyOutput("hivcascadeplot")))
        }
    })
    
    
    
    output$hivcascadeplot <- renderPlotly({
        
        if (input$plhivb == 1) {
            cascadehiv <- c("PLHIV","Diagnosed","Linkage to care","ART initiation","VL suppression","Retained in care")
            cascade_hiv6 <- cbind.data.frame(cascade_hiv4p(),cascadehiv)}
        else {
            cascadehiv <- c("Diagnosed","Linkage to care","ART initiation","VL suppression","Retained in care")
            cascade_hiv6 <- cbind.data.frame(cascade_hiv4(),cascadehiv)}
        
      cascade_hiv6$cascadehiv <- factor(cascade_hiv6$cascadehiv,levels = cascadehiv)
      plot_ly(cascade_hiv6,x = ~cascadehiv,
                y = ~Percentage,
                type = "bar") %>% 
            layout(title = "HIV Cascade", xaxis = list(title = ""), yaxis = list(title = "Percentage"))})
    
    observe({
        for (i in 1:n_files_hiv())
        {str <- paste0("dthiv_r_",i,"<- reactive({dt_files_hiv()[[",i,"]] %>% summarize('Diagnosed' = sum(hiv_posresult,na.rm = T),
                                                   'Linkage to care' = sum(linkagetocare_hiv,na.rm = T),
                                                   'ART initiation' = sum(art_ini,na.rm = T),
                                                   'VL suppression' = sum(vl_sup,na.rm = T),
                                                   'Retained in care' = sum(retained_care_hiv,na.rm = T))})
                    cascade_hiv1_r_",i," <- reactive({as.data.frame(t(dthiv_r_",i,"()))})
            
                    cascade_hiv2p_r_",i," <- reactive({rbind(input$input1, cascade_hiv1_r_",i,"())})
                    Percentagep_r_",i," <- reactive({(round((cascade_hiv2p_r_",i,"()$V1*100/cascade_hiv2p_r_",i,"()$V1[1]),1))})
                    cascade_hiv3p_r_",i," <- reactive({cbind(cascade_hiv2p_r_",i, "(),Percentagep_r_",i,"())})
                    cascade_hiv4p_r_",i," <- reactive({cascade_hiv3p_r_",i,"() %>% rename(Total = V1, Percentage = 'Percentagep_r_",i,"()')})

                    Percentage_r_",i,"<- reactive({round((cascade_hiv1_r_",i,"()$V1*100/cascade_hiv1_r_",i,"()$V1[1]),1)})
                    cascade_hiv3_r_",i," <- reactive({cbind(cascade_hiv1_r_",i,"(),Percentage_r_",i,"())})
                    cascade_hiv4_r_",i,"<- reactive({cascade_hiv3_r_",i,"() %>% rename(Total = V1, Percentage = 'Percentage_r_",i,"()')})")
        eval(parse(text = str))}
        
       
         for (i in 1:n_files_hiv()) {
            str <- paste0("output$hivcasplotr_",i," <- renderPlotly({
            if (input$plhivb == 1) {
                    cascadehiv <- c('PLHIV','Diagnosed','Linkage to care','ART initiation','VL suppression','Retained in care')
                    cascade_hiv6_",i," <- cbind.data.frame(cascade_hiv4p_r_",i,"(),cascadehiv)
            } else {
                    cascadehiv <- c('Diagnosed','Linkage to care','ART initiation','VL suppression','Retained in care')
                    cascade_hiv6_",i," <- cbind.data.frame(cascade_hiv4_r_",i,"(),cascadehiv)}
                    
                    cascade_hiv6_",i,"$cascadehiv <- factor(cascade_hiv6_",i,"$cascadehiv,levels = cascadehiv)
            plot_ly(cascade_hiv6_",i,", x = ~cascadehiv, y = ~Percentage,
                type = 'bar') %>% 
            layout(title = 'HIV Cascade', xaxis = list(title = ''), yaxis = list(title = 'Percentage'))
                           })")
            eval(parse(text = str))}
    })
    
    
    ##### HIV Cascade - duration
    
    output$tabshivcascadedur <- renderUI({
      
      if (input$multiplehiv == 1) {
        str <- "tabBox(id = 'hivcasdurbox',
                   tabPanel(id = 'taball', title = 'HIV Cascade - Duration' ,tableOutput('hivcascadeduration')),"
        for (i in 1:n_files_hiv()) {str <- paste0(str, "tabPanel(id = paste('tab', ",i,") , title = paste('Data', ",i,") , tableOutput('hivcascadedurr_",i,"')),")}
        str <- gsub(",$",")",str)
        eval(parse(text = str)) 
      }
      else {
        tabBox(id = "hivcasdurbox", 
               tabPanel(id = "tab1", title = "HIV Cascade - Duration",tableOutput("hivcascadeduration")))
      }
    })
    
    #making a dataset with only the important variables for this section
    cascadedur_hiv <- reactive({dthiv() %>% select(date_hiv_test,date_hiv_posresult,
                                                   date_linkagetocare_hiv,
                                                   date_art_ini,date_vl_sup)})
    
    #making new variables calculating the duration between the different steps using the previously definied function durfunct for each obs
    test_diag <- reactive({durfunct(cascadedur_hiv()$date_hiv_test,cascadedur_hiv()$date_hiv_posresult, units = "weeks", floor = TRUE)})
    diag_link <- reactive({durfunct(cascadedur_hiv()$date_hiv_posresult,cascadedur_hiv()$date_linkagetocare_hiv, units = "weeks", floor = TRUE)})
    link_art <- reactive({durfunct(cascadedur_hiv()$date_linkagetocare_hiv, cascadedur_hiv()$date_art_ini ,units = "weeks", floor = TRUE)})
    #adding these new variables to the dataset with the important variables for this section
    cascadedur_hiv1 <- reactive({cbind(cascadedur_hiv(),test_diag(),diag_link(),link_art())})
    
    #aggregating the individual-level data
    #calculate the mean duration for each step
    mean_cascadedur_hiv <- reactive({cascadedur_hiv1() %>% summarize("Testing to diagnosis" = round(mean(test_diag(),na.rm = T),1),
                                                                     "Diagnosis to Linkage to care" = round(mean(diag_link(), na.rm = T),1),
                                                                     "Linkage to care to ART initiation" = round(mean(link_art(), na.rm = T),1))})
    mean_cascadedur_hiv1 <- reactive({as.data.frame(t(mean_cascadedur_hiv()))})
    #calculate the median duration for each step
    median_cascadedur_hiv <- reactive({cascadedur_hiv1() %>% summarize("Testing to diagnosis" = round(median(test_diag(),na.rm = T),1),
                                                                       "Diagnosis to Linkage to care" = round(median(diag_link(), na.rm = T),1),
                                                                       "Linkage to care to ART initiation" = round(median(link_art(), na.rm = T),1))})
    median_cascadedur_hiv1 <- reactive({as.data.frame(t(median_cascadedur_hiv()))})
    #calculate the SE for the duration for each step - to be used in calculating the CIs
    se_cascadedur_hiv <- reactive({cascadedur_hiv1() %>% summarize("Testing to diagnosis" = sd(test_diag(),na.rm = T)/sum(!is.na(test_diag())),
                                                                   "Diagnosis to Linkage to care" = sd(diag_link(), na.rm = T)/sum(!is.na(diag_link())),
                                                                   "Linkage to care to ART initiation" = sd(link_art(), na.rm = T)/sum(!is.na(link_art())))})
    se_cascadedur_hiv1 <- reactive({as.data.frame(t(se_cascadedur_hiv()))})
    n_duration_hiv <- reactive({nrow(se_cascadedur_hiv1())})
    
    #function to calculate the CI for mean duration
    funci <- function(n,p,v){
        
        v[1:n] <- qnorm(p = p, mean = mean_cascadedur_hiv1()$V1[1:n],
                        sd = se_cascadedur_hiv1()$V1[1:n])
    }
    
    #making an empty vector for the CI
    ci95_hivcasdur <- reactive({rep(NA,n_duration_hiv())})
    #calculating the lower bound of CI for each step
    ci95_hivcasdur_l <- reactive({funci(n_duration_hiv(),0.025,ci95_hivcasdur())})
    #calculating the upper bound of CI for each step
    ci95_hivcasdur_u <- reactive({funci(n_duration_hiv(),0.975,ci95_hivcasdur())})
    #putting the lower and upper bound together
    ci95_hivcasdur1 <- reactive({paste("[",round(ci95_hivcasdur_l(),1),",",round(ci95_hivcasdur_u(),1),"]")})
    
    
    output$hivcascadeduration <- renderTable({
        duration_hiv <- cbind.data.frame(median_cascadedur_hiv1(),mean_cascadedur_hiv1(),ci95_hivcasdur1())
        colnames(duration_hiv) <- c("Median (weeks)","Mean (weeks)","95% CI for Mean (weeks)")
        duration_hiv},
        include.rownames = TRUE)
    
    #duration table for each tab when there are multiple files uploaded
    observe({
      #aggregating the data and calculating mean, median and CI
      for (i in 1:n_files_hiv())
      {str <- paste0("cascadedur_hiv_r_",i,"<- reactive({dt_files_hiv()[[",i,"]] %>% select(date_hiv_test,date_hiv_posresult,
                                                   date_linkagetocare_hiv, date_art_ini,date_vl_sup)})
                    test_diag_r_",i," <- reactive({durfunct(cascadedur_hiv_r_",i,"()$date_hiv_test,cascadedur_hiv_r_",i,"()$date_hiv_posresult, units = 'weeks', floor = TRUE)})
                    diag_link_r_",i," <- reactive({durfunct(cascadedur_hiv_r_",i,"()$date_hiv_posresult,cascadedur_hiv_r_",i,"()$date_linkagetocare_hiv, units = 'weeks', floor = TRUE)})
                    link_art_r_",i," <- reactive({durfunct(cascadedur_hiv_r_",i,"()$date_linkagetocare_hiv, cascadedur_hiv_r_",i,"()$date_art_ini ,units = 'weeks', floor = TRUE)})
                    
                    cascadedur_hiv1_r_",i," <- reactive({cbind(cascadedur_hiv_r_",i,"(),test_diag_r_",i,"(),diag_link_r_",i,"(),link_art_r_",i,"())})
                    mean_cascadedur_hiv_r_",i," <- reactive({cascadedur_hiv_r_",i,"() %>% summarize('Testing to diagnosis' = round(mean(test_diag_r_",i,"(),na.rm = T),1),
                                                                     'Diagnosis to Linkage to care' = round(mean(diag_link_r_",i,"(), na.rm = T),1),
                                                                     'Linkage to care to ART initiation' = round(mean(link_art_r_",i,"(), na.rm = T),1))})
                    mean_cascadedur_hiv1_r_",i," <- reactive({as.data.frame(t(mean_cascadedur_hiv_r_",i,"()))})
                    median_cascadedur_hiv_r_",i," <- reactive({cascadedur_hiv1_r_",i,"() %>% summarize('Testing to diagnosis' = round(median(test_diag_r_",i,"(),na.rm = T),1),
                                                                       'Diagnosis to Linkage to care' = round(median(diag_link_r_",i,"(), na.rm = T),1),
                                                                       'Linkage to care to ART initiation' = round(median(link_art_r_",i,"(), na.rm = T),1))})
                    median_cascadedur_hiv1_r_",i," <- reactive({as.data.frame(t(median_cascadedur_hiv_r_",i,"()))})
                    se_cascadedur_hiv_r_",i," <- reactive({cascadedur_hiv1_r_",i,"() %>% summarize('Testing to diagnosis' = sd(test_diag_r_",i,"(),na.rm = T)/sum(!is.na(test_diag_r_",i,"())),
                                                                   'Diagnosis to Linkage to care' = sd(diag_link_r_",i,"(), na.rm = T)/sum(!is.na(diag_link_r_",i,"())),
                                                                   'Linkage to care to ART initiation' = sd(link_art_r_",i,"(), na.rm = T)/sum(!is.na(link_art_r_",i,"())))})
                    se_cascadedur_hiv1_r_",i," <- reactive({as.data.frame(t(se_cascadedur_hiv_r_",i,"()))})
                    n_duration_hiv_r_",i," <- reactive({nrow(se_cascadedur_hiv1_r_",i,"())})
                    funci_r_",i," <- function(n,p,v){ 
                                              v[1:n] <- qnorm(p = p, mean = mean_cascadedur_hiv1_r_",i,"()$V1[1:n],
                                                              sd = se_cascadedur_hiv1_r_",i,"()$V1[1:n])
                    }
                    ci95_hivcasdur_r_",i," <- reactive({rep(NA,n_duration_hiv_r_",i,"())})
                    ci95_hivcasdur_l_r_",i," <- reactive({funci_r_",i,"(n_duration_hiv_r_",i,"(),0.025,ci95_hivcasdur_r_",i,"())})
                    ci95_hivcasdur_u_r_",i," <- reactive({funci_r_",i,"(n_duration_hiv_r_",i,"(),0.975,ci95_hivcasdur_r_",i,"())})
                    ci95_hivcasdur1_r_",i," <- reactive({paste('[',round(ci95_hivcasdur_l_r_",i,"(),1),',',round(ci95_hivcasdur_u_r_",i,"(),1),']')})")
      
      eval(parse(text = str))}
      
      #making the table
      for (i in 1:n_files_hiv()) {
        str <- paste0("output$hivcascadedurr_",i," <- renderTable({
        duration_hiv_",i," <- cbind.data.frame(median_cascadedur_hiv1_r_",i,"(),mean_cascadedur_hiv1_r_",i,"(),ci95_hivcasdur1_r_",i,"())
        colnames( duration_hiv_",i,") <- c('Median (weeks)','Mean (weeks)','95% CI for Mean (weeks)')
        duration_hiv_",i,"},
        include.rownames = TRUE)")
        eval(parse(text = str))}
    })
    
    ####################################################################################################################
    ##############################                            HCV                         ##############################  
    ####################################################################################################################
    
    
    abteststatus <- c(1,2)
    abnuminput <- paste0("inputat",abteststatus)
    
    output$abtestinput <- renderUI({
        abnum_inputs <- lapply(1:2, function(i){
            numericInput(inputId = abnuminput[i],label = 'Number of Ab tests',80, min = 1, max = 1000)})
        shinyjs::hidden(abnum_inputs)
    })
    
    abposstatus <- c(1,2)
    abposnuminput <- paste0("inputap",abposstatus)
    
    output$abposinput <- renderUI({
        abposnum_input <- lapply(1:2, function(i){
            numericInput(inputId = abposnuminput[i],label = 'Number of Ab positive tests', 60, min = 1, max = 1000)})
        shinyjs::hidden(abposnum_input)
    })
    
    rnateststatus <- c(1,2)
    rnanuminput <- paste0("inputrt",rnateststatus)
    
    output$rnatestinput <- renderUI({
        rnanum_input <- lapply(1:2, function(i){
            numericInput(inputId = rnanuminput[i],label = 'Number of RNA tests', 50, min = 1, max = 1000)})
        shinyjs::hidden(rnanum_input)
    })
    
    
    observe({
        for(i in abteststatus){
            if (i %in% input$abtestsb){
                shinyjs::show(id = paste0("inputat",i))
            } else {
                shinyjs::hide(id = paste0("inputat",i))
            }
        }
    })
    
    observe({
        
        for(i in abposstatus){
            if (i %in% input$abposb){
                shinyjs::show(id = paste0("inputap",i))
            } else {
                shinyjs::hide(id = paste0("inputap",i))
            }
        }
    })
    
    observe({
        
        for(i in rnateststatus){
            if (i %in% input$rnatestsb){
                shinyjs::show(id = paste0("inputrt",i))
            } else {
                shinyjs::hide(id = paste0("inputrt",i))
            }
        }
    })
    
    dthcv <- reactive({req(input$dt_hcv)
        rbindlist(lapply(input$dt_hcv$datapath, fread, header = input$hivheader, quote = input$hivquote, sep = input$hivsep),
                  use.names = TRUE, fill = TRUE)
    })
    
    n_files_hcv <- reactive({length(input$dt_hcv$datapath)})
    dt_files_hcv <- reactive({lapply(input$dt_hcv$datapath[1:n_files_hcv()],read.csv)})
    
    ####HCV Cascade Table
    
    output$tabshcvcascade <- renderUI({
      
      if (input$multiplehcv == 1) {
        str <- "tabBox(id = 'hcvcasbox', width = 6,
                   tabPanel(id = 'taball', title = 'HCV Cascade' ,tableOutput('hcvcascade')),"
        for (i in 1:n_files_hcv()) {str <- paste0(str, "tabPanel(id = paste('tab', ",i,") , title = paste('Data', ",i,") , tableOutput('hcvcascader_",i,"')),")}
        str <- gsub(",$",")",str)
        eval(parse(text = str)) 
      }
      else {
        tabBox(id = "hcvcasbox", width = 6,
               tabPanel(id = "tab1", title = "HCV Cascade",tableOutput("hcvcascade")))
      }
    })
    
    
    cascade_hcv <- reactive({dthcv() %>% summarize("Diagnosed" = sum(rna_posresult, na.rm = T),
                                                   "Linkage to care" = sum(linkagetocare_hcv, na.rm = T),
                                                   "Treatment initiation" = sum(tx_ini_hcv, na.rm = T),
                                                   "Treatment completion" = sum(tx_comp_hcv, na.rm = T),
                                                   "SVR" = sum(svr, na.rm = T),
                                                   "Retained in care" = sum(retainedincare_hcv, na.rm = T),
                                                   "Re-infection" = sum(reinfection, na.rm = T))})
    
    cascade_hcv1 <- reactive({as.data.frame(t(cascade_hcv()))})
    
    cascade_hcv2t <- reactive({rbind(input$inputat1,input$inputap1,input$inputrt1, cascade_hcv1())})
    Percentagehcvt <- reactive({(round((cascade_hcv2t()$V1*100/cascade_hcv2t()$V1[1]),1))})
    cascade_hcv3t <- reactive({cbind(cascade_hcv2t(),Percentagehcvt())})
    cascade_hcv4t <- reactive({cascade_hcv3t() %>% rename(Total = V1, Percentage = "Percentagehcvt()")})
    
    Percentagehcv <- reactive({(round((cascade_hcv1()$V1*100/cascade_hcv1()$V1[1]),1))})
    cascade_hcv2 <- reactive({cbind(cascade_hcv1(),Percentagehcv())})
    cascade_hcv3 <- reactive({cascade_hcv2() %>% rename(Total = V1, Percentage = "Percentagehcv()")})

    output$hcvcascade <- renderTable({
        
        if (input$abtestsb == 1 & input$abposb == 1 & input$rnatestsb == 1) {
            cascade_hcv5 <- as.data.frame(cascade_hcv4t())
            rownames(cascade_hcv5) <- c("Ab & Ag test","Positive Ab & Ag","HCV RNA test",
                                        "Diagnosed","Linkage to care","Treatment initiation",
                                        "Treatment completion","SVR","Retained in care","Re-infection")
        } else {
            cascade_hcv5 <- as.data.frame(cascade_hcv3())
            rownames(cascade_hcv5) <- c("Diagnosed","Linkage to care","Treatment initiation",
                                        "Treatment completion","SVR","Retained in care","Re-infection")
        }
        cascade_hcv5},include.rownames = TRUE)
    
    observe({
      for (i in 1:n_files_hcv())
      {str <- paste0("cascade_hcv_r_",i," <- reactive({dt_files_hcv()[[",i,"]] %>% summarize('Diagnosed' = sum(rna_posresult, na.rm = T),
                                                   'Linkage to care' = sum(linkagetocare_hcv, na.rm = T),
                                                   'Treatment initiation' = sum(tx_ini_hcv, na.rm = T),
                                                   'Treatment completion' = sum(tx_comp_hcv, na.rm = T),
                                                   'SVR' = sum(svr, na.rm = T),
                                                   'Retained in care' = sum(retainedincare_hcv, na.rm = T),
                                                   'Re-infection' = sum(reinfection, na.rm = T))})
    
    cascade_hcv1_r_",i," <- reactive({as.data.frame(t(cascade_hcv_r_",i,"()))})
    
    cascade_hcv2t_r_",i," <- reactive({rbind(input$inputat1,input$inputap1,input$inputrt1, cascade_hcv1_r_",i,"())})
    Percentagehcvt_r_",i," <- reactive({(round((cascade_hcv2t_r_",i,"()$V1*100/cascade_hcv2t_r_",i,"()$V1[1]),1))})
    cascade_hcv3t_r_",i," <- reactive({cbind(cascade_hcv2t_r_",i,"(),Percentagehcvt_r_",i,"())})
    cascade_hcv4t_r_",i," <- reactive({cascade_hcv3t_r_",i,"() %>% rename(Total = V1, Percentage = 'Percentagehcvt_r_",i,"()')})
    
    Percentagehcv_r_",i," <- reactive({(round((cascade_hcv1_r_",i,"()$V1*100/cascade_hcv1_r_",i,"()$V1[1]),1))})
    cascade_hcv2_r_",i," <- reactive({cbind(cascade_hcv1_r_",i,"(),Percentagehcv_r_",i,"())})
    cascade_hcv3_r_",i," <- reactive({cascade_hcv2_r_",i,"() %>% rename(Total = V1, Percentage = 'Percentagehcv_r_",i,"()')})")
      eval(parse(text = str))}

      for (i in 1:n_files_hcv()) {
        str <- paste0("output$hcvcascader_",i," <- renderTable({
        
        if (input$abtestsb == 1 & input$abposb == 1 & input$rnatestsb == 1) {
            cascade_hcv5_r_",i," <- as.data.frame(cascade_hcv4t_r_",i,"())
            rownames(cascade_hcv5_r_",i,") <- c('Ab & Ag test','Positive Ab & Ag','HCV RNA test',
                                        'Diagnosed','Linkage to care','Treatment initiation',
                                        'Treatment completion','SVR','Retained in care','Re-infection')
        } else {
            cascade_hcv5_r_",i," <- as.data.frame(cascade_hcv3_r_",i,"())
            rownames(cascade_hcv5_r_",i,") <- c('Diagnosed','Linkage to care','Treatment initiation',
                                        'Treatment completion','SVR','Retained in care','Re-infection')
        }
        cascade_hcv5_r_",i,"},include.rownames = TRUE)")
        eval(parse(text = str))}
    })
    
    ####HCV Cascade Plot - retained in care
    
    output$tabshcvcascaderetained <- renderUI({
      
      if (input$multiplehcv == 1) {
        str <- "tabBox(id = 'hcvcasplot',
                   tabPanel(id = 'plotall', title = 'HCV Cascade' ,plotlyOutput('hcvcascaderetained')),"
        for (i in 1:n_files_hcv()) {str <- paste0(str, "tabPanel(id = paste('tab', ",i,") , title = paste('Data', ",i,") , plotlyOutput('hcvcasretained_r_",i,"')),")}
        str <- gsub(",$",")",str)
        eval(parse(text = str)) 
      }
      else {
        tabBox(id = "hcvcasplot", 
               tabPanel(id = "tab1", title = "HCV Cascade",plotlyOutput("hcvcascaderetained")))
      }
    })
    
    output$hcvcascaderetained <- renderPlotly({
        if (input$abtestsb == 1 & input$abposb == 1 & input$rnatestsb == 1) {
            cascadehcv <- c("Ab & Ag test","Positive Ab & Ag","HCV RNA test",
                            "Diagnosed","Linkage to care","Treatment initiation",
                            "Treatment completion","SVR","Retained in care","Re-infection")
            cascade_hcv6 <- reactive({cbind.data.frame(cascade_hcv4t(),cascadehcv)})
            cascade_hcv7 <- as.data.frame(cascade_hcv6()[1:9,])
            
        } else {
            cascadehcv <- c("Diagnosed","Linkage to care","Treatment initiation",
                            "Treatment completion","SVR","Retained in care","Re-infection")
            cascade_hcv6 <- reactive({cbind.data.frame(cascade_hcv3(),cascadehcv)})
            cascade_hcv7 <- as.data.frame(cascade_hcv6()[1:6,])
        } 
        
        cascade_hcv7$cascadehcv <- factor(cascade_hcv7$cascadehcv,levels = cascadehcv)
        plot_ly(cascade_hcv7,x = ~cascadehcv,
                y = ~Percentage,
                type = "bar") %>%
            layout(title = "HCV Cascade - retained in care", xaxis = list(title = ""), yaxis = list(title = "Percentage"))})
    
    observe({
      for (i in 1:n_files_hcv())
      {str <- paste0("cascade_hcv_r_",i," <- reactive({dt_files_hcv()[[",i,"]] %>% summarize('Diagnosed' = sum(rna_posresult, na.rm = T),
                                                   'Linkage to care' = sum(linkagetocare_hcv, na.rm = T),
                                                   'Treatment initiation' = sum(tx_ini_hcv, na.rm = T),
                                                   'Treatment completion' = sum(tx_comp_hcv, na.rm = T),
                                                   'SVR' = sum(svr, na.rm = T),
                                                   'Retained in care' = sum(retainedincare_hcv, na.rm = T),
                                                   'Re-infection' = sum(reinfection, na.rm = T))})
    
    cascade_hcv1_r_",i," <- reactive({as.data.frame(t(cascade_hcv_r_",i,"()))})
    
    cascade_hcv2t_r_",i," <- reactive({rbind(input$inputat1,input$inputap1,input$inputrt1, cascade_hcv1_r_",i,"())})
    Percentagehcvt_r_",i," <- reactive({(round((cascade_hcv2t_r_",i,"()$V1*100/cascade_hcv2t_r_",i,"()$V1[1]),1))})
    cascade_hcv3t_r_",i," <- reactive({cbind(cascade_hcv2t_r_",i,"(),Percentagehcvt_r_",i,"())})
    cascade_hcv4t_r_",i," <- reactive({cascade_hcv3t_r_",i,"() %>% rename(Total = V1, Percentage = 'Percentagehcvt_r_",i,"()')})
    
    Percentagehcv_r_",i," <- reactive({(round((cascade_hcv1_r_",i,"()$V1*100/cascade_hcv1_r_",i,"()$V1[1]),1))})
    cascade_hcv2_r_",i," <- reactive({cbind(cascade_hcv1_r_",i,"(),Percentagehcv_r_",i,"())})
    cascade_hcv3_r_",i," <- reactive({cascade_hcv2_r_",i,"() %>% rename(Total = V1, Percentage = 'Percentagehcv_r_",i,"()')})")
      eval(parse(text = str))}
      
      
      for (i in 1:n_files_hcv()) 
      {str <- paste0("output$hcvcasretained_r_",i," <- renderPlotly({
        
          if (input$abtestsb == 1 & input$abposb == 1 & input$rnatestsb == 1) {
            cascadehcv <- c('Ab & Ag test','Positive Ab & Ag','HCV RNA test',
                                        'Diagnosed','Linkage to care','Treatment initiation',
                                        'Treatment completion','SVR','Retained in care','Re-infection')
            cascade_hcv6_r_",i," <- reactive({cbind.data.frame(cascade_hcv4t_r_",i,"(),cascadehcv)})
            cascade_hcv7_r_",i," <- as.data.frame(cascade_hcv6_r_",i,"()[1:9,])
            
        } else {
            cascadehcv <- c('Diagnosed','Linkage to care','Treatment initiation',
                                        'Treatment completion','SVR','Retained in care','Re-infection')
            cascade_hcv6_r_",i," <- reactive({cbind.data.frame(cascade_hcv3_r_",i,"(),cascadehcv)})
            cascade_hcv7_r_",i," <- as.data.frame(cascade_hcv6_r_",i,"()[1:6,])
        } 
        
        cascade_hcv7_r_",i,"$cascadehcv <- factor(cascade_hcv7_r_",i,"$cascadehcv,levels = cascadehcv)
        plot_ly(cascade_hcv7_r_",i,",x = ~cascadehcv, y = ~Percentage, type = 'bar') %>%
            layout(title = 'HCV Cascade - retained in care', xaxis = list(title = ''), yaxis = list(title = 'Percentage'))})")
        eval(parse(text = str))}
    })
    
    ####HCV Cascade Plot - reinfection
    
    output$tabshcvcascadereinf <- renderUI({
      
      if (input$multiplehcv == 1) {
        str <- "tabBox(id = 'hcvcasplotreinf',
                   tabPanel(id = 'plotall', title = 'HCV Cascade' ,plotlyOutput('hcvcascadereinf')),"
        for (i in 1:n_files_hcv()) {str <- paste0(str, "tabPanel(id = paste('tab', ",i,") , title = paste('Data', ",i,") , plotlyOutput('hcvcasreinf_r_",i,"')),")}
        str <- gsub(",$",")",str)
        eval(parse(text = str)) 
      }
      else {
        tabBox(id = "hcvcasplotreinf", 
               tabPanel(id = "tab1", title = "HCV Cascade",plotlyOutput("hcvcascadereinf")))
      }
    })
    
    output$hcvcascadereinf <- renderPlotly({
        if (input$abtestsb == 1 & input$abposb == 1 & input$rnatestsb == 1) {
            cascadehcv <- c("Ab & Ag test","Positive Ab & Ag","HCV RNA test",
                            "Diagnosed","Linkage to care","Treatment initiation",
                            "Treatment completion","SVR","Retained in care","Re-infection")
            cascade_hcv6 <- reactive({cbind.data.frame(cascade_hcv4t(),cascadehcv)})
        } else {
            cascadehcv <- c("Diagnosed","Linkage to care","Treatment initiation",
                            "Treatment completion","SVR","Retained in care","Re-infection")
            cascade_hcv6 <- reactive({cbind.data.frame(cascade_hcv3(),cascadehcv)})
        } 
        cascade_hcv8 <- as.data.frame(cascade_hcv6()[c(1:8,10),])
        cascade_hcv8$cascadehcv <- factor(cascade_hcv8$cascadehcv,levels = c("Ab & Ag test","Positive Ab & Ag","HCV RNA test",
                                                                             "Diagnosed","Linkage to care","Treatment initiation",
                                                                             "Treatment completion","SVR","Re-infection"))
        plot_ly(cascade_hcv8,x = ~cascadehcv,
                y = ~Percentage,
                type = "bar") %>%
            layout(title = "HCV Cascade - reinfection", xaxis = list(title = ""), yaxis = list(title = "Percentage"))})
    
    observe({
      for (i in 1:n_files_hcv())
      {str <- paste0("cascade_hcv_r_",i," <- reactive({dt_files_hcv()[[",i,"]] %>% summarize('Diagnosed' = sum(rna_posresult, na.rm = T),
                                                   'Linkage to care' = sum(linkagetocare_hcv, na.rm = T),
                                                   'Treatment initiation' = sum(tx_ini_hcv, na.rm = T),
                                                   'Treatment completion' = sum(tx_comp_hcv, na.rm = T),
                                                   'SVR' = sum(svr, na.rm = T),
                                                   'Retained in care' = sum(retainedincare_hcv, na.rm = T),
                                                   'Re-infection' = sum(reinfection, na.rm = T))})
    
    cascade_hcv1_r_",i," <- reactive({as.data.frame(t(cascade_hcv_r_",i,"()))})
    
    cascade_hcv2t_r_",i," <- reactive({rbind(input$inputat1,input$inputap1,input$inputrt1, cascade_hcv1_r_",i,"())})
    Percentagehcvt_r_",i," <- reactive({(round((cascade_hcv2t_r_",i,"()$V1*100/cascade_hcv2t_r_",i,"()$V1[1]),1))})
    cascade_hcv3t_r_",i," <- reactive({cbind(cascade_hcv2t_r_",i,"(),Percentagehcvt_r_",i,"())})
    cascade_hcv4t_r_",i," <- reactive({cascade_hcv3t_r_",i,"() %>% rename(Total = V1, Percentage = 'Percentagehcvt_r_",i,"()')})
    
    Percentagehcv_r_",i," <- reactive({(round((cascade_hcv1_r_",i,"()$V1*100/cascade_hcv1_r_",i,"()$V1[1]),1))})
    cascade_hcv2_r_",i," <- reactive({cbind(cascade_hcv1_r_",i,"(),Percentagehcv_r_",i,"())})
    cascade_hcv3_r_",i," <- reactive({cascade_hcv2_r_",i,"() %>% rename(Total = V1, Percentage = 'Percentagehcv_r_",i,"()')})")
      eval(parse(text = str))}
      
      
      for (i in 1:n_files_hcv()) 
      {str <- paste0("output$hcvcasreinf_r_",i," <- renderPlotly({
        if (input$abtestsb == 1 & input$abposb == 1 & input$rnatestsb == 1) {
            cascadehcv <- c('Ab & Ag test','Positive Ab & Ag','HCV RNA test',
                                        'Diagnosed','Linkage to care','Treatment initiation',
                                        'Treatment completion','SVR','Retained in care','Re-infection')
            cascade_hcv6_r_",i," <- reactive({cbind.data.frame(cascade_hcv4t_r_",i,"(),cascadehcv)})
        } else {
            cascadehcv <- c('Diagnosed','Linkage to care','Treatment initiation',
                                        'Treatment completion','SVR','Retained in care','Re-infection')
            cascade_hcv6_r_",i," <- reactive({cbind.data.frame(cascade_hcv3_r_",i,"(),cascadehcv)})
        } 
        cascade_hcv8_r_",i," <- as.data.frame(cascade_hcv6_r_",i,"()[c(1:8,10),])
        cascade_hcv8_r_",i,"$cascadehcv <- factor(cascade_hcv8_r_",i,"$cascadehcv,levels = c('Ab & Ag test','Positive Ab & Ag','HCV RNA test',
                                        'Diagnosed','Linkage to care','Treatment initiation',
                                        'Treatment completion','SVR','Re-infection'))
        plot_ly(cascade_hcv8_r_",i,",x = ~cascadehcv,
                y = ~Percentage,
                type = 'bar') %>%
            layout(title = 'HCV Cascade - reinfection', xaxis = list(title = ''), yaxis = list(title = 'Percentage'))})")
      eval(parse(text = str))}
    })
    
    
    
    ##### HCV Cascade - duration
    
    output$tabshcvcascadedur <- renderUI({
      
      if (input$multiplehcv == 1) {
        str <- "tabBox(id = 'hcvcasdurbox',
                   tabPanel(id = 'taball', title = 'HCV Cascade - Duration' ,tableOutput('hcvcascadeduration')),"
        for (i in 1:n_files_hcv()) {str <- paste0(str, "tabPanel(id = paste('tab', ",i,") , title = paste('Data', ",i,") , tableOutput('hcvcascadedurr_",i,"')),")}
        str <- gsub(",$",")",str)
        eval(parse(text = str)) 
      }
      else {
        tabBox(id = "hcvcasdurbox", 
               tabPanel(id = "tab1", title = "HCV Cascade - Duration",tableOutput("hcvcascadeduration")))
      }
    })
    
    
    cascadedur_hcv <- reactive({dthcv() %>% select(date_ab_ag_test,date_ab_ag_posresult,
                                                   date_rna_test, date_rna_posresult,
                                                   date_linkagetocare_hcv,
                                                   date_tx_ini_hcv, date_tx_comp_hcv, date_svr, reinfection_date)})
  
    abagtest_result <- reactive({durfunct(cascadedur_hcv()$date_ab_ag_test,cascadedur_hcv()$date_ab_ag_posresult, units = "weeks", floor = TRUE)})
    abagres_rnatest <- reactive({durfunct(cascadedur_hcv()$date_ab_ag_posresult,cascadedur_hcv()$date_rna_test, units = "weeks", floor = TRUE)})
    rnatest_diag <- reactive({durfunct(cascadedur_hcv()$date_rna_test,cascadedur_hcv()$date_rna_posresult, units = "weeks", floor = TRUE)})
    diag_link_hcv <- reactive({durfunct(cascadedur_hcv()$date_rna_posresult,cascadedur_hcv()$date_linkagetocare_hcv, units = "weeks", floor = TRUE)})
    link_txini <- reactive({durfunct(cascadedur_hcv()$date_linkagetocare_hcv,cascadedur_hcv()$date_tx_ini_hcv, units = "weeks", floor = TRUE)})
    txini_txcomp <- reactive({durfunct(cascadedur_hcv()$date_tx_ini_hcv,cascadedur_hcv()$date_tx_comp_hcv, units = "weeks", floor = TRUE)})
    txcomp_svr <- reactive({durfunct(cascadedur_hcv()$date_tx_comp_hcv,cascadedur_hcv()$date_svr, units = "weeks", floor = TRUE)})
    svr_reinfection <- reactive({durfunct(cascadedur_hcv()$date_svr,cascadedur_hcv()$reinfection_date, units = "weeks", floor = TRUE)})
    
    cascadedur_hcv1 <- reactive({cbind.data.frame(cascadedur_hcv(),abagtest_result(),abagres_rnatest(),rnatest_diag(),
                                                  diag_link_hcv(),link_txini(),txini_txcomp(),txcomp_svr(),svr_reinfection())})
    
    mean_cascadedur_hcv <- reactive({cascadedur_hcv1() %>% summarize("Ab&Ag test to Ab&Ag result" = round(mean(abagtest_result(), na.rm=TRUE),1),
                                                                     "Ab&Ag result to RNA test" = round(mean(abagres_rnatest(), na.rm = TRUE),1),
                                                                     "RNA test to diagnosis" = round(mean(rnatest_diag(), na.rm = TRUE),1),
                                                                     "Diagnosis to linkage to care" = round(mean(diag_link_hcv(), na.rm = TRUE),1),
                                                                     "Linkage to care to treatment initiation" = round(mean(link_txini(), na.rm = TRUE),1),
                                                                     "Treatment initiation to treatment completion" = round(mean(txini_txcomp(), na.rm = TRUE),1),
                                                                     "Treatment completion to SVR" = round(mean(txcomp_svr(), na.rm = TRUE),1),
                                                                     "SVR to reinfection" = round(mean(svr_reinfection(), na.rm = TRUE),1))})
    mean_cascadedur_hcv1 <- reactive({as.data.frame(t(mean_cascadedur_hcv()))})
    
    median_cascadedur_hcv <- reactive({cascadedur_hcv1() %>% summarize("Ab&Ag test to Ab&Ag result" = round(median(abagtest_result(), na.rm = T),1),
                                                                       "Ab&Ag result to RNA test" = round(median(abagres_rnatest(), na.rm = T),1),
                                                                       "RNA test to diagnosis" = round(median(rnatest_diag(), na.rm = T),1),
                                                                       "Diagnosis to linkage to care" = round(median(diag_link_hcv(), na.rm = T),1),
                                                                       "Linkage to care to treatment initiation" = round(median(link_txini(), na.rm = T),1),
                                                                       "Treatment initiation to treatment completion" = round(median(txini_txcomp(), na.rm = T),1),
                                                                       "Treatment completion to SVR" = round(median(txcomp_svr(), na.rm = T),1),
                                                                       "SVR to reinfection" = round(median(svr_reinfection(), na.rm = T),1))})
    median_cascadedur_hcv1 <- reactive({as.data.frame(t(median_cascadedur_hcv()))})
    
    se_cascadedur_hcv <- reactive({cascadedur_hcv1() %>%
            summarize("Ab&Ag test to Ab&Ag result" = sd(abagtest_result(),na.rm = T)/sum(!is.na(abagtest_result())),
                      "Ab&Ag result to RNA test" = sd(abagres_rnatest(), na.rm = T)/sum(!is.na(abagres_rnatest())),
                      "RNA test to diagnosis" = sd(rnatest_diag(), na.rm = T)/sum(!is.na(rnatest_diag())),
                      "Diagnosis to linkage to care" = sd(diag_link_hcv(), na.rm = T)/sum(!is.na(diag_link_hcv())),
                      "Linkage to care to treatment initiation" = sd(link_txini(), na.rm = T)/sum(!is.na(link_txini())),
                      "Treatment initiation to treatment completion" = sd(txini_txcomp(), na.rm = T)/sum(!is.na(txini_txcomp())),
                      "Treatment completion to SVR" = sd(txcomp_svr(), na.rm = T)/sum(!is.na(txcomp_svr())),
                      "SVR to reinfection" = sd(svr_reinfection(), na.rm = T)/sum(!is.na(svr_reinfection())))})
    se_cascadedur_hcv1 <- reactive({as.data.frame(t(se_cascadedur_hcv()))})
    n_duration_hcv <- reactive({nrow(se_cascadedur_hcv1())})
    
    funcihcv <- function(n,p,v){
        
        v[1:n] <- qnorm(p = p, mean = mean_cascadedur_hcv1()$V1[1:n],
                        sd = se_cascadedur_hcv1()$V1[1:n])
    }
    
    ci95_hcvcasdur <- reactive({rep(NA,n_duration_hcv())})
    ci95_hcvcasdur_l <- reactive({funcihcv(n_duration_hcv(),0.025,ci95_hcvcasdur())})
    ci95_hcvcasdur_u <- reactive({funcihcv(n_duration_hcv(),0.975,ci95_hcvcasdur())})
    ci95_hcvcasdur1 <- reactive({paste("[",round(ci95_hcvcasdur_l(),1),",",round(ci95_hcvcasdur_u(),1),"]")})
    
    
    output$hcvcascadeduration <- renderTable({
        duration_hcv <- cbind.data.frame(median_cascadedur_hcv1(),mean_cascadedur_hcv1(),ci95_hcvcasdur1())
        colnames(duration_hcv) <- c("Median (weeks)","Mean (weeks)","95% CI for Mean (weeks)")
        duration_hcv},
        include.rownames = TRUE)
    

    observe({
      for (i in 1:n_files_hcv())
      {str <- paste0("cascadedur_hcv_r_",i," <- reactive({dt_files_hcv()[[",i,"]] %>% select(date_ab_ag_test,date_ab_ag_posresult,
                                                   date_rna_test, date_rna_posresult,
                                                   date_linkagetocare_hcv,
                                                   date_tx_ini_hcv, date_tx_comp_hcv, date_svr, reinfection_date)})

    abagtest_result_r_",i," <- reactive({durfunct(cascadedur_hcv_r_",i,"()$date_ab_ag_test,cascadedur_hcv_r_",i,"()$date_ab_ag_posresult, units = 'weeks', floor = TRUE)})
    abagres_rnatest_r_",i," <- reactive({durfunct(cascadedur_hcv_r_",i,"()$date_ab_ag_posresult,cascadedur_hcv_r_",i,"()$date_rna_test, units = 'weeks', floor = TRUE)})
    rnatest_diag_r_",i," <- reactive({durfunct(cascadedur_hcv_r_",i,"()$date_rna_test,cascadedur_hcv_r_",i,"()$date_rna_posresult, units = 'weeks', floor = TRUE)})
    diag_link_hcv_r_",i," <- reactive({durfunct(cascadedur_hcv_r_",i,"()$date_rna_posresult,cascadedur_hcv_r_",i,"()$date_linkagetocare_hcv, units = 'weeks', floor = TRUE)})
    link_txini_r_",i," <- reactive({durfunct(cascadedur_hcv_r_",i,"()$date_linkagetocare_hcv,cascadedur_hcv_r_",i,"()$date_tx_ini_hcv, units = 'weeks', floor = TRUE)})
    txini_txcomp_r_",i," <- reactive({durfunct(cascadedur_hcv_r_",i,"()$date_tx_ini_hcv,cascadedur_hcv_r_",i,"()$date_tx_comp_hcv, units = 'weeks', floor = TRUE)})
    txcomp_svr_r_",i," <- reactive({durfunct(cascadedur_hcv_r_",i,"()$date_tx_comp_hcv,cascadedur_hcv_r_",i,"()$date_svr, units = 'weeks', floor = TRUE)})
    svr_reinfection_r_",i," <- reactive({durfunct(cascadedur_hcv_r_",i,"()$date_svr,cascadedur_hcv_r_",i,"()$reinfection_date, units = 'weeks', floor = TRUE)})

    cascadedur_hcv1_r_",i," <- reactive({cbind.data.frame(cascadedur_hcv_r_",i,"(),abagtest_result_r_",i,"(),abagres_rnatest_r_",i,"(),rnatest_diag_r_",i,"(),
                                                  diag_link_hcv_r_",i,"(),link_txini_r_",i,"(),txini_txcomp_r_",i,"(),txcomp_svr_r_",i,"(),svr_reinfection_r_",i,"())})

    mean_cascadedur_hcv_r_",i," <- reactive({cascadedur_hcv1() %>% summarize('Ab&Ag test to Ab&Ag result' = round(mean(abagtest_result_r_",i,"(), na.rm=TRUE),1),
                                                                     'Ab&Ag result to RNA test' = round(mean(abagres_rnatest_r_",i,"(), na.rm = TRUE),1),
                                                                     'RNA test to diagnosis' = round(mean(rnatest_diag_r_",i,"(), na.rm = TRUE),1),
                                                                     'Diagnosis to linkage to care' = round(mean(diag_link_hcv_r_",i,"(), na.rm = TRUE),1),
                                                                     'Linkage to care to treatment initiation' = round(mean(link_txini_r_",i,"(), na.rm = TRUE),1),
                                                                     'Treatment initiation to treatment completion' = round(mean(txini_txcomp_r_",i,"(), na.rm = TRUE),1),
                                                                     'Treatment completion to SVR' = round(mean(txcomp_svr_r_",i,"(), na.rm = TRUE),1),
                                                                     'SVR to reinfection' = round(mean(svr_reinfection_r_",i,"(), na.rm = TRUE),1))})
    mean_cascadedur_hcv1_r_",i," <- reactive({as.data.frame(t(mean_cascadedur_hcv()))})

    median_cascadedur_hcv_r_",i," <- reactive({cascadedur_hcv1_r_",i,"() %>% summarize('Ab&Ag test to Ab&Ag result' = round(median(abagtest_result_r_",i,"(), na.rm = T),1),
                                                                       'Ab&Ag result to RNA test' = round(median(abagres_rnatest_r_",i,"(), na.rm = T),1),
                                                                       'RNA test to diagnosis' = round(median(rnatest_diag_r_",i,"(), na.rm = T),1),
                                                                       'Diagnosis to linkage to care' = round(median(diag_link_hcv_r_",i,"(), na.rm = T),1),
                                                                       'Linkage to care to treatment initiation' = round(median(link_txini_r_",i,"(), na.rm = T),1),
                                                                       'Treatment initiation to treatment completion' = round(median(txini_txcomp_r_",i,"(), na.rm = T),1),
                                                                       'Treatment completion to SVR' = round(median(txcomp_svr_r_",i,"(), na.rm = T),1),
                                                                       'SVR to reinfection' = round(median(svr_reinfection_r_",i,"(), na.rm = T),1))})
    median_cascadedur_hcv1_r_",i," <- reactive({as.data.frame(t(median_cascadedur_hcv_r_",i,"()))})

    se_cascadedur_hcv_r_",i," <- reactive({cascadedur_hcv1_r_",i,"() %>%
            summarize('Ab&Ag test to Ab&Ag result' = sd(abagtest_result_r_",i,"(),na.rm = T)/sum(!is.na(abagtest_result_r_",i,"())),
                      'Ab&Ag result to RNA test' = sd(abagres_rnatest_r_",i,"(), na.rm = T)/sum(!is.na(abagres_rnatest_r_",i,"())),
                      'RNA test to diagnosis' = sd(rnatest_diag_r_",i,"(), na.rm = T)/sum(!is.na(rnatest_diag_r_",i,"())),
                      'Diagnosis to linkage to care' = sd(diag_link_hcv_r_",i,"(), na.rm = T)/sum(!is.na(diag_link_hcv_r_",i,"())),
                      'Linkage to care to treatment initiation' = sd(link_txini_r_",i,"(), na.rm = T)/sum(!is.na(link_txini_r_",i,"())),
                      'Treatment initiation to treatment completion' = sd(txini_txcomp_r_",i,"(), na.rm = T)/sum(!is.na(txini_txcomp_r_",i,"())),
                      'Treatment completion to SVR' = sd(txcomp_svr_r_",i,"(), na.rm = T)/sum(!is.na(txcomp_svr_r_",i,"())),
                      'SVR to reinfection' = sd(svr_reinfection_r_",i,"(), na.rm = T)/sum(!is.na(svr_reinfection_r_",i,"())))})
    se_cascadedur_hcv1_r_",i," <- reactive({as.data.frame(t(se_cascadedur_hcv_r_",i,"()))})
    n_duration_hcv_r_",i," <- reactive({nrow(se_cascadedur_hcv1_r_",i,"())})

    funcihcv_r_",i," <- function(n,p,v){

        v[1:n] <- qnorm(p = p, mean = mean_cascadedur_hcv1_r_",i,"()$V1[1:n],
                        sd = se_cascadedur_hcv1_r_",i,"()$V1[1:n])
    }

    ci95_hcvcasdur_r_",i," <- reactive({rep(NA,n_duration_hcv_r_",i,"())})
    ci95_hcvcasdur_l_r_",i," <- reactive({funcihcv_r_",i,"(n_duration_hcv(),0.025,ci95_hcvcasdur_r_",i,"())})
    ci95_hcvcasdur_u_r_",i," <- reactive({funcihcv_r_",i,"(n_duration_hcv(),0.975,ci95_hcvcasdur_r_",i,"())})
    ci95_hcvcasdur1_r_",i," <- reactive({paste('[',round(ci95_hcvcasdur_l_r_",i,"(),1),',',round(ci95_hcvcasdur_u_r_",i,"(),1),']')})")
      eval(parse(text = str))}

      for (i in 1:n_files_hcv()) {
        str <- paste0("output$hcvcascadedurr_",i," <- renderTable({
        duration_hcv_r_",i," <- cbind.data.frame(median_cascadedur_hcv1_r_",i,"(),mean_cascadedur_hcv1_r_",i,"(),ci95_hcvcasdur1_r_",i,"())
        colnames(duration_hcv_r_",i,") <- c('Median (weeks)','Mean (weeks)','95% CI for Mean (weeks)')
        duration_hcv_r_",i,"},
        include.rownames = TRUE)")
        eval(parse(text = str))}
    })
}

shinyApp(ui, server)

