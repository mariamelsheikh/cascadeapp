######################## Version 3
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
library(table1)
library(summarytools)

# Define UI 

ui <- dashboardPage(
  
  #App title
  dashboardHeader(title = "Treatment and Care Cascade",
                  titleWidth = 300),
  
  #Sidebar contents (tabs)
  dashboardSidebar(width = 170,
                   sidebarMenu(
                     menuItem("Introduction",tabName = "intro",icon=icon("book-open")),
                     menuItem("Code", tabName = "code", icon = icon("code")),
                     menuItem("Demographics", tabName = "demo", icon = icon("table")),
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
      tabItem(tabName = "demo",
              h2("Demographics and Risk factors"),
              fluidRow(
                box(fluidRow(
                  box(fileInput("dt_master","Choose CSV File",
                                multiple = FALSE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")), width = 12, solidHeader = TRUE, height = 75),
                  box(checkboxInput("masterheader", "Header",TRUE), width = 3, solidHeader = TRUE, height = 100),
                  box(radioButtons("mastersep","Separator",
                                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                                   selected = ","), width = 3, solidHeader = TRUE, height = 120),
                  box(radioButtons("masterquote","Quote",
                                   choices = c(None = "","Double Quote" = '"',"Single Quote" = "'"),
                                   selected = '"'), width = 3, solidHeader = TRUE, height = 120)
                  ), width = 12, height = 250),
                box(uiOutput(outputId = "demotable"),title = "Demographics Summary", solidHeader = TRUE, status = "primary"),
                box(tableOutput("riskfactorsco"), title = "Risk Factors Summary among Co-infected", solidHeader = TRUE, status = "primary"),
                box(tableOutput("riskfactorshcv"), title = "Risk Factors Summary among only HCV", solidHeader = TRUE, status = "primary"),
                box(tableOutput("riskfactorshiv"), title = "Risk Factors Summary among only HIV", solidHeader = TRUE, status = "primary")
              )),
      tabItem(tabName = "hiv",
              h2("HIV Treatment and Care Cascade"),
              fluidRow(
                #making a box for all the inputs to divide the tab page into two sections one for inputs
                #and one for the outputs 
                box("Check the box if PLHIV estimate is known",
                    fluidRow(
                  #Input: PLHIV
                  useShinyjs(),
                  box(checkboxInput("plhivb", "PLHIV",FALSE), width = 3, solidHeader = TRUE, height = 40),
                  box(uiOutput(outputId = "numinput"), width = 3, solidHeader = TRUE, height = 75)
                ),status = "primary", width = 12, height = 130),
                
                box(tableOutput("hivcascade"),title = "HIV Cascade", solidHeader = TRUE, status = "primary"),
                box(tableOutput("hivcascadeduration"), title = "HIV Cascade - duration", solidHeader = TRUE, status = "primary"),
                box(plotlyOutput("hivcascadeplhiv"),title = "HIV Cascade", status = "primary", width = 10, solidHeader = TRUE))),
      
      ##fourth tab content "HCV" and organizing the different sections that will go in
      tabItem(tabName = "hcv",
              h2("HCV Treatment and Care Cascade"),
              fluidRow(
                #making a box for all the inputs to divide the tab page into two sections one for inputs
                #and one for the outputs
                box("Check boxes if numbers of Ab tests, Ab positive tests and RNA tests are known",
                  fluidRow(
                  #Input: Ab test, Ab positive, RNA test
                  useShinyjs(),
                  box(checkboxInput("abtestsb","Ab tests",FALSE),width = 4,solidHeader = TRUE,height = 40),
                  box(checkboxInput("abposb","Ab positive tests",FALSE),width = 4,solidHeader = TRUE,height = 40),
                  box(checkboxInput("rnatestsb","RNA test",FALSE),width = 4,solidHeader = TRUE,height = 40),
                  box(uiOutput(outputId = "abtestinput"),width = 4,solidHeader = TRUE, height = 75),
                  box(uiOutput(outputId = "abposinput"),width = 4,solidHeader = TRUE, height = 75),
                  box(uiOutput(outputId = "rnatestinput"),width = 4,solidHeader = TRUE, height = 75)), status = "primary", width = 12, height = 175),
                
                
                
                box(tableOutput("hcvcascade"),title = "HCV Cascade", solidHeader = TRUE, status = "primary"),
                box(tableOutput("hcvcascadeduration"),title = "HCV Cascade - duration", solidHeader = TRUE, status = "primary"),
                box(plotlyOutput("hcvcascaderetained"),width = 10,title = "HCV Cascade - retention in care", solidHeader = TRUE, status = "primary"),
                box(plotlyOutput("hcvcascadereinf"),width = 10,title = "HIV Cascade - reinfection", solidHeader = TRUE, status = "primary")
              ))  
    )))


durfunct <- function(time1, time2, units = "weeks", floor = TRUE) 
{
  calc.dur = interval(time1, time2) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.dur)))
  return(calc.dur)
}

#Define server 
server <- function(input, output){

  dtmaster <- reactive({
    req(input$dt_master)
    read.csv(input$dt_master$datapath, header = input$masterheader, 
             quote = input$masterquote, sep = input$mastersep)
  })
  
  ####################################################################################################################
  ###################### Demographics & risk factors
  
  demo <- reactive({dtmaster() %>% select(dob,gender,ethnicity, community,
                                       status, residence,sexual_orientation,
                                       smoking, alcohol,hiv, hcv)})
  n_demo <- reactive({nrow(demo())})
  
  
  output$demotable <- renderUI({
    
    dt_baseline <- as.data.frame(demo())
    
    dt_baseline$infection <- rep(NA,n_demo())
    for(i in 1:n_demo()) if (dt_baseline$hcv[i]==1 & dt_baseline$hiv[i] == 1){
      dt_baseline$infection[i] <- 3} else if (dt_baseline$hcv[i]==1){
        dt_baseline$infection[i] <- 2} else {
          dt_baseline$infection[i] <- 1}
    dt_baseline$infection <- factor(dt_baseline$infection,
                                    labels = c("Co-infection","Only HCV","Only HIV"), 
                                    levels = c(3,2,1))
    
    dt_baseline$status <- factor(dt_baseline$status, labels = c("Non-status","Status"), 
                                 levels = c(1, 0))
    dt_baseline$residence <- factor(dt_baseline$residence, 
                                    labels = c("On-reserve","Off-reserve"), levels = c(0,1))
    dt_baseline$gender <- factor(dt_baseline$gender, 
                                 labels = c("Male","Female"), levels = c(1,0))
    dt_baseline$smoking <- factor(dt_baseline$smoking, 
                                  labels = c("Smoker","Non-smoker"), levels = c(1,0))
    
    
    dt_baseline$dob <- ymd(dt_baseline$dob)
    dt_baseline$age <- dt_baseline$dob %--% today() 
    dt_baseline$age <- floor(as.duration(dt_baseline$age)/dyears(1))
    
   
    label(dt_baseline$ethnicity) <- "Ethnicity"
    label(dt_baseline$community) <- "Community"
    label(dt_baseline$status) <- "Status"
    label(dt_baseline$residence) <- "Residence"
    label(dt_baseline$gender) <- "Gender"
    label(dt_baseline$sexual_orientation) <- "Sexual Orientation"
    label(dt_baseline$smoking) <- "Smoking"
    label(dt_baseline$alcohol) <- "Alcohol"
    label(dt_baseline$age) <- "Age"
    
    units(dt_baseline$age) <- "years"
    
    
    
    labels <- list(
      variables = list(age= render.varlabel(dt_baseline$age),
                       gender = render.varlabel(dt_baseline$gender),
                       ethnicity = render.varlabel(dt_baseline$ethnicity),
                       community = render.varlabel(dt_baseline$community),
                       status = render.varlabel(dt_baseline$status),
                       residence = render.varlabel(dt_baseline$residence),
                       sexual_orientation = render.varlabel(dt_baseline$sexual_orientation),
                       smoking = render.varlabel(dt_baseline$smoking),
                       alcohol = render.varlabel(dt_baseline$alcohol)),
      groups = list("", "Infection"))
    
    strata <- c(list(Total=dt_baseline), split(dt_baseline, dt_baseline$infection))
    
    
    my.render.cont <- function(x) {
      with(
        stats.apply.rounding (stats.default(x), digits = 2),
        c("","Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD))
      )
    }
    my.render.cat <- function(x) {
      c("", sapply(stats.default(x,useNA = NULL), function(y) 
        with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))
    }
    
    print(table1(strata, labels, groupspan=c(1, 3),
           render.continuous=my.render.cont, render.categorical=my.render.cat))
    }) 
  ##################################################################
  ######risk factors
  
  #####co
  n_co <- reactive(nrow(dtmaster()[dtmaster()$hiv ==1 & dtmaster()$hcv ==1 ,]))
  
  output$riskfactorsco <- renderTable({
    dt_risk_co <- dtmaster() %>% filter(dtmaster()$hiv == 1 & dtmaster()$hcv ==1) %>% 
      select(idu,heterosexual,msm,needle_injury,blood_products,methadone, suboxone)
    
    risk_co <- dt_risk_co %>% summarize(IDU = sum(idu, na.rm = T),
                                          "Heterosexual Transmission" = sum(heterosexual, na.rm = T),
                                          MSM = sum(msm, na.rm = T),
                                          "Needle Injury" = sum(needle_injury, na.rm = T),
                                          "Blood Products" = sum(blood_products, na.rm = T))
    
    risk_co <- as.data.frame(t(risk_co))
    colnames(risk_co) <- "Total"
    risk_co$Percentage <- round((risk_co$Total*100/n_co()),1)
    
    risk_co_na <- dt_risk_co %>% summarize(IDU = sum(is.na(idu)),
                                             "Heterosexual Transmission" = sum(is.na(heterosexual)),
                                             MSM = sum(is.na(msm)),
                                             "Needle Injury" = sum(is.na(needle_injury)),
                                             "Blood Products" = sum(is.na(blood_products)))
    risk_co_na <- as.data.frame(t(risk_co_na))
    colnames(risk_co_na) <- c("Missing")
    risk_co$`Missing` <- risk_co_na$`Missing`       
    
    risk_co <- risk_co %>% select(Total,`Percentage`,`Missing`)
  },include.rownames = TRUE)
  
  ###### hcv
  
  n_hcv <- reactive(nrow(dtmaster()[dtmaster()$hiv ==0 & dtmaster()$hcv ==1 ,]))
  
  output$riskfactorshcv <- renderTable({
    dt_risk_hcv <- dtmaster() %>% filter(dtmaster()$hiv == 0 & dtmaster()$hcv ==1) %>% 
      select(idu,heterosexual,msm,needle_injury,blood_products,methadone, suboxone)
    
    risk_hcv <- dt_risk_hcv %>% summarize(IDU = sum(idu, na.rm = T),
                                        "Heterosexual Transmission" = sum(heterosexual, na.rm = T),
                                        MSM = sum(msm, na.rm = T),
                                        "Needle Injury" = sum(needle_injury, na.rm = T),
                                        "Blood Products" = sum(blood_products, na.rm = T))
    
    risk_hcv <- as.data.frame(t(risk_hcv))
    colnames(risk_hcv) <- "Total"
    risk_hcv$Percentage <- round((risk_hcv$Total*100/n_hcv()),1)
    
    risk_hcv_na <- dt_risk_hcv %>% summarize(IDU = sum(is.na(idu)),
                                           "Heterosexual Transmission" = sum(is.na(heterosexual)),
                                           MSM = sum(is.na(msm)),
                                           "Needle Injury" = sum(is.na(needle_injury)),
                                           "Blood Products" = sum(is.na(blood_products)))
    risk_hcv_na <- as.data.frame(t(risk_hcv_na))
    colnames(risk_hcv_na) <- c("Missing")
    risk_hcv$`Missing` <- risk_hcv_na$`Missing`       
    
    risk_hcv <- risk_hcv %>% select(Total,`Percentage`,`Missing`)
  },include.rownames = TRUE)
  
  ###### hiv
  
  n_hiv <- reactive(nrow(dtmaster()[dtmaster()$hiv ==1 & dtmaster()$hcv ==0 ,]))
  
  output$riskfactorshiv <- renderTable({
    dt_risk_hiv <- dtmaster() %>% filter(dtmaster()$hiv == 1 & dtmaster()$hcv ==0) %>% 
            select(idu,heterosexual,msm,needle_injury,blood_products,methadone, suboxone)
    
    risk_hiv <- dt_risk_hiv %>% summarize(IDU = sum(idu, na.rm = T),
                                          "Heterosexual Transmission" = sum(heterosexual, na.rm = T),
                                          MSM = sum(msm, na.rm = T),
                                          "Needle Injury" = sum(needle_injury, na.rm = T),
                                          "Blood Products" = sum(blood_products, na.rm = T))
    
    risk_hiv <- as.data.frame(t(risk_hiv))
    colnames(risk_hiv) <- "Total"
    risk_hiv$Percentage <- round((risk_hiv$Total*100/n_hiv()),1)
    
    risk_hiv_na <- dt_risk_hiv %>% summarize(IDU = sum(is.na(idu)),
                                             "Heterosexual Transmission" = sum(is.na(heterosexual)),
                                             MSM = sum(is.na(msm)),
                                             "Needle Injury" = sum(is.na(needle_injury)),
                                             "Blood Products" = sum(is.na(blood_products)))
    risk_hiv_na <- as.data.frame(t(risk_hiv_na))
    colnames(risk_hiv_na) <- c("Missing")
    risk_hiv$`Missing` <- risk_hiv_na$`Missing`       
    
    risk_hiv <- risk_hiv %>% select(Total,`Percentage`,`Missing`)
  },include.rownames = TRUE)
  
  
  
  
  ####################################################################################################################
  ###################### HIV 
  plhivstatus <- c(1,2)
  numinput2 <- paste0("input", plhivstatus)
  
  
  output$numinput <- renderUI({
    
    num_inputs <- lapply(1:2, function(i){
      numericInput(inputId = numinput2[i], label = 'Number of PLHIV', 80, min = 1, max = 10000)})
    
    shinyjs::hidden(num_inputs)
  })
  
  observe({
    for (i in plhivstatus) {
      if (i %in% input$plhivb) {
        shinyjs::show(id = paste0("input", i))
      } else {
        shinyjs::hide(id = paste0("input", i))
      }  
    }
  })
  

  
  cascade_hiv <- reactive({dtmaster() %>% summarize("Diagnosed" = sum(hiv_posresult,na.rm = T),
                                                 "Linkage to care" = sum(linkagetocare_hiv,na.rm = T),
                                                 "ART initiation" = sum(art_ini,na.rm = T),
                                                 "VL suppression" = sum(vl_sup,na.rm = T),
                                                 "Retained in care" = sum(retained_care_hiv,na.rm = T))})
  
  cascade_hiv1 <- reactive({as.data.frame(t(cascade_hiv()))})
  
  
  
  cascade_hiv2p <- reactive({rbind(input$input1, cascade_hiv1())})
  Percentagep <- reactive({(round((cascade_hiv2p()$V1*100/cascade_hiv2p()$V1[1]),1))})
  cascade_hiv3p <- reactive({cbind(cascade_hiv2p(),Percentagep())})
  cascade_hiv4p <- reactive({cascade_hiv3p() %>% rename(Total = V1, Percentage = "Percentagep()")})
  
  Percentage <- reactive({(round((cascade_hiv1()$V1*100/cascade_hiv1()$V1[1]),1))}) 
  cascade_hiv3 <- reactive({cbind(cascade_hiv1(),Percentage())})
  cascade_hiv4 <- reactive({cascade_hiv3() %>% rename(Total = V1, Percentage = "Percentage()")})
  
  
  
  output$hivcascade <- renderTable({
    
    if (input$plhivb == 1) {
      cascade_hiv5 <- as.data.frame(cascade_hiv4p())
      rownames(cascade_hiv5) <- c("PLHIV","Diagnosed","Linkage to care","ART initiation","VL suppression","Retained in care")
    } else {
      cascade_hiv5 <- as.data.frame(cascade_hiv4())
      rownames(cascade_hiv5) <- c("Diagnosed","Linkage to care","ART initiation","VL suppression","Retained in care")        
    }
    
    cascade_hiv5},include.rownames = TRUE)
  
  
  output$hivcascadeplhiv <- renderPlotly({
    
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
  
#### hiv duration
  cascadedur_hiv <- reactive({dtmaster() %>% select(date_hiv_test,date_hiv_posresult,
                                                 date_linkagetocare_hiv,
                                                 date_art_ini,date_vl_sup)})
  
  
  
  test_diag <- reactive({durfunct(cascadedur_hiv()$date_hiv_test,cascadedur_hiv()$date_hiv_posresult, units = "weeks", floor = TRUE)})
  diag_link <- reactive({durfunct(cascadedur_hiv()$date_hiv_posresult,cascadedur_hiv()$date_linkagetocare_hiv, units = "weeks", floor = TRUE)})
  link_art <- reactive({durfunct(cascadedur_hiv()$date_linkagetocare_hiv, cascadedur_hiv()$date_art_ini ,units = "weeks", floor = TRUE)})
  
  cascadedur_hiv1 <- reactive({cbind(cascadedur_hiv(),test_diag(),diag_link(),link_art())})
  
  
  mean_cascadedur_hiv <- reactive({cascadedur_hiv1() %>% summarize("Testing to diagnosis" = round(mean(test_diag(),na.rm = T),1),
                                                                   "Diagnosis to Linkage to care" = round(mean(diag_link(), na.rm = T),1),
                                                                   "Linkage to care to ART initiation" = round(mean(link_art(), na.rm = T),1))})
  
  mean_cascadedur_hiv1 <- reactive({as.data.frame(t(mean_cascadedur_hiv()))})
  
  
  median_cascadedur_hiv <- reactive({cascadedur_hiv1() %>% summarize("Testing to diagnosis" = round(median(test_diag(),na.rm = T),1),
                                                                     "Diagnosis to Linkage to care" = round(median(diag_link(), na.rm = T),1),
                                                                     "Linkage to care to ART initiation" = round(median(link_art(), na.rm = T),1))})
  
  median_cascadedur_hiv1 <- reactive({as.data.frame(t(median_cascadedur_hiv()))})
  
  
  se_cascadedur_hiv <- reactive({cascadedur_hiv1() %>% summarize("Testing to diagnosis" = sd(test_diag(),na.rm = T)/sum(!is.na(test_diag())),
                                                                 "Diagnosis to Linkage to care" = sd(diag_link(), na.rm = T)/sum(!is.na(diag_link())),
                                                                 "Linkage to care to ART initiation" = sd(link_art(), na.rm = T)/sum(!is.na(link_art())))})
  
  se_cascadedur_hiv1 <- reactive({as.data.frame(t(se_cascadedur_hiv()))})
  n_duration_hiv <- reactive({nrow(se_cascadedur_hiv1())})
  
  
  
  funci <- function(n,p,v){
    
    v[1:n] <- qnorm(p = p, mean = mean_cascadedur_hiv1()$V1[1:n],
                    sd = se_cascadedur_hiv1()$V1[1:n])
  }
  
  ci95_hivcasdur <- reactive({rep(NA,n_duration_hiv())})
  ci95_hivcasdur_l <- reactive({funci(n_duration_hiv(),0.025,ci95_hivcasdur())})
  ci95_hivcasdur_u <- reactive({funci(n_duration_hiv(),0.975,ci95_hivcasdur())})
  
  ci95_hivcasdur1 <- reactive({paste("[",round(ci95_hivcasdur_l(),1),",",round(ci95_hivcasdur_u(),1),"]")})
  
  
  output$hivcascadeduration <- renderTable({
    duration_hiv <- cbind.data.frame(median_cascadedur_hiv1(),mean_cascadedur_hiv1(),ci95_hivcasdur1())
    colnames(duration_hiv) <- c("Median (weeks)","Mean (weeks)","95% CI for Mean (weeks)")
    duration_hiv},
    include.rownames = TRUE)
  
  ####################################################################################################################
  ###################### HCV
  
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
  
  
  
  
  cascade_hcv <- reactive({dtmaster() %>% summarize("Diagnosed" = sum(rna_posresult, na.rm = T),
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
      layout(title = "HCV Cascade - retained in care as an outcome", xaxis = list(title = ""), yaxis = list(title = "Percentage"))})
  
  
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
      layout(title = "HCV Cascade - reinfection as an outcome", xaxis = list(title = ""), yaxis = list(title = "Percentage"))})
  
  
  cascadedur_hcv <- reactive({dtmaster() %>% select(date_ab_ag_test,date_ab_ag_posresult,
                                                 date_rna_test, date_rna_posresult,
                                                 date_linkagetocare_hcv,
                                                 date_tx_ini_hcv, date_tx_comp_hcv, date_svr, date_reinfection)})
  
  
  
  abagtest_result <- reactive({durfunct(cascadedur_hcv()$date_ab_ag_test,cascadedur_hcv()$date_ab_ag_posresult, units = "weeks", floor = TRUE)})
  abagres_rnatest <- reactive({durfunct(cascadedur_hcv()$date_ab_ag_posresult,cascadedur_hcv()$date_rna_test, units = "weeks", floor = TRUE)})
  rnatest_diag <- reactive({durfunct(cascadedur_hcv()$date_rna_test,cascadedur_hcv()$date_rna_posresult, units = "weeks", floor = TRUE)})
  diag_link_hcv <- reactive({durfunct(cascadedur_hcv()$date_rna_posresult,cascadedur_hcv()$date_linkagetocare_hcv, units = "weeks", floor = TRUE)})
  link_txini <- reactive({durfunct(cascadedur_hcv()$date_linkagetocare_hcv,cascadedur_hcv()$date_tx_ini_hcv, units = "weeks", floor = TRUE)})
  txini_txcomp <- reactive({durfunct(cascadedur_hcv()$date_tx_ini_hcv,cascadedur_hcv()$date_tx_comp_hcv, units = "weeks", floor = TRUE)})
  txcomp_svr <- reactive({durfunct(cascadedur_hcv()$date_tx_comp_hcv,cascadedur_hcv()$date_svr, units = "weeks", floor = TRUE)})
  svr_reinfection <- reactive({durfunct(cascadedur_hcv()$date_svr,cascadedur_hcv()$date_reinfection, units = "weeks", floor = TRUE)})
  
  
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
  
  
  
}

# Run the application 
shinyApp(ui, server)
