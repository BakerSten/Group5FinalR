### R Library USED #######################################################################
####### Please kindly install them before you run the APP ################################
library(flexdashboard)
library(shiny)
library(tidyverse)
library(glue)
library(hms)
library(dplyr)
library(ggplot2)
library("shinydashboard")
library("dashboard") 
library(gridExtra)
library(readxl)
library(caret)
library(e1071)
set.seed(123)
library(scales)
library(dplyr)
library(tidyr)



##########################################################################################

######################################LOGISTIC REGRESSION MODEL Development ######################################
logisticReg <- function(){
  d <- read_excel("REventData.xlsx",sheet="Model",col_name=T)
  d <- as.data.frame(d)
  str(d)
  tr <- d[,2:8]
  names(tr) <- c("y","CustomerOrProspect","Far","InterestLevel","Industry","Position","YearsExperience")
  tr$y <- as.factor(tr$y)
  tr$CustomerOrProspect <- as.factor(tr$CustomerOrProspect)
  tr$Far <- as.factor(tr$Far)
  tr$InterestLevel <- factor(tr$InterestLevel,levels=c("Low","Neutral","High"),ordered=T)
  tr$Industry <- as.factor(tr$Industry)
  tr$Position <- as.factor(tr$Position)
  tr$YearsExperience <- as.factor(tr$YearsExperience)
  tr$HighInterest <- as.factor(ifelse(tr$InterestLevel=="High",1,0))
  tr$LowInterest <- as.factor(ifelse(tr$InterestLevel=="Low",1,0))
  
  # dataset used for modelling
  d <- tr[,c("y","CustomerOrProspect","Far","HighInterest","LowInterest")]
  
  # Create a 70/30 train/test set
  
  
  # identify records that will be used in the training set. Here we are doing a
  # 70/30 train-test split. You might modify this.
  inTrain <- createDataPartition(y = d$y,   # outcome variable
                                 p = .70,   # % of training data you want
                                 list = F)
  # create your partitions
  train <- d[inTrain,]  # training data set
  test <- d[-inTrain,]  # test data set
  
  # Using trainControl(), specify a 3-fold cross-validation design for a regression problem
  
  
  ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                       number=3,        # k number of times to do k-fold
                       classProbs = T,  # if you want probabilities
                       summaryFunction = twoClassSummary, # for classification
                       allowParallel=T)
  
  
  # train a logistic regession on down-sampled train set 
  logit <- train(y ~ .,                # model specification
                 data = train,         # train set used to build model
                 method = "glm",       # type of model you want to build
                 trControl = ctrl,     # how you want to learn
                 family = "binomial",  # specify the type of glm
                 metric = "ROC")       # performance measure
  
  ## save this model
  save(logit, file = "my_logit.rda")
  
  return(logit)}
# prepare the lositic regression function before kicking off the shiny app
logisticReg()

################ CALCULATION FUNCTIONS ----###############################################

# function to calculate total cost
calculate_wedding_cost <- function(fixed_cost, 
                                   variable_guest_cost, 
                                   guest_base, total_guests) {
  # calculate added guest cost
  variable_cost <- variable_guest_cost * (total_guests - guest_base)
  # add to fixed cost
  total_cost <- variable_cost + fixed_cost
  return(total_cost)
}

# function to calculate risk
calculate_risk <- function(budget, total_cost) {
  return(budget - total_cost)
}

# function to recommend action
calculate_recommendation <- function(risk) {
  recommendation <- ifelse(risk >= 0, "Invite All", "Invite Less")
  return(recommendation)
  
}
##########################PLOTTING FUNCTIONS##############################################

# get recommendation function


# SETUP ----

# set theme for plots
theme_set(theme_minimal(base_family = "Avenir"))

# The palette with grey:
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "green", "#F0E442", 
               "#0072B2", "orange", "#CC79A7")

# PLOT FUNCTIONS ----

# plot guest count distribution with 95% confidence interval
plot_guest_count <- function(simulation_tbl) {
  
  # generate 2.5 and 97.5 percentiles from simulation data
  ci_int_guest <- quantile(simulation_tbl$total_guests, p = c(0.025, .975))
  
  # create histogram plot of results
  p <- simulation_tbl %>%
    ggplot(aes(total_guests)) +
    geom_histogram(binwidth = 1, 
                   fill = cbPalette[6],
                   color = "white") +
    geom_hline(yintercept = 0, size = .5, colour="#333333") +
    geom_vline(xintercept = ci_int_guest[1], linetype = "dashed") +
    geom_vline(xintercept = ci_int_guest[2], linetype = "dashed") +
    labs(title = "Total Guest Count",
         subtitle = glue("95% Confidence Estimate: {ci_int_guest[1]} -",
                         "{ci_int_guest[2]} guests"),
         x = "Total Guests",
         y = "Simulation Trials",
         caption = "Source: Simulation Results")
  
  return(p) 
}


# plot cost distribution with 95% confidence interval
plot_cost <- function(simulation_tbl, variable_guest_cost = 125) {
  
  ci_int_cost <- quantile(simulation_tbl$total_cost, p = c(0.025, .975))
  
  # create histogram of results
  p <- simulation_tbl %>%
    ggplot(aes(total_cost)) +
    # histogram bin size should be equal to per guest variable cost
    geom_histogram(binwidth = variable_guest_cost,
                   fill = cbPalette[8],
                   color = "white") +
    geom_hline(yintercept = 0, size = .5, colour="#333333") +
    labs(title = "Total Guest Cost",
         subtitle = glue("95% Confidence Estimate: ${ci_int_cost[1]} -",
                         "${ci_int_cost[2]}"),
         x = "Total Cost",
         y = "Simulation Trials",
         caption = "Source: Simulation Results") +
    geom_vline(xintercept = ci_int_cost[1], linetype = "dashed") +
    geom_vline(xintercept = ci_int_cost[2], linetype = "dashed")
  
  return(p)
}


# plot the risk profile with probability of going over budget
plot_risk <- function(simulation_tbl, variable_guest_cost = 125) {
  
  ci_int_risk <- quantile(simulation_tbl$risk, p = c(0.025, .975))
  
  # risk profile
  p_over_budget <- simulation_tbl %>%
    summarise(p_val = mean(risk < 0))
  
  # plot risk profile with 95% confidence interval
  p <- simulation_tbl %>%
    # color histrogram bins based on positive/negative risk
    ggplot(aes(risk, fill = over_budget)) +
    # histogram bin size should be equal to per guest variable cost
    geom_histogram(binwidth = variable_guest_cost,
                   color = "white") +
    geom_hline(yintercept = 0, size = .5, colour = "#333333") +
    scale_fill_manual(values = c(cbPalette[4], cbPalette[5], cbPalette[7])) +
    labs(title = "Total Budget Risk",
         subtitle = glue("95% Confidence Estimate: ${ci_int_risk[1]} - ${ci_int_risk[2]} ",
                         "({p_over_budget * 100}% risk)"),
         x = "Net Risk",
         y = "Simulation Trials",
         caption = "Source: Simulation Results") +
    guides(fill = FALSE) +
    geom_vline(xintercept = ci_int_risk[1], linetype = "dashed") +
    geom_vline(xintercept = ci_int_risk[2], linetype = "dashed")
  
  # if there are no even budget outcomes
  if (length(unique(simulation_tbl$over_budget)) < 3) {
    p <- p +
      scale_fill_manual(values = c(cbPalette[4], cbPalette[7]))
  }
  
  return(p)
}

# plot the distribution of recommendation outcomes
plot_recommendation <- function(simulation_tbl, risk_tolerance) {
  
  # get recommendation for subtitle
  ovr_recommendation <- recommend(simulation_tbl, risk_tolerance)
  
  # build main plot
  p <- simulation_tbl %>%
    ggplot(aes(recommendation, fill = recommendation)) +
    # convert counts to percentages
    geom_bar(aes(y = (..count..) / sum(..count..))) + 
    scale_y_continuous(labels = function(x) glue("{round(x * 100, 2)}%")) +
    labs(
      title = "Recommendation Outcomes",
      subtitle = glue("Overall Recommendation: {ovr_recommendation}"),
      x = "",
      y = "Simulation Trials",
      caption = "Source: Simulation Results"
    ) +
    guides(fill = FALSE) +
    geom_hline(yintercept = 0, size = .5, colour = "#333333") +
    # add dashed line for risk tolernace
    geom_hline(yintercept = risk_tolerance, linetype = "dashed")
  
  # if risk tolerance is exceeded, color the Invite Less bar 
  if (mean(simulation_tbl$risk < 0) >= risk_tolerance) {
    p <- p + 
      scale_fill_manual(values = c(cbPalette[1], cbPalette[7]))
  } else {
    # else color the Invite All bar
    p <- p + 
      scale_fill_manual(values = c(cbPalette[4], cbPalette[1]))
  }
  
  return(p)
}

##########################SIMULATING FUNCTIONS##############################################


# SIMULATION FUNCTIONS ----

# function to return a sample of guests for a single trial
sample_guests <- function(n, p) {
  # sum of random sample of size n with prob p of attending
  count <- sum(rbinom(n, 1, p))
}

# a function to simulate k weddings of n guests with probability p[1] to p[2]
simulate_weddings <- function(k, n, p,
                              fixed_cost, 
                              variable_guest_cost, 
                              guest_base, budget) {
  
  total_guests  <- replicate(
    # for k trials...
    k, 
    # sample n guests of probability p_1 to p_2
    sample_guests(n, p = runif(1, min = p[1], max = p[2]))
  )
  
  df <- data.frame(total_guests) %>%
    as.tibble() %>%
    mutate(trial = row_number()) %>%
    # total cost 
    mutate(total_cost = calculate_wedding_cost(fixed_cost, 
                                               variable_guest_cost,
                                               guest_base, total_guests)
    ) %>%
    # total risk
    mutate(risk = calculate_risk(budget, total_cost)) %>%
    # budget indicator
    mutate(over_budget = case_when(
      risk < 0 ~ "Yes",
      risk == 0 ~ "Even",
      risk > 0 ~ "No"),
      # used for coloring risk profile
      over_budget = factor(over_budget, levels = c("No", "Even", "Yes"))
    ) %>%
    # recommendation
    mutate(recommendation = calculate_recommendation(risk)) %>%
    select(trial, everything())
  
  return(df)
}

# function to provide recommendation based on simulation outcomes
recommend <- function(simulation_tbl, risk_tolerance) {
  
  # percentage of simulations that go over budget
  risk_pct <- simulation_tbl %>%
    summarize(mean(risk < 0)) %>% 
    pull()
  
  # determine if risk percentage exceeds tolerance
  recommendation <- ifelse(risk_pct > risk_tolerance, "Invite Less", "Invite All")
  
  return(recommendation)
}


#########################  MAIN APP ##############################################


ui <- dashboardPage(
  dashboardHeader(title = "Planning Pal"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("App User Guide", tabName = "UserGuide"),
                menuItem("Sample Data Review", tabName = "SampleFiles"),
                menuItem("User Data File Input", tabName = "UserInputModel"),
                menuItem("Simulation Model", tabName = "SimulationModel"),
                menuItem("Acknowledgement", tabName = "Acknowledgement")
    )),
  dashboardBody(
    mainPanel(
      tabItems(
        tabItem(tabName = "UserGuide",
                h5("There are five tabs for this planning pal app."),
                h4("App User Guide:"),
                h5("This is the user manual of the app."),
                h4("Sample Data Review:"),
                h5("You can view and download the embedded datasets."),
                h5("One is 'User Input Sample File'. This is a sample of what you can upload to the 3rd tab for the app to analyze your data and make predictions/suggestions."),
                h5("Another one is 'Logistic Regression Model Training Dataset'. This is the training dataset we used to build the logistic regression model on predicting whether your invitee will come or not."), 
                h5("We used 3-fold cross validation on the model.The ROC for training/testing data is around 0.86."),
                h4("User Data File Input:"),
                h5("At the top of the User Data File Input tab, there is an option to load your own data from your survey. You can select if there are column names in the first column, what type of separator is used, and if there are quotes. If you do not have your own file, you can just click the 'Feed' button at the bottom, and it will use a pre-loaded dataset that you saw before."),
                h5("If you choose to load a file, it must follow a format with the columns in the below order. The indent is the possible inputs for the column, with all columns being strings. All records must be complete with valid answers in all columns, nulls or NA are not allowed:"),
                h5("1.	ID"),
                h5(HTML('&emsp;'),"a.	*You can use any you prefer here. Must be a valid string.*"),
                h5("2.	Customer vs Prospect"),
                h5(HTML('&emsp;'), "a.	Customer",HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"b.	Prospect"),
                h5("3.	Distance"),
                h5(HTML('&emsp;'), "a.	> 150km",HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"b.	< 150km"),
                h5("4.	Level of Interest"),
                h5(HTML('&emsp;'), "a.	High", HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'), "b.	Neutral",HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"c.	Low"),
                h5("5.	Industry"),
                h5(HTML('&emsp;'),"a.	*You can use any you prefer here. Must be a valid string.*"),
                h5("6.	Position"),
                h5(HTML('&emsp;'),"a.	*You can use any you prefer here. Must be a valid string.*"),
                h5("7.	Years Experience"),
                h5(HTML('&emsp;'),"a.	1-4",HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"b.	5-9",HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"c.	10-14",HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"d.	15-19",HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),"e.	20+"),
                h4("Simulation Model:"),
                h5("At the top left of the Simulation Model tab, the first input is your total budget in dollars. The second is a slider where you set your Risk Tolerance. This is the amount you are willing to go over if enough guests show up. Ex. Budget is $10k and Risk Tolerance is 20% means you are willing to go up to $12k. Next you will enter your Guest Base Cost. This is the amount you will pay that is fixed up to the amount of Guest Base Count, which you will set next. Following that you will set your Guest Base Count. This is how many guests you may have without incurring any additional cost over the fixed amount you set in Guest Base Cost. Next you will set the Total Guests Invited and the Guest Probability to Attend. You can base this off past experience or general industry numbers. "),
                h4("Acknowledgement:"),
                h5("We listed out the referencing materials we used to build our app.")
                
        ),
        tabItem(tabName = "SampleFiles",
                h3("Download Sample Files of the Underlying Datasets."),
                # Input: Choose dataset ----
                selectInput("dataset", "Choose a dataset:",
                            choices = c("User Input Sample File", "Logistic Regression Model Training Dataset")),
                
                # Button
                downloadButton("downloadData", "Download"),
                tableOutput("sampletable")
                ),
        tabItem(tabName = "UserInputModel",
                # App title ----
                titlePanel("Uploading Files"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Select a file ----
                    fileInput("file1", "Choose CSV / TXT File",
                              multiple = TRUE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv",
                                         ".txt")),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Input: Checkbox if file has header ----
                    checkboxInput("header", "Header", TRUE),
                    
                    # Input: Select separator ----
                    radioButtons("sep", "Separator",
                                 choices = c(Comma = ",",
                                             Semicolon = ";",
                                             Tab = "\t"),
                                 selected = ","),
                    
                    # Input: Select quotes ----
                    radioButtons("quote", "Quote",
                                 choices = c(None = "",
                                             "Double Quote" = '"',
                                             "Single Quote" = "'"),
                                 selected = '"'),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Input: Select number of rows to display ----
                    radioButtons("disp", "Display",
                                 choices = c(Head = "head"),
                                 selected = "head"),
                    # The run button
                    br(),
                    h2("Feed your CSV/TXT file into the logistic regression model and see potential guest profile."),
                    h5("If you just want to test out the functionality, you can click 'Feed' without uploading your file. The app will use it's underlying user input file as a demo to proceed."),
                    actionButton(inputId="feed", label="Feed")
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    # Output: Data file ----
                    tableOutput("contents"),
                    valueBoxOutput("ProptoAttend",width=NULL),
                    plotOutput(outputId = "Plot3",width="1000px",height="400px"),
                    plotOutput(outputId = "Plot4",width="1000px",height="400px"),
                    plotOutput(outputId = "Plot5",width="500px",height="400px")
                    
                  ))
        ),
        tabItem(tabName = "SimulationModel",
                sidebarLayout(
                  sidebarPanel(
                    # left-hand side user inputs
                    
                    numericInput("budget", "Total Budget: ", 30000, min = 0, max = 1e9, step = 100, width = NULL),
                    sliderInput("risk_tolerance", "Risk Tolerance: ", min = 0, max = 100, value = 20, step = 5, post  = " %"),
                    numericInput("fixed_cost", "Guest Base Cost: ", 22000, min = 0, max = 1e9, step = 100, width = NULL),
                    numericInput("guest_base", "Guest Base Count: ", 50, min = 0, max = 1e6, step = 1, width = NULL),
                    numericInput("var_cost", "Variable Guest Cost: ", 125, min = 0, max = 1000, step = NA, width = NULL),
                    numericInput("invited_guests", "Total Guests Invited: ", 150, min = 0, max = 1e6, step = NA, width = NULL),
                    sliderInput("guest_prob", "Guest Probability to Attend: ", min = 0, max = 100, value = c(60, 85), step = 5, post  = " %"),
                    actionButton("run", "Run Model")),
                  
                  # right-hand side top bar statistics - dynamic infoBoxes
                  mainPanel(responsive=FALSE,
                            column(4,div(style="display:inline-block;vertical-align:top;height=40px;",valueBoxOutput("Recom",width=NULL))),
                            column(4,div(style="display:inline-block;vertical-align:top;height=40px;",valueBoxOutput("RiskPorb",width=NULL))),
                            column(4,div(style="display:inline-block;vertical-align:top;height=40px;",valueBoxOutput("RiskPoten",width=NULL))),
                            br(),
                            
                            plotOutput(outputId = "Plot1",width="1000px",height="400px"),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            plotOutput(outputId = "Plot2",width="1000px",height="400px"))
                  
                )
        ),
        tabItem(tabName = "Acknowledgement",
                h4("Acknowledgement"),
                h5("The 'Simulation Model' tab of our App referenced the code from another shiny App called Wedding Risk Modelling App, which was written in R Markdown. We have translate the original code into R so the dashboard may look slight different from the original app."),
                h5("App Link:", a("https://bclark.shinyapps.io/WeddingRiskModel_App/", href="https://bclark.shinyapps.io/WeddingRiskModel_App/")),
                h5("Code Link:", a("https://github.com/bclark86/WeddingRiskModel", href="https://github.com/bclark86/WeddingRiskModel")),
                h5("The dashboard structure of our App referenced some code from another shiny App called Fifa-Search-Engine"),
                h5("App Link:", a("https://escape-sequence.shinyapps.io/Rfinalproject/", href="https://escape-sequence.shinyapps.io/Rfinalproject/")),
                h5("Code Link:", a("https://github.com/ruthvik07071995/R-Shiny-App---Fifa-Search-Engine", href="https://github.com/ruthvik07071995/R-Shiny-App---Fifa-Search-Engine")),
                h5("The Download File function of our App referenced the code from ",a("https://shiny.rstudio.com/articles/download.html", href="https://shiny.rstudio.com/articles/download.html")),
                h5("The Upload File function of our app referenced the code from ",a("https://shiny.rstudio.com/articles/upload.html", href="https://shiny.rstudio.com/articles/upload.html"))
      ))))
)


server <- function(input, output) {
  
  ############ USER Input TAB ########################################
  # Reactive value for selected dataset ----
  
  datasetInput <- reactive({
    switch(input$dataset,
           "User Input Sample File" = as.data.frame(read_excel("REventData.xlsx",sheet="UserInput",col_name=T)),
           "Logistic Regression Model Training Dataset" = as.data.frame(read_excel("REventData.xlsx",sheet="Model",col_name=T)))
  })
  
  # Table of selected dataset ----
  output$sampletable <- renderTable({
    head(datasetInput())
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
  ############ USER INPUT FILE TAB ####################################
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  # du: USER INPUT DATASET
  du <- reactive({
    
    input$feed
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, this will feed into the logistic regression
    print(is.null(input$file1))
    if (is.null(input$file1) == FALSE){
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      du <- as.data.frame(df)}
    
    # If user doesn't input any dataset
    else {df <- read_excel("REventData.xlsx",sheet="UserInput",col_name=T)
    du <- as.data.frame(df)}
    
    print(str(du))
    # pre-processing
    te <- du[,2:7]
    names(te) <- c("CustomerOrProspect","Far","InterestLevel","Industry","Position","YearsExperience")
    te$CustomerOrProspect <- as.factor(te$CustomerOrProspect)
    te$Far <- as.factor(te$Far)
    te$InterestLevel <- factor(te$InterestLevel,levels=c("Low","Neutral","High"),ordered=T)
    te$Industry <- as.factor(te$Industry)
    te$Position <- as.factor(te$Position)
    te$YearsExperience <- as.factor(te$YearsExperience)
    te$HighInterest <- as.factor(ifelse(te$InterestLevel=="High",1,0))
    te$LowInterest <- as.factor(ifelse(te$InterestLevel=="Low",1,0))
    
    # load the saved model
    load("my_logit.rda")
    
    # identify potential guests using the logistic regression model
    estU <- predict(logit, newdata=te)
    te$estY <- ifelse(estU=="Y",1,0) 
    
    dufinal <- te
    
  })
  
  
  ####  OUPUT
  ### Guest probability to attend based on logistic regression model
  observeEvent(input$feed,{
    output$ProptoAttend <- renderValueBox({
      
      valueBox(paste(round(sum(du()$estY)/length(du()$estY)*100,0),"%"),
               subtitle="Guest probability to attend based on logistic regression model"
      )
      
    })
    
    g <- reactive({
      PotentialCustomerProfDf <- du()[du()$estY == 1,]
    })
    
    pt11 <- reactive({
      
      p <- isolate(
        # guest type chart
        ggplot(data=g(), aes(x=CustomerOrProspect,fill=CustomerOrProspect)) + geom_bar() + labs(title = "Potential Guests Identified by the Logistic Regression Model",
                                                                                                subtitle = " - Existing vs Potential Customers",
                                                                                                x = "",
                                                                                                y = "total number of guests",
                                                                                                caption = "Source: User-Input File")
      )
      
      print(p)
    })
    
    pt12 <- reactive({
      # guest distance chart
      p <- isolate(
        ggplot(data=g(), aes(x=Far,fill=Far)) + geom_bar() + labs(title = "Potential Guests Identified by the Logistic Regression Model",
                                                                  subtitle = " - Potential Guests' Travel Distance to the Conference",
                                                                  x = "",
                                                                  y = "total number of guests",
                                                                  caption = "Source: User-Input File")
        
      )
      
      print(p)
    })
    
    pt13 <- reactive({
      
      ## set the levels in order we want
      theTable <- within(g(), 
                         Industry <- factor(Industry, 
                                            levels=names(sort(table(Industry), 
                                                              decreasing=TRUE))))
      # Industry of guests
      p <- isolate(
        
        ggplot(data=theTable, aes(x=Industry,fill=Industry)) + geom_bar() + theme_void() + coord_polar("y", start=0) + labs(title = "Potential Guests Identified by the Logistic Regression Model",
                                                                                                                            subtitle = " - Industries of Potential Guests",
                                                                                                                            x = "",
                                                                                                                            y = "total number of guests",
                                                                                                                            caption = "Source: User-Input File")
      )
      
      print(p)
    })
    
    pt14 <- reactive({
      
      p <- isolate(
        # Position of guests
        ggplot(data=g(), aes(x="", y=Position, fill=Position)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + 
          labs(title = "Potential Guests Identified by the Logistic Regression Model", subtitle = " - Positions of Potential Guests",caption = "Source: User-Input File") +
          scale_fill_brewer() + 
          theme_void() +
          theme(axis.text.x=element_blank())
      )
      
      print(p)
    })
    
    
    pt15 <- reactive({
      
      # prepare the summary table
      g1 <- g() %>%
        group_by(Industry,YearsExperience) %>%
        summarize(sum(estY))
      
      g1 <- as.data.frame(g1)
      g1$YearsExperience <- factor(g1$YearsExperience,levels=c("1-4","5-9","10-14","15-19","20+"),ordered=T)
      names(g1) <- c("Industry","YearsExperience","numb")
      
      p <- isolate(
        # Experience of guests
        ggplot(g1, aes(x = Industry, y = numb, fill = YearsExperience)) + 
          geom_bar(position = "fill",stat = "identity") +
          scale_y_continuous(labels = scales::percent_format())+ labs(title = "Potential Guests Identified by the Logistic Regression Model",
                                                                      subtitle = " - Experience of Potential Guests by Industry",
                                                                      x = "Industry",
                                                                      y = "proportion of guests",
                                                                      caption = "Source: User-Input File")
      )
      
      print(p)
    })
    
    
    output$Plot3 <- renderPlot({
      ptlist <- list(pt11(),pt12())
      # remove the null plots from ptlist
      to_delete <- !sapply(ptlist,is.null)
      ptlist <- ptlist[to_delete]
      p <- grid.arrange(grobs=ptlist,ncol=length(ptlist))
      print(p)
    })
    
    output$Plot4 <- renderPlot({
      ptlist <- list(pt13(),pt14())
      # remove the null plots from ptlist
      to_delete <- !sapply(ptlist,is.null)
      ptlist <- ptlist[to_delete]
      p <- grid.arrange(grobs=ptlist,ncol=length(ptlist))
      print(p)
    })
    
    output$Plot5 <- renderPlot({
      ptlist <- list(pt15())
      # remove the null plots from ptlist
      to_delete <- !sapply(ptlist,is.null)
      ptlist <- ptlist[to_delete]
      p <- grid.arrange(grobs=ptlist,ncol=length(ptlist))
      print(p)
    })
    
  })
  
  
  ############ SIMULATION MODEL TAB ###################################
  # generate simulated values
  simulation_tbl <- reactive({
    
    input$run
    
    sim_tbl <- isolate(simulate_weddings(
      k = 10000,
      n = input$invited_guests,
      p = c((input$guest_prob[1] / 100), (input$guest_prob[2] / 100)),
      fixed_cost = input$fixed_cost, 
      variable_guest_cost = input$var_cost, 
      guest_base = input$guest_base, 
      budget = input$budget
    )
    )
    
  })
  
  
  # get recommendation
  recommendation <- reactive({
    
    input$run
    
    rec <- isolate(recommend(simulation_tbl(),
                             (input$risk_tolerance / 100))
    )
    
  })
  
  # summarize outcomes
  risk_pct <- reactive({
    
    input$run
    
    risk <- isolate(mean(simulation_tbl()$risk < 0))
    
  })
  
  risk_ci <- reactive({
    
    input$run
    
    ci_int_risk <- isolate(
      quantile(simulation_tbl()$risk, p = c(0.025, .975)) %>%
        round(0)
    )
    
  })
  
  ########   OUPUT  ###########
  
  ### Recommendation
  output$Recom <- renderValueBox({
    
    valueBox(recommendation(),
             subtitle="Recommendation",
             color = ifelse(risk_pct() > (input$risk_tolerance / 100),
                            cbPalette[7], cbPalette[4])
    )
    
  })
  
  ### Risk Probability
  output$RiskPorb <- renderValueBox({
    
    valueBox(paste0(risk_pct() * 100, "%"),
             subtitle="Risk Probability",
             color = ifelse(risk_pct() > (input$risk_tolerance / 100),
                            cbPalette[7], cbPalette[4])
    )
    
  })
  
  ### Risk Potential
  output$RiskPoten <- renderValueBox({
    
    valueBox(
      paste0("$", risk_ci()[1], " to ", "$", risk_ci()[2]),
      subtitle="Risk Potential",
      color = ifelse(risk_pct() > (input$risk_tolerance / 100),
                     cbPalette[7], cbPalette[4])
    )
    
  })
  
  ### Recommendation Summary
  pt1 <- reactive({
    input$run
    
    p <- isolate(plot_recommendation(simulation_tbl(), (input$risk_tolerance / 100)))
    
    print(p)
  })
  
  ### Risk Profile
  pt2 <- reactive({
    input$run
    
    p <- isolate(plot_risk(simulation_tbl(), input$var_cost))
    
    print(p)
  })
  
  ### Guest Count Profile
  pt3 <- reactive({
    input$run
    
    p <- isolate(plot_guest_count(simulation_tbl()))
    
    print(p)
  })
  
  ### Cost Profile
  pt4 <- reactive({
    input$run
    
    p <- isolate(plot_cost(simulation_tbl(), input$var_cost))
    
    print(p)
  })
  
  ## OUTPUT PLOTS
  output$Plot1 <- renderPlot({
    ptlist <- list(pt1(),pt2())
    # remove the null plots from ptlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete]
    p <- grid.arrange(grobs=ptlist,ncol=length(ptlist))
    print(p)
    
    
  })
  
  
  output$Plot2 <- renderPlot({
    
    ptlist <- list(pt3(),pt4())
    # remove the null plots from ptlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete]
    p <- grid.arrange(grobs=ptlist,ncol=length(ptlist))
    print(p)
    
  })
  
}

shinyApp(ui = ui, server = server)



