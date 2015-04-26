shinyUI(fluidPage(
  titlePanel("Sampling Simulator"),  
  sidebarLayout(
    
    sidebarPanel(
      wellPanel(
        radioButtons("In_pmfType", "Draw sample from the following",
                     list("Select from below", "Custom Input"), inline = T),
        uiOutput("In_SetPMF")
      ),
      
      wellPanel(
        radioButtons("In_equiprobable", "Probability for each of the above to be sampled",
                     list(`Equally probable`= "equi", `Specify the probability of each being sampled` = "custom" )),
        uiOutput("Out_SetProb")
      ),
      
      wellPanel(
        radioButtons("In_SizeType", "Method of sampling",
                     list(`Fixed sample size` = "fixed",
                          `Keep sampling until...` = "keepdrawing"),
                     inline = T),
        conditionalPanel(
          condition = "input.In_SizeType == 'fixed'",
          numericInput("In_size", "Sample size", 1, step = 1, max = 100)
        ),
        conditionalPanel(
          condition = "input.In_SizeType == 'keepdrawing'",
          uiOutput("Out_StopDrawCond")
        )
      ),
      
      numericInput("In_mCopy", "Number of samples to produce", 1, step = 1, max = 5000),
      
      actionButton("Btn_Simulate", "Sample Now!!"),
      actionButton("Btn_clear", "Clear Results")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Results",
                 br(),
                 actionButton("Btn_SwapOutput", "Change output format"),
                 verbatimTextOutput("Out_AllOutcome")
        ),
        tabPanel("Counts",
                 br(),
                 fluidRow(
                   column(6,
                          radioButtons(
                            "In_Set_CountMethod", "What to count",
                            list(`Count all different outcomes appeared` = "All",
                                 `Count the number of draw in each sample` = "Geometric"))
                   ),
                   column(6,
                          strong("No. of samples drawn so far:"),
                          textOutput("out_N_Exp"),
                          br(),
                          uiOutput("Out_SelectBinomialVar")
                   )
                 ),
                 tableOutput("Out_df_Drawn")
        ),
        tabPanel("Plot",
                 br(),
                 radioButtons("In_P1_yaxis", "Select y-axis",
                              list("Count", "Probability"), inline = T),
                 plotOutput("out_P1")
        ),
        tabPanel("Sample Statistics",
                 br(),
                 fluidRow(
                   column(6,
                          selectInput("In_Stat", "Select sample statistics to calculate",
                                      list(`mean` = "mean", `median` = "median",
                                           `variance` = "var", `standard deviation` = "sd",
                                           `minimum` = "min", `maximum` = "max"))
                   ),
                   column(6,
                          checkboxInput("In_ShowStatValues", "Show values", T),
                          checkboxInput("In_ShowStatGraph", "Show graph", F)
                   )
                 ),
                 conditionalPanel(
                   condition = "input.In_ShowStatValues == true",
                   verbatimTextOutput("data_stat")
                 ),
                 conditionalPanel(
                   condition = "input.In_ShowStatGraph == true",
                   radioButtons("In_P2_yaxis", "Select y-axis",
                                list("Count", "Density"), inline = T),
                   plotOutput("out_P2")
                 )
                 
        )
      )
    )
  ),
  p(h4("This app can simulate various types of random sampling from a
       specified list of objects. The", strong("sample from below"),
"drop-down list contains some commonly used population to choose from.
       Alternatively, you can enter a comma-separated list of objects by
       selecting", strong('custom input'), ". By default, each of the objects to be
       sampled are equally probable, but you can also specify them by
       checking", strong('Specify the probability of each being sampled')," and then
       enter a list of comma-separated probabilities. There are two types
       of sampling method, either sample a fixed number of times from the 
       list specified above or keep sampling until a certain element
       appears a number of times you want. You can specify this in the
       part", strong('Method of sampling'), ". Lastly, you can repeat this sampling
       procedure any number of time (but preferably less than 2000 or 
       else the response could become slow) and the results will be
       shown on the right."))
))