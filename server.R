library(gdata)
library(ggplot2)
theme_set(theme_grey(base_size = 22))

lt2vec <- function(LIST, sort1st = F) {
  max_length <- max(sapply(as.character(unlist(LIST)), nchar))
  max_length_str <- paste0("%", as.character(max_length), "s")
  sapply(LIST,
         function(x) {
           paste0(
             "(",
             if (!sort1st) {
               paste(sprintf(max_length_str, as.character(x)), collapse = ",")
             } else {
               paste(sprintf(max_length_str, as.character(sort(x))), collapse = ",")
             },
             ")"
           )
         }
  )
}

shinyServer(function(input, output, session) {
  
  ###### Declaring editable reactive values ###########################
  values <- reactiveValues()
  values$lt_Drawn <- list()
  
  ###### renderUI #######################################
  output$Out_SetProb <- renderUI({
    if (input$In_equiprobable == "custom") {
      textInput("In_Prob", "Enter probability for each of the above")
    } else if (input$In_equiprobable == "equi") {
      conditionalPanel(
        condition = "false",
        textInput("In_Prob", "probability unused", paste0("rep(1,", length(pmf_object()),")"))
      )
    }
  })
  
  output$Out_StopDrawCond <- renderUI({
    list(
      fluidRow(
        column(width = 4,
               selectInput("In_CondOnWhich", "When", as.list(pmf_object()))
        ),
        column(width = 8,
               numericInput("In_nStop",
                            "appeared this number of time",
                            value = 1, step = 1)
        )
      )
    )
  })
  
  # Alter the In_Set_CountMethod button depend on geomtric/binomial#
  observe({
    input$Btn_Simulate
    if (input$In_SizeType == "fixed") {
      updateRadioButtons(session, "In_Set_CountMethod",
                         choices = list(`Count all different outcomes appeared` = "All",
                                        `Count different types of combinations` = "Combination",
                                        `Count the number of a certain outcome in a sample (Plot shown in the next tab)` = "Binomial"),
                         selected = "Binomial")
    } else if (input$In_SizeType == "keepdrawing") {
      updateRadioButtons(session, "In_Set_CountMethod",
                         choices = list(`Count all different outcomes appeared` = "All",
                                        `Count the number of draw in each sample (Plot shown in the next tab)` = "Geometric"),
                         selected = "Geometric")
    }
  })
  
  output$Out_SelectBinomialVar <- renderUI({
    if (input$In_Set_CountMethod == "Binomial") {
      selectInput("In_BinomialVar", "Variable to count",
                  as.list(pmf_object()))
    }
  })
  
  output$In_SetPMF <- renderUI({
    if (input$In_pmfType == "Select from below") {
      selectizeInput("In_PresetedPMF", "Sample from below",
                     choices = list(
                       Coin = c("Head", "Tail"),
                       Bernoulli = 0:1,
                       Dice = c(1, 2, 3, 4, 5, 6),
                       Color = c("Black", "Blue", "Brown", "Green", "Orange", "Purple", "Red", "Yellow", "Violet", "White")
                     ),
                     multiple = T
      )
    } else if (input$In_pmfType == "Custom Input") {
      textInput("In_CustomPMF", "Input a list of choices to sample from")
    }
  })
  
  
  ###### reactive ###############################################
  pmf_object <- reactive({
    # The 1st if is to avoid error at first launch due to selectize
    # input in In_PresetedPMF with no default
    #     if (!is.null(input$In_PresetedPMF) | !is.null(input$In_CustomPMF)) {
    if (is.null(input$In_PresetedPMF) & is.null(input$In_CustomPMF)) {
      c(1)
    } else if (input$In_pmfType == "Select from below") {
      input$In_PresetedPMF
    } else if (input$In_pmfType == "Custom Input") {
      eval(parse( text = paste0("c(", input$In_CustomPMF, ")") ))
    }
  })
  
  Prob <- reactive({
    eval(parse(text = paste0("c(", input$In_Prob, ")")))
  })
  
  # Create values$lt_Drawn, the list of sampled data #
  observe({
    input$Btn_Simulate
    isolate({
      # The 1st if is to avoid length of Prob != length of object to sample
      if (length(Prob()) == length(pmf_object())) {
        values$lt_Drawn <-
          append(values$lt_Drawn,
                 replicate(
                   n = input$In_mCopy, 
                   if (input$In_SizeType == "fixed") {
                     sample(pmf_object(), input$In_size, replace = T, prob = Prob())
                   } else if (input$In_SizeType == "keepdrawing") {
                     Drawn <- sample(pmf_object(), 1, prob = Prob())
                     counter <- 1
                     appeared <- ifelse(Drawn == input$In_CondOnWhich, 1, 0)
                     while (appeared < input$In_nStop & counter < 1000) {
                       Drawn <- append(Drawn, sample(pmf_object(), 1, prob = Prob()))
                       appeared <- ifelse(Drawn[length(Drawn)] == input$In_CondOnWhich,
                                          appeared +1, appeared)
                       counter <- counter + 1
                     }
                     Drawn
                   }, simplify = F
                 )
          )
      }
    })
  })
  
  N_Exp <- reactive({
    length(values$lt_Drawn)
  })
  
  # Create the ready-to-output table on count tab #
  df_Drawn <- reactive({
    if (input$In_SizeType == "fixed") {
      vec_Drawn <- if (input$In_Set_CountMethod == "All") {
        lt2vec(LIST = values$lt_Drawn, sort1st = F)
      } else if (input$In_Set_CountMethod == "Combination") {
        lt2vec(LIST = values$lt_Drawn, sort1st = T)
      } else if (input$In_Set_CountMethod == "Binomial") {
        sapply(values$lt_Drawn,
               function (x) sum(x == input$In_BinomialVar))
      }
      tmp_df_Drawn <- setNames(as.data.frame(table(vec_Drawn)),
                               c("Outcome", "Count"))
    } else if (input$In_SizeType == "keepdrawing") {
      if (input$In_Set_CountMethod == "All") {
        vec_Drawn <- lt2vec(LIST = values$lt_Drawn, sort1st = F)
        tmp_df_Drawn <- setNames(as.data.frame(table(vec_Drawn)),
                                 c("Outcome", "Count"))
        tmp_df_Drawn <- tmp_df_Drawn[order(sapply(as.character(tmp_df_Drawn$Outcome),
                                  nchar)),]
      } else if (input$In_Set_CountMethod == "Geometric") {
        tmp_df_Drawn <- setNames(as.data.frame(table(sapply(values$lt_Drawn, length))),
                                 c("Outcome", "Count"))
      }
    }
    tmp_df_Drawn
  })
  
  # Make the clear button effective #
  observe({
    input$Btn_clear
    isolate(
      values$lt_Drawn <- list()
    )
  })
  
  P1 <- reactive({
    if (input$In_Set_CountMethod %in% c("Binomial", "Geometric")) {
      tmp.P1 <-
        ggplot(df_Drawn(), aes(x = as.numeric(as.character(Outcome)))) +
        geom_linerange(aes(ymin = 0, color = Count), size = 6) +
        guides(color = F) +
        if (input$In_P1_yaxis == "Count") {
          aes(ymax = Count)
        } else {
          aes(ymax = Count / sum(Count))
        }
      
      if (input$In_Set_CountMethod == "Binomial") {
        tmp.P1 <- tmp.P1 +
          labs(x = paste0("Number of ", input$In_BinomialVar),
               y = input$In_P1_yaxis)
      } else {
        tmp.P1 <- tmp.P1 +
          labs(x = sprintf("Number of draws until %s appears %i times",
                           input$In_CondOnWhich, input$In_nStop),
               y = input$In_P1_yaxis)
      }
      return(tmp.P1)
    }
  })
  
  data_stat <- reactive({
    sapply(values$lt_Drawn, function(x) get(input$In_Stat)(as.numeric(x)))
  })
  
  P2 <- reactive({
    StatMean <- mean(data_stat())
    tmp.str <- paste0("data.frame(", input$In_Stat, " = data_stat() )" )
    df_data_stat <- eval(parse(text = tmp.str))
    
    tmp.str2 <- paste0("ggplot(df_data_stat, aes(x = ", input$In_Stat,"))")
    tmp.P2 <- eval(parse(text = tmp.str2))
    
    tmp.P2a <- if (input$In_P2_yaxis == "Count") {
      geom_histogram(aes(fill = ..count..),
                     binwidth = (max(data_stat()) - min(data_stat()))/8,
                     color = "white")
    } else {
      geom_histogram(aes(y = ..density.., fill = ..density..),
                     binwidth = (max(data_stat()) - min(data_stat()))/8,
                     color = "white")
    }
    
    tmp.P2b <- geom_vline(xintercept = StatMean, color = "red", size = 1)
    
    tmp.P2c <- geom_text(aes_string(x = StatMean, y = 0,
                                    label = sprintf("%4.2f", StatMean)),
                         vjust = -1, hjust = -0.1, 
                         size = 10, color = "white")
    
    if (input$In_P2_yaxis == "Count") {
      tmp.P2 + tmp.P2a + tmp.P2b + tmp.P2c + guides(fill = F)
    } else {
      tmp.P2 + tmp.P2a + tmp.P2b + tmp.P2c +
        geom_density(color = "salmon", linetype = "dotted", size = 1) +
        guides(fill = F)
    }
  })
  
  ###### render Outcome (except UI) ###########################
  output$Out_AllOutcome <- renderPrint({
    input$Btn_Simulate
    input$Btn_clear
    input$Btn_SwapOutput
    isolate(
      if (input$Btn_SwapOutput %% 3 == 0) {
        values$lt_Drawn
      } else if (input$Btn_SwapOutput %% 3 == 1) {
        tmp <- Reduce(cbindX, lapply(values$lt_Drawn, as.matrix))
        ifelse(is.na(tmp), "", tmp )
      } else if (input$Btn_SwapOutput %% 3 == 2) {
        tmp <- Reduce(cbindX, lapply(values$lt_Drawn, as.matrix))
        tmp <- ifelse(is.na(tmp), "", tmp )
        t(tmp)
      }
    )
  })
  output$Out_df_Drawn <- renderTable(df_Drawn())
  output$out_P1 <- renderPlot(P1())
  output$data_stat <- renderPrint(data_stat())
  output$out_P2 <- renderPlot(P2())
  output$out_N_Exp <- renderText(N_Exp())
  
  ####### message dump to std output ########
  observe(cat(input$In_Set_CountMethod, "\n"))
  observe(cat(input$In_Stat, "\n"))
})














