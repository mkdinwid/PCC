# server.r
# By Matt Dinwiddie
# 9/04/2023

# Load needed libraries
library(shiny)
library(shinytest)
library(shinycssloaders)
library(shinybusy)
library(DT)
library(ppcor)
library(fastDummies)
library(boot)
library(readxl)
library(readr)

# Define server logic
server <- function(session, input, output) {

  # Null / placeholder value for fields of the download object
  selected = NULL
  
  # Function to standardize (upcase) fields download object 
  uppercase_first <- function(string) {
    sub(
      "^(.)"
      , "\\U\\1"
      , string
      , perl = TRUE
      )
  }
  
  # Default delimiter
  comma = ","
  
  # Create empty reactive object
  for.download = reactiveValues()
  for.download$corr.method      = "NA"
  for.download$corr.alpha       = "NA"
  for.download$raw.corr.est     = "NA"
  for.download$raw.corr.p       = "NA"
  for.download$partial.corr.est = "NA"
  for.download$partial.corr.p   = "NA"
  for.download$corr.ci.method   = "NA"
  for.download$corr.ci.lower    = "NA"
  for.download$corr.ci.upper    = "NA"
  
  #---------------------------------------------------------------------------#
  
  # Read and react to the uploaded data
  data <- reactive({
    
    req(input$dataset)
    inFile <- input$dataset
    if (is.null(inFile)) {
      return(NULL)
    }
    
    filePath <- inFile$datapath[1]
    fileExt <- tools::file_ext(filePath)
    
    if (fileExt == 'csv') {
      read.csv(input$dataset$datapath, header = input$header, sep = comma)
    } else if (fileExt == 'xlsx' || fileExt == 'xls') {
      read_excel(input$dataset$datapath, sheet = 1)
    } else {
      return(NULL)
    }
    
  })
  
  #---------------------------------------------------------------------------#

  # Subset data for analysis based on chosen fields
  filtereddata <- eventReactive(input$update, {
    req(data())
    if (is.null(input$selectXY)) {
      data()
    } else {
      selected <- c(input$selectXY, input$selectZCont, input$selectZCat)
      data()[, colnames(data()) %in% selected]
    }
  })
  
  #---------------------------------------------------------------------------#
  
  # Initial columns update based on the data
  observe({
    updateSelectInput(session, "selectXY", choices = colnames(data()))
    updateSelectInput(session, "selectZCont", choices = colnames(data()))
    updateSelectInput(session, "selectZCat", choices = colnames(data()))
  })
  
  # Function to update all select inputs' choices
  update_select_inputs <- function(selected_XY, selected_ZCont, selected_ZCat) {
    current_cols <- colnames(data())
    
    # Update choices for selectXY
    available_choices_XY <- setdiff(
      current_cols, 
      c(selected_ZCont, selected_ZCat)
    )
    
    updateSelectInput(
      session, 
      "selectXY", 
      choices = available_choices_XY, 
      selected = selected_XY
    )
    
    # Update choices for selectZCont
    available_choices_ZCont <- setdiff(
      current_cols, 
      c(selected_XY, selected_ZCat)
    )
    
    updateSelectInput(
      session, 
      "selectZCont", 
      choices = available_choices_ZCont, 
      selected = selected_ZCont
    )
    
    # Update choices for selectZCat
    available_choices_ZCat <- setdiff(
      current_cols, 
      c(selected_XY, selected_ZCont)
    )
    updateSelectInput(
      session, 
      "selectZCat", 
      choices = available_choices_ZCat, 
      selected = selected_ZCat
    )
  }
  
  # Observe changes in each input and update choices accordingly
  observeEvent(input$selectXY, {
    update_select_inputs(
      input$selectXY, 
      input$selectZCont, 
      input$selectZCat
    )
  }, ignoreNULL = FALSE)
  
  observeEvent(input$selectZCont, {
    update_select_inputs(
      input$selectXY, 
      input$selectZCont, 
      input$selectZCat
    )
  }, ignoreNULL = FALSE)
  
  observeEvent(input$selectZCat, {
    update_select_inputs(
      input$selectXY, 
      input$selectZCont, 
      input$selectZCat
    )
  }, ignoreNULL = FALSE)
  
  #---------------------------------------------------------------------------#
  
  # Visualize the subset data table
  output$mytable  <- renderDataTable(
    filtereddata(), 
    options = list(pageLength = 5)
  )
  
  # Compute and output correlations, as well as their confidence intervals 
  observeEvent(input$update, {
    
    # Output unadjusted correlation coefficient
    output$unadj <- renderText({
      
      # Generate measures
      data = filtereddata()
      XY = as.data.frame(data[, colnames(data) %in% input$selectXY])
      XY = XY[complete.cases(XY), ]
      
      corr.est = round(
        cor.test(
          x = XY[, 1], 
          y = XY[, 2], 
          method = input$method, 
          exact = FALSE
        )$estimate, 4)
      
      corr.pval = formatC(
        cor.test(
          x = XY[, 1], 
          y = XY[, 2], 
          method = input$method, 
          exact = FALSE
        )$p.value,  
        format = "e", 
        digits = 2
      )
      
      # for CIs - unchanging between method
      z_r = 1/2 * log((1 + corr.est) / (1 - corr.est))
      z_crit = qnorm(1 - as.numeric(input$alpha) / 2)
      n = nrow(XY)
      
      # Handle CIs
      if (input$method == 'pearson') {
        
        se = sqrt(1 / (n - 3))
        
      } else if (input$method == 'spearman') {
        
        se = sqrt((1 + (corr.est^2)/2) / (n - 3))
        
      } else if (input$method == 'kendall') {
        
        se = sqrt(0.437 / (n - 4))
        
      } 
      
      # for cis - formula unchanged between methods
      z_l = z_r - (z_crit * se)
      z_u = z_r + (z_crit * se)
      
      corr.int.lower = round((exp(2*z_l) - 1) / (exp(2*z_l) + 1), 4)
      corr.int.upper = round((exp(2*z_u) - 1) / (exp(2*z_u) + 1), 4)
      
      # Print results
      int.width = 100 * (1 - as.numeric(input$alpha))
      paste0("The unadjusted ", uppercase_first(input$method), 
             " correlation between ", colnames(XY[1]), " and ", 
             colnames(XY[2]), " is ", corr.est, " (p = ", corr.pval, ", ", 
             int.width, "% CI = (", corr.int.lower, ", ", corr.int.upper, 
             ")).")

    })
    
    # Output partial correlation coefficient
    output$adj <- renderText({
      
      # Load data
      data = filtereddata()
      
      # Subset for handling
      XY    = as.data.frame(data[, colnames(data) %in% input$selectXY])
      ZCont = as.data.frame(data[, colnames(data) %in% input$selectZCont])
      ZCat  = as.data.frame(data[, colnames(data) %in% input$selectZCat])

      # General approach - handle many categorical variables
      if(ncol(ZCat) != 0) {
        colCount = ncol(ZCat)
        n.group = c()
        counter = 0

        for (i in 1:colCount) {
          n.group[i] = length(unique(ZCat[, i])) + counter
          counter = n.group[i]
        }

        ZCat = dummy_cols(ZCat)
        ZCat = ZCat[ , -(1:colCount)]
        ZCat = ZCat[, -n.group]
        XYZ = cbind(XY, ZCont, ZCat)

      } else {
        XYZ = cbind(XY, ZCont)
      }
      
      # Drop rows that are incomplete cases
      XYZ <- XYZ[complete.cases(XYZ), ]
      
      # Compute partial correlation coefficient
      p.corr.obj = pcor.test(
        x = as.data.frame(XYZ[,1]),
        y = as.data.frame(XYZ[,2]),
        z = as.data.frame(XYZ[,3:ncol(XYZ)]),
        method = input$method
      )

      p.corr.est  = round(p.corr.obj$estimate, 4)
      p.corr.pval = formatC(p.corr.obj$p.value, format = "e", digits = 2)
      
      # for CIs - unchanging between method
      z_r = 1/2 * log((1 + p.corr.est) / (1 - p.corr.est))
      z_crit = qnorm(1 - as.numeric(input$alpha) / 2)
      n = nrow(XYZ)
      
      # Handle CIs
      if (input$method == 'pearson') {
        
        se = sqrt(1 / (n - 3))
        
      } else if (input$method == 'spearman') {
        
        se = sqrt((1 + (p.corr.est^2)/2) / (n - 3))
        
      } else if (input$method == 'kendall') {
        
        se = sqrt(0.437 / (n - 4))
        
      } 
      
      # for cis - formula unchanged between methods
      z_l = z_r - (z_crit * se)
      z_u = z_r + (z_crit * se)
      
      p.corr.int.lower = round((exp(2*z_l) - 1) / (exp(2*z_l) + 1), 4)
      p.corr.int.upper = round((exp(2*z_u) - 1) / (exp(2*z_u) + 1), 4)
      
      # Re-compute unadjusted output so it's in the environment for export
      corr.est  = round(cor.test(XY[, 1], XY[, 2], method = input$method, exact = FALSE)$estimate, 4)
      corr.pval = formatC(cor.test(XY[, 1], XY[, 2], method = input$method, exact = FALSE)$p.value, format = "e", digits = 2)
      
      # Take RSHINY input parameters and coerce them to a type that plays nice with lists
      input.method    = input$method
      input.alpha     = input$alpha
      input.ci.method = input$ci.method
      
      # Store computed values for later export
      for.download$corr.method      = input.method
      for.download$corr.alpha       = input.alpha
      for.download$raw.corr.est     = corr.est
      for.download$raw.corr.p       = corr.pval
      for.download$partial.corr.est = p.corr.est
      for.download$partial.corr.p   = p.corr.pval
      for.download$corr.ci.method   = input.ci.method
      #for.download$corr.ci.lower    = p.corr.int.lower
      #for.download$corr.ci.upper    = p.corr.int.upper
      
      # Print results
      int.width = 100 * (1 - as.numeric(input$alpha))
      paste0("The partial ", uppercase_first(input$method), " correlation between ", 
             colnames(XY[1]), " and ", colnames(XY[2]), " is ", p.corr.est, 
             " (p = ", p.corr.pval, ", ", int.width, "% CI = (", p.corr.int.lower, ", ", p.corr.int.upper, ")).")

    })
  
  })
    
  # Handle download
  output$download = downloadHandler(
    
    # Declare filename function
    filename = function() {
      paste("output.txt")
    },
    
    # Declare content function
    content = function(file) {
      my_list = list(
        paste0("ANALYSIS-LEVEL DETAIL: "),
        paste0("──────────────────────"),
        paste0("Correlation Method: ", isolate(for.download$corr.method)),
        paste0("Significance Level: ", isolate(for.download$corr.alpha)),
        paste0(""), 
        
        paste0("UNADJUSTED CORRELATION METRICS: "),
        paste0("──────────────────────────────"),
        paste0("Coefficient: ", isolate(for.download$raw.corr.est)),
        paste0("P-Value: ", isolate(for.download$raw.corr.p)),
        paste0(""), 
        
        paste0("PARTIAL CORRELATION METRICS: "),
        paste0("────────────────────────────"),
        paste0("Coefficient: ", isolate(for.download$partial.corr.est)),
        paste0("P-Value: ", isolate(for.download$partial.corr.p)),
        paste0("CI Method: ", isolate(for.download$corr.ci.method)),
        paste0("CI Lower Bound: ", isolate(for.download$corr.ci.lower)),
        paste0("CI Upper Bound: ", isolate(for.download$corr.ci.upper))
      )

    # Export
    writeLines(unlist(my_list), file)
    
    }
  )
  
  # App reload functionality
  observeEvent(input$clear, {
    session$reload()
  })

} 
