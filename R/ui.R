# ui.r
# By Matt Dinwiddie
# 9/04/2023

# Load libraries
library(shinythemes)
library(shinytest)
library(shinyjs)
library(shinybusy)

ui <- fluidPage(theme = shinytheme("flatly"),
                
  navbarPage(
    "◑ PCC | Partial Correlation Calculator",
    tabPanel("📊 Calculator",
             
             # Enable ShinyJS for download button
             useShinyjs(),
             
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 
                 # Add configuration for loading symbol #------------------------------#
                 add_busy_spinner(spin = "radar",
                                  position = c("bottom-right"),
                                  margins = c(10, 20)),

                 # Pick correlation method #-------------------------------------------#
                 div(style = "position: relative; margin-bottom: 10px;",
                     selectInput("method", "Select correlation type:",
                                 list("Spearman (Default)" = "spearman",
                                      "Pearson" = "pearson",
                                      "Kendall" = "kendall"),
                                 width = '90%'),
                     tags$span(style = "position: absolute; right: 10px; top: 50%; transform: translateY(-50%); font-size: 16px; cursor: pointer; color: #007bff; border-bottom: 1px dotted;",
                               title = "Spearman and Kendall are best for low sample sizes and nonlinear relationships.",
                               "ⓘ")
                 ),

                 # Pick significance level #-------------------------------------------#
                 div(style = "position: relative; margin-bottom: 10px;",
                     selectInput("alpha", "Select significance level:",
                                 list("0.05 (Default)" = "0.05",
                                      "0.10" = "0.10",
                                      "0.01" = "0.01"),
                                 width = '90%'),
                     tags$span(style = "position: absolute; right: 10px; top: 50%; transform: translateY(-50%); font-size: 16px; cursor: pointer; color: #007bff; border-bottom: 1px dotted;",
                               title = "Specify the significance level of your analysis.",
                               "ⓘ")
                 ),
                 
                 # Require file to load. #---------------------------------------------#
                 div(style = "position: relative; margin-bottom: 10px;",
                     fileInput("dataset", "Upload dataset (.csv, .xls, .xlsx):",
                               multiple = TRUE,
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv",
                                          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                          "application/vnd.ms-excel",
                                          ".xlsx",
                                          ".xls")),
                     tags$span(style = "position: absolute; right: 10px; top: -5px; transform: translateY(0); font-size: 16px; cursor: pointer; color: #007bff; border-bottom: 1px dotted;",
                               title = "Upload your data file in one of the supported formats. Make sure it contains all variables needed for analysis.",
                               "ⓘ")
                 ),
                 
                 # Input: Checkbox if file has header ----
                 div(style = "position: relative; margin-bottom: 10px;",
                     checkboxInput("header", "My data has headers", TRUE)
                     # tags$span(style = "position: absolute; right: 10px; top: 50%; transform: translateY(-50%); font-size: 16px; cursor: pointer; color: #007bff; border-bottom: 1px dotted;",
                     #           title = "Check this box if the columns of your data are named.",
                     #           "ⓘ")
                 ),
                 
                 # Select X and Y Columns #--------------------------------------------#
                 div(style = "position: relative; margin-bottom: 10px;",
                     selectizeInput('selectXY',
                                    'Choose X and Y Variables',
                                    choices = colnames(data()),
                                    multiple = TRUE,
                                    options = list(maxItems = 2),
                                    width = '90%'),
                     tags$span(style = "position: absolute; right: 10px; top: 50%; transform: translateY(-50%); font-size: 16px; cursor: pointer; color: #007bff; border-bottom: 1px dotted;",
                               title = "Select the main X and Y variables of your analysis.",
                               "ⓘ")
                 ),
                 
                 # Select Z Continuous Columns #---------------------------------------#
                 div(style = "position: relative; margin-bottom: 10px;",
                     selectizeInput('selectZCont',
                                    'Choose Z Continuous Variable(s)',
                                    choices = colnames(data()),
                                    multiple = TRUE,
                                    width = '90%'),
                     tags$span(style = "position: absolute; right: 10px; top: 50%; transform: translateY(-50%); font-size: 16px; cursor: pointer; color: #007bff; border-bottom: 1px dotted;",
                               title = "Select the continuous confounding variables of your analysis. Remember, continuous variables are numerical values that can take on any real number within a specific range or interval.",
                               "ⓘ")
                 ),
                 
                 # Select Z Categorical Columns #--------------------------------------#
                 div(style = "position: relative; margin-bottom: 10px;",
                     selectizeInput('selectZCat',
                                    'Choose Z Categorical Variable(s)',
                                    choices = colnames(data()),
                                    multiple = TRUE,
                                    width = '90%'),
                     tags$span(style = "position: absolute; right: 10px; top: 50%; transform: translateY(-50%); font-size: 16px; cursor: pointer; color: #007bff; border-bottom: 1px dotted;",
                               title = "Select the categorical confounding variables of your analysis. Remember, categorical variables are distinct categories represented by labels or codes, used for classification.",
                               "ⓘ")
                 ),
                 
                 # Analysis Button ----------------------------------------------------#
                 div(style = "position: relative; margin-bottom: 10px;",
                     actionButton("update",
                                  " Analyze ",
                                  class = "btn-primary",
                                  style='padding:4px; font-size:120%')
                 ),
                 
                 # "Clear uploads" button ---------------------------------------------#
                 div(style = "position: relative; margin-bottom: 10px;",
                     actionButton("clear",
                                  " Reset ",
                                  style='padding:4px; font-size:120%')
                 ),
                 
                 # "Download" button --------------------------------------------------#
                 div(style = "position: relative; margin-bottom: 10px;",
                     uiOutput("downloadUI")
                 )
                 
               ),
               
               #-----------------------------------------------------------------------#
               
               # Show a plot of the generated distribution
               # In ui.R
               
               mainPanel(
                 h2('Analysis Output'),
                 
                 # Output unadjusted correlation
                 helpText("Unadjusted Correlation:"),
                 textOutput('unadj'),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Output partial correlation
                 helpText("Partial Correlation:"),
                 textOutput('adj'),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Output table being used in analysis
                 helpText("Uploaded Data:"),
                 DT::dataTableOutput("mytable"),
                 
               )
               
             )
             
    ),
    
    tabPanel("🔍 Information",
             
             h2("About this Calculator"),
             helpText("This calculator facilitates the computation of partial correlation coefficients. Partial correlation allows researchers to quantify the strength of an association between two variables, while controlling for a set of extraneous variables."),
             
             h2("Calculator Operation"),
             helpText("Usage of the calculator is fairly straightforward; select from the prompts to define the parameters of your partial correlation calculation. Hover over prompts that you are unsure of to view helpful tooltips. To use this calculator, please upload your data in .CSV, .XLS, or .XLSX format. Be mindful of missingness in your data; this may hurt the quality of the analysis."),
             
             h2("Sample Data"),
             helpText("If you would like to test this calculator's functionality but lack data, visit [this link] to download sample data to use with this calculator."),
             
             h2("Creditation"),
             helpText("This calculator was created by Matthew Dinwiddie, MSc, and Ann Marie Weideman, MSc, in association with the University of North Carolina at Chapel Hill's Centers for AIDS Research."),
             
             h2("References"),
             helpText("Various R packages were utilized as well, cited below."),
             helpText("  • Kim S. (2015). ppcor: An R Package for a Fast Calculation to Semi-partial Correlation Coefficients. Communications for statistical applications and methods, 22(6), 665–674. https://doi.org/10.5351/CSAM.2015.22.6.665"),
             helpText("  • Thomas J. DiCiccio. Bradley Efron. 'Bootstrap confidence intervals.' Statist. Sci. 11 (3) 189 - 228, August 1996. https://doi.org/10.1214/ss/1032280214"),
             helpText("  • R Core Team (2022). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.")
             
    )
  ),
  
)