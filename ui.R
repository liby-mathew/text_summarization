shinyUI(fluidPage(
  
  # Application title
  tags$head(
    tags$style(
      ".title {margin: auto; width: 800px}"
    )
  ),
  tags$div(class="title", titlePanel("Text Analytics - Assignment - Q3")),
  hr(),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a text file ----
      fileInput("txtFile", "Choose Text File",
                multiple = FALSE,
                accept = c(".txt", ".TXT")),
      # Input: Select a udpipe file ----
      fileInput("modelFile", "Choose Model File",
                multiple = FALSE,
                accept = c( ".udpipe")),
      checkboxGroupInput("checkbox", label = h3("Part-Of-Speech Tags"), 
                         choices = list("Adjective" = "ADJ", "Noun" = "NOUN", "Proper Noun" = "PROPN", "Adverb" = "ADV", "Verb" = "VERB"),
                         selected = list("Adjective" = "ADJ", "Noun" = "NOUN", "Proper Noun" = "PROPN")),
      h2(""), h2(""),
      h3("Submitted By"),
      h3(""),
      h5("Liby Mathew - 11915062"),
      h5("Sandeep Ramesh - 11915016"),
      h5("Hari Prasad Reddy - 11915006")
      
    ),
    
    # Show a plot of the generated distribution
    
    mainPanel(
      
      tabsetPanel(type = "tabs",   
                  
                  tabPanel("Overview",  
                           
                           h4(p("Data input")),
                           
                           p("This app supports only text data file (.txt) and udpipe model file (.udpipe)", align="justify"),
                           
                           p("Please refer to the link below for sample txt file and english udpipe model file."),
                           a(href="https://github.com/liby-mathew/text_summarization/blob/master/Chandrayaan2.txt"
                             ,"Sample data input file"),   
                           br(),
                           a(href="https://github.com/liby-mathew/text_summarization/blob/master/english-ud-2.1-20180111.udpipe"
                             ,"Sample udpipe model file"),   
                           
                           br(),
                           
                           h4('How to use this App'),
                           
                           p('To use this app, click on', 
                             span(strong("Upload text file")),
                             'and udpipe model file. You can also change the POS tags for different results')),
                  
                  tabPanel("Co-occurence Graph", 
                           withSpinner(plotOutput('plot')),
                           textOutput("txt")),
                  
                  
                  tabPanel("Word Cloud",
                           withSpinner(plotOutput('wcloud', width = "100%"))),
                  
                  tabPanel("Data",
                           withSpinner(dataTableOutput('datatable')))
                  
      ) 
    )
  )
))