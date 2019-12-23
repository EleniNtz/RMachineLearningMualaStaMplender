
library('caret')
source('clRF.R')
source('cleanSVMscript.R')
namesChoice <<- list(
  'Age' = 'age',
  'Sex' = 'sex',
  'Chest Pain Type (4 Values)' = 'chestPainType',
  'Resting Blood Pressure' = 'restBloodPress',
  'Serum Cholesterol (mg/dl)' = 'serumChol',
  'Fasting Blood Sugar > 120 mg/dl' = 'fastBloodSug',
  'Resting ECG Results (0, 1, 2)' = 'restECG',
  'Max Heart Rate Achieved' = 'maxHR',
  'Exercise Induced Angina' = 'exerAngina',
  'ST depression induced by exercise relative to rest' ='oldpeak',
  'Slope of the peak exercise ST segment' = 'slope',
  'Major Vessels Colored (0-3)' = 'coloredVessels',
  'Thalassemia' = 'thal'
)
catIndex <<- c(2,3,6,7,9,11,13)
numIndex <<- c(1,4,5,8,10,12)
library('shiny')
ui <- fluidPage(
  titlePanel('First Attempt'),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      selectInput('selectClass','Select Classifier',
                  choices = list(' ' = 1,
                                 'Random Forest' = 2,
                                 'SVM' = 3),
                  selected = 1)
    ),
    mainPanel(
      tabsetPanel(type = 'tabs',
                  tabPanel('Plot', selectInput('selectVar','Select Variable',
                                       choices = namesChoice,
                                       selected = 1
                                       ),
                           plotOutput('plot')
                  ),
                  tabPanel('Box Plot', selectInput('selectBoxX','Select X Variable (Categorical)',
                                                  choices = namesChoice[-numIndex],
                                                  selected = 1),
                           selectInput('selectBoxY','Select Y Variable (Numerical)',
                                       choices = namesChoice[-catIndex],
                                       selected = 1
                           ),
                           plotOutput('boxPlot')
                  
                  ), 
                          
                  tabPanel('Statistical Results',
                           helpText("P-Values of all the features extracted",
                                    "by a chi^2 test. The variables chosen ",
                                    "for the models are those with p-value < e-3."),
                           tableOutput(outputId = "pValues")),
                  tabPanel('Classifier Result',
                           
                           helpText('1 Negative for coronary disease',
                                    '2 Positive for coronary disease'),
                           
                           fluidRow(tableOutput(outputId = "cmRes")),
                           fluidRow(
                             column(width = 5,tableOutput('statresult')),
                             column(width = 5,tableOutput(outputId = "cmStats"))
                             )
                           )
                          
                  
                  ),
      textOutput("selected_var")
    )
  )
)
server <- function(input, output) {
  heart <- reactiveValues(variable = NULL)
  cm <- reactiveValues(variable = NULL)
  observeEvent(input$file1, {
    if (is.null(input$file1)) return ()
    heart$variable <- read.table(input$file1$datapath, sep=' ', header=F,
                                 col.names =  c('age', 
                                                'sex', 
                                                'chestPainType',
                                                'restBloodPress',
                                                'serumChol',
                                                'fastBloodSug',
                                                'restECG', 
                                                'maxHR', 
                                                'exerAngina',
                                                'oldpeak',
                                                'slope',
                                                'coloredVessels',
                                                'thal',
                                                'disease'))
    a <- NULL
    b <- NULL
    
    observeEvent(input$selectClass, {
      if (input$selectClass == 2) {
        if (is.null(a)) {
          a <<- ranFor(heart$variable)
        }
        cm$variable = a       #[[1]]
        }
      else if (input$selectClass == 3) {
        if (is.null(b)) {
          b <<- clSVM(heart$variable)
        }
        cm$variable = b
        }
    })
    
    
 #   if (input$selectClass == 2) {output$cmRes <- renderTable({a[[1]]}, rownames = T, colnames=T)}
  #  else if (input$selectClass == 3) {output$cmRes <- renderTable({b[[1]]}, rownames = T, colnames=T)}
    output$cmRes <- renderTable({cm$variable[[1]]}, rownames = T, colnames=T)
    output$cmStats <- renderTable({cm$variable[[2]]}, rownames = T, colnames=F)
    output$statresult <- renderTable({cm$variable[[3]]}, rownames = T, colnames=F)
    output$plot <- renderPlot({
    plot(heart$variable[,input$selectVar], xlab = "Patient Index", 
         ylab=names(which(namesChoice==input$selectVar)))
    })
    output$boxPlot <- renderPlot({
    boxplot(heart$variable[,input$selectBoxY]~ heart$variable[,input$selectBoxX],
            xlab = names(which(namesChoice==input$selectBoxX)),
            ylab = names(which(namesChoice==input$selectBoxY)))
    })
  })
  output$pValues <- renderTable({cbind(c('Age', 'Sex', 'Chest Pain Type (4 Values)',
                                         'Resting Blood Pressure', 'Serum Cholesterol (mg/dl)',
                                         'Fasting Blood Sugar > 120 mg/dl',
                                         'Resting ECG Results (0, 1, 2)',
                                         'Max Heart Rate Achieved' ,'Exercise Induced Angina',
                                         'ST depression induced by exercise relative to rest',
                                         'Slope of the peak exercise ST segment',
                                         'Major Vessels Colored (0-3)',
                                         'Thalassemia' ),
                                       c(0.12,1.93e-06,8.56e-15,0.59,0.08,0.92,
                                         0.01,0.14,1.38e-11,2.00e-04,1.71e-09,
                                         1.44e-14,6.42e-17))},
    colnames=F

  )
"  observeEvent(input$selectClass==2, {
       if(!is.null(heart$variable)){
       'if(is.null(a)) {
          
       }'
       output$cmRes <- renderTable({a[[1]]}, rownames = T, colnames=T)
     }
  })
  observeEvent(input$selectClass==3, {
    if(!is.null(heart$variable)){
      if(is.null(b)) {
        output$cmRes <- renderTable({table(c(1,1,1,1,2,2))}, rownames = T, colnames=T)
      }
      output$cmRes <- renderTable({b[[1]]}, rownames = T, colnames=T)
    }
  })"
'       if((input$selectClass == 2) & (is.null(b))) {
         b <- ranFor(heart$variable)
       }}
     output$cmRes <- renderTable({
       if(input$selectClass==3){
         a[[1]]
         } else if (input$selectClass==2) {
         b[[1]]
           }
       }, rownames = T, colnames=T)   
  output$cmStats <- renderTable({
    if(input$selectClass==3){
      a[[2]]
      } else if (input$selectClass==2) {
      b[[2]]
    }
  }, rownames = T, colnames=F)
  
  output$statresult <- renderTable({
    if(!is.null(heart$variable)){

        #a[[3]]
        
      
    }
  }, rownames = T, colnames = F, align = "c")
  })'
  
  
  
  #if (nrow(heart) != 0) {output$statresult <- renderText({'True'})}
  #else{output$statresult <- renderText({'False'})}
}

shinyApp(ui = ui, server = server)