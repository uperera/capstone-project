library(shiny)
shinyUI(pageWithSidebar(
        headerPanel("Next Word Prediction using N-GRAM Model"),
        sidebarPanel(
                #h3('Type a phrase here'),
                textInput('phrase',label="Type phrase here"),
                #numericInput('mu','enter mean value',5,min = 10,max=100,step=5),
                #sliderInput('ss','Select a Sample size',value = 10,min= 100,max=2000,step=100,),
                submitButton('Submit')
                ),
        mainPanel(
                h3('How to Use the Application'),
                h5('This application illustrates the use of Natural Language Processing for predicting the next expected word of a phrase'),
                h5('Type in a phrase in the text box and hit submit to see the prediction.'),
                h4('The phrase you typed is:'),
                verbatimTextOutput("phrase"),
                h4('The expected next word is:'),
                verbatimTextOutput("word")
                #h4('sample mean distribution'),
                #plotOutput('plot2')
                
                                )
        ))