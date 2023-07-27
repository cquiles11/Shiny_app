# First we open the libraries to use
library(shiny)
library(ggplot2)

ui <- fluidPage(

    # Application title
    titlePanel("Linear Regression Model"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
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
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterPlot"),
           plotOutput("LMPlot"),
           tableOutput("contents")
        )
    )
)

# Define the server to graph the scatter plot and linear model

server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
model <- lm(formula = y~x, dataInput())
    
 output$scatterPlot <- renderPlot({
        ggplot(dataInput(), aes(x = dataInput()$x, y = dataInput()$y),
             colour = 'red') +
  geom_point()
})
    
 output$LRPlot <- renderPlot({
     ggplot(dataInput(), aes(x = dataInput()$x, y = dataInput()$y),
             colour = 'red') +
  geom_point() +
  geom_line(dataInput(), aes(x = dataInput()$x, y = predict(model, newdata = dataInput())),
            colour = 'blue')
})
 
 output$contents <- renderTable({
     if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
}
# Run the application 
shinyApp(ui = ui, server = server)