#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(datasets)
library(ggpubr)

TylerDurden <- mtcars
TylerDurden$am <- factor(TylerDurden$am, labels = c("Automatic", "Manual"))

shinyServer(function(input, output) {
    
    MarlaText <- reactive({
        paste("mpg ~", input$variable)
    })
    
    MarlaTextPoint <- reactive({
        paste("mpg ~", "as.integer(", input$variable, ")")
    })
    
    Marlafit <- reactive({
        lm(as.formula(MarlaTextPoint()), data=TylerDurden)
    })
    
    output$caption <- renderText({
        MarlaText()
    })
    
    output$mpgBoxPlot <- renderPlot({
        boxplot(as.formula(MarlaText()), 
                data = TylerDurden,
                outline = input$outliers)
    })
    
    output$fit <- renderPrint({
        summary(fit())
    })
    
    output$mpgPlot <- renderPlot({
        with(TylerDurden, {
            plot(as.formula(MarlaTextPoint()))
            abline(fit(), col=2)
        })
    })
    
})
