library(shiny)
library(tidyverse)
library(visNetwork)
library(DT)
library(fmsb)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Career Ponderer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Interests", label = "Area",
                  choices = list("Interests", "Skills", "Career Desires", "Career Requirements"), selected = ""),
      textInput(inputId = "Content", label = "Add here", value = ""),
      radioButtons(inputId = "Priority", label = "Priority", 
        choices = list("High", "Medium / High", "Medium", "Low / Medium", "Low"), selected = ""),
      actionButton("submit", "Submit"),
      bookmarkButton()
    ),
  mainPanel(
    DT::dataTableOutput("Table1"),
    plotOutput("Table2")
  )
)
)  


v <- reactiveValues()
v$df <- data.frame(Interests = character(), Content = character(), Priority = character(), stringsAsFactors = FALSE)

server <- function(input, output, session) {
  observeEvent(input$submit, {
    req(input$Interests, input$Content, input$Priority)
    tmp <- data.frame(Interests = input$Interests, Content = input$Content, Priority = input$Priority)
    v$df <- rbind(v$df, tmp)
  })
  output$Table1 <- DT::renderDataTable({
    v$df
  })
  
  output$Table2 <- renderPlot({
    ggplot(v$df) +
    geom_segment(aes(x=0, xend=Priority, y=Content, yend=Content, colour = Priority)) +
    geom_point( aes(x=Priority, y=Content, colour = Priority), size=3) + 
    scale_x_discrete(limits=c("Low", "Low / Medium", "Medium", "Medium / High", "High")) +
    coord_flip()+
    facet_wrap(~Interests, ncol=4, scale="free") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      

  # output$Table2 <- renderPlot({
  # ggplot(v$df, aes(x = Priority, y = Content)) +
  #   geom_segment(aes(x = Priority, xend = Priority, y = , yend = Content)) +
  #   geom_point() +
  #   coord_flip()
})
}
shinyApp(ui = ui, server = server, enableBookmarking = "url")
