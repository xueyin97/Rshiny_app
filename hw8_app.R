library(shiny)
library(ggplot2)

sig<-read.table(file.path("sig_intensities-1.txt"),sep = "\t",header=T,row.names = 1)
sig.pca<-prcomp(t(sig),center = TRUE, scale = TRUE)
ui <- fluidPage(
  sidebarPanel("Select the ordered number of PC for each axis",
  selectInput("X","X-axis", colnames(sig.pca$x)),
  selectInput("Y","Y-axis",colnames(sig.pca$x)),
  actionButton("go", "Go")
  ),
  mainPanel(
    h3("Plot of selected two PCs for SIG"),
    plotOutput("PCAPlot")
  )
)
server <- function(input, output) {
  sig.pcs <- eventReactive(input$go, {
    as.data.frame(sig.pca$x[,c(input$X,input$Y)])
  })
  output$PCAPlot <- renderPlot({
    plot(sig.pcs()[,c(1:2)], pch = 20, cex = 2)
    points(sig.pcs()[,c(1:2)])
    })
}
shinyApp(ui, server)
