data("iris")
library("shiny")
library("ca")
library("MASS")
library("factoextra")
library("FactoMineR")
library("gplots")
library("ggbiplot")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # nav bar
    navbarPage("徐宇文_資管碩一_111356003",
               
               tabPanel("PCA",
                        
                        tabsetPanel(
                            tabPanel("pca plot", titlePanel("PCA"),
                                     sidebarLayout(
                                         
                                         sidebarPanel(
                                             radioButtons(
                                                 inputId = "xpca",
                                                 label = "X-axis", 
                                                 choices = c(1, 2, 3, 4),
                                                 selected = 1
                                             ),
                                             radioButtons(
                                                 inputId = "ypca",
                                                 label = "y-axis", 
                                                 choices = c(1, 2, 3, 4),
                                                 selected = 2
                                             )
                                         ),
                                         mainPanel(
                                             plotOutput("pcaPlot")
                                         )
                                     )),
                            tabPanel("input data (log)", titlePanel("input data (log)"),
                                     dataTableOutput("log")),
                        ),
                        
               ),
               tabPanel("CA",
                        titlePanel("CA (K-means)"),
                        sidebarLayout(
                            # Sidebar panel for inputs ----
                            
                            sidebarPanel(
                                # Input: Slider for the number of bins ----
                                sliderInput(inputId = "centers",
                                            label = "number of centers(k)",
                                            min = 3,
                                            max = 10,
                                            value = 3)
                                
                            ),
                            mainPanel(
                                plotOutput("caPlot")
                            )
                        )
                        
               ),
               tabPanel("iris data intro.",
                        titlePanel("iris data"),
                        sidebarLayout(
                            
                            sidebarPanel(textOutput("irisIntro")),
                            mainPanel(
                                dataTableOutput("irisTable")
                            )
                        )
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)

    
    output$pcaPlot <- renderPlot({
        x <- as.numeric(input$xpca)
        y <- as.numeric(input$ypca)
        g <- ggbiplot(ir.pca, choices = c(x, y), obs.scale = 1, var.scale = 1, groups = ir.species)
        g <- g + scale_color_discrete(name = '')
        g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
        print(g)
    })
    
    output$log <- renderDataTable(log.ir)
    
    output$caPlot <- renderPlot({
        
        iris2 <- iris
        iris2$Species <- NULL
        kmeans.result <- kmeans(iris2, as.numeric(input$centers))
        
        tab <- table(iris$Species, kmeans.result$cluster)
        
        ir.ca <- ca(tab, ncp = 3, graph = FALSE)
        p <- plot(ir.ca, mass = TRUE, contrib = "absolute", map =
                      "rowgreen", arrows = c(FALSE, TRUE))
        print(p)
    })
    output$irisIntro <- renderText("This data sets consists of 3 different types of irises’ (Setosa, Versicolour, and Virginica) petal and sepal length.")
    output$irisTable <- renderDataTable(iris)
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
