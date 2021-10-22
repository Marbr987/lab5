#' @title Shiny app
#' @name app
#' @description A shiny application visualizing the position of two swedish cities and calculating their p-norm distance.
#' @import shiny
#' @importFrom ggplot2 ggplot geom_point geom_line geom_vline theme aes element_line element_rect labs
#' @source get_coordinates

library(shiny)

if(!("lab5" %in% installed.packages()[,"Package"])){
    devtools::install_github("Marbr987/lab5")
    library("lab5")
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("P-norm City Distance"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId="city1", label="Distance between", value="Linkoping"),
            textInput(inputId="city2", label="and", value="Stockholm"),
            sliderInput("p_value", "p-norm:", min = 1, max = 8, value = 2, step=0.25)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("posPlot"),
           plotOutput("distPlot"),
           verbatimTextOutput("coordinates_msg"),
           verbatimTextOutput("distance_msg")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$posPlot <- renderPlot({
        city_coordinates <- data.frame(longitude=c(lab5::get_coordinates(input$city1)["longitude"], lab5::get_coordinates(input$city2)["longitude"]),
                                       latitude=c(lab5::get_coordinates(input$city1)["latitude"], lab5::get_coordinates(input$city2)["latitude"]))
        ggplot2::ggplot(mapping=ggplot2::aes(x=longitude, y=latitude)) +
            ggplot2::geom_point(data=sweden_border, size=0.000001) +
            ggplot2::geom_point(data=city_coordinates, size=3, color="red") +
            ggplot2::theme(aspect.ratio=1, panel.background = ggplot2::element_rect(fill = "white", colour = "grey50"),
                  panel.grid.major = ggplot2::element_line(colour = "grey"), panel.grid.minor = ggplot2::element_line(colour = "grey")) +
            ggplot2::xlim(10, 25) +
            ggplot2::ylim(55, 70)
    })
    
    output$distPlot <- renderPlot({
        p_values <- seq(from=1, to=8, by=0.25)
        city_coordinates <- data.frame(longitude=c(lab5::get_coordinates(input$city1)["longitude"], lab5::get_coordinates(input$city2)["longitude"]),
                                       latitude=c(lab5::get_coordinates(input$city1)["latitude"], lab5::get_coordinates(input$city2)["latitude"]))
        pn_distance_values <- unlist(lapply(p_values, function(x){dist(matrix(c(city_coordinates[1,],city_coordinates[2,]), ncol=2, nrow = 2, byrow = TRUE),
                                                                       p=x, method = "minkowski")}))
        distance_df <- data.frame(p_values=p_values, pn_distance_values=pn_distance_values)
        ggplot2::ggplot() +
            ggplot2::geom_line(data=distance_df, mapping=ggplot2::aes(x=p_values, pn_distance_values)) +
            ggplot2::geom_vline(mapping=ggplot2::aes(xintercept=input$p_value), color="red") +
            ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = "grey50"),
                  panel.grid.major = ggplot2::element_line(colour = "grey"), panel.grid.minor = ggplot2::element_line(colour = "grey")) +
            ggplot2::labs(x="p value",y="p-norm-distance")
    })
    
    output$coordinates_msg <- renderPrint({
        paste("The coordinates of ", input$city1, " are (", toString(lab5::get_coordinates(input$city1)),
                ") and the coordinates of ", input$city2, " are (", toString(lab5::get_coordinates(input$city2)), ")", sep="")
    })
    
    output$distance_msg <- renderPrint({
        pn_distance <- dist(matrix(c(lab5::get_coordinates(input$city1), lab5::get_coordinates(input$city2)), ncol=2, nrow = 2, byrow = TRUE),
                            p=input$p_value, method = "minkowski")
        paste("The ", toString(input$p_value),"-norm distance is ", toString(round(pn_distance, 3)), sep="")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
