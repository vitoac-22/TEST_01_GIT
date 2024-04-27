# Modificado - Incluido Función Distribución
library(shiny)

ui <- fluidPage(
  titlePanel("Distribución Binomial"),
  substitute("Gráficas Función y Distribución de la Función de Probabilidad Binomial"),
  substitute("VÍCTOR E. ACARO B."),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Número de veces realizado el experimento:", value = 5, min = 1),
      sliderInput("p", "Probabilidad de éxito (p):", min = 0, max = 1, value = 0.5),
      numericInput("x", "Número de veces que desea se repita el éxito", value = 5, min = 1)
    ),
    mainPanel(
      plotOutput("plot"),
      plotOutput("plot2")
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    n <- input$n
    p <- input$p
    x <- input$x
    
    # Validación para asegurar que n >= x
    if (n < x) {
      return(NULL)
    }
    
    # Cálculo de la probabilidad
    prob <- dbinom(x, n, p)
    
    # Cálculo de la función de densidad
    x_vals <- 0:n
    f_x <- dbinom(x_vals, n, p)
    
    plot(x_vals, f_x, main = "Distribución Binomial",
         xlab = "Valores de X", ylab = "Valores de f(x)",
         type = "b", col = "red")
    
    abline(v = n * p, lty = 3, col = "blue")
    
    text(x = min(x_vals), y = max(f_x), labels = paste("Probabilidad:", round(prob, 4)), pos = 4)
  })
  
  output$plot2 <- renderPlot({
    n <- input$n
    p <- input$p
    
    # Cálculo de la función de distribución acumulada
    x <- seq(0, n)
    Fx <- pbinom(x, n, p)
    
    plot(x, Fx, main = "Distribución Binomial (Función de Distribución)",
         xlab = "Valores de X", ylab = "Valores de F(x)", 
         type = "b", col = "red")
    
    abline(h = 1, lty = 3, col = "blue")
  })
}

shinyApp(ui = ui, server = server)
