library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Precision statistics"),
    sidebarPanel(
        sliderInput("n", "Number of ETAs", 
                    min = 0, max = 100000, value = 100, 
                    step = 1000, ticks = 5000, 
                    round = TRUE, animate = TRUE),
        sliderInput("precision", "Precision (factor)", 
                    min = 0, max = 2, value = .0, 
                    step = .05, ticks = .2),
        sliderInput("noise", "Noise (minutes)", 
                    min = 0, max = 60, value = .0, 
                    step = 2, ticks = 10),
        sliderInput("bias_factor", "Bias (factor)", 
                    min = -1, max = 1, value = .0, 
                    step = .1, ticks = .1),
        sliderInput("bias_constant", "Bias (constant)", 
                    min = -60, max = 60, value = .0, 
                    step = 10, ticks = 10),
        checkboxInput("outliers", "Add outliers", value = FALSE)
    ),
    mainPanel(
        tabsetPanel(type = "tabs", selected = "Raw",
          tabPanel("Histogram", plotOutput("histogram", height="540px")),
          tabPanel("Density", plotOutput("density", height="540px")),
          tabPanel("Raw", plotOutput("raw", height="540px")),
          tabPanel("Raw (error)", plotOutput("rawError", height="540px")),
          tabPanel("Grouped", plotOutput("grouped", height="540px")),
          tabPanel("Bias", plotOutput("bias", height="540px")),
          tabPanel("MAE", plotOutput("mae", height="540px")),
          tabPanel("RMSE", plotOutput("rmse", height="540px")),
          tabPanel("MAPE", plotOutput("mape", height="540px")))
)))
