library(shiny)
require(ggplot2);
require(DT);

shinyServer(function(input, output) {
            mydata <- reactive({
                n <- input$n;
                noise <- input$noise;
                precision <- input$precision;
                bias_factor <- input$bias_factor;
                bias_constant <- input$bias_constant;
                outliers <- input$outliers;
                df <- data.frame(realDistance = runif(n, min=0, max=180));
                df$eta <- df$realDistance + 
                    rnorm(n) * df$realDistance * precision +
                    df$realDistance * bias_factor +
                    bias_constant +
                    rnorm(n) * input$noise +
                    (outliers * (df$realDistance > 20) * (df$realDistance < 30) * (rnorm(n) > 2) * 100)
                    ;
                df
            });
            filtered <- reactive({mydata() %>% dplyr::filter(realDistance > 50 & realDistance < 60)});
            grouped <- reactive({
                grouped <- mydata() %>% 
                    dplyr::group_by(groupedRealDistance = round(realDistance)) %>%
                    dplyr::summarize(n = n(), 
                                     meanEta = mean(eta),
                                     mae = mean(abs(realDistance-eta)),
                                     mape = mean(abs(realDistance-eta) / ((realDistance + eta) / 2)),
                                     rmse = sqrt(mean((realDistance-eta) ^ 2)),
                                     bias = mean(realDistance-eta))
            });
            output$histogram <- renderPlot({
                ggplot(filtered(), aes(eta)) + xlim(-60, 240) + geom_histogram(binwidth=1)
            });
            output$density <- renderPlot({
                ggplot(filtered(), aes(eta)) + xlim(-60, 240) + geom_density()
            });
            output$raw <- renderPlot({
                df <- mydata()
                qplot(df$realDistance, df$eta, xlim=c(0,120), ylim=c(-60, 240))
            });
            output$rawError <- renderPlot({
                df <- mydata()
                qplot(df$realDistance, df$realDistance - df$eta, xlim=c(0,120), ylim=c(-120, 120))
            });
            output$grouped <- renderPlot({
                df <- grouped()
                qplot(df$groupedRealDistance, df$meanEta, xlim=c(0,120), ylim=c(0, 240))
            })
            output$bias <- renderPlot({
                df <- grouped()
                qplot(df$groupedRealDistance, df$bias, xlim=c(0,120), ylim=c(-60, 60))
            })
            output$mae <- renderPlot({
                df <- grouped()
                qplot(df$groupedRealDistance, df$mae, xlim=c(0,120), ylim=c(0, 60))
            })
            output$mape <- renderPlot({
                df <- grouped()
                qplot(df$groupedRealDistance, df$mape, xlim=c(0,120), ylim=c(0,1))
            })
            output$rmse <- renderPlot({
                df <- grouped()
                qplot(df$groupedRealDistance, df$rmse, xlim=c(0,120), ylim=c(0, 60))
            })
})
