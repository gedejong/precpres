library(shiny)
require(ggplot2);
require(DT);

shinyServer(function(input, output) {
            mydata <- reactive({
                n <- input$n;
                df <- data.frame(realDistance = runif(n, min=0, max=180));
                df$eta <- rnorm(n, 
                      mean = df$realDistance * (1 + input$bias_factor) + input$bias_constant,
                      sd = df$realDistance * input$precision + input$noise) +
                    (input$outliers * (df$realDistance > 20) * (df$realDistance < 30) * (rnorm(n) > 2) * 100);
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
                qplot(data = mydata(), x = realDistance, y = eta, xlim=c(0,120), ylim=c(-60, 240))
            });
            output$rawError <- renderPlot({
                qplot(data = mydata(), x = realDistance, y = realDistance-eta, xlim=c(0,120), ylim=c(-120, 120))
            });
            output$grouped <- renderPlot({
                qplot(data = grouped(), x = groupedRealDistance, y = meanEta, xlim=c(0,120), ylim=c(0, 240))
            })
            output$bias <- renderPlot({
                qplot(data = grouped(), x = groupedRealDistance, y = bias, xlim=c(0,120), ylim=c(-60, 60))
            })
            output$mae <- renderPlot({
                qplot(data = grouped(), x = groupedRealDistance, y = mae, xlim=c(0,120), ylim=c(0, 60))
            })
            output$mape <- renderPlot({
                qplot(data = grouped(), x = groupedRealDistance, y = mape, xlim=c(0,120), ylim=c(0,1))
            })
            output$rmse <- renderPlot({
                qplot(data = grouped(), x = groupedRealDistance, y = rmse, xlim=c(0,120), ylim=c(0, 60))
            })
})
