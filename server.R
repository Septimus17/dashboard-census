# Server logic ----
shinyServer(function(input, output) {
  
  #### checkbox ####
  observe({ 
    if(is.character(new_adult_data[[input$Var.dens]])==T)
      shinyjs::show("checkrate")
    else 
      shinyjs::hide("checkrate")
  })
  
  #### plots ####
  output$density.plot <- renderPlotly({
    prog()
    dens_plot(input$Var.dens, input$checkrate, input$checkimp1, input$checktest1)
  })

  output$two.plot <- renderPlotly({
    prog()
    two_plot(input$Var1, input$Var2, input$checkimp2, input$checktest2)
  })
  
  output$three.plot <- renderPlotly({
    prog()
    ggplotly(three_plot(input$Var3.1, input$Var3.2, input$Var3.3, input$checkimp3, input$checktest3))
  })
  
  output$age.plot <- renderPlotly({
    ggplotly(density_plot(adult_data,"age"))%>% 
      layout(xaxis = list(title = "age")) %>% 
      style(text = "age", 
            hoverinfo = "x+text")
  })
  
  output$work.plot <- renderPlotly({
    ggplotly(per_plot(adult_data, "workclass")) %>% 
      layout(xaxis = list(title = "workclass")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  output$fnlw.plot <- renderPlotly({
    ggplotly(density_plot(adult_data,"fnlwgt"))%>% 
      layout(xaxis = list(title = "fnlwgt")) %>% 
      style(text = "fnlwgt", 
            hoverinfo = "x+text")
  })
  
  output$edu.plot <- renderPlotly({
    ggplotly(per_plot(adult_data, "education"))%>% 
      layout(xaxis = list(title = "education")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  output$edu.num.plot <- renderPlotly({
    ggplotly(density_plot(adult_data,"education.num"))%>% 
      layout(xaxis = list(title = "education.num")) %>% 
      style(text = "education.num", 
            hoverinfo = "x+text")
  })
  
  output$mar.plot <- renderPlotly({
    ggplotly(per_plot(adult_data, "marital.status"))%>% 
      layout(xaxis = list(title = "marital.status")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  output$occ.plot <- renderPlotly({
    ggplotly(per_plot(adult_data, "occupation"))%>% 
      layout(xaxis = list(title = "occupation")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  output$rel.plot <- renderPlotly({
    ggplotly(per_plot(adult_data, "relationship"))%>% 
      layout(xaxis = list(title = "relationship")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  output$race.plot <- renderPlotly({
    ggplotly(per_plot(adult_data, "race"))%>% 
      layout(xaxis = list(title = "race")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  output$sex.plot <- renderPlotly({
    ggplotly(per_plot(adult_data, "sex"))%>% 
      layout(xaxis = list(title = "sex")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  output$gain.plot <- renderPlotly({
    ggplotly(density_plot(adult_data,"capital.gain"))%>% 
      layout(xaxis = list(title = "capital.gain")) %>% 
      style(text = "capital.gain", 
            hoverinfo = "x+text")
  })
  
  output$loss.plot <- renderPlotly({
    ggplotly(density_plot(adult_data,"capital.loss"))%>% 
      layout(xaxis = list(title = "capital.loss")) %>% 
      style(text = "capital.loss", 
            hoverinfo = "x+text")
  })
  
  output$hours.plot <- renderPlotly({
    ggplotly(density_plot(adult_data,"hours.per.week"))%>% 
      layout(xaxis = list(title = "hours.per.week")) %>% 
      style(text = "hours.per.week", 
            hoverinfo = "x+text")
  })
  
  output$country.plot <- renderPlotly({
    ggplotly(per_plot(adult_data, "native.country"))%>% 
      layout(xaxis = list(title = "native.country")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  output$new.work.plot <- renderPlotly({
    prog()
    ggplotly(per_plot(new_adult_data, "workclass")) %>% 
      layout(xaxis = list(title = "workclass")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  output$new.mar.plot <- renderPlotly({
    ggplotly(per_plot(new_adult_data, "marital.status")) %>% 
      layout(xaxis = list(title = "marital.status")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  output$new.occ.plot <- renderPlotly({
    ggplotly(per_plot(new_adult_data, "occupation")) %>% 
      layout(xaxis = list(title = "occupation")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  output$new.gain.plot <- renderPlotly({
    ggplotly( per_plot(new_adult_data, "gain")) %>% 
      layout(xaxis = list(title = "gain")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  output$new.loss.plot <- renderPlotly({
    ggplotly(per_plot(new_adult_data, "loss"))%>% 
      layout(xaxis = list(title = "loss")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  output$new.country.plot <- renderPlotly({
    ggplotly(per_plot(new_adult_data, "native.country")) %>% 
      layout(xaxis = list(title = "native.country")) %>% 
      style(text = "rate", 
            hoverinfo = "y+text")
  })
  
  #### prints ####
  output$test2 <- renderUI({
    box(width = 12, 
        {if (input$checkimp2 == T && input$checktest2 == T)
          dati <- testimp
        else {
          if (input$checkimp2 == F && input$checktest2 == F)
            dati <- new_adult_data
          else 
            if (input$checkimp2 == F && input$checktest2 == T)
              dati <- new_adult_test
            else dati <- dataimp
        }
        if (input$Var1 != input$Var2){
          facto <- select_if(dati,is.character)
          if (is.character(dati[[input$Var1]]) == F && 
              is.character(dati[[input$Var2]]) == F){
            pears<-cor(dati[[input$Var1]],dati[[input$Var2]])
            p(tags$b("Coefficiente di correlazione lineare di Pearson:"),
              print(round(pears, digits = 3), quote = F, row.names = F))
          } 
          else {
            if (is.character(dati[[input$Var1]]) == T && 
                is.character(dati[[input$Var2]]) == F) {
              form <- as.formula(
                paste(input$Var2, 
                      paste(names(facto), collapse = " + "), 
                      sep = " ~ "))
              test <- summary(
                aov(form, dati)
              )
              Var <- names(facto)
              P.value <- na.omit(round(test[[1]][["Pr(>F)"]], digits=3))
              Fstatistics <- na.omit(round(test[[1]][["F value"]], digits=3))
              df <- data.frame(Var, Fstatistics, P.value)
              div(style = 'overflow-x: scroll',
                  DT::renderDataTable(
                    DT::datatable({
                      df
                    },
                    rownames = F,
                    class = "compact",
                    options = list(dom = 't')
                    )
                  )
              )
            }
            else {
              if (is.character(dati[[input$Var1]]) == F && 
                  is.character(dati[[input$Var2]]) == T) {
                form <- as.formula(
                  paste(input$Var1, 
                        paste(names(facto), collapse = " + "), 
                        sep = " ~ "))
                test <- summary(aov(form, dati))
                Var <- names(facto)
                P.value <- na.omit(round(test[[1]][["Pr(>F)"]], digits=3))
                Fstatistics <- na.omit(round(test[[1]][["F value"]], digits=3))
                df <- data.frame(Var, Fstatistics, P.value)
                div(style = 'overflow-x: scroll', #aggiunge lo scroll orizzontale
                    DT::renderDataTable(
                      DT::datatable({
                        df
                      },
                      rownames = F,
                      class = "compact",
                      options = list(dom = 't') #toglie search e show entries
                      )
                    )
                )
              }
              else { 
                tbl <- table(dati[[input$Var1]], dati[[input$Var2]])
                ct <- chisq.test(tbl)
                p(tags$b("Test Chi-quadrato:"),
                  print(round(ct$statistic, digits = 3)), br(),
                  tags$b("P-value:") ,print(round(ct$p.value, digits = 3))
                )
              }
            }
          }
        }
        else {
          p("Seleziona due variabili diverse.")
        }
        }
    )
  })
  
  #### table ####  
  output$table <- DT::renderDataTable(
    DT::datatable({
      prog()
      data <- adult_data
      if (input$work != "All") {
        data <- data[data$workclass == input$work,]
      }
      if (input$race != "All") {
        data <- data[data$race == input$race,]
      }
      if (input$rel != "All") {
        data <- data[data$relationship == input$rel,]
      }
      data
    })
  )
})
