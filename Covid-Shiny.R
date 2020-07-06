library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)

font_import()
loadfonts()
fonts()
par(family="Roboto Thin") 


download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv", "corona.csv")
cor<-read.csv("corona.csv")

cor$date<-as.Date(cor$date)
CountryList<-as.character(unique(cor$location))



ui<-shinyUI(
  fluidPage(
    tags$head(
      tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Abril+Fatface&family=Orbitron&display=swap');
      
      h1 {
        font-family: 'Orbitron', sans-serif;
        font-weight:100;
        line-height: 3;
        color: #fdffe3;
        background-color:#3d3899; 
        text-align:center;
        font-size: 50px
                      }",
                      ".small-box.bg-blue { background-color: #3d3899 !important; color: #fdffe3 !important; text-align:center; font-size: 15px; padding-bottom: 10px; padding-top: 5px; margin: 10px; border-radius: 15px}"
                      ,".selectize-input { font-size: 32px; line-height: 32px;}"
      ))),
    headerPanel("Covid-19 Country Comparison"),
    column(
      width=2, style="font-family: 'Roboto Thin'",
      selectInput(inputId = "location",label ="",
                  choices = CountryList,
                  selected = "China"), 
      valueBoxOutput('text1', width=9),
      valueBoxOutput('text2', width=9),
      valueBoxOutput('text3', width=9),
      valueBoxOutput('text4', width=9)),
    column(
      width=4, style="font-family: 'Roboto Thin'",
      plotlyOutput("mainPlot"),
      plotlyOutput("secondPlot")
    ),
    column(
      width=2, style="font-family: 'Roboto Thin'",
      selectInput(inputId = "location2",label ="",
                  choices = CountryList,
                  selected = "United States"), 
      valueBoxOutput('text5', width=9),
      valueBoxOutput('text6', width=9),
      valueBoxOutput('text7', width=9),
      valueBoxOutput('text8', width=9)),
    column(
      width=4, style="font-family: 'Roboto Thin'",
      plotlyOutput("mainPlot2"),
      plotlyOutput("secondPlot2")
    )
  ))


server<-function(input, output) {
  output$text1 <- renderValueBox({
    beds<- formatC(unique(cor$hospital_beds_per_thousand[cor$location==input$location]), format="d", big.mark=',')
    valueBox(strong(beds),"Hospital Beds per Thousand", color="blue")
  })
  output$text2 <- renderValueBox({
    life<- formatC(unique(cor$life_expectancy[cor$location==input$location]), format="d", big.mark=',')
    valueBox(strong(life),"Years of Life Expentancy", color="blue")
  })
  output$text3 <- renderValueBox({
    pop<- formatC(unique(cor$population_density[cor$location==input$location]), format="d", big.mark=',')
    valueBox(strong(pop),"Population Density[per km²]", color="blue")
  })
  output$text4 <- renderValueBox({
    gdp<- formatC(unique(cor$gdp_per_capita[cor$location==input$location]), format="d", big.mark=',')
    valueBox(strong(gdp),"GDP per Capita in US$", color="blue")
  })
  output$text5 <- renderValueBox({
    beds<- formatC(unique(cor$hospital_beds_per_thousand[cor$location==input$location2]), format="d", big.mark=',')
    valueBox(strong(beds),"Hospital Beds per Thousand", color="blue")
  })
  output$text6 <- renderValueBox({
    life<- formatC(unique(cor$life_expectancy[cor$location==input$location2]), format="d", big.mark=',')
    valueBox(strong(life),"Years of Life Expentancy",  color="blue")
  })
  output$text7 <- renderValueBox({
    pop<- formatC(unique(cor$population_density[cor$location==input$location2]), format="d", big.mark=',')
    valueBox(strong(pop),"Population Density[per km²]", color="blue")
  })
  output$text8 <- renderValueBox({
    gdp<- formatC(unique(cor$gdp_per_capita[cor$location==input$location2]), format="d", big.mark=',')
    valueBox(strong(gdp),"GDP per Capita in US$", color="blue")
  })
  output$mainPlot <- renderPlotly({ggplotly(ggplot(data=cor[c(cor$location==input$location),], aes(x=date)) +  
                                              geom_line(aes(y=new_cases),color="#ff6073", size=0.1)+
                                              geom_point(aes(y=new_cases),color="#ff6073", size=1) +
                                              geom_line(aes(y=new_deaths),color="#21364b", size=0.1)+
                                              geom_point(aes(y=new_deaths),color="#21364b", size=1) +
                                              xlab('Date') +
                                              ylab('Daily New Cases')+
                                              #ylim(0,  max(cor$new_cases[cor$location==input$location],cor$new_cases[cor$location==input$location2]))+
                                              ggtitle( paste("Cases Per Day ~", input$location))+
                                              theme_minimal()+
                                              theme(plot.title = element_text(family="Roboto Thin", size=16),text=element_text(family="Roboto Thin", size=12)))
  })
  output$secondPlot <- renderPlotly({ggplotly(ggplot(data=cor[c(cor$location==input$location),], aes(x=date)) +  
                                                geom_bar(aes(y=total_cases/population), stat="identity", fill="#3d3899", width = 0.5)+
                                                geom_bar(aes(y=total_deaths/population), stat="identity", fill="#21364b", width = 0.5)+
                                                xlab('Date') +
                                                ylab('Percentage')+
                                                #ylim(0, max((cor$total_cases[cor$location==input$location]/cor$population[cor$location==input$location]),(cor$total_cases[cor$location==input$location2]/cor$population[cor$location==input$location2])))+
                                                ggtitle(paste("Percentage Total Cases of Population ~", input$location))+
                                                labs(colour='Location') +
                                                theme_minimal()+
                                                theme(plot.title = element_text(family="Roboto Thin", size=16),text=element_text(family="Roboto Thin", size=12)))
  })
  output$mainPlot2 <- renderPlotly({ggplotly(ggplot(data=cor[c(cor$location==input$location2),], aes(x=date)) +  
                                               geom_line(aes(y=new_cases),color="#ff6073", size=0.1)+
                                               geom_point(aes(y=new_cases),color="#ff6073", size=1) +
                                               geom_line(aes(y=new_deaths),color="#21364b", size=0.1)+
                                               geom_point(aes(y=new_deaths),color="#21364b", size=1) +
                                               xlab('Date') +
                                               ylab('Daily New Cases')+
                                               #ylim(0,  max(cor$new_cases[cor$location==input$location],cor$new_cases[cor$location==input$location2]))+
                                               ggtitle( paste("Cases Per Day ~", input$location2))+
                                               theme_minimal()+
                                               theme(plot.title = element_text(family="Roboto Thin", size=16),text=element_text(family="Roboto Thin", size=12)))
  })
  output$secondPlot2 <- renderPlotly({ggplotly(ggplot(data=cor[c(cor$location==input$location2),], aes(x=date)) +  
                                                 geom_bar(aes(y=total_cases/population), stat="identity", fill="#3d3899", width = 0.5)+
                                                 geom_bar(aes(y=total_deaths/population), stat="identity", fill="#21364b", width = 0.5)+
                                                 xlab('Date') +
                                                 ylab('Percentage')+
                                                 #ylim(0, max((cor$total_cases[cor$location==input$location]/cor$population[cor$location==input$location]),(cor$total_cases[cor$location==input$location2]/cor$population[cor$location==input$location2])))+
                                                 ggtitle(paste("Percentage Total Cases of Population ~", input$location2))+
                                                 labs(colour='Location') +
                                                 theme_minimal()+
                                                 theme(plot.title = element_text(family="Roboto Thin", size=16),text=element_text(family="Roboto Thin", size=12)))
  })
}


shinyApp(ui = ui, server = server)
