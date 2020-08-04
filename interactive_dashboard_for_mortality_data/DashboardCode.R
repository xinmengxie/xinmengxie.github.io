#install.packages("shiny")
#install.packages("shinydashboard")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(sandwich)
library(lmtest)
library(carData)
library(car)
library(ggplot2)
library(forecast)


# up load the dashboard to http://shiny.rstudio.com/


Mortality <- read.csv("NCHS_-_Injury_Mortality__United_States.csv")

#Mortality <- na.remove(Mortality)

ui<-shinyUI(
  dashboardPage(
    dashboardHeader(title = "Mortality"),
    dashboardSidebar(
      menuItem("Mortality Dashboard", tabName = "dashboard")
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                h1("Mortality trends for all kinds of types"),
                fluidRow(
                  column(2, 
                         selectInput("Sex","Sex options", choices = c("Both sexes","Female","Male")),
                         selectInput("Injury.Mechanism","Injury Mechanism options", choices = unique(Mortality$Injury.Mechanism)),
                         selectInput("Injury.Intent","Injury Intent options", choices = unique(Mortality$Injury.Intent)),
                         selectInput("Age.Group..Years.","Age options", choices = unique(Mortality$Age.Group..Years.)),
                         selectInput("Race","Race options", choices = unique(Mortality$Race))),
                  
                  box(plotOutput("time_series"))
                )
        )
      )
    )
  )
)



server<-shinyServer(function(input,output){
  output$time_series <- renderPlot({
    ggplot(Mortality %>% filter(Sex == input$Sex & Injury.Mechanism == input$Injury.Mechanism & Injury.Intent == input$Injury.Intent & Age.Group..Years. == input$Age.Group..Years. & Race == input$Race)) + 
      geom_line(aes(x=Year, y=Age.Specific.Rate),color = "blue", lwd = 1) +
      geom_point(aes(x=Year, y=Age.Specific.Rate),color = "red")+
      geom_ribbon(aes(x=Year, ymin =Age.Specific.Rate.Lower.Confidence.Limit , ymax = Age.Specific.Rate.Upper.Confidence.Limit), alpha = 0.2) +
      ylab("Mortality rate(*10^-3 %)") +
      theme(axis.title.y = element_text(angle = 0))
    
  })
})

shinyApp(ui,server)
