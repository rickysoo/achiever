library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)

ui <- fluidPage(
    theme = shinytheme('united'),
    
    titlePanel('Achiever\'s Dashboard'),
    p('Your interactive Toastmasters dashboard. Development is ongoing. Send your ideas to ricky [at] rickysoo.com'),
    hr(),
    
    fluidRow(
        column(
            width = 4,
            uiOutput(outputId = 'district')
        ),
        column(
            width = 4,
            uiOutput(outputId = 'division')
        ),
        column(
            width = 4,
            uiOutput(outputId = 'area')
        )
    ),
    
    fluidRow(
        DTOutput(outputId = 'clubs')
            
        # tabsetPanel(
        #     tabPanel('Clubs', DTOutput(outputId = 'clubs'))
        # )
    ),
    
    hr(),
    p('This is a personal data science project based on open data on Toastmasters International web site.'),
    p('This site is neither authorized nor endorsed by Toastmasters International.')
    
)
