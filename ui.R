library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)

ui <- fluidPage(
    theme = shinytheme('united'),
    
    titlePanel(textOutput('district_title'), windowTitle = 'Achiever\'s Dashboard | Interactive Toastmasters Dashboard'),
    p('Your visual and interactive Toastmasters dashboard. Best viewed in computer browser.', br(), 'Work-in-progress. Send your feedback to ricky [at] rickysoo.com'),
    p(),
    hr(),
    
    fluidRow(
        column(
            width = 3,
            uiOutput('district')
        ),
        column(
            width = 3,
            uiOutput('division')
        ),
        column(
            width = 3,
            uiOutput('area')
        ),
        column(
            width = 3,
            tags$head(
                tags$style(HTML('#showall{background-color:#004165}'))
            ),
            actionButton('showall', 'Show All Clubs')
        )
    ),

    tabsetPanel(
        tabPanel(
            'Clubs', 
            DTOutput('clubs', width = '98%')
        ),
        tabPanel(
            'Education',

            h3('Education Goals'),
            plotOutput('education_barplot'),
            tableOutput('education_performance'),
            hr(),
            
            h3('Connecting to DCP Goals'),
            plotOutput('education_goals')
        ),
        tabPanel(
            'Membership',

            h3('Club Membership'),
            plotOutput('members_histogram'),
            hr(),
            
            h3('Charter Strength'),
            plotOutput('charter_barplot'),
            tableOutput('charter_performance'),
            hr(),
            
            h3('Connecting to DCP Goals'),
            plotOutput('members_goals')
        ),
        tabPanel(
            'Achievements',

            h3('Club Goals'),
            plotOutput('goals'),
            tableOutput('table_goals'),
            hr(),
            
            h3('Distinguished Clubs'),
            plotOutput('distinguished'),
            tableOutput('table_distinguished')
        )
        # tabPanel(
        #     'Areas',
        #     h4('This page is still under construction. Please check back later.')
        # ),
        # tabPanel(
        #     'Divisions',
        #     h4('This page is still under construction. Please check back later.')
        # ),
        # tabPanel(
        #     'Grouping',
        #     h4('This page is still under construction. Please check back later.')
        # ),
        # tabPanel(
        #     'Forecast',
        #     h4('This page is still under construction. Please check back later.')
        # ),
        # tabPanel(
        #     'Time Machine',
        #     h4('This page is still under construction. Please check back later.')
        # ),
        # tabPanel(
        #     'Feedback',
        #     h4('This page is still under construction. Please check back later.')
        # )
    ),
    
    hr(),
    textOutput('datetime'),
    p('This site is neither authorized nor endorsed by Toastmasters International.')
)
