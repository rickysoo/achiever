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
            
            tabsetPanel(
                tabPanel(
                    'Education Goals',
                    h3('Education Goals'),
                    plotOutput('education_barplot'),
                    tableOutput('education_performance')
                ),
                tabPanel(
                    'Club Goals',
                    h3('Relating to Club Goals'),
                    plotOutput('education_goals')
                )
            )
        ),
        
        tabPanel(
            'Membership',
            
            tabsetPanel(
                tabPanel(
                    'Club Membership',
                    h3('Club Membership'),
                    plotOutput('members_histogram')
                ),
                tabPanel(
                    'Charter Strength',
                    h3('Charter Strength'),
                    plotOutput('charter_barplot'),
                    tableOutput('charter_performance')
                ),
                tabPanel(
                    'Club Goals',
                    h3('Relating to Club Goals'),
                    plotOutput('members_goals')
                )
            )
        ),
        
        tabPanel(
            'Achievements',
            
            tabsetPanel(
                tabPanel(
                    'Club Goals',
                    h3('Club Goals'),
                    plotOutput('goals'),
                    tableOutput('table_goals')
                ),
                tabPanel(
                    'Distinguished Clubs',
                    h3('Distinguished Clubs'),
                    plotOutput('distinguished'),
                    tableOutput('table_distinguished')
                )
            )
        ),

        tabPanel(
            'Divisions',
            
            tabsetPanel(
                tabPanel(
                    'Education Goals',    
                    h3('Education Goals'),
                    plotOutput('divisions_education')
                ),
                tabPanel(
                    'Membership',
                    h3('Membership'),
                    plotOutput('divisions_members')
                ),
                tabPanel(
                    'Total Goals',
                    h3('Total Goals'),
                    plotOutput('divisions_goals')
                ),
                tabPanel(
                    'Club Rank',
                    h3('Club Rank'),
                    plotOutput('divisions_rank')
                )
            )
        )
        # tabPanel(
        #     'Areas',
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
