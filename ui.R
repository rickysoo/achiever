library(tidyverse)
library(shiny)
library(shinythemes)
library(shinycustomloader)
library(DT)
library(ggplot2)
library(plotly)

source('data.R')

ui <- fluidPage(
    theme = shinytheme('united'),
    
    titlePanel(textOutput('district_title'), windowTitle = 'Achiever\'s Dashboard | Interactive Toastmasters Dashboard For Data Analysis'),
    p('Your visual and interactive Toastmasters dashboard for data analysis. Best viewed in computer browser.'),
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
                tags$style(HTML('#showall {background-color:#004165}'))
            ),
            actionButton('showall', 'Show All Clubs')
        )
    ),
    
    tabsetPanel(
        tabPanel(
            'Home',
            br(),
            
            withLoader(DTOutput('home', width = '98%'), loader = 'pacman')
        ),
        
        tabPanel(
            'Clubs', 
            
            br(),
            fluidRow(
                column(
                    width = 6,
                    p('View clubs on', align = 'right')
                ),
                column(
                    width = 3,
                    selectInput(
                        inputId = 'clubs_yaxis',
                        label = NULL,
                        choices = setNames(KPI$Variable, KPI$Name),
                        selected = 'Net Growth'
                    )
                ),
                column(
                    width = 3,
                    selectInput(
                        inputId = 'clubs_xaxis',
                        label = NULL,
                        choices = setNames(KPI$Variable, KPI$Name),
                        selected = 'Total Goals'
                    )
                )
            ),
            
            withLoader(plotlyOutput('clubs_performance', width = '98%'), loader = 'pacman')
        ),
        
        tabPanel(
            'Areas',
            
            br(),
            fluidRow(
                column(
                    width = 9,
                    p('View areas on', align = 'right')
                ),
                column(
                    width = 3,
                    selectInput(
                        inputId = 'areas_yaxis',
                        label = NULL,
                        choices = setNames(KPI$Variable, KPI$Name),
                        selected = 'Total Goals'
                    )
                )
            ),
            
            withLoader(plotlyOutput('areas_performance', width = '98%'), loader = 'pacman')
        ),
        
        tabPanel(
            'Divisions',
            
            br(),
            fluidRow(
                column(
                    width = 9,
                    p('View divisions on', align = 'right')
                ),
                column(
                    width = 3,
                    selectInput(
                        inputId = 'divisions_yaxis',
                        label = NULL,
                        choices = setNames(KPI$Variable, KPI$Name),
                        selected = 'Total Goals'
                    )
                )
            ),
            
            withLoader(plotlyOutput('divisions_performance', width = '98%'), loader = 'pacman')
        ),
        
        tabPanel(
            'Education',
            
            tabsetPanel(
                tabPanel(
                    'Education Goals',
                    plotOutput('education_barplot'),
                    tableOutput('education_performance')
                ),
                tabPanel(
                    'Club Goals',
                    plotOutput('education_goals')
                )
            )
        ),
        
        tabPanel(
            'Membership',
            
            tabsetPanel(
                tabPanel(
                    'Club Membership',
                    plotOutput('members_histogram')
                ),
                tabPanel(
                    'Charter Strength',
                    plotOutput('charter_barplot'),
                    tableOutput('charter_performance')
                ),
                tabPanel(
                    'Club Goals',
                    plotOutput('members_goals')
                )
            )
        ),
        
        tabPanel(
            'Achievements',
            
            tabsetPanel(
                tabPanel(
                    'Club Goals',
                    plotOutput('goals'),
                    tableOutput('table_goals')
                ),
                tabPanel(
                    'Distinguished Clubs',
                    plotOutput('distinguished'),
                    tableOutput('table_distinguished')
                )
            )
        )
        
    ),
    
    hr(),
    p(
        a('Data source: Toastmasters International Dashboard', href = 'http://dashboards.toastmasters.org', target = '_blank')
    ),
    p('This site is neither authorized nor endorsed by Toastmasters International.')
)

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
