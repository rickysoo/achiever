library(tidyverse)
library(shiny)
library(shinythemes)
library(shinycustomloader)
library(DT)
library(ggplot2)
library(plotly)
library(highcharter)

source('data.R')

ui <- fluidPage(
    tags$head(includeHTML('google-analytics.html')),
    
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
            actionButton('showall', 'Show All')
        )
    ),
    
    tabsetPanel(
        tabPanel(
            'Overview',
            br(),
            
            withLoader(DTOutput('overview', width = '98%'), loader = 'pacman')
        ),
        
        tabPanel(
            'Education',
            br(),
            
            withLoader(DTOutput('education', width = '98%'), loader = 'pacman')
        ),
        
        tabPanel(
            'Membership',
            br(),
            
            withLoader(DTOutput('membership', width = '98%'), loader = 'pacman')
        ),
        
        # tabPanel(
        #     'Education',
        #     
        #     tabsetPanel(
        #         tabPanel(
        #             'Education Goals',
        #             plotOutput('education_barplot'),
        #             tableOutput('education_performance')
        #         ),
        #         tabPanel(
        #             'Club Goals',
        #             plotOutput('education_goals')
        #         )
        #     )
        # ),
        # 
        # tabPanel(
        #     'Membership',
        #     
        #     tabsetPanel(
        #         tabPanel(
        #             'Club Membership',
        #             plotOutput('members_histogram')
        #         ),
        #         tabPanel(
        #             'Charter Strength',
        #             plotOutput('charter_barplot'),
        #             tableOutput('charter_performance')
        #         ),
        #         tabPanel(
        #             'Club Goals',
        #             plotOutput('members_goals')
        #         )
        #     )
        # ),
        
        tabPanel(
            'Clubs',
            
            br(),
            fluidRow(
                column(
                    width = 9,
                    p('', align = 'right')
                ),
                column(
                    width = 3,
                    selectInput(
                        inputId = 'charts_xvar',
                        label = NULL,
                        choices = getKPIlist(),
                        selected = 'Total Goals'
                    )
                )
            ),
            
            # withLoader(highchartOutput('clubs_performance', width = '98%'), loader = 'pacman')
            withLoader(plotlyOutput('clubs_performance', width = '98%'), loader = 'pacman')
        ),

        tabPanel(
            'Areas',
            
            br(),
            fluidRow(
                column(
                    width = 9,
                    p('', align = 'right')
                ),
                column(
                    width = 3,
                    selectInput(
                        inputId = 'areas_yvar',
                        label = NULL,
                        choices = getKPIlist(),
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
                    p('', align = 'right')
                ),
                column(
                    width = 3,
                    selectInput(
                        inputId = 'divisions_yvar',
                        label = NULL,
                        choices = getKPIlist(),
                        selected = 'Total Goals'
                    )
                )
            ),
            
            withLoader(plotlyOutput('divisions_performance', width = '98%'), loader = 'pacman')
        ),
        
        tabPanel(
            'Chart',
            
            br(),
            fluidRow(
                column(
                    width = 6
                ),
                column(
                    width = 3,
                    selectInput(
                        inputId = 'chart_xvar',
                        label = NULL,
                        choices = getKPIlist(),
                        selected = 'Total Goals'
                    )
                ),
                column(
                    width = 3,
                    selectInput(
                        inputId = 'chart_yvar',
                        label = NULL,
                        choices = getKPIlist(),
                        selected = 'Members'
                    )
                )
            ),
            
            withLoader(highchartOutput('chart', height = '600'), loader = 'pacman')
        )
        
        # tabPanel(
        #     'Achievements',
        #     
        #     tabsetPanel(
        #         tabPanel(
        #             'Club Goals',
        #             plotOutput('goals'),
        #             tableOutput('table_goals')
        #         ),
        #         tabPanel(
        #             'Distinguished Clubs',
        #             plotOutput('distinguished'),
        #             tableOutput('table_distinguished')
        #         )
        #     )
        # )
        
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
