library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)

ui <- fluidPage(
    theme = shinytheme('united'),
    
    titlePanel(textOutput('district_title'), windowTitle = 'Achiever\'s Dashboard | Interactive Toastmasters Dashboard'),
    p('Your visual and interactive Toastmasters dashboard. Best viewed in computer browser.', br(), 'Work-in-progress. Send your ideas to ricky [at] rickysoo.com'),
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

            h3('Club Achievements'),
            p('There are 6 education goals up for grab. How many goals have your clubs achieved in your selected District, Division, or Area?'),
            plotOutput('education_barplot'),
            tableOutput('education_performance'),
            hr(),
            
            h3('Performance Statistics'),
            p('Some maths are essential for data analysis. But we make it easier for everyone with some explanation.'),
            tableOutput('education_stats'),
            p(
                a('Box plot', href = 'https://en.wikipedia.org/wiki/Box_plot', target = '_blank'), 
                'shows you graphically how spread out a value is. You can visually read the median, the upper quartile, the lower quartile, the maximum and minimum values, as well as the outliers if any.'
            ),
            plotOutput('education_boxplot'),
            hr(),
            
            h3('Overall Performance'),
            p('Why are education goals important for club success? Here we show you how the goals contribute to total DCP goals achieved and your club rank.'),
            plotOutput('education_goals'),
            plotOutput('education_rank')
        ),
        tabPanel(
            'Membership',

            h3('Club Membership'),
            p('Members are the lifeblood of a vibrant Toastmasters club. How many members do your clubs have now?'),
            plotOutput('members_histogram'),
            # tableOutput('members_performance'),

            h3('Charter Strength'),
            p('Clubs with charter strength are clubs with at least 20 members.'),
            plotOutput('charter_barplot'),
            tableOutput('charter_performance'),
            hr(),
            
            h3('Performance Statistics'),
            p('Some maths are essential for data analysis. But we make it easier for everyone with some explanation.'),
            tableOutput('members_stats'),
            p(
                a('Box plot', href = 'https://en.wikipedia.org/wiki/Box_plot', target = '_blank'), 
                'shows you graphically how spread out a value is. You can visually read the median, the upper quartile, the lower quartile, the maximum and minimum values, as well as the outliers if any.'
            ),
            plotOutput('members_boxplot'),
            hr(),
            
            h3('Overall Performance'),
            p('Why are members important for club success? Here we show you how they contribute to total DCP goals achieved and your club rank.'),
            plotOutput('members_goals'),
            plotOutput('members_rank')
        ),
        tabPanel(
            'Achievements',

            h3('Club Goals'),
            p('There are 10 goals to achieve in the Distinguished Club Program (DCP). How many goals have your clubs attained?'),
            plotOutput('goals'),
            tableOutput('table_goals'),
            hr(),
            
            h3('Distinguished Clubs'),
            p('How many clubs have achieved the Distinguished, Select Distinguished and President\'s Distinguished awards?'),
            plotOutput('distinguished'),
            tableOutput('table_distinguished'),
            hr(),
            
            h3('Club Rank'),
            p('Club rank is the ranking method used on this dashboard for listing clubs and data analysis. Club rank is determined by club status, Distinguished club award, DCP goals, membership, education goals, and net growth of membership. It is a good indicator of club success as it is in line with the DCP goals as shown below.'),
            plotOutput('goals_rank')
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
