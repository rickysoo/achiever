library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)

source('data.R')

function(input, output, session) {
    load_clubs <- reactive({
        df <- load_data_clubs() %>%
            # mutate(
            #     Club = paste0('<a href="', URL, '" target="blank">', Club, '</a>')
            # ) %>%
            select(
                'Division',
                'Area',
                'Club',
                'Base',
                'Members',
                'Education Goals' = 'EduGoals',
                'Membership Goals' = 'MemGoals',
                'Training Goals' = 'TrnGoals',
                'Admin Goals' = 'AdmGoals',
                'Total Goals' = 'Goals',
                'Distinguished'
            ) %>%
            arrange(desc(Distinguished), desc(`Total Goals`), desc(Members), desc(`Education Goals`))
    })
    
    output$clubs <- renderDT(
        {load_clubs()},
        rownames = FALSE,
        class = 'cell-border stripe',
        extensions = 'Responsive',
        options = list(
            pageLength = nrow(load_clubs()),
            lengthMenu = list(
                c(10, 25, 50, 100, nrow(load_clubs())),
                c(10, 25, 50, 100, nrow(load_clubs()))
            ),
            columnDefs = list(
                list(
                    className = 'dt-left',
                    targets = 2
                ),
                list(
                    className = 'dt-center',
                    targets = c(0:1, 3:10)
                )
            )
        )
    ) 
    # %>%
    # formatStyle(
    #     'Total Goals',
    #     fontWeight = 'bold'
    # )
    
    output$district <- renderUI(
        selectInput(
            inputId = 'district',
            label = 'Select District',
            choices = c('51', '102'),
            selected = '102'
        )
    )
    
    output$division <- renderUI(
        selectInput(
            inputId = 'division',
            label = 'Select Division',
            choices = sort(unique(load_clubs()[['Division']]))
        )
    )
    
    output$area <- renderUI(
        selectInput(
            inputId = 'area',
            label = 'Select Area',
            choices = sort(unique(load_clubs()[['Area']]))
        )
    )
    
    
    # observe({
    #     updateSelectInput(session, inputId = 'Division', choices = unique(load_clubs()[['Division']]))
    # })
}
