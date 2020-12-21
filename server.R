library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)

source('data.R')

ggplot_theme <- theme(
    plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
    axis.title = element_text(face = 'bold'),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = 'grey')
)

function(input, output, session) {
    values <- reactiveValues(
        district = default_district,
        division = '',
        area = '',
        title = paste0('District ', default_district)
    )
    
    load_clubs <- reactive({
        df <- load_data_clubs(values$district) %>%
            arrange(
                Status,
                desc(Distinguished), 
                desc(Goals), 
                desc(Members), 
                desc(EduGoals),
                desc(Growth)
            )
        
        df <- df %>%
            mutate(
                Rank = seq.int(nrow(df))
            ) %>%
            select(
                'Division',
                'Area',
                'Club',
                'Members',
                'Education Goals' = 'EduGoals',
                'Membership Goals' = 'MemGoals',
                'Training Goals' = 'TrnGoals',
                'Admin Goals' = 'AdmGoals',
                'Total Goals' = 'Goals',
                'Distinguished',
                'Rank'
            )
        
        return(df)
    })
    
    load_selected_clubs <- reactive({
        load_clubs() %>%
        { if (values$division == '') filter(., TRUE) else filter(., Division == values$division) } %>%
        { if (values$area == '') filter(., TRUE) else filter(., Area == values$area) }
    })  
    
    output$clubs <- renderDT(
        { load_selected_clubs() },
        
        rownames = FALSE,
        class = 'cell-border compact stripe',
        extensions = c('Responsive'),
        
        options = list(
            pageLength = 10,
            lengthMenu = list(
                c(10, 20, 30, 50, 100, nrow(load_clubs())),
                c(10, 20, 30, 50, 100, nrow(load_clubs()))
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

    GetTitle <- function() {
        title <- paste0('District ', values$district)
        
        if (values$division != '') {
            title <- paste0(title, ' Division ', values$division)
        }
        else if (values$area != '') {
            title <- paste0(title, ' Area ', values$area)
        }
        
        return (title)
    }
    
    GetChartTitle <- function(title) {
        title <- paste0(values$title, ' - ', title, ' as on ', format(Sys.time(), "%B %d, %Y"))
    }
    
    output$district <- renderUI({
        values$title <- GetTitle()
        items <- districts
        
        selectInput(
            inputId = 'district',
            label = NULL,
            choices = setNames(items, paste0('District ', items)),
            selected = values$district
        )
    })
    
    output$division <- renderUI({
        items <- sort(unique(load_clubs()[['Division']]))
        
        selectInput(
            inputId = 'division',
            label = NULL,
            choices = c('Select Division' = '', setNames(items, paste0('Division ', items)))
        )
    })
    
    output$area <- renderUI({
        items <- sort(unique(load_clubs()[['Area']]))
        
        selectInput(
            inputId = 'area',
            label = NULL,
            choices = c('Select Area' = '', setNames(items, paste0('Area ', items)))
        )
    })
    
    output$datetime <- renderText({
        paste0('Report generated on ', format(Sys.time(), tz = 'UTC'), ' (UTC)')
    })
    
    observeEvent(
        c(values$district, values$division, values$area),
        {
            values$title <- GetTitle()
            output$district_title <- renderText({
                paste0('Achiever\'s Dashboard - ', values$title)
            })
            
        }
    )
    
    observeEvent(input$district, {
        values$district <- input$district
        
        values$division <- ''
        updateSelectInput(session, inputId = 'division', selected = '')
        
        values$area <- ''
        updateSelectInput(session, inputId = 'area', selected = '')
    })
    
    observeEvent(input$division, {
        if (input$division == '') {
            return()
        }
        
        values$division <- input$division
        values$area <- ''
        
        updateSelectInput(session, inputId = 'area', selected = '')
    })
    
    observeEvent(input$area, {
        if (input$area == '') {
            return()
        }
        
        values$area <- input$area
        values$division <- ''
        
        updateSelectInput(session, inputId = 'division', selected = '')
    })
    
    observeEvent(input$showall, {
        values$division <- ''
        values$area <- ''
        
        updateSelectInput(session, inputId = 'division', selected = '')
        updateSelectInput(session, inputId = 'area', selected = '')
        
        clearSearch(proxy = dataTableProxy('clubs'))
    })
    
    tablePerformance <- function(df_clubs, col) {
        df <- data.frame(table(df_clubs[[col]]))
        
        colnames(df) <- c('Performance', 'Number of Clubs')
        df <- df %>%
            mutate(Percentage = sprintf('%.2f%%', df[['Number of Clubs']] / sum(df[['Number of Clubs']]) * 100))
        
        return (df)
    }
    
    output$education_performance <- renderTable(
        { tablePerformance(load_selected_clubs(), 'Education Goals') },
        
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$education_barplot <- renderPlot({
        ggplot(data = load_selected_clubs(), aes(x = `Education Goals`)) +
            geom_bar(fill = color) +
            labs(
                title = GetChartTitle('Education Goals Achieved'),
                x = 'Education Goals',
                y = 'Number of Clubs'
            ) +
            ggplot_theme
    })
    
    output$education_goals <- renderPlot({
        ggplot(data = load_selected_clubs(), aes(x = `Education Goals`, y = `Total Goals`)) +
            geom_point() +
            geom_smooth(method = 'loess', formula = y ~ x) +
            labs(
                title = GetChartTitle('Education Goals vs. DCP Goals'),
                x = 'Education Goals',
                y = 'Total Goals'
            ) +
            ggplot_theme
    })
    
    output$members_histogram <- renderPlot({
        ggplot(data = load_selected_clubs(), aes(x = Members)) +
            geom_bar(fill = color) +
            scale_x_binned() +
            labs(
                title = GetChartTitle('Club Membership'),
                x = 'Number of Members',
                y = 'Number of Clubs'
            ) +
            ggplot_theme
    })
    
    output$charter_barplot <- renderPlot({
        df <- load_selected_clubs() %>%
            mutate(
                'Charter Strength' = ifelse(Members >= 20, 'Yes', 'No')
            )

        ggplot(data = df, aes(x = `Charter Strength`)) +
            geom_bar(fill = color) +
            labs(
                title = GetChartTitle('Number of Clubs with Charter Strength'),
                x = 'Charter Strength',
                y = 'Number of Clubs'
            ) +
            ggplot_theme
    })
    
    output$charter_performance <- renderTable(
        { 
            df <- load_selected_clubs() %>%
                mutate(
                    'Charter Strength' = ifelse(Members >= 20, 'Yes', 'No')
                )
            
            tablePerformance(df, 'Charter Strength')
        },
        
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$members_goals <- renderPlot({
        ggplot(data = load_selected_clubs(), aes(x = Members, y = `Total Goals`)) +
            geom_point() +
            geom_smooth(method = 'loess', formula = y ~ x) +
            labs(
                title = GetChartTitle('Membership vs. DCP Goals'),
                x = 'Number of Members',
                y = 'Total Goals'
            ) +
            ggplot_theme
    })
    
    output$table_goals <- renderTable(
        { tablePerformance(load_selected_clubs(), 'Total Goals') },
        
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$goals <- renderPlot({
        ggplot(data = load_selected_clubs(), aes(x = `Total Goals`)) +
            geom_bar(fill = color) +
            scale_x_binned() +
            labs(
                title = GetChartTitle('DCP Goals Achieved'),
                x = 'DCP Goals',
                y = 'Number of Clubs'
            ) +
            ggplot_theme
    })
    
    output$table_distinguished <- renderTable(
        { tablePerformance(load_selected_clubs(), 'Distinguished') },
        
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$distinguished <- renderPlot({
        ggplot(data = load_selected_clubs(), aes(x = Distinguished)) +
            geom_bar(fill = color) +
            scale_x_discrete(drop = FALSE) +
            labs(
                title = GetChartTitle('Distinguished Club Status'),
                x = 'Distinguished Club Status',
                y = 'Number of Clubs'
            ) +
            ggplot_theme
    })
}