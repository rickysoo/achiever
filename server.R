library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)

source('data.R')

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
    # %>%
    # formatStyle(
    #     'Total Goals',
    #     fontWeight = 'bold'
    # )
    
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
    
    tableNumbers <- function(col, units) {
        df <- load_selected_clubs()
        x <- df[[col]]
        
        x_mean <- mean(x)
        x_sd <- sd(x)
        x_min <- min(x)
        x_q1 <- quantile(x, 0.25)
        x_median <- median(x)
        x_min <- min(x)
        x_q3 <- quantile(x, 0.75)
        x_max <- max(x)
        
        text_mean <- sprintf('Clubs achieve an average of %.2f %s.', x_mean, tolower(units))
        text_sd <- sprintf('Most clubs achieve %.2f plus/minus %.2f %s.', x_mean, x_sd, tolower(units))
        text_min <- sprintf('The lowest performance is %.2f %s.', x_min, tolower(units))
        text_q1 <- sprintf('The 25th percentile is %.2f %s. 25%% of the clubs achieve less than this number of %s.', x_q1, tolower(units), tolower(units))
        text_median <- sprintf('The middle performance is %.2f %s.', x_median, tolower(units))
        text_q3 <- sprintf('The 75th percentile is %.2f %s. 75%% of the clubs achieve less than this number of %s.', x_q3, tolower(units), tolower(units))
        text_max <- sprintf('The highest performance is %.2f %s.', x_max, tolower(units))
        
        table <- data.frame(
            c('Mean', 'Standard Deviation', 'Minimum', 'Lower Quartile', 'Median', 'Upper Quartile', 'Maximum'),
            sprintf(paste0('%.2f ', units), c(x_mean, x_sd, x_min, x_q1, x_median, x_q3, x_max)),
            c(text_mean, text_sd, text_min, text_q1, text_median, text_q3, text_max)
        )            
        
        colnames(table) <- c('Statistic', 'Value', 'Explanation')
        return (table)
    }
    
    output$education_performance <- renderTable(
        { tablePerformance(load_selected_clubs(), 'Education Goals') },
        
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$education_barplot <- renderPlot({
        df <- load_selected_clubs()
        x <- table(df[['Education Goals']])
        
        barplot(
            x,
            xlab = 'Education Goals',
            ylab = 'Number of Clubs',
            col = color,
            main = GetChartTitle('Education Goals Achieved')
        )
    })
    
    output$education_stats <- renderTable(
        { tableNumbers('Education Goals', 'Goal(s)') },
        
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$education_boxplot <- renderPlot({
        df <- load_selected_clubs()
        x <- df[['Education Goals']]
        
        boxplot(x,
                horizontal = TRUE,
                xlab = 'Education Goals',
                col = color,
                main = GetChartTitle('Distribution of Education Goals Achieved')
        )
        
    })  
    
    output$education_goals <- renderPlot({
        df <- load_selected_clubs()
        x <- df[['Education Goals']]
        y <- df[['Total Goals']]
        
        plot(x, y,
             main = GetChartTitle('Education Goals vs. DCP Goals'),
             xlab = 'Education Goals',
             ylab = 'Goals'
        )
        
        lines(lowess(x, y), col = color)
    })
    
    output$education_rank <- renderPlot({
        df <- load_selected_clubs()
        x <- df[['Education Goals']]
        y <- df[['Rank']]
        
        plot(x, y,
             ylim = c(max(y), 1),
             main = GetChartTitle('Education Goals vs. Club Rank'),
             xlab = 'Education Goals',
             ylab = 'Club Rank'
        )
        
        lines(lowess(x, y), col = color)
    })
    
    output$members_histogram <- renderPlot({
        df <- load_selected_clubs()
        x <- df[['Members']]
        # breaks <- seq(0, max(x), by = 5)
        
        hist(x, breaks = 'sturges',
             col = color,
             xlab = 'Number of Members',
             ylab = 'Number of Clubs',
             main = GetChartTitle('Club Membership')
        )
    })
    
    # output$members_performance <- renderTable(
    #     {
    #         # df <- load_selected_clubs()
    #         # x <- df[['Members']]
    #         # bins <- seq(0, max(x), by = 5)
    #         # scores <- cut(x, bins)
    #         # table(scores)
    #         tablePerformance(load_selected_clubs(), 'Members')
    #     },
    #     
    #     bordered = TRUE,
    #     striped = TRUE,
    #     hover = TRUE
    # )
    
    output$charter_barplot <- renderPlot({
        df <- load_selected_clubs() %>%
            mutate(
                'Charter Strength' = ifelse(Members >= 20, 'Yes', 'No')
            )
        
        x <- table(df[['Charter Strength']])
        
        barplot(
            x,
            xlab = 'Charter Strength',
            ylab = 'Number of Clubs',
            col = color,
            main = GetChartTitle('Number of Clubs with Charter Strength')
        )
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
    
    output$members_stats <- renderTable(
        { tableNumbers('Members', 'Member(s)') },
        
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$members_boxplot <- renderPlot({
        df <- load_selected_clubs()
        x <- df[['Members']]
        
        boxplot(x,
                horizontal = TRUE,
                xlab = 'Membership Goals',
                col = color,
                main = GetChartTitle('Distribution of Membership Size')
        )
    })  
    
    output$members_goals <- renderPlot({
        df <- load_selected_clubs()
        x <- df[['Members']]
        y <- df[['Total Goals']]
        
        plot(x, y,
             main = GetChartTitle('Membership vs. DCP Goals'),
             xlab = 'Number of Members',
             ylab = 'Goals'
        )
        
        lines(lowess(x, y), col = color)
    })
    
    output$members_rank <- renderPlot({
        df <- load_selected_clubs()
        x <- df[['Members']]
        y <- df[['Rank']]
        
        plot(x, y,
             ylim = c(max(y), 1),
             main = GetChartTitle('Membership vs. Club Rank'),
             xlab = 'Number of Members',
             ylab = 'Club Rank'
        )
        
        lines(lowess(x, y), col = color)
    })
    
    output$table_goals <- renderTable(
        { tablePerformance(load_selected_clubs(), 'Total Goals') },
        
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$goals <- renderPlot({
        df <- load_selected_clubs()
        x <- table(df[['Total Goals']])
        
        barplot(
            x,
            main = GetChartTitle('DCP Goals Achieved'),
            xlab = 'DPC Goals',
            ylab = 'Number of Clubs',
            col = color
        )
    })
    
    output$table_distinguished <- renderTable(
        { tablePerformance(load_selected_clubs(), 'Distinguished') },
        
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$distinguished <- renderPlot({
        df <- load_selected_clubs()
        x <- table(df[['Distinguished']])
        
        barplot(
            x,
            main = GetChartTitle('Distinguished Club Status'),
            xlab = 'Distinguished Club Status',
            ylab = 'Number of Clubs',
            col = color
        )
    })
    
    output$goals_rank <- renderPlot({
        df <- load_selected_clubs()
        x <- df[['Total Goals']]
        y <- df[['Rank']]
        
        plot(x, y,
             ylim = c(max(y), 1),
             main = GetChartTitle('DCP Goals vs. Club Rank'),
             xlab = 'Total Goals Achieved',
             ylab = 'Club Rank'
        )
        
        lines(lowess(x, y), col = color)
    })
}