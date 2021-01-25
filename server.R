library(tidyverse)
library(shiny)
library(shinythemes)
library(shinycustomloader)
library(DT)
library(ggplot2)
library(plotly)
library(scales)

source('data.R')

ggplot_defaults <- theme(
    plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
    axis.title = element_text(face = 'bold'),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = 'grey')
)

function(input, output, session) {
    # cdata <- session$clientData
    
    # get_default_district <- function() {
    #     return('51')
    # print(district)
    # 
    # if(is.null(district)) {
    #     print(district)
    #     default_district
    # }
    # else {
    #     print(district)
    #     district <- str_remove(district, '\\?')
    #     print(district)
    #     
    #     print(default_district)
    #     
    #     ifelse(district %in% districts, district, default_district)
    # }
    # }
    
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
                desc(NetGrowth)
            )
        
        df <- df %>%
            mutate(
                Rank = row_number(),
                `Level 1` = Edu1,
                `Level 2` = Edu2 + Edu3,
                `Level 3` = Edu4,
                `Level 4+` = Edu5 + Edu6,
                Awards = Edu1 + Edu2 + Edu3 + Edu4 + Edu5 + Edu6,
                `Awards / Base` = ifelse(Base > 0, Awards / Base, 0),
                `Awards / Members` = ifelse(Members > 0, Awards / Members, 0)
            ) %>%
            select(
                'Division',
                'Area',
                'Club',
                'Level 1',
                'Level 2',
                'Level 3',
                'Level 4+',
                'Awards',
                'Awards / Base',
                'Awards / Members',
                'Education Goals' = 'EduGoals',
                'Base',
                'Retained',
                'Retention',
                'New',
                'Members',
                'Net Growth' = 'NetGrowth',
                'Club Growth' = 'ClubGrowth',
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
    
    output$overview <- renderDT({
        datatable(
            load_selected_clubs() %>%
                select(Division, Area, Club, Members, `Education Goals`, `Membership Goals`, `Training Goals`, `Admin Goals`, `Total Goals`, Distinguished, Rank),
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
        ) %>%
            formatStyle(c('Total Goals'), fontWeight = 'bold')
    }) 
    
    output$education <- renderDT({
        datatable(
            load_selected_clubs() %>%
                select(Division, Area, Club, `Level 1`, `Level 2`,`Level 3`,`Level 4+`, Awards, `Awards / Base`, `Awards / Members`, `Education Goals`, Distinguished, Rank),
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
                        targets = c(0:1, 3:12)
                    )
                )
            )
        ) %>%
            formatRound(c('Awards / Base', 'Awards / Members'), 1) %>%
            formatStyle(c('Education Goals'), fontWeight = 'bold')
    }) 
    
    output$membership <- renderDT({
        datatable(
            load_selected_clubs() %>%
                select(Division, Area, Club, Base, Retained, New, Members, `Net Growth`, `Club Growth`, Retention, `Membership Goals`, Distinguished, Rank),
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
                        targets = c(0:1, 3:11)
                    )
                )
            )
        ) %>%
            formatPercentage(c('Club Growth', 'Retention'), 1) %>%
            formatStyle(c('Membership Goals'), fontWeight = 'bold')
    }) 
    
    output$clubs_performance2 <- renderPlotly({
        xaxis <- input$clubs_xaxis
        yaxis <- input$clubs_yaxis
        
        viz <- ggplot(data = load_selected_clubs(), aes(x = .data[[xaxis]], y = .data[[yaxis]], label = Club)) +
            geom_point(aes(color = Division, size = Members), alpha = 0.7, position = 'jitter') +
            labs(
                x = getKPIname(xaxis),
                y = getKPIname(yaxis)
            ) +
            guides(
                size = FALSE
            ) +
            geom_smooth(method = 'loess', formula = y ~ x, se = FALSE) +
            ggplot_defaults
        
        ggplotly(viz, height = 600) %>%
            layout(
                title = GetChartTitle('Club Performance'),
                xaxis = list(fixedrange = TRUE),
                yaxis = list(fixedrange = TRUE),
                margin = 50
            )
    })
    
    output$areas_performance <- renderPlotly({
        data <- load_selected_clubs()
        yaxis <- input$areas_yaxis
        percentage <- (yaxis %in% c('Retention', 'Club Growth'))
        
        viz <- ggplot(data = data, aes(x = reorder(Area, .data[[yaxis]], FUN = mean), y = .data[[yaxis]], fill = Area)) +
            geom_boxplot() +
            { if (percentage) scale_y_continuous(labels = scales::percent_format(scale = 100)) else geom_blank() } +
            labs(
                x = 'Area',
                y = getKPIname(yaxis)
            ) +
            coord_flip() +
            ggplot_defaults +
            theme(legend.position = 'none')
        
        ggplotly(viz, height = nrow(data) * 10 + 250) %>%
            layout(
                title = GetChartTitle('Area Performance'),
                xaxis = list(fixedrange = TRUE),
                yaxis = list(fixedrange = TRUE),
                margin = 50
            )
    })
    
    output$divisions_performance <- renderPlotly({
        data <- load_selected_clubs()
        yaxis <- input$divisions_yaxis
        percentage <- (yaxis %in% c('Retention', 'Club Growth'))
        
        viz <- ggplot(data = data, aes(x = reorder(Division, .data[[yaxis]], FUN = mean), y = .data[[yaxis]], fill = Division)) +
            geom_boxplot() +
            { if (percentage) scale_y_continuous(labels = scales::percent_format(scale = 100)) else geom_blank() } +
            labs(
                x = 'Division',
                y = getKPIname(yaxis)
            ) +
            coord_flip() +
            ggplot_defaults +
            theme(legend.position = 'none')
        
        ggplotly(viz, height = nrow(data) * 2 + 250) %>%
            layout(
                title = GetChartTitle('Division Performance'),
                xaxis = list(fixedrange = TRUE),
                yaxis = list(fixedrange = TRUE),
                margin = 50
            )
    })
    
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
    
    GetChartTitle <- function(title, district = FALSE) {
        if (district) {
            title <- paste0('District ', values$district, ' - ', title, ' as on ', format(Sys.time(), "%B %d, %Y"))
        }
        else {
            title <- paste0(values$title, ' - ', title, ' as on ', format(Sys.time(), "%B %d, %Y"))
        }
    }
    
    output$district <- renderUI({
        values$title <- GetTitle()
        items <- districts
        
        # query_district <- cdata$url_search
        # 
        # if(is.null(query_district)) {
        #     district <- default_district
        # }
        # else {
        #     query_district <- str_remove(query_district, '\\?')
        #     district <- ifelse(query_district %in% districts, query_district, default_district)
        # }
        # 
        selectInput(
            inputId = 'district',
            label = NULL,
            choices = setNames(items, paste0('District ', items)),
            selected = values$district #district
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
        
        proxy <- dataTableProxy('home')
        clearSearch(proxy = proxy)
        # replaceData(proxy = proxy, data = load_clubs(), resetPaging = TRUE, clearSelection = FALSE)
    })
    
    output$clubs_performance <- renderPlotly({
        data <- load_selected_clubs()
        xaxis <- input$charts_xaxis
        histogram <- (xaxis %in% c('Awards', 'Level 1', 'Level 2', 'Level 3', 'Level 4+', 'Base', 'Members', 'New', 'Retained', 'Net Growth', 'Retention', 'Club Growth', 'Awards / Base', 'Awards / Members'))
        percentage <- (xaxis %in% c('Retention', 'Club Growth'))
        
        viz <- ggplot(data = data, aes(x = .data[[xaxis]], fill = Division)) +
            { if (histogram) geom_histogram(bins = 10) else geom_bar() } +
            {
                if (histogram)
                    if (percentage)
                        scale_x_continuous(breaks = pretty_breaks(), labels = scales::percent_format(scale = 100))
                    else
                        scale_x_continuous(breaks = pretty_breaks())
                else
                    geom_blank()
            } +
            labs(
                x = getKPIname(xaxis),
                y = 'Number of Clubs'
            ) +
            ggplot_defaults
        
        ggplotly(viz, height = 600) %>%
            layout(
                title = GetChartTitle('Club Performance'),
                xaxis = list(fixedrange = TRUE),
                yaxis = list(fixedrange = TRUE),
                margin = 50
            )
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
        ggplot(data = load_selected_clubs(), aes(x = factor(`Education Goals`), fill = Division)) +
            geom_bar() +
            labs(
                title = GetChartTitle('Education Goals Achieved'),
                x = 'Education Goals',
                y = 'Number of Clubs'
            ) +
            ggplot_defaults
    })
    
    output$education_goals <- renderPlot({
        ggplot(data = load_selected_clubs(), aes(x = `Education Goals`, y = `Total Goals`)) +
            geom_point() +
            geom_smooth(method = 'loess', formula = y ~ x) +
            labs(
                title = GetChartTitle('Education Goals and Club Goals'),
                x = 'Education Goals',
                y = 'Club Goals'
            ) +
            ggplot_defaults
    })
    
    output$members_histogram <- renderPlot({
        ggplot(data = load_selected_clubs(), aes(x = Members, fill = Division)) +
            geom_bar() +
            scale_x_binned() +
            labs(
                title = GetChartTitle('Club Membership'),
                x = 'Number of Members',
                y = 'Number of Clubs'
            ) +
            ggplot_defaults
    })
    
    output$charter_barplot <- renderPlot({
        df <- load_selected_clubs() %>%
            mutate(
                'Charter Strength' = ifelse(Members >= 20, 'Yes', 'No')
            )
        
        ggplot(data = df, aes(x = `Charter Strength`, fill = Division)) +
            geom_bar() +
            labs(
                title = GetChartTitle('Clubs with Charter Strength'),
                x = 'Charter Strength',
                y = 'Number of Clubs'
            ) +
            ggplot_defaults
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
                title = GetChartTitle('Membership and Club Goals'),
                x = 'Number of Members',
                y = 'Club Goals'
            ) +
            ggplot_defaults
    })
    
    output$table_goals <- renderTable(
        { tablePerformance(load_selected_clubs(), 'Total Goals') },
        
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$goals <- renderPlot({
        ggplot(data = load_selected_clubs(), aes(x = factor(`Total Goals`), fill = Division)) +
            geom_bar() +
            # scale_x_binned() +
            labs(
                title = GetChartTitle('Club Goals Achieved'),
                x = 'Club Goals',
                y = 'Number of Clubs'
            ) +
            ggplot_defaults
    })
    
    output$table_distinguished <- renderTable(
        { tablePerformance(load_selected_clubs(), 'Distinguished') },
        
        bordered = TRUE,
        striped = TRUE,
        hover = TRUE
    )
    
    output$distinguished <- renderPlot({
        ggplot(data = load_selected_clubs(), aes(x = Distinguished, fill = Division)) +
            geom_bar() +
            scale_x_discrete(drop = FALSE) +
            labs(
                title = GetChartTitle('Distinguished Club Status'),
                x = 'Distinguished Club Status',
                y = 'Number of Clubs'
            ) +
            ggplot_defaults
    })
    
    output$divisions_education <- renderPlot({
        ggplot(data = load_clubs(), aes(x = reorder(Division, `Education Goals`, FUN = mean), y = `Education Goals`, fill = Division)) +
            geom_boxplot() +
            labs(
                title = GetChartTitle('Division Performance on Education Goals', district = TRUE),
                x = 'Division',
                y = 'Education Goals'
            ) +
            coord_flip() +
            ggplot_defaults +
            theme(legend.position = 'none')
    })
    
    output$divisions_members <- renderPlot({
        ggplot(data = load_clubs(), aes(x = reorder(Division, Members, FUN = mean), y = Members, fill = Division)) +
            geom_boxplot() +
            labs(
                title = GetChartTitle('Division Performance on Membership', district = TRUE),
                x = 'Division',
                y = 'Membership'
            ) +
            coord_flip() +
            ggplot_defaults +
            theme(legend.position = 'none')
    })
    
    output$divisions_goals <- renderPlot({
        ggplot(data = load_clubs(), aes(x = reorder(Division, `Total Goals`, FUN = mean), y = `Total Goals`, fill = Division)) +
            geom_boxplot() +
            labs(
                title = GetChartTitle('Division Performance on Total Goals', district = TRUE),
                x = 'Division',
                y = 'Total Goals'
            ) +
            coord_flip() +
            ggplot_defaults +
            theme(legend.position = 'none')
    })
    
    output$divisions_rank <- renderPlot({
        ggplot(data = load_clubs(), aes(x = reorder(Division, -Rank, FUN = mean), y = Rank, fill = Division)) +
            geom_boxplot() +
            scale_y_reverse() +
            labs(
                title = GetChartTitle('Division Performance on Club Rank', district = TRUE),
                x = 'Division',
                y = 'Club Rank'
            ) +
            coord_flip() +
            ggplot_defaults +
            theme(legend.position = 'none')
    })
    
    output$areas_education <- renderPlot({
        ggplot(data = load_clubs(), aes(x = reorder(Area, `Education Goals`, FUN = mean), y = `Education Goals`, fill = Area)) +
            geom_boxplot() +
            labs(
                title = GetChartTitle('Area Performance on Education Goals', district = TRUE),
                x = 'Area',
                y = 'Education Goals'
            ) +
            coord_flip() +
            ggplot_defaults +
            theme(legend.position = 'none')
    })
    
    output$areas_members <- renderPlot({
        ggplot(data = load_clubs(), aes(x = reorder(Area, Members, FUN = mean), y = Members, fill = Area)) +
            geom_boxplot() +
            labs(
                title = GetChartTitle('Area Performance on Membership', district = TRUE),
                x = 'Area',
                y = 'Membership'
            ) +
            coord_flip() +
            ggplot_defaults +
            theme(legend.position = 'none')
    })
    
    output$areas_goals <- renderPlot({
        ggplot(data = load_clubs(), aes(x = reorder(Area, `Total Goals`, FUN = mean), y = `Total Goals`, fill = Area)) +
            geom_boxplot() +
            labs(
                title = GetChartTitle('Area Performance on Total Goals', district = TRUE),
                x = 'Area',
                y = 'Total Goals'
            ) +
            coord_flip() +
            ggplot_defaults +
            theme(legend.position = 'none')
    })
    
    output$areas_rank <- renderPlot({
        ggplot(data = load_clubs(), aes(x = reorder(Area, -Rank, FUN = mean), y = Rank, fill = Area)) +
            geom_boxplot() +
            scale_y_reverse() +
            labs(
                title = GetChartTitle('Area Performance on Club Rank', district = TRUE),
                x = 'Area',
                y = 'Club Rank'
            ) +
            coord_flip() +
            ggplot_defaults +
            theme(legend.position = 'none')
    })
    }