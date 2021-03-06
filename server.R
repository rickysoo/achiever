library(tidyverse)
library(shiny)
library(shinythemes)
library(shinycustomloader)
library(DT)
library(ggplot2)
library(plotly)
library(scales)
library(highcharter)
# library(broom)

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
        title = paste0('District ', default_district),
        search = ''
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
        xvar <- input$clubs_xvar
        yvar <- input$clubs_yvar
        
        viz <- ggplot(data = load_selected_clubs(), aes(x = .data[[xvar]], y = .data[[yvar]], label = Club)) +
            geom_point(aes(color = Division, size = Members), alpha = 0.7, position = 'jitter') +
            labs(
                x = getKPIname(xvar),
                y = getKPIname(yvar)
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
    output$clubs_performance <- renderHighchart({
        # output$clubs_performance <- renderPlotly({
        data <- load_selected_clubs()
        xvar <- input$charts_xvar
        xKPI <- getKPIname(xvar)
        
        # histogram <- (xvar %in% c('Awards', 'Level 1', 'Level 2', 'Level 3', 'Level 4+', 'Base', 'Members', 'New', 'Retained', 'Net Growth', 'Retention', 'Club Growth', 'Awards / Base', 'Awards / Members'))
        # percentage <- (xvar %in% c('Retention', 'Club Growth'))
        
        type <- ifelse(xvar %in% c('Awards', 'Level 1', 'Level 2', 'Level 3', 'Level 4+', 'Base', 'Members', 'New', 'Retained', 'Net Growth', 'Retention', 'Club Growth', 'Awards / Base', 'Awards / Members'), 'histogram', 'column')
        percentage <- (xvar %in% c('Retention', 'Club Growth'))
        
        data2 <- data %>%
            count(data[[xvar]], Division)
        
        colnames(data2) <- c('xvar', 'Division', 'n')
        
        {
            if (type == 'column')
                hchart(data2, type = type, hcaes(x = xvar, y = n, group = Division)) %>%
                hc_colors(DivisionColors) %>%
                hc_tooltip(shared = FALSE, headerFormat = '', pointFormat = paste0(xKPI, ': {point.x}<br>Division {series.name}<br>Number of clubs: {point.y}')) %>%
                hc_plotOptions(series = list(stacking = 'normal'))
            else if (type == 'histogram')
                # hchart(data, type = 'histogram', name = xKPI, hcaes(x = .data[[xvar]]))
                hchart(data[[xvar]], type = 'histogram', name = xKPI, color = '#004165') %>%
                # hchart(data, type = 'area', hcaes(x = .data[[xvar]], y = Members, group = Division)) %>%
                hc_plotOptions(series = list(stacking = 'normal')) %>%
                hc_tooltip(borderWidth = 1, sort = TRUE, crosshairs = TRUE, headerFormat = '', pointFormat = paste0('Number of clubs: {point.y}')) %>%
                hc_legend(enabled = FALSE)
                
        } %>%
            hc_title(text = GetChartTitle('Club Performance'), align = 'center', style = list(fontWeight = 'bold')) %>%
            hc_xAxis(title = list(text = xKPI, style = list(fontWeight = 'bold'))) %>%
            # {
            #     if (percentage)
            #         TRUE
            #         # hc_xAxis(label = scales::percent_format(scale = 100))
            #     else
            #         TRUE
            # } %>%
            hc_yAxis(title = list(text = 'Number of Clubs', style = list(fontWeight = 'bold')))
        
        # viz <- ggplot(data = data, aes(x = .data[[xvar]], fill = Division)) +
        #     { if (histogram) geom_histogram(bins = 10) else geom_bar() } +
        #     {
        #         if (histogram)
        #             if (percentage)
        #                 scale_x_continuous(breaks = pretty_breaks(), labels = scales::percent_format(scale = 100))
        #             else
        #                 scale_x_continuous(breaks = pretty_breaks())
        #         else
        #             geom_blank()
        #     } +
        #     labs(
        #         x = getKPIname(xvar),
        #         y = 'Number of Clubs'
        #     ) +
        #     ggplot_defaults
        # 
        # ggplotly(viz, height = 600) %>%
        #     layout(
        #         title = GetChartTitle('Club Performance'),
        #         xaxis = list(fixedrange = TRUE),
        #         yaxis = list(fixedrange = TRUE),
        #         margin = 50
        #     )
    })
    
    output$areas_performance <- renderPlotly({
        data <- load_selected_clubs()
        yvar <- input$areas_yvar
        percentage <- (yvar %in% c('Retention', 'Club Growth'))
        
        viz <- ggplot(data = data, aes(x = reorder(Area, .data[[yvar]], FUN = mean), y = .data[[yvar]], fill = Area, label = Club)) +
            geom_boxplot() +
            geom_jitter(position = position_jitter(0.2)) +
            { if (percentage) scale_y_continuous(labels = scales::percent_format(scale = 100)) else geom_blank() } +
            labs(
                x = 'Area',
                y = getKPIname(yvar)
            ) +
            coord_flip() +
            ggplot_defaults +
            theme(legend.position = 'none')
        
        ggplotly(viz, height = nrow(data) * 10 + 250, tooltip = c('label', 'y')) %>%
            layout(
                title = GetChartTitle('Area Performance'),
                xaxis = list(fixedrange = TRUE),
                yaxis = list(fixedrange = TRUE),
                margin = 50
            )
    })
    
    output$divisions_performance <- renderPlotly({
        data <- load_selected_clubs()
        yvar <- input$divisions_yvar
        percentage <- (yvar %in% c('Retention', 'Club Growth'))
        
        viz <- ggplot(data = data, aes(x = reorder(Division, .data[[yvar]], FUN = mean), y = .data[[yvar]], fill = Division, label = Club)) +
            geom_boxplot() +
            geom_jitter(position = position_jitter(0.2)) +
            { if (percentage) scale_y_continuous(labels = scales::percent_format(scale = 100)) else geom_blank() } +
            labs(
                x = 'Division',
                y = getKPIname(yvar)
            ) +
            coord_flip() +
            ggplot_defaults +
            theme(legend.position = 'none')
        
        ggplotly(viz, height = nrow(data) * 5 + 250, tooltip = c('label', 'y')) %>%
            layout(
                title = GetChartTitle('Division Performance'),
                xaxis = list(fixedrange = TRUE),
                yaxis = list(fixedrange = TRUE),
                margin = 50
            )
    })
    
    output$chart <- renderHighchart({
        data <- load_selected_clubs()
        
        xvar <- input$chart_xvar
        yvar <- input$chart_yvar
        xKPI <- getKPIname(xvar)
        yKPI <- getKPIname(yvar)
        
        # model <- loess(yvar ~ xvar, data = data)
        # fit <- augment(model) %>% arrange(xvar)
        
        data %>%
            hchart(type = 'scatter', hcaes(x = .data[[xvar]], y = .data[[yvar]], group = Division)) %>%
            # hc_add_series(fit, type = 'line', hcaes(x = .data[[xvar]], y = .fitted), name = 'Fit', id = 'fit') %>%
            hc_colors(DivisionColors) %>%
            hc_title(text = GetChartTitle('Club Performance'), align = 'center', style = list(fontWeight = 'bold')) %>%
            hc_xAxis(title = list(text = xKPI, style = list(fontWeight = 'bold'))) %>%
            hc_yAxis(title = list(text = yKPI, style = list(fontWeight = 'bold'))) %>%
            # hc_add_theme(hc_theme_ffx()) %>%
            hc_tooltip(shared = TRUE, headerFormat = '', pointFormat = paste0('Division {series.name}<br>{point.Club}<br><br>', xKPI, ': {point.x}', '<br>', yKPI, ': ', '{point.y}'))
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
        
        for (table in c('overview', 'education', 'membership')) {
            proxy <- dataTableProxy(table)
            clearSearch(proxy = proxy)
            reloadData(proxy, resetPaging = TRUE, clearSelection = 'all')
            # replaceData(proxy = proxy, data = load_clubs(), resetPaging = TRUE, clearSelection = FALSE)
        }
    })
    
    observeEvent(
        c(input$overview_search),
        {
            values$search <- input$overview_search
        }
    )   

    observeEvent(
        c(input$education_search),
        {
            values$search <- input$education_search
        }
    )   
    
    observeEvent(
        c(input$membership_search),
        {
            values$search <- input$membership_search
        }
    )   
    
    observeEvent(
        input$tab,
        {
            if (input$tab %in% c('Overview', 'Education', 'Membership')) {
                for (table in c('overview', 'education', 'membership')) {
                    proxy <- dataTableProxy(table)
                    updateSearch(proxy, keywords = list(global = values$search))
                }
            }
        }
    )
    
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