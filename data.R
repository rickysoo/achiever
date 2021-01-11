library(tidyverse)

districts <- c('51', '80', '87', '102')
default_district <- '102'
time_zone <- 'Etc/GMT-12'
color <- '#CD202C'

KPI <- data.frame(
    Variable = c('Members', 'Retained', 'Retention', 'New', 'Growth', 'Education Goals', 'Total Goals'),
    Name = c('Members', 'Retained Members', 'Retention', 'New Members', 'Net Growth', 'Education Goals', 'Total Goals')
)    

getKPIname <- function(var) {
    KPI %>% 
        filter(Variable == var) %>%
        pull(Name)
}

get_filename <- function(district) {
    return(paste0('clubs-', district, '-', substr(strptime(format(Sys.time(), tz = time_zone), '%Y-%m-%d'), 1, 12), '.csv'))
}

obtain_data_clubs <- function(district) {
    ClubsURL <- paste0('http://dashboards.toastmasters.org/export.aspx?type=CSV&report=clubperformance~', district)
    df <- read.csv(ClubsURL)
    df <- df[-nrow(df), ]
    
    filename <- get_filename(district)
    write.csv(df, filename, row.names = FALSE)
    print(paste0('Saving ', filename))
    
    return(df)
}

load_data_clubs <- function(district) {
    if (is.null(district)) {
        district <- default_district
    }
    
    filename <- get_filename(district)
    
    if (file.exists(filename)) {
        df <- read.csv(filename)
        print(paste0('Reading from local ', filename))
    }
    else {
        df <- obtain_data_clubs(district)
        print(paste0('Reading from source ', filename))
    }
    
    colnames(df) <- c('District', 'Division', 'Area', 'Number', 'Club', 'Status', 'Base', 'Members', 'Goals', 'Edu1', 'Edu2', 'Edu3', 'Edu4', 'Edu5', 'Edu6', 'Mem1', 'Mem2', 'Train1', 'Train2', 'Due1', 'Due2', 'List', 'Distinguished')

    df$Status <- as.factor(df$Status)
    levels(df$Status) <- c('Active', 'Low', 'Ineligible', 'Suspended')
    
    df$Distinguished <- replace_na(df$Distinguished, 'Not Yet')
    df$Distinguished <- as.factor(df$Distinguished)
    levels(df$Distinguished) <- c('Not Yet', 'Distinguished', 'Select Distinguished', 'President\'s Distinguished')
    
    df <- df %>%
        mutate(
            Area = paste0(Division, Area),
            Number = str_pad(Number, width = 7, pad = '0'),
            New = Mem1 + Mem2,
            Retained = Members - New,
            Retention = ifelse(Base == 0, 0, Retained / Base),
            Growth = Members - Base,
            Renewals = Due1 + Due2,
            EduGoals = CountEduGoals(Edu1, Edu2, Edu3, Edu4, Edu5, Edu6),
            MemGoals = CountMemGoals(Mem1, Mem2),
            TrnGoals = CountTrnGoals(Train1, Train2),
            AdmGoals = CountAdmGoals(Due1, Due2, List),
            URL = paste0('https://www.toastmasters.org/Find-a-Club/', Number)
        )

    row.names(df) <- df$Number
    return(df)
}

CountEduGoals <- function(Edu1, Edu2, Edu3, Edu4, Edu5, Edu6) {
    Goals <- as.integer(Edu1 >= 4) + as.integer(Edu2 >= 2) + as.integer(Edu3 >= 2) + as.integer(Edu4 >= 2) + as.integer(Edu5 >= 1) + as.integer(Edu6 >= 1)
    return(Goals)
}

CountMemGoals <- function(Mem1, Mem2) {
    Goals <- as.integer(Mem1 >= 4) + as.integer(Mem2 >= 4)
    return(Goals)
}

CountTrnGoals <- function(Train1, Train2) {
    Goals <- as.integer((Train1 >= 4) & (Train2 >= 4))
    return(Goals)
}

CountAdmGoals <- function(Due1, Due2, List) {
    Goals <- as.integer((Due1 + Due2 >= 1) & (List >= 1))
    return(Goals)
}
