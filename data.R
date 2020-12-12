library(tidyverse)

District <- 102

obtain_data_clubs <- function(District = 102) {
    ClubsURL <- paste0('http://dashboards.toastmasters.org/export.aspx?type=CSV&report=clubperformance~', District)
    df <- read.csv(ClubsURL)
    write.csv(df, paste0('clubs-', District, '.csv'), row.names = FALSE)
}

load_data_clubs <- function() {
    df <- read.csv(paste0('clubs-', District, '.csv'))
    df <- df[-nrow(df), ]
    colnames(df) <- c('District', 'Division', 'Area', 'Number', 'Club', 'Status', 'Base', 'Members', 'Goals', 'Edu1', 'Edu2', 'Edu3', 'Edu4', 'Edu5', 'Edu6', 'Mem1', 'Mem2', 'Train1', 'Train2', 'Due1', 'Due2', 'List', 'Distinguished')

    df$Distinguished <- replace_na(df$Distinguished, 'Not Yet')
    df$Distinguished <- as.factor(df$Distinguished)
    levels(df$Distinguished) <- c('Not Yet', 'Distinguished', 'Select Distinguished', 'President\'s Distinguished')
    
    df <- df %>%
        mutate(
            Area = paste0(Division, Area),
            Number = str_pad(Number, width = 7, pad = '0'),
            # Club = paste0(Name, ' (', Number, ')'),
            Renewals = Due1 + Due2,
            EduGoals = CountEduGoals(Edu1, Edu2, Edu3, Edu4, Edu5, Edu6),
            MemGoals = CountMemGoals(Mem1, Mem2),
            TrnGoals = CountTrnGoals(Train1, Train2),
            AdmGoals = CountAdmGoals(Due1, Due2, List),
            URL = paste0('https://www.toastmasters.org/Find-a-Club/', Number)
        )

    df$District <- as.factor(df$District)
    df$Division <- as.factor(df$Division)
    df$Area <- as.factor(df$Area)

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

# sum(is.na(df$Distinguished))
# 
# head(df)
# tail(df)
# 
# str(df)
