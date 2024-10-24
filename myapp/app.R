# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(DBI)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tools)
library(thematic)
library(bsicons)
library(fontawesome)
library(plotly)
library(ggthemes)
library(shinyBS)
library(viridis)
library(formattable)
library(ggcorrplot)
library(shinylive)
library(httpuv)




# Helper function to generate random US states and categorical values
usa_states <- state.name
random_state <- function(n) sample(usa_states, n, replace = TRUE)
sports <- c("Basketball", "Baseball", "Football", "Soccer", "Hockey")
stat_names <- c("Points", "Rebounds", "Assists", "Goals", "Yards")
locker_types <- c("Standard", "Premium", "Bonus")
teams <- paste0("Team", sample(LETTERS, 10))
seasons <- c("2024", "2025")
bet_types <- c("game_bet")
reasons <- c("game_bet purchase", "game_bet winnings")

# Set seed for reproducibility
set.seed(123)

# Number of rows (adjust as needed)
n <- 500

# Generate the dataframe
df <- tibble(
  # Character columns
  idTransaction = as.character(sample(100000:999999, n, replace = TRUE)),
  contestanttransaction_idContestant = as.character(sample(1000:1050, n, replace = TRUE)),
  contestant_idContestant = as.character(sample(1000:1050, n, replace = TRUE)),
  
  # Contestant account dates
  contestant_account_created_on = sample(seq(ymd('2024-10-10'), ymd('2025-02-01'), by = "day"), n, replace = TRUE),
  contestant_account_created_on_day = as.Date(contestant_account_created_on),
  contestant_account_created_on_month = floor_date(contestant_account_created_on, "month"),
  contestant_account_created_on_week_first_date = floor_date(contestant_account_created_on, "week"),
  
  # Contestant information
  contestant_city = ifelse(runif(n) > 0.1, random_state(n), "Unknown"),
  contestant_state = random_state(n),
  contestant_country = "USA",
  contestant_status = sample(c("active", "inactive"), n, replace = TRUE),
  contestant_isDeleted = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.05, 0.95)),
  contestant_isBlocked = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.05, 0.95)),
  contestant_referralCode = ifelse(runif(n) > 0.1, paste0("REF", sample(1000:9999, n, replace = TRUE)), "Unknown"),
  referredByCode = ifelse(runif(n) > 0.2, paste0("REF", sample(1000:9999, n, replace = TRUE)), "Not Used / Unknown"),
  new_vs_old_user = sample(c("New User", "Old User"), n, replace = TRUE, prob = c(0.1, 0.9)),
  
  # Transaction details
  bet_type = sample(bet_types, n, replace = TRUE),
  transaction_reason = sample(reasons, n, prob = c(0.7, 0.3), replace = TRUE),
  transaction_date_time = sample(seq(ymd_hms('2024-08-01 00:00:00'), ymd_hms('2025-02-01 23:59:59'), by = "hour"), n, replace = TRUE),
  transaction_day = as.Date(transaction_date_time),
  transaction_month = floor_date(transaction_day, "month"),
  transaction_week_number = isoweek(transaction_day),
  transaction_week_first_date = floor_date(transaction_day, "week"),
  transaction_weekday = weekdays(transaction_day),
  transaction_hour = hour(transaction_date_time),
  transaction_minute = minute(transaction_date_time),
  transaction_balance = sample(c("card charges", "deposits", "winnings"), n, replace = TRUE, prob = c(0.33, 0.33, 0.33)),  # Transaction balance
  
  # Prop entry columns
  propentry_idPropEntry = as.character(sample(10000:99999, n, replace = TRUE)),
  propentry_result = sample(c("win", "loss", "void", "placed"), n, replace = TRUE, prob = c(0.475, 0.475, 0.025, 0.025)),
  propentry_entryAmount = sample(seq(10, 100, by = 5), n, replace = TRUE),
  propentry_payout = propentry_entryAmount * sample(seq(1.5, 5, by = 0.5), n, replace = TRUE),
  propentry_multiplier = runif(n, min = 1.1, max = 3.0),
  
  propentry_createdOn = sample(seq(ymd_hms('2024-08-01 00:00:00'), ymd_hms('2025-02-01 23:59:59'), by = "hour"), n, replace = TRUE),
  propentry_lastUpdatedOn = propentry_createdOn + days(sample(1:30, n, replace = TRUE)),
  propentry_placedOn = sample(seq(ymd_hms('2024-08-01 00:00:00'), ymd_hms('2025-02-01 23:59:59'), by = "hour"), n, replace = TRUE),
  propentry_placedOn_day = as.Date(propentry_placedOn),
  propentry_placedOn_month = floor_date(propentry_placedOn_day, "month"),
  propentry_placedOn_week_first_date = floor_date(propentry_placedOn_day, "week"),
  propentry_placedOn_weekday = weekdays(propentry_placedOn_day),
  
  # Game columns
  gameTime = sample(seq(ymd_hms('2024-08-01 00:00:00'), ymd_hms('2025-02-01 23:59:59'), by = "hour"), n, replace = TRUE),
  gameTime_day = as.Date(gameTime),
  propgame_sport = sample(sports, n, replace = TRUE),
  propgame_season = sample(seasons, n, replace = TRUE),
  
  # Prop details
  createdOn = sample(seq(ymd_hms('2024-08-01 00:00:00'), ymd_hms('2025-02-01 23:59:59'), by = "hour"), n, replace = TRUE),
  lastUpdatedOn = createdOn + days(sample(1:30, n, replace = TRUE)),
  sport = sample(sports, n, replace = TRUE),
  statName = sample(stat_names, n, replace = TRUE),
  playerName = paste0("Player", sample(LETTERS, n, replace = TRUE)),
  playerTeamCode = sample(teams, n, replace = TRUE),
  opponentTeamCode = sample(teams, n, replace = TRUE),
  lockerLine = runif(n, min = 0, max = 10),
  lockerType = sample(locker_types, n, replace = TRUE),
  bonusMultiplier = runif(n, min = 1.1, max = 3.0),
  
  # Picks summary
  entry_number_of_win_picks = sample(0:5, n, replace = TRUE),
  entry_number_of_loss_picks = sample(0:5, n, replace = TRUE),
  entry_number_of_void_picks = sample(0:2, n, replace = TRUE),
  
  # Choices
  contestantChoice = sample(c("over", "under"), n, replace = TRUE),
  
  # New column added
  idValue = as.character(sample(1:1000, n, replace = TRUE)), # Add idValue column
  
  transaction_amount = runif(n, 10, 200),
  transaction_idValue = sample(1:500, n, replace = TRUE),
  propentry_idContestant = sample(1:200, n, replace = TRUE)
) %>%
  mutate(
    entry_number_of_placed_picks = ifelse(propentry_result == 'placed', 1, 0),
    entry_number_of_win_picks = ifelse(propentry_result == 'win', 1, 0),
    entry_number_of_void_picks = ifelse(propentry_result == 'void', 1, 0),
    entry_number_of_loss_picks = ifelse(propentry_result == 'loss', 1, 0),
    entry_number_of_over_choice = ifelse(contestantChoice == 'over', 1, 0),
    entry_number_of_under_choice = ifelse(contestantChoice == 'under', 1, 0),
    entry_number_of_picks = entry_number_of_win_picks + entry_number_of_loss_picks + entry_number_of_void_picks,
    entry_number_of_placed_picks = ifelse(propentry_result == 'placed', 1, 0)  # Summarizes placed picks
  )


main_transaction_data <- df

# Transform data --------------------------------------------------------------------
## Initial steps --------------------------------------------------------------------

# Convert to date

convert_to_date <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ymd))
}

## Convert to DateTime

convert_to_datetime <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ymd_hms))
}


## Convert to Factor(Define the function to convert specified columns to factors)

convert_to_factor <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), as.factor))
}

### Convert data --------------------------------------------------------------------

# Convert to date

date_columns <- c("propentry_placedOn_day","propentry_placedOn_month",
                  "propentry_placedOn_week_first_date", "gameTime_day",
                  "transaction_month", "transaction_day", "transaction_week_first_date")

main_transaction_data <- convert_to_date(main_transaction_data, date_columns)  

# Convert to DateTime

datetime_columns <- c( "createdOn", "lastUpdatedOn", "gameTime",
                       "propentry_createdOn",
                       "propentry_lastUpdatedOn", "propentry_placedOn" )

main_transaction_data <- convert_to_datetime(main_transaction_data, datetime_columns)  

# Convert to Factor

factor_columns <- c("sport", "statName", "playerTeamCode", "opponentTeamCode",
                    "lockerType", "propentry_result", "contestantChoice", "propentry_result",
                    "propentry_placedOn_weekday", "propgame_sport", "propgame_season",
                    "new_vs_old_user", "contestant_city", "contestant_state", "contestant_country",
                    "bet_type", "transaction_reason", "transaction_balance", "transaction_weekday")

main_transaction_data <- convert_to_factor(main_transaction_data, factor_columns)


## Fixed values and functions ---------------------------------------------------

default_point_size <- 3 
custom.col <- c("#CC79A7", "#00AFBB", "#FFA500", "#D16103", "#E7B800", "#FFDB6D", "#FFAEB9",
                "#FFDB6D", "#C4961A", "#F4EDCA", "#C3D7A4", "#52854C", "#4E84C4", "#293352")


## Theme colors
PRIMARY <- "#0675DD" 
main_chart_color <- "#0675DD"
#  "#A74AC7"
second_chart_color <- "#FF7B00"

# Error graph
error_graph <-  ggplot() +
  annotate("text", x = 1, y = 1,
           label = "The data does not exist. \nPlease change the filters.",
           size = 8, hjust = 0.5, vjust = 0.5) +
  theme_void()

# To clean the column names in a table
convert_column_names <- function(df) {
  colnames(df) <- colnames(df) %>%
    str_replace_all("_", " ") %>%     # Replace underscores with spaces
    str_to_title()                    # Convert to title case
  return(df)
}

# Function to round numerical columns to two decimal places
round_decimal_columns <- function(data) {
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      data[[col]] <- round(data[[col]], 2)
    }
  }
  return(data)
}


ui <- page_navbar(
  selected = "Main Page",
  ### Logo  
  title = tags$span(
    tags$img(src = "locker_logo.png", width = "76px", height = "76px", class = "me-3", alt = "Author: Sajad Ghashami", "Sajad Ghashami"),
  ),
  id = "nav",
  ### Theme 
  theme = bs_theme(
    preset = "shiny",
    "primary" = PRIMARY
  ),
  ### Sidebar
  sidebar = sidebar(
    width = 350,
    open = list(desktop = "closed", mobile = "closed"),
    # conditionalPanel(
    #   "input.nav === 'Overview'",
    accordion(
      open = FALSE,
      # Date Filter      
      accordion_panel(
        "Date Filters",
        icon = fontawesome::fa("calendar"),
        dateRangeInput("gameTime_day", "Game Date:",
                       start  = "2023-01-01",
                       end    = max(main_transaction_data$gameTime_day, na.rm = T),
                       min    = "2023-01-01",
                       max    = max(main_transaction_data$gameTime_day, na.rm = T),
                       format = "yy/mm/dd",
                       separator = "to"),
        dateRangeInput("propentry_placedOn_day", "Placed On date:",
                       start  = "2023-01-01",
                       end    = today(),
                       min    = "2023-01-01",
                       max    = today(),
                       format = "yy/mm/dd",
                       separator = "to"),
        selectInput(
          inputId = "propgame_season",
          label = "Season Year:",
          choices = na.omit( unique(main_transaction_data$propgame_season)),
          multiple = TRUE),
      ),
      # Game Filter
      accordion_panel(
        "Game Filters",
        icon = fontawesome::fa("football"),
        selectizeInput(
          "propgame_sport", "Sport",
          choices = na.omit(unique(main_transaction_data$propgame_sport)),
          multiple = TRUE,
          options = list(plugins = "remove_button", closeAfterSelect = TRUE)
        ),
        selectizeInput(
          "statName", "Stat Name",
          choices = na.omit(unique(main_transaction_data$statName)),
          multiple = TRUE,
          options = list(plugins = "remove_button", closeAfterSelect = TRUE)
        ),
        # selectInput(
        #   inputId = "playerTeamCode",
        #   label = "Player Team Code:",
        #   choices = na.omit(unique(main_transaction_data$playerTeamCode)) ,
        #   multiple = TRUE
        # ),
        # selectInput(
        #   inputId = "opponentTeamCode",
        #   label = "Opponent Team Code:",
        #   choices = na.omit(unique(main_transaction_data$opponentTeamCode)) ,
        #   multiple = TRUE
        # )
      ),
      # Location Filter
      accordion_panel(
        "Location Filters",
        icon = fontawesome::fa("map-pin"),
        selectizeInput(
          "contestant_country", "User Country",
          choices = na.omit(sort(main_transaction_data$contestant_country)),
          multiple = TRUE,
          options = list(plugins = "remove_button", closeAfterSelect = TRUE)
        ),
        selectizeInput(
          "contestant_state", "User State",
          choices = na.omit(sort(main_transaction_data$contestant_state)),
          multiple = TRUE,
          options = list(plugins = "remove_button", closeAfterSelect = TRUE)
        )
      ),
      accordion_panel(
        "User Filters",
        icon = fontawesome::fa("user"),
        selectizeInput(
          "new_vs_old_user", "New vs Old Users",
          choices = na.omit(sort(main_transaction_data$new_vs_old_user)),
          multiple = TRUE,
          options = list(plugins = "remove_button", closeAfterSelect = TRUE)
        ),
        selectizeInput(
          "entry_number_of_picks", "Number of Picks",
          choices = na.omit(sort(unique(main_transaction_data$entry_number_of_picks))),
          multiple = TRUE,
          options = list(plugins = "remove_button", closeAfterSelect = TRUE)
        ),
        selectizeInput(
          "referredByCode", "Referred By Code",
          choices = na.omit(sort(main_transaction_data$referredByCode)),
          multiple = TRUE,
          options = list(plugins = "remove_button", closeAfterSelect = TRUE)
        )
      ),
    ),
    input_task_button("update_results", "Update Data", type="success", icon=fontawesome::fa("play"))
  ),
  
  ## Main Page ----------------------------------------------------------------
  nav_spacer(),
  ### First Page --------------------------------------------------------------
  nav_panel("Main Page",
            fillable = TRUE,
            fill = TRUE,
            value_box(showcase = bs_icon("view-stacked"), title="", value= h2(strong("Game Analytics:")), h5("Includes top Game KPIS", bs_icon("emoji-smile")), br(),"",theme = "secondary", min_height = 120),
            card(card_header(h4(strong("General Numbers:"))), "Includes basic numbers Regarding the financial and performance metrics", min_height = 120),
            # 1th Row            
            layout_columns(
              fill = FALSE,
              value_box(
                title = fluidRow( "Revenue",
                                  HTML("&nbsp;"),
                                  tooltip(bsicons::bs_icon("info-circle",
                                                           title = ""),
                                          "Total purchase amount. Withdraw and deposit are not included. Zero Values are excluded from the graph")
                ),
                value = uiOutput("revenue_output"),
                showcase = bsicons::bs_icon("currency-dollar")
              ),
              value_box(
                title = fluidRow( "Total Winnings",
                                  HTML("&nbsp;"),
                                  tooltip(bsicons::bs_icon("info-circle",
                                                           title = ""),
                                          "Total Winnings. Zero Values are excluded from the graph")
                ),
                value = uiOutput("winning_output"),
                showcase = bsicons::bs_icon("trophy")
              ),
              value_box(
                title = fluidRow( "Total Profit",
                                  HTML("&nbsp;"),
                                  tooltip(bsicons::bs_icon("info-circle",
                                                           title = ""),
                                          "Revenue - Winning. Zero Values are excluded from the graph")
                ),
                
                value = uiOutput("profit_output"),
                showcase = bsicons::bs_icon("cash")
              ),
              col_widths = c(4, 4, 4)
            ),
            # 2th Row
            layout_columns(
              fill = FALSE,
              navset_card_underline(
                full_screen = TRUE,
                title = "Revenue Trend",
                # nav_panel("Weekly",
                #           plotlyOutput(outputId = "revenue_trend_week_output")
                # ),
                nav_panel("Monthly",
                          plotlyOutput(outputId = "revenue_trend_month_output")
                )
              ),
              navset_card_underline(
                full_screen = TRUE,
                title = "Winnings Trend",
                # nav_panel("Weekly",
                #           plotlyOutput(outputId = "winning_trend_week_output")
                # ),
                nav_panel("Monthly",
                          plotlyOutput(outputId = "winning_trend_month_output")
                )
              ),
              navset_card_underline(
                full_screen = TRUE,
                title = "Profit Trend",
                # nav_panel("Weekly",
                #           plotlyOutput(outputId = "profit_trend_week_output")
                # ),
                nav_panel("Monthly",
                          plotlyOutput(outputId = "profit_trend_month_output")
                )
              ),
              
              col_widths = c(4, 4, 4),
            ),
            # 3th Row
            layout_columns(
              fill = FALSE,
              value_box(
                title = fluidRow( "Profit Margin",
                                  HTML("&nbsp;"),
                                  tooltip(bsicons::bs_icon("info-circle",
                                                           title = ""),
                                          "Profit divided by revenue. Zero Values are excluded from the graph")
                ),
                value = uiOutput("profitmargin_output"),
                showcase = bsicons::bs_icon("bank")
              ),
              value_box(
                title = fluidRow( "Number of Bets",
                                  HTML("&nbsp;"),
                                  tooltip(bsicons::bs_icon("info-circle",
                                                           title = ""),
                                          "Total Bets placed. Zero Values are excluded from the graph")
                ),
                value = uiOutput("numberofbets_output"),
                showcase = bsicons::bs_icon("hash")
              ),
              value_box(
                title = fluidRow( "% bets won",
                                  HTML("&nbsp;"),
                                  tooltip(bsicons::bs_icon("info-circle",
                                                           title = ""),
                                          "Percentage bets won. Zero Values are excluded from the graph")
                ),
                
                value = uiOutput("percwon_output"),
                showcase = bsicons::bs_icon("percent")
              ),
              col_widths = c(4, 4, 4)
            ),
            # 4th Row
            layout_columns(
              fill = FALSE,
              navset_card_underline(
                full_screen = TRUE,
                title = "Profit Margin Trend",
                nav_panel("Weekly",
                          plotlyOutput(outputId = "profitmargin_trend_week_output")
                ),
                nav_panel("Monthly",
                          plotlyOutput(outputId = "profitmargin_trend_month_output")
                )
              ),
              navset_card_underline(
                full_screen = TRUE,
                title = "Number of Bets Trend",
                nav_panel("Weekly",
                          plotlyOutput(outputId = "numberofbets_trend_week_output")
                ),
                nav_panel("Monthly",
                          plotlyOutput(outputId = "numberofbets_trend_month_output")
                )
              ),
              navset_card_underline(
                full_screen = TRUE,
                title = "% bets won Trend",
                nav_panel("Weekly",
                          plotlyOutput(outputId = "percwon_trend_week_output")
                ),
                nav_panel("Monthly",
                          plotlyOutput(outputId = "percwon_trend_month_output")
                )
              ),
              
              col_widths = c(4, 4, 4),
            ),
            layout_columns(
              fill = FALSE,
              navset_card_underline(
                full_screen = TRUE,
                title = "# of bets by weekday",
                sidebar = sidebar(
                  position = "right",
                  open = FALSE,
                  checkboxInput("log_transform2", "Log Transform", FALSE)
                ),
                nav_panel(
                  "Graph",
                  card_body(
                    plotlyOutput(outputId = "numberofbets_weekday_distribution_output")
                  )
                ),
                nav_panel(
                  "Weekly",
                  card_body(
                    plotlyOutput(outputId = "weekly_numberofbets_weekday_distribution_output")
                  )
                ),
                nav_panel(
                  "Monthly",
                  card_body(
                    plotlyOutput(outputId = "monthly_numberofbets_weekday_distribution_output")
                  )
                )
              ),
              card_body(
                navset_card_underline(
                  sidebar = sidebar(
                    position = "right",
                    open = FALSE,
                    checkboxInput("log_transform3", "Log Transform", FALSE),
                    numericInput("numberofbins", "Number of Bins:", 8, min = 1, max = 15),
                    checkboxInput("exclude_complete", "Exclude Zeros", FALSE)
                  ),  # Shared sidebar UI
                  full_screen = TRUE,
                  title = "# of bets by buy-in amount",
                  
                  # nav_panel(
                  #   "Bar Chart",
                  #   plotlyOutput(outputId = "numberofbets_buyin_distribution_output")
                  # ),
                  nav_panel(
                    "Histogram with Standard Deviation",
                    plotlyOutput(outputId = "numberofbets_buyin_distribution_output2")
                  ),
                  nav_panel(
                    "Data",
                    DT::dataTableOutput(outputId = "buyin_distribution_table_output")
                  )
                )
              )
            ),
            layout_columns(
              fill = FALSE,
              min_height = 700,
              navset_card_underline(
                sidebar = sidebar(
                  position = "right",
                  open = FALSE,
                  uiOutput("y_axis_user_varselect"),
                  uiOutput("x_axis_user_varselect"),
                  uiOutput("color_user_varselect"),
                  uiOutput("size_user_varselect"),
                  checkboxInput("user_scatter_y_log_transform", "Y Log Transform", FALSE),
                  checkboxInput("user_scatter_x_log_transform", "X Log Transform", FALSE),
                  checkboxInput("user_scatter_se", "Include Confidence Interval?", TRUE)
                ),  # Shared sidebar UI
                full_screen = TRUE,
                title = "Scatter Plot( Each point is a user)",
                nav_panel("User Scatter Plot",
                          plotlyOutput(outputId = "user_scatter_plot_output")
                ),
              ),
              navset_card_underline(
                full_screen = TRUE,
                title = "Correlation Analysis. (It should be interpreted and analyzed per user)",
                nav_panel("Correlation Matrix",
                          plotlyOutput(outputId = "user_correlation_output")
                )
              ),
              
              col_widths = c(6, 6),
            ),
            
            
            card(full_screen = TRUE,
                 card_header(h4(strong("Weekly Prop Summary Table"))),
                 # markdown(
                 #   "Summary Table"
                 # ),
                 fillable_mobile = TRUE,
                 DT::dataTableOutput(outputId = "weeklysummary_output"),
                 min_height = 700
            ),
  ),
  ### Other Pages --------------------------------------------------------------
  nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
  br(),
  br(),
  nav_spacer()
)

### Thematic Shiny -------------------------------------------------------------

thematic_shiny()

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  updated_main_game_bets_data <- eventReactive(input$update_results, {
    temp_data <- main_transaction_data %>%
      # filter date columns and make sure it also include dates that are NULL OR NA because
      #    filter( (gameTime_day >= input$gameTime_day[1] & gameTime_day <= input$gameTime_day[2]) | is.null(gameTime_day) | is.na(gameTime_day)) %>%
      # filter( (gameTime >= input$gameTime[1] & gameTime <= input$gameTime[2]) | is.null(gameTime) | is.na(gameTime)) %>%
      filter((gameTime_day >= input$gameTime_day[1] & gameTime_day <= input$gameTime_day[2]) | is.null(gameTime_day) | is.na(gameTime_day)) %>%
      filter((propentry_placedOn_day >= input$propentry_placedOn_day[1] & propentry_placedOn_day <= input$propentry_placedOn_day[2]) | is.null(propentry_placedOn_day) | is.na(propentry_placedOn_day))
    # filter seasonyear and make sure it also include dates that are NULL OR NA  
    
    if (
      !is.null(input$propgame_season) && length(input$propgame_season) > 0) {
      temp_data <- temp_data %>% filter(propgame_season %in% input$propgame_season | is.na(propgame_season) | is.null(propgame_season))
    }
    
    if (
      !is.null(input$contestant_state) && length(input$contestant_state) > 0) {
      temp_data <- temp_data %>% filter(contestant_state %in% input$contestant_state | is.na(contestant_state) | is.null(contestant_state))
    }
    
    if (
      !is.null(input$contestant_country) && length(input$contestant_country) > 0) {
      temp_data <- temp_data %>% filter(contestant_country %in% input$contestant_country | is.na(contestant_country) | is.null(contestant_country))
    }
    
    if (
      !is.null(input$propgame_sport) && length(input$propgame_sport) > 0) {
      temp_data <- temp_data %>% filter(propgame_sport %in% input$propgame_sport | is.na(propgame_sport) | is.null(propgame_sport))
    }
    
    if (
      !is.null(input$statName) && length(input$statName) > 0) {
      temp_data <- temp_data %>% filter(statName %in% input$statName | is.na(statName) | is.null(statName))
    }
    
    if (
      !is.null(input$new_vs_old_user) && length(input$new_vs_old_user) > 0) {
      temp_data <- temp_data %>% filter(new_vs_old_user %in% input$new_vs_old_user | is.na(new_vs_old_user) | is.null(new_vs_old_user))
    }
    
    
    if (
      !is.null(input$entry_number_of_picks) && length(input$entry_number_of_picks) > 0) {
      temp_data <- temp_data %>% filter(entry_number_of_picks %in% input$entry_number_of_picks | is.na(entry_number_of_picks) | is.null(entry_number_of_picks))
    }
    
    if (
      !is.null(input$referredByCode) && length(input$referredByCode) > 0) {
      temp_data <- temp_data %>% filter(referredByCode %in% input$referredByCode | is.na(referredByCode) | is.null(referredByCode))
    }
    
    temp_data
    
  }, ignoreNULL = FALSE)
  
  distinct_propentry_level_data <- reactive({
    updated_main_game_bets_data() %>%
      select(propentry_idPropEntry, idTransaction, contestanttransaction_idContestant, new_vs_old_user,
             bet_type, idValue, transaction_month, transaction_day, transaction_week_first_date,
             transaction_amount, transaction_reason, transaction_balance, transaction_idValue, 
             propentry_idContestant, propentry_placedOn, propentry_result, propentry_entryAmount,
             entry_number_of_picks, entry_number_of_placed_picks, entry_number_of_win_picks,
             entry_number_of_void_picks, entry_number_of_loss_picks, entry_number_of_over_choice,
             entry_number_of_under_choice,
             transaction_weekday) %>% 
      distinct(idTransaction, .keep_all = TRUE)
  })
  
  top_level_propentry_data <- reactive({
    distinct_propentry_level_data() %>%
      summarise(distinct_user_count = n_distinct(ifelse( !is.na(propentry_placedOn), contestanttransaction_idContestant, NA), na.rm = TRUE),
                distinct_new_user_count = n_distinct(ifelse( !is.na(propentry_placedOn) & new_vs_old_user == 'New User', contestanttransaction_idContestant, NA), na.rm = TRUE),
                distinct_old_user_count = n_distinct(ifelse( !is.na(propentry_placedOn) & new_vs_old_user == 'Old User', contestanttransaction_idContestant, NA), na.rm = TRUE),
                
                entries = n_distinct(ifelse(!is.na(propentry_placedOn), paste( contestanttransaction_idContestant, idValue), NA) , na.rm = TRUE),
                entries_new_users = n_distinct(ifelse(!is.na(propentry_placedOn) & new_vs_old_user == 'New User', paste( contestanttransaction_idContestant, idValue), NA), na.rm = TRUE),
                entries_old_users = n_distinct(ifelse(!is.na(propentry_placedOn) &  new_vs_old_user == 'Old User', paste( contestanttransaction_idContestant, idValue), NA), na.rm = TRUE),
                
                revenue = sum(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn), transaction_amount,0 ), na.rm = TRUE),
                #  avg_revenue = mean(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn), transaction_amount,0 ), na.rm = TRUE),
                revenue_new_users = sum(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn) & new_vs_old_user == 'New User', transaction_amount, NA), na.rm = TRUE),
                revenue_old_users = sum(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn) & new_vs_old_user == 'Old User', transaction_amount, NA), na.rm = TRUE),
                
                winning = sum(ifelse(transaction_reason %in% c('game_bet winnings') & !is.na(propentry_placedOn) & propentry_result == 'win', transaction_amount, 0 ), na.rm = TRUE),
                profit = revenue - winning,
                profit_margin = ifelse(revenue == 0, NA, profit/revenue),
                
                bet_won = n_distinct(ifelse(!is.na(propentry_placedOn) & propentry_result == 'win', paste( contestanttransaction_idContestant, idValue), NA) , na.rm = TRUE),
                perc_bet_won = ifelse(entries == 0, NA, 100*bet_won/entries)
                
                # avg_number_of_picks = mean(ifelse(!is.na(propentry_placedOn), entry_number_of_picks, NA), na.rm = TRUE),
                # avg_number_of_placed_picks = mean(ifelse(!is.na(propentry_placedOn), entry_number_of_placed_picks, NA), na.rm = TRUE),
                # avg_number_of_win_picks = mean(ifelse(!is.na(propentry_placedOn), entry_number_of_win_picks, NA), na.rm = TRUE),
                # avg_number_of_void_picks = mean(ifelse(!is.na(propentry_placedOn), entry_number_of_void_picks, NA), na.rm = TRUE),
                # avg_number_of_loss_picks = mean(ifelse(!is.na(propentry_placedOn), entry_number_of_loss_picks, NA), na.rm = TRUE),
                # avg_number_of_over_choice = mean(ifelse(!is.na(propentry_placedOn), entry_number_of_over_choice, NA), na.rm = TRUE),
                # avg_number_of_under_choice = mean(ifelse(!is.na(propentry_placedOn), entry_number_of_under_choice, NA), na.rm = TRUE)
      )
  })
  
  weekly_level_propentry_data <- reactive({
    distinct_propentry_level_data() %>%
      filter(!is.na(transaction_week_first_date)) %>%
      group_by(transaction_week_first_date) %>%
      summarise(distinct_user_count = n_distinct(ifelse( !is.na(propentry_placedOn), contestanttransaction_idContestant, NA), na.rm = TRUE),
                distinct_new_user_count = n_distinct(ifelse( !is.na(propentry_placedOn) & new_vs_old_user == 'New User', contestanttransaction_idContestant, NA), na.rm = TRUE),
                distinct_old_user_count = n_distinct(ifelse( !is.na(propentry_placedOn) & new_vs_old_user == 'Old User', contestanttransaction_idContestant, NA), na.rm = TRUE),
                
                entries = n_distinct(ifelse(!is.na(propentry_placedOn), paste( contestanttransaction_idContestant, idValue), NA) , na.rm = TRUE),
                entries_new_users = n_distinct(ifelse(!is.na(propentry_placedOn) & new_vs_old_user == 'New User', paste( contestanttransaction_idContestant, idValue), NA), na.rm = TRUE),
                entries_old_users = n_distinct(ifelse(!is.na(propentry_placedOn) &  new_vs_old_user == 'Old User', paste( contestanttransaction_idContestant, idValue), NA), na.rm = TRUE),
                
                revenue = sum(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn), transaction_amount,0 ), na.rm = TRUE),
                #   avg_revenue = mean(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn), transaction_amount,0 ), na.rm = TRUE),
                revenue_new_users = sum(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn) & new_vs_old_user == 'New User', transaction_amount, NA), na.rm = TRUE),
                revenue_old_users = sum(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn) & new_vs_old_user == 'Old User', transaction_amount, NA), na.rm = TRUE),
                
                winning = sum(ifelse(transaction_reason %in% c('game_bet winnings') & !is.na(propentry_placedOn) & propentry_result == 'win', transaction_amount, 0 ), na.rm = TRUE),
                profit = revenue - winning,
                profit_margin = ifelse(revenue == 0, NA, profit/revenue),
                
                bet_won = n_distinct(ifelse(!is.na(propentry_placedOn) & propentry_result == 'win', paste( contestanttransaction_idContestant, idValue), NA) , na.rm = TRUE),
                perc_bet_won = ifelse(entries == 0, NA, 100*bet_won/entries)
                
                
      ) %>%
      mutate(
        # add abs otherwise the calculations would be wrong
        mutate(across(where(is.numeric),
                      ~ {
                        weekly_growth <- 100 * (. - lag(.)) / abs(lag(.))
                        ifelse(is.na(weekly_growth), 0, weekly_growth)
                      },
                      .names = "{.col}_weekly_growth"))
      ) %>%
      ungroup()
  })
  
  monthly_level_propentry_data <- reactive({
    distinct_propentry_level_data() %>%
      filter(!is.na(transaction_month)) %>%
      group_by(transaction_month) %>%
      summarise(distinct_user_count = n_distinct(ifelse( !is.na(propentry_placedOn), contestanttransaction_idContestant, NA), na.rm = TRUE),
                distinct_new_user_count = n_distinct(ifelse( !is.na(propentry_placedOn) & new_vs_old_user == 'New User', contestanttransaction_idContestant, NA), na.rm = TRUE),
                distinct_old_user_count = n_distinct(ifelse( !is.na(propentry_placedOn) & new_vs_old_user == 'Old User', contestanttransaction_idContestant, NA), na.rm = TRUE),
                
                entries = n_distinct(ifelse(!is.na(propentry_placedOn), paste( contestanttransaction_idContestant, idValue), NA) , na.rm = TRUE),
                entries_new_users = n_distinct(ifelse(!is.na(propentry_placedOn) & new_vs_old_user == 'New User', paste( contestanttransaction_idContestant, idValue), NA), na.rm = TRUE),
                entries_old_users = n_distinct(ifelse(!is.na(propentry_placedOn) &  new_vs_old_user == 'Old User', paste( contestanttransaction_idContestant, idValue), NA), na.rm = TRUE),
                
                revenue = sum(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn), transaction_amount,0 ), na.rm = TRUE),
                #   avg_revenue = mean(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn), transaction_amount,0 ), na.rm = TRUE),
                revenue_new_users = sum(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn) & new_vs_old_user == 'New User', transaction_amount, NA), na.rm = TRUE),
                revenue_old_users = sum(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn) & new_vs_old_user == 'Old User', transaction_amount, NA), na.rm = TRUE),
                
                winning = sum(ifelse(transaction_reason %in% c('game_bet winnings') & !is.na(propentry_placedOn) & propentry_result == 'win', transaction_amount, 0 ), na.rm = TRUE),
                profit = revenue - winning,
                profit_margin = ifelse(revenue == 0, NA, profit/revenue),
                
                bet_won = n_distinct(ifelse(!is.na(propentry_placedOn) & propentry_result == 'win', paste( contestanttransaction_idContestant, idValue), NA) , na.rm = TRUE),
                perc_bet_won = ifelse(entries == 0, NA, 100*bet_won/entries)
                
                
      ) %>%
      mutate(
        # add abs otherwise the calculations would be wrong
        mutate(across(where(is.numeric),
                      ~ {
                        monthly_growth <- 100 * (. - lag(.)) / abs(lag(.))
                        ifelse(is.na(monthly_growth), 0, monthly_growth)
                      },
                      .names = "{.col}_monthly_growth"))
      ) %>%
      ungroup()
  })
  
  
  
  # Functions --------------------------------------------------------------------   
  weekly_default_chart <- function(source_data=source_data, value_column=value_column, growth_value=growth_value, ylabel = ylabel){
    data <- tryCatch({
      data_check <- {{source_data}} %>% filter(!is.na({{value_column}}))
      # %>%
      # filter({{value_column}} != 0)
    }, error = function(e) NULL)
    
    if (is.null(data) || nrow(data) == 0 ) {
      p <-  error_graph
    }
    else {
      last_point <- data_check %>% arrange(transaction_week_first_date) %>% slice_tail(n = 1)
      
      min_value_y <- data_check %>% summarise(min = min({{value_column}}, na.rm = TRUE)) %>% pull(min)
      max_value_y <- data_check %>% summarise(max = max({{value_column}}, na.rm = TRUE)) %>% pull(max)
      middle_value_y <- (max_value_y - min_value_y)/2
      
      min_value_x <- min(data_check$transaction_week_first_date, na.rm = TRUE)
      max_value_x <- max(data_check$transaction_week_first_date, na.rm = TRUE)
      middle_value_x <- as.numeric(max_value_x - min_value_x)/5
      
      last_point_growth <- last_point %>% select({{growth_value}})
      
      title_color <- ifelse( last_point_growth > 0, "#43CD80", ifelse(last_point_growth == 0, "black", "#CD4F39"))
      
      p <- data %>% ggplot(aes(x = transaction_week_first_date, y={{value_column}},  )) + 
        geom_line(alpha = 0.5) +
        geom_point(color="#0675DD", alpha = 0.5, size = default_point_size*1) +
        xlab("Prop Placed On Week") +
        ylab(ylabel) +
        theme_tufte() +
        scale_color_identity() + 
        ggtitle(paste("\n", "Growth % (Last week):", round(last_point_growth,2), "%"))  +
        theme(
          legend.position="none",
          text = element_text(family = "Open Sans"),
          plot.margin = unit(c(0.5, 0.1, 0, 0.1),"inches"),
          plot.title = element_text(color = title_color),
          axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=10))
      
      # Adjust y-axis based on unique values
      unique_values <- range(data %>% pull({{value_column}}))
      
      if ( (unique_values[2]- unique_values[1] ) == 0) {
        p <- p + 
          scale_y_continuous(expand = expansion(mult = c(0.4, 0.4))) +
          geom_blank(aes(y = {{value_column}} + middle_value_y))
      }
      
    }
    ggplotly(p) %>%
      layout(
        xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
        legend = list(orientation = "h",
                      x = 0.5,            # center the legend horizontally
                      y = 1,            # move the legend to the top
                      xanchor = "center", # anchor the legend at the center
                      yanchor = "bottom"  # anchor the legend at the bottom
        )
      )
  }
  
  monthly_default_chart <- function(source_data=source_data, value_column=value_column, growth_value=growth_value, ylabel = ylabel){
    data <- tryCatch({
      data_check <- {{source_data}} %>% filter(!is.na({{value_column}}))
    }, error = function(e) NULL)
    
    if (is.null(data) || nrow(data) == 0 ) {
      p <-  error_graph
    }
    else {
      last_point <- data_check %>% arrange(transaction_month) %>% slice_tail(n = 1)
      
      min_value_y <- data_check %>% summarise(min = min({{value_column}}, na.rm = TRUE)) %>% pull(min)
      max_value_y <- data_check %>% summarise(max = max({{value_column}}, na.rm = TRUE)) %>% pull(max)
      middle_value_y <- (max_value_y - min_value_y)/2
      
      min_value_x <- min(data_check$transaction_month, na.rm = TRUE)
      max_value_x <- max(data_check$transaction_month, na.rm = TRUE)
      middle_value_x <- as.numeric(max_value_x - min_value_x)/5
      
      last_point_growth <- last_point %>% select({{growth_value}})
      
      title_color <- ifelse( last_point_growth > 0, "#43CD80", ifelse(last_point_growth == 0, "black", "#CD4F39"))
      
      p <- data %>% ggplot(aes(x = transaction_month, y={{value_column}},  )) + 
        geom_line(alpha = 0.5) +
        geom_point(color="#0675DD", alpha = 0.5, size = default_point_size*2) +
        xlab("Prop Placed On Month") +
        ylab(ylabel) +
        theme_tufte() +
        scale_color_identity() + 
        ggtitle(paste("\n", "Growth % (Last month):", round(last_point_growth,2), "%")) +
        theme(
          legend.position="none",
          text = element_text(family = "Open Sans"),
          plot.margin = unit(c(0.5, 0.1, 0, 0.1),"inches"),
          plot.title = element_text(color = title_color),
          axis.text.x=element_text(size=10),
          axis.text.y=element_text(size=10))
      
      # Adjust y-axis based on unique values
      unique_values <- range(data %>% pull({{value_column}}))
      
      if ( (unique_values[2]- unique_values[1] ) == 0) {
        p <- p + 
          scale_y_continuous(expand = expansion(mult = c(0.4, 0.4))) +
          geom_blank(aes(y = {{value_column}} + middle_value_y))
      }
    }
    ggplotly(p) %>%
      layout(
        xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
        legend = list(orientation = "h",
                      x = 0.5,            # center the legend horizontally
                      y = 1,            # move the legend to the top
                      xanchor = "center", # anchor the legend at the center
                      yanchor = "bottom"  # anchor the legend at the bottom
        )
      )
  }
  
  
  ### Output Big Values
  
  output$revenue_output <- renderUI({
    formatted_value <- scales::unit_format(unit = "USD", big.mark = ",")(top_level_propentry_data() %>% pull(revenue))
    tags$span(style = "font-size: 80%;", formatted_value)  # Wrap in tags$span for proper rendering
  })
  
  output$winning_output <- renderUI({
    formatted_value <- scales::unit_format(unit = "USD", big.mark = ",")(top_level_propentry_data() %>% pull(winning))
    tags$span(style = "font-size: 80%;", formatted_value)  # Wrap in tags$span for proper rendering
  })
  
  output$profit_output <- renderUI({
    formatted_value <- scales::unit_format(unit = "USD", big.mark = ",")(top_level_propentry_data() %>% pull(profit))
    tags$span(style = "font-size: 80%;", formatted_value)  # Wrap in tags$span for proper rendering
  })
  
  output$profitmargin_output <- renderUI({
    temp_data <- top_level_propentry_data() %>%
      pull(profit_margin)
    if(is.na(temp_data)) {
      return(tags$span(style = "font-size: 80%;", "It is NA"))  # Return a message indicating NA
    } else {
      formatted_value <- scales::unit_format(unit = "", accuracy = 0.01, big.mark = ",")( temp_data)
      tags$span(style = "font-size: 80%;", formatted_value)  # Wrap in tags$span for proper rendering
    }
  })
  
  output$numberofbets_output <- renderUI({
    formatted_value <- scales::unit_format(unit = "bets", big.mark = ",")(top_level_propentry_data() %>% pull(entries))
    tags$span(style = "font-size: 80%;", formatted_value )  # Wrap in tags$span for proper rendering
  })
  
  output$percwon_output <- renderUI({
    temp_data <- top_level_propentry_data() %>%
      pull(perc_bet_won)
    if(is.na(temp_data)) {
      return(tags$span(style = "font-size: 80%;", "It is NA"))  # Return a message indicating NA
    } else {
      formatted_value <- scales::unit_format(unit = "", accuracy = 0.01, big.mark = ",")( temp_data)
      tags$span(style = "font-size: 80%;", formatted_value)  # Wrap in tags$span for proper rendering
    }
  })
  
  
  ### Output Lines
  ### Default weekly line chart Output------------------------------------------------------------------------
  
  output$revenue_trend_week_output <- renderPlotly({
    weekly_default_chart(weekly_level_propentry_data(),revenue, revenue_weekly_growth, "Revenue")
  })
  
  output$winning_trend_week_output <- renderPlotly({
    weekly_default_chart(weekly_level_propentry_data(),winning, winning_weekly_growth,"Winnings")
  })
  
  output$profit_trend_week_output <- renderPlotly({
    weekly_default_chart(weekly_level_propentry_data(),profit, profit_weekly_growth,"Profit")
  })
  
  output$profitmargin_trend_week_output <- renderPlotly({
    weekly_default_chart(weekly_level_propentry_data(),profit_margin, profit_margin_weekly_growth,"Profit Margin")
  })
  
  output$numberofbets_trend_week_output <- renderPlotly({
    weekly_default_chart(weekly_level_propentry_data(),entries, entries_weekly_growth,"# of Bets")
  })
  
  output$percwon_trend_week_output <- renderPlotly({
    weekly_default_chart(weekly_level_propentry_data(),perc_bet_won, perc_bet_won_weekly_growth,"% Bets Won")
  })
  
  ### Default weekly line chart Output------------------------------------------------------------------------
  
  output$revenue_trend_month_output <- renderPlotly({
    monthly_default_chart(monthly_level_propentry_data(),revenue, revenue_monthly_growth, "Revenue")
  })
  
  output$winning_trend_month_output <- renderPlotly({
    monthly_default_chart(monthly_level_propentry_data(),winning, winning_monthly_growth,"Winnings")
  })
  
  output$profit_trend_month_output <- renderPlotly({
    monthly_default_chart(monthly_level_propentry_data(),profit, profit_monthly_growth,"Profit")
  })
  
  output$profitmargin_trend_month_output <- renderPlotly({
    monthly_default_chart(monthly_level_propentry_data(),profit_margin, profit_margin_monthly_growth,"Profit Margin")
  })
  
  output$numberofbets_trend_month_output <- renderPlotly({
    monthly_default_chart(monthly_level_propentry_data(),entries, entries_monthly_growth,"# of Bets")
  })
  
  output$percwon_trend_month_output <- renderPlotly({
    monthly_default_chart(monthly_level_propentry_data(),perc_bet_won, perc_bet_won_monthly_growth,"% Bets Won")
  })
  
  ### Weekday---------------------------------------------------------------------- 
  
  ### Weekday Data
  
  ## Group by Weekday
  
  numberofbets_groupby_weekday_distribution_data <- reactive({
    temp <- distinct_propentry_level_data() %>%
      filter(transaction_reason %in% ( 'game_bet purchase')
             & transaction_balance %in% c('card charges', 'deposits', 'winnings')) %>%
      group_by(transaction_weekday) %>%
      summarise(entries = n_distinct( idValue , na.rm = TRUE)) %>%
      mutate(perc_entries = 100*entries/sum(entries)) %>%
      ungroup()
    temp$transaction_weekday <- factor(temp$transaction_weekday, ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    temp
    
  })
  
  weekly_numberofbets_groupby_weekday_distribution_data <- reactive({
    temp <- distinct_propentry_level_data() %>%
      filter(transaction_reason %in% ( 'game_bet purchase')
             & transaction_balance %in% c('card charges', 'deposits', 'winnings')) %>%
      group_by(transaction_weekday, transaction_week_first_date) %>%
      summarise(entries = n_distinct( idValue , na.rm = TRUE)) %>%
      mutate(perc_entries = 100*entries/sum(entries)) %>%
      ungroup()
    temp$transaction_weekday <- factor(temp$transaction_weekday, ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    temp
    
  })
  
  monthly_numberofbets_groupby_weekday_distribution_data <- reactive({
    temp <- distinct_propentry_level_data() %>%
      filter(transaction_reason %in% ( 'game_bet purchase')
             & transaction_balance %in% c('card charges', 'deposits', 'winnings')) %>%
      group_by(transaction_weekday, transaction_month) %>%
      summarise(entries = n_distinct( idValue , na.rm = TRUE)) %>%
      mutate(perc_entries = 100*entries/sum(entries)) %>%
      ungroup()
    temp$transaction_weekday <- factor(temp$transaction_weekday, ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    temp
    
  })
  
  ### Weekday Plots
  
  output$numberofbets_weekday_distribution_output <- renderPlotly({
    data <- tryCatch({
      data_check <- numberofbets_groupby_weekday_distribution_data()
    }, error = function(e) NULL)
    
    if (is.null(data) || nrow(data) == 0 ) {
      p <-  error_graph
    }
    else {
      
      p <- data %>% ggplot(aes(x=as.factor(transaction_weekday), y = entries, fill= entries)) + 
        geom_bar(stat = "identity", alpha=0.6, width = 0.5, color='black') +
        geom_text(aes(label = paste0( "#", entries, "\n", round(perc_entries), "%")), 
                  position = position_stack(vjust = 0.5),
                  size = 4,
                  color = "black") +
        # scale_color_gradient2(low='red', mid='snow3', high='darkgreen', space='Lab') +
        # scale_fill_gradient2(low='red', mid='snow3', high='darkgreen', space='Lab') +
        scale_color_binned(type = "viridis") +
        scale_fill_binned(type = "viridis") +
        xlab("Week Days") +
        ylab("Entries Frequency") +
        theme_tufte() +
        theme(legend.position="none", text = element_text(family = "Open Sans"),
              plot.margin = unit(c(0.5, 0.5, 0, 0),"inches"),
              axis.text.x = element_text(face="bold", size = 15, angle = 45, hjust=1),
              axis.text.y = element_text( size = 10))
      if(input$log_transform2) {
        p <- p +
          scale_y_continuous(trans = "log10")
      }
    }
    ggplotly(p) %>%
      layout(
        # xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
        legend = list(orientation = "h",
                      x = 0.5,            # center the legend horizontally
                      y = 1,            # move the legend to the top
                      xanchor = "center", # anchor the legend at the center
                      yanchor = "bottom"  # anchor the legend at the bottom
        )
      )
    
  })
  
  output$weekly_numberofbets_weekday_distribution_output <- renderPlotly({
    
    data <- tryCatch({
      data_check <- weekly_numberofbets_groupby_weekday_distribution_data()
    }, error = function(e) NULL)
    
    if (is.null(data) || nrow(data) == 0 ) {
      p <-  error_graph
    }
    else {
      
      p <- data %>% 
        ggplot(aes(x=transaction_week_first_date, y = entries, color= as.factor(transaction_weekday))) + 
        geom_line(linewidth=1, alpha = 0.6) +
        geom_point(size=3, alpha= 0.8) +
        # scale_color_manual(values = custom.col) +
        # scale_fill_manual(values = custom.col) +
        # scale_color_brewer(palette = "Spectral") +
        # scale_fill_brewer(palette = "Spectral") +
        scale_color_viridis_d(direction = -1) +
        scale_fill_viridis_d(direction = -1) +
        xlab("Week") +
        ylab("Entries Frequency") +
        theme_tufte() +
        theme(legend.position="bottom", text = element_text(family = "Open Sans"),
              plot.margin = unit(c(0.5, 0.5, 0, 0),"inches"),
              axis.text.x = element_text(face="bold", size = 15),
              axis.text.y=element_text(size=10))
      #+
      #  facet_wrap(~ propgame_sport, ncol = 3, scales="free")
      
      if(input$log_transform2) {
        p <- p +  scale_y_continuous(trans = "log10")
      }
    }
    ggplotly(p) %>%
      layout(
        xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
        legend = list(orientation = "h",
                      x = 0.5,            # center the legend horizontally
                      y = 1,            # move the legend to the top
                      xanchor = "center", # anchor the legend at the center
                      yanchor = "bottom"  # anchor the legend at the bottom
        )
      )
  })
  
  output$monthly_numberofbets_weekday_distribution_output <- renderPlotly({
    
    data <- tryCatch({
      data_check <- monthly_numberofbets_groupby_weekday_distribution_data()
    }, error = function(e) NULL)
    
    if (is.null(data) || nrow(data) == 0 ) {
      p <-  error_graph
    }
    else {
      
      p <- data %>% ggplot(aes(x= transaction_month, y = entries, color= transaction_weekday)) + 
        geom_line(linewidth=1.5, alpha= 0.6) +
        geom_point(size=4, alpha= 0.8) +
        scale_color_viridis_d(direction = -1) +
        scale_fill_viridis_d(direction = -1) +
        xlab("Month") +
        ylab("Entries Frequency") +
        theme_tufte() +
        theme(legend.position="bottom", text = element_text(family = "Open Sans"),
              plot.margin = unit(c(0.5, 0.5, 0, 0),"inches"),
              axis.text.x = element_text(face="bold", size = 15),
              axis.text.y=element_text(size=10))
      #+
      #  facet_wrap(~ propgame_sport, ncol = 3, scales="free")
      
      if(input$log_transform2) {
        p <- p + scale_y_continuous(trans = "log10")
      }
    }
    ggplotly(p) %>%
      layout(
        xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
        legend = list(orientation = "h",
                      x = 0.5,            # center the legend horizontally
                      y = 1,            # move the legend to the top
                      xanchor = "center", # anchor the legend at the center
                      yanchor = "bottom"  # anchor the legend at the bottom
        )
      )
  })
  
  ### BUY-IN----------------------------------------------------------------------  
  
  ### Data
  
  each_purchase_data <- reactive({
    distinct_propentry_level_data() %>%
      filter(transaction_reason %in% ( 'game_bet purchase') &
               transaction_balance %in%  c('card charges', 'deposits', 'winnings')) %>%
      group_by(contestanttransaction_idContestant, idValue) %>%
      summarise(entry_amount_by_user_prop_entry = sum(transaction_amount)) %>%
      ungroup()
  })
  
  numberofbets_groupby_buyin_distribution_data <- reactive({ 
    each_purchase_data() %>%
      group_by(entry_amount_by_user_prop_entry) %>%
      summarise(count = n_distinct(paste(contestanttransaction_idContestant, idValue), na.rm =TRUE)) %>%
      arrange(-count)
    
  })
  ### charts
  output$numberofbets_buyin_distribution_output2 <- renderPlotly({
    
    data <- tryCatch({
      data_check <- each_purchase_data()
      
    }, error = function(e) NULL)
    
    if (is.null(data) || nrow(data) == 0 ) {
      p <-  error_graph
    }
    else {
      
      avg_entry_amount <- each_purchase_data() %>%
        summarise(AVG_entry = round(mean(entry_amount_by_user_prop_entry),2)) %>%
        pull(AVG_entry)
      
      p <- data %>% ggplot() +
        geom_histogram(aes(entry_amount_by_user_prop_entry), bins = input$numberofbins, alpha= 0.6, color = "#000000", fill = "#0099F8") +
        geom_vline(aes(xintercept = mean(entry_amount_by_user_prop_entry)), color = "#000000", size = 1.25) +
        geom_vline(aes(xintercept = mean(entry_amount_by_user_prop_entry) + sd(entry_amount_by_user_prop_entry)), color = "#000000", size = 1, linetype = "dashed") +
        geom_vline(aes(xintercept = mean(entry_amount_by_user_prop_entry) - sd(entry_amount_by_user_prop_entry)), color = "#000000", size = 1, linetype = "dashed") +
        # scale_x_continuous(breaks = seq(data %>% summarize(min= min(entry_amount_by_user_prop_entry)) %>% pull(min),
        #                                 data %>% summarize(max= max(entry_amount_by_user_prop_entry)) %>% pull(max),
        #                                 by=100)) +
        scale_color_binned(type = "viridis") +
        scale_fill_binned(type = "viridis") +
        xlab("Buy-In Amount") +
        ylab("Entries Frequency") +
        theme_tufte() +
        theme(legend.position="none", text = element_text(family = "Open Sans"),
              plot.margin = unit(c(0.5, 0.5, 0, 0),"inches"),
              axis.text.x = element_text(face="bold", size = 15),
              axis.text.y = element_text( size = 10)) +
        ggtitle(paste("\n", "AVG Entry Amount:", avg_entry_amount)  ) +
        ylim(0, NA) 
      
      if(input$log_transform3) {
        p <- p +
          scale_y_continuous(trans = "log10")
      }
    }
    ggplotly(p) %>%
      layout(
        # xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
        legend = list(orientation = "h",
                      x = 0.5,            # center the legend horizontally
                      y = 1,            # move the legend to the top
                      xanchor = "center", # anchor the legend at the center
                      yanchor = "bottom"  # anchor the legend at the bottom
        )
      )
    #  ,                      xlab = "Buy-In Amount", ylab="Entries Frequency")
  })
  
  
  output$buyin_distribution_table_output <- DT::renderDT({ 
    
    data <- convert_column_names(round_decimal_columns(
      numberofbets_groupby_buyin_distribution_data()
    ))
    DT::datatable(
      data,
      options = list(
        #  dom = 'Bfrtip',
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 5
      )
    ) %>%
      DT::formatCurrency(c('Entry Amount By User Prop Entry'))
  })
  
  
  # User Analytics
  # Data
  user_total_data <- reactive({
    distinct_propentry_level_data() %>%
      group_by(contestanttransaction_idContestant) %>%
      summarise(distinct_user_count = n_distinct(ifelse( !is.na(propentry_placedOn), contestanttransaction_idContestant, NA), na.rm = TRUE),
                distinct_new_user_count = n_distinct(ifelse( !is.na(propentry_placedOn) & new_vs_old_user == 'New User', contestanttransaction_idContestant, NA), na.rm = TRUE),
                distinct_old_user_count = n_distinct(ifelse( !is.na(propentry_placedOn) & new_vs_old_user == 'Old User', contestanttransaction_idContestant, NA), na.rm = TRUE),
                
                entries = n_distinct(ifelse(!is.na(propentry_placedOn), paste( contestanttransaction_idContestant, idValue), NA) , na.rm = TRUE),
                entries_new_users = n_distinct(ifelse(!is.na(propentry_placedOn) & new_vs_old_user == 'New User', paste( contestanttransaction_idContestant, idValue), NA), na.rm = TRUE),
                entries_old_users = n_distinct(ifelse(!is.na(propentry_placedOn) &  new_vs_old_user == 'Old User', paste( contestanttransaction_idContestant, idValue), NA), na.rm = TRUE),
                
                revenue = sum(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn), transaction_amount,0 ), na.rm = TRUE),
                #   avg_revenue = mean(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn), transaction_amount,0 ), na.rm = TRUE),
                revenue_new_users = sum(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn) & new_vs_old_user == 'New User', transaction_amount, NA), na.rm = TRUE),
                revenue_old_users = sum(ifelse(transaction_reason %in% c('game_bet purchase') & transaction_balance %in% c('card charges', 'deposits', 'winnings') & !is.na(propentry_placedOn) & new_vs_old_user == 'Old User', transaction_amount, NA), na.rm = TRUE),
                
                winning = sum(ifelse(transaction_reason %in% c('game_bet winnings') & !is.na(propentry_placedOn) & propentry_result == 'win', transaction_amount, 0 ), na.rm = TRUE),
                profit = revenue - winning,
                profit_margin = ifelse(revenue == 0, NA, profit/revenue),
                
                bet_won = n_distinct(ifelse(!is.na(propentry_placedOn) & propentry_result == 'win', paste( contestanttransaction_idContestant, idValue), NA) , na.rm = TRUE),
                perc_bet_won = ifelse(entries == 0, NA, 100*bet_won/entries)
                
                
      ) %>%
      ungroup()
  })
  
  # Filters
  
  num_var_choices <- reactive({
    req(user_total_data())
    names(user_total_data() %>% 
            select_if(is.numeric)
    )
  })
  
  # Render the selectInput dynamically based on the numerical columns
  output$y_axis_user_varselect <- renderUI({
    req(num_var_choices())
    selectizeInput("variabley", "Y Axis:", choices = num_var_choices(), selected = "revenue")
  })
  
  # Render the selectInput dynamically based on the numerical columns
  output$x_axis_user_varselect <- renderUI({
    req(num_var_choices())
    selectizeInput("variablex", "X Axis:", choices = num_var_choices(), selected = "entries")
  })
  
  # Render the selectInput dynamically based on the numerical columns
  output$color_user_varselect <- renderUI({
    req(num_var_choices())
    selectizeInput("color", "Color:", choices = c("Default", num_var_choices()), selected = "Default")
  })
  
  # Render the selectInput dynamically based on the numerical columns
  output$size_user_varselect <- renderUI({
    req(num_var_choices())
    selectizeInput("size", "Size:", choices = c("Default", num_var_choices()), selected = "perc_bet_won")
  })
  # Plot
  output$user_scatter_plot_output <-  renderPlotly({
    #    req(input$variablex, input$variabley)
    data <- tryCatch({
      data_check <- user_total_data()
    }, error = function(e) NULL)
    
    if (is.null(data) || nrow(data) == 0 ) {
      p <-  error_graph
    }
    else {
      
      x_var <- if (is.null(input$variablex))  {"entries"
      } else {
        input$variablex    
      }
      
      y_var <- if (is.null(input$variabley))  {"revenue"
      } else {
        input$variabley    
      }
      
      fill_var <- if (is.null(input$color))  { "Default"
      } else {
        input$color    
      }
      
      size_var <- if (is.null(input$size))  { "perc_bet_won"
      } else {
        input$size    
      }
      
      p <- data %>% ggplot(aes(.data[[x_var]], .data[[y_var]])) + 
        # geom_bar(stat = "identity", alpha=0.6, width = 0.5, color='black') +
        #   geom_point(aes_string(fill= fill_var, color=fill_var), alpha= 0.6)  +
        scale_size(range = c(2, 7)) +
        stat_smooth(method = "gam", alpha=0.5, se= input$user_scatter_se) +
        scale_color_binned(low = "#FC4E07",
                           high = "#008B99") +
        scale_fill_binned(low = "#FC4E07",
                          high = "#008B99") +
        # scale_color_viridis_c()+
        # scale_fill_viridis_c() +
        # scale_color_steps(low='#D2691E',
        #                   #    mid='snow3',
        #                       high='#D2691E', space='Lab') +
        # scale_fill_steps(low='#D2691E',,
        #                   #   mid='snow3',
        #                      high='#D2691E', space='Lab') +
        theme_tufte() +
        theme(legend.position="none",
              text = element_text(family = "Open Sans"),
              plot.margin = unit(c(0.5, 0.5, 0, 0),"inches"),
              axis.text.x = element_text(face="bold", size = 10),
              axis.text.y = element_text( size = 10)) +
        ggtitle(paste0("X AVG ", x_var,": <b>", data %>%
                         summarise(avg_x_var = round(mean(.data[[x_var]], na.rm = TRUE),2)) %>%
                         pull(avg_x_var), "</b>", "\n",
                       "Y AVG ", y_var,": <b>",  data %>%
                         summarise(avg_y_var = round(mean(.data[[y_var]], na.rm = TRUE),2) ) %>%
                         pull(avg_y_var), "</b>"
        )
        )+
        xlab(str_to_title(x_var))+
        ylab(str_to_title(y_var))
      
      if (size_var == "Default" & fill_var == "Default") {
        p <- p + geom_point(size = 6, alpha = 0.5)
      } else if (size_var == "Default") {
        p <- p + geom_point(size = 6, aes(fill= !!sym(fill_var)), alpha = 0.5)
      } else if (fill_var == "Default") {  
        p <- p + geom_point(aes(size = !!sym(size_var)), colour = "black", alpha = 0.5)
        
      } else {
        p <- p + geom_point(aes(fill= !!sym(fill_var), color=!!sym(fill_var), size = !!sym(size_var)), alpha = 0.5)
      }
      
      
      if(input$user_scatter_y_log_transform) {
        p <- p +
          #  scale_y_log10() +
          scale_y_continuous(trans = "log10")
      }
      
      if(input$user_scatter_x_log_transform) {
        p <- p +
          #  scale_x_log10() + 
          scale_x_continuous(trans = "log10")
      }
    }
    ggplotly(p) %>%
      layout(
        #  xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
        legend = list(orientation = "h",
                      x = 0.5,            # center the legend horizontally
                      y = 1,            # move the legend to the top
                      xanchor = "center", # anchor the legend at the center
                      yanchor = "bottom"  # anchor the legend at the bottom
        )
      )
  })
  
  
  
  
  output$user_correlation_output <- renderPlotly({
    data <- tryCatch({
      data_check <- user_total_data() %>%
        select_if(is.numeric) %>%
        na.omit() %>%
        # remove the column if it has only 1 distinct values
        select_if(~n_distinct(.) > 1) %>%
        convert_column_names(.)
    }, error = function(e) NULL)
    ## add ncol(data) < 2 condition to make sure we have at least 2 columns
    if (is.null(data) || nrow(data) == 0 || ncol(data) < 2 ) {
      p <-  error_graph
    }
    else {
      corr <- round(cor(data), 1)
      # Visualize
      
      p <- ggcorrplot(corr, p.mat = cor_pmat(data),
                      hc.order = TRUE, type = "lower",
                      color = c("#FC4E07", "white", "#00AFBB"),
                      outline.col = "white", lab = TRUE)
      ggplotly(p) %>%
        layout(
          xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
          legend = list(orientation = "h",
                        x = 0.5,            # center the legend horizontally
                        y = 1,            # move the legend to the top
                        xanchor = "center", # anchor the legend at the center
                        yanchor = "bottom"  # anchor the legend at the bottom
          )
        )
    }
  })
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
  
  ### Table and text Output---------------------------------------------    
  output$weeklysummary_output <- DT::renderDT({ 
    
    data <- convert_column_names(round_decimal_columns(
      
      weekly_level_propentry_data() %>%
        arrange(desc(transaction_week_first_date))
    ))
    DT::datatable(
      data,
      options = list(
        lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15', 'All')),
        pageLength = 5
      ) 
    )
  })
  
  distinct_pick_level_data <- reactive({
    updated_main_game_bets_data() %>%
      select(idProp,
             idPropEntry,
             createdOn,
             lastUpdatedOn,
             sport,
             statName,
             playerName,
             playerTeamCode,                            
             opponentTeamCode,
             gameTime,
             gameTime_day,
             line,
             lockerLine,
             lockerType,
             bonusMultiplier,
             idPropGame,
             idPropPlayer,
             contestantChoice,
             entry_number_of_picks, entry_number_of_placed_picks, entry_number_of_win_picks,
             entry_number_of_void_picks, entry_number_of_loss_picks, entry_number_of_over_choice,
             entry_number_of_under_choice,) %>% 
      distinct(idProp, .keep_all = TRUE)
  })
  
  
  
  ### Table and text Output---------------------------------------------    
  # output$datatableid <- DT::renderDT({
  # 
  #   data <-
  #      convert_column_names(round_decimal_columns(
  #     numberofbets_groupby_buyin_distribution_data()
  #   ))
  #   DT::datatable(
  #     data,
  #     options = list(
  #       lengthMenu = list(c(5, 10, 15, -1), c('5', '10', '15', 'All')),
  #       pageLength = 5
  #     )
  #   )
  # })
  
  # output$selected_vars <- renderPrint({
  #   input$n_stats
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
