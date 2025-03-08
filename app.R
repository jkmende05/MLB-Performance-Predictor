# Load necessary libraries
library(shiny)
library(shinydashboard)
library(later)

source("stat_prediction.R")

ui <- dashboardPage(
  dashboardHeader(title = "MLB 2024 Batting Stats Predictor", titleWidth = 350),
  dashboardSidebar(
    collapsed = FALSE,
    sidebarMenu(
      menuItem("Stat Predictions", tabName = "prediction",
               icon = icon("chart-simple")),
      menuItem("Player ID Lookup", tabName = "player_id_lookup",
               icon = icon("magnifying-glass")),
      menuItem("Instructions", tabName = "instructions",
               icon = icon("question"))
    ),
    textInput("player_id", "Player ID:", value = "player_id"),
    actionButton("predict_button", "Get Predicted Stats")
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .navbar .sidebar-toggle { 
          display: none;
        }
        .skin-blue .main-header .logo {
          background-color : #041E42;
        }
        .skin-blue .main-header .logo:hover {
          background-color : #041E42;
        }
        .skin-blue .main-header .navbar {
          background-color : #041E42;
        }
        .skin-blue .sidebar-menu i {
          margin-right: 10px;
          width: 15px;
        }
        .sidebar .action-button {
          display: block;
          margin-left: auto;
          margin-right: auto;
          width: 87%;
        }
        .instructions_box .box-header {
          background-color: #BF0D3E !important;
        }
        .instructions_box .box{
          border-color: #BF0D3E !important;
        }
        .predictions_box .box-header {
          background-color: #BF0D3E !important;
        }
        .predictions_box .box{
          border-color: #BF0D3E !important;
        }
        .lookup_box .box-header {
          background-color: #BF0D3E !important;
        }
        .lookup_box .box{
          border-color: #BF0D3E !important;
        }
        .table-title {
          font-size: 1.5em;
          font-weight: bold;
          color: #041E42;
        }
        .scrollable-table {
          overflow-x: auto;
          white-space: nowrap;
        }

        .table-container {
          max-width: 100%;
        }
      ")),
      tags$script(HTML("
          Shiny.addCustomMessageHandler('triggerPrediction', function(message) {
            Shiny.setInputValue('triggerPrediction', Math.random()); 
          });
      "))
    ),
    tabItems(
      tabItem(
        tabName = "prediction",
        fluidRow(
          box(
            title = "Predicted Player Stats", width = 12, status = "primary",
            solidHeader = TRUE,
            p("Enter a valid first and last name and then click the 'Get
             Predicted Stats' button."),
            tags$h3(class = "table-title", "Career Stats"),
            div(class = "scrollable-table table-container",
                tableOutput("past_stats_table")),
            tags$h3(class = "table-title", "2024 Predicted Stats"),
            div(class = "scrollable-table table-container",
                tableOutput("prediction_table"))
          ),
          class = "predictions_box"
        )
      ),
      tabItem(
        tabName = "player_id_lookup",
        fluidRow(
          box(
            title = "Player ID Lookup", width = 12, status = "primary",
            solidHeader = TRUE,
            p("Since throughout MLB History, there have been players with 
            the same name, use their unique ID to get predicted stats. Use 
            below to determine your desired player's ID."),
            textInput("first_name", "Player First Name:", value = "First Name"),
            textInput("last_name", "Player Last Name:", value = "First Name"),
            actionButton("lookup_button", "Find Player ID"),
            div(class = "scrollable-table table-container",
                tableOutput("player_id_table"))
          ),
          class = "lookup_box"
        )
      ),
      tabItem(
        tabName = "instructions",
        fluidRow(
          box(
            title = "Instructions", width = 12, status = "info",
            solidHeader = TRUE,
            p("Enter the first and last name of the desired player in the 
            input boxes found on the left. To get their 2024 Predicted Stats, 
            click on the button labeled 'Get Predicted Stats'. This program 
            uses a combination of machine learning techniques, such as XGBoost 
            and Clustering methods, to attempt to accurately predict an MLB 
            Player's stats for the 2024 Season. Currently, only hitting 
            statistics are available. Additionally, since no 2024 stats were 
            available at the time of creation, this program can only predict 
            stats for the past 2024 season. One important thing to note is that 
            since the model requires extensive training and testing for 
            accurate predictions for each stat, it can take up to four 
            minutes once the player's name has been entered. The predictor will 
            only work if the player played in 2023. For example, it will not 
            predict stats for Babe Ruth.")
          ),
          class = "instructions_box"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$lookup_button, {
    req(input$first_name, input$last_name)

    player_id_data <- tryCatch({
      player_name_data(input$first_name, input$last_name)
    }, error = function(e) {
      NULL
    })

    if (is.null(player_id_data) || nrow(player_id_data) < 1) {
      output$player_id_table <- renderTable({
        tibble(Error = "Error: Player not found or data issue.")
      })
    } else {
      output$player_id_table <- renderTable({
        player_id_data
      })
    }
  })
  observeEvent(input$predict_button, {
    req(input$player_id)

    # Fetch past stats synchronously
    past_stats <- tryCatch({
      get_past_stats(input$player_id)
    }, error = function(e) {
      NULL
    })

    # Render past stats immediately
    if (is.null(past_stats) || nrow(past_stats) < 1) {
      output$past_stats_table <- renderTable({
        tibble(Error = "Error: Player not found or data issue.")
      })
    } else {
      decimal_col <- c("BA", "OBP", "SLG")

      past_stats <- past_stats %>%
        mutate(across(all_of(decimal_col), ~ round(., 3))) %>%
        mutate(across(all_of(decimal_col), ~ format(., nsmall = 3)))

      output$past_stats_table <- renderTable({
        past_stats
      })

    }

    session$sendCustomMessage("triggerPrediction", list())
  })

  observeEvent(session$input$triggerPrediction, {
    req(input$player_id)

    output$prediction_table <- renderTable({
      tibble()
    })

    withProgress(message = "Calculating predicted stats: ", value = 0, {
      incProgress(0.3, detail = "Currently training model...")

      past_stats <- tryCatch({
        get_past_stats(input$player_id)
      }, error = function(e) {
        NULL
      })

      active_player <- any(past_stats[["yearID"]] == 2023)

      # Fetch predictions
      if (!active_player){
        output$prediction_table <- renderTable({
          tibble(Error = "Error: Invalid Player. Ensure spelling is correct and player has played in 2023.")
        })
      } else {
        predictions <- tryCatch({
          get_predicted_stats(input$player_id)
        }, error = function(e) {
          NULL
        })

        incProgress(0.6, detail = "Calculating predictions...")

        # Render predictions
        if (is.null(predictions)) {
          output$prediction_table <- renderTable({
            tibble()
          })
        } else {
          decimal_col <- c("pred_ba", "pred_obp", "pred_slg")

          predictions <- predictions %>%
            mutate(across(all_of(decimal_col), ~ round(., 3))) %>%
            mutate(across(all_of(decimal_col), ~ format(., nsmall = 3))) %>%
            mutate(across(setdiff(names(predictions), decimal_col),
                          ~ round(., 0))) %>%
            mutate(across(setdiff(names(predictions), decimal_col),
                          ~ format(., nsmall = 0)))
          output$prediction_table <- renderTable({
            predictions
          })
        }

        incProgress(1, detail = "Finalizing...")
      }
    })
  }, ignoreInit = TRUE)
}

# Run the application
shinyApp(ui, server)