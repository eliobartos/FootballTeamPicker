#setwd("~/GitHub/FootballTeamPicker")

library(shiny)
library(shinyjs)
source("Functions.R")

# CSS
# css <- 
#   tags$head(tags$style(HTML("
#                            .outline { 
#                           
#                            margin-left: 20px;
#                            padding-left: 20px;
#                            padding-bottom: 20px;
#                            }
#                            .center_div {
#                            padding-left: 40px;
#                            padding-top: 10px;
#                            }
#                            ")) 
#   )


# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  useShinyjs(),
  
  # Link CSS
  tags$head(
    includeCSS("www/my.css")
  ),
  fluidRow(
    titlePanel("Football Team Picker")
  ),
  
  fluidRow(
    column(width = 2,
      tags$div(class = 'panel_background outline',
               checkboxGroupInput("players_coming", label = h3("Tko dolazi?"), 
                                  choices = players_names_to_list(players_database),
                                  selected = NULL),
               actionButton("get_teams", "Izaberi ekipe!")
               
      )
    ),
    column(width = 3,
      
        fluidRow(class = "panel_background center_text outline",
          tags$h3(
            textOutput("players_selected")  
          )
        ),
        br(),
        fluidRow(class = "panel_background", id = "best_teams_div",
            tableOutput("best_teams")
        )
      
    )
  )
 
    
  
)


# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  removeClass("best_teams_div", "panel_background")
  
  # Called when user clicks on a button
  calculate_best_teams = eventReactive(input$get_teams, {
   
    players_data = players_database %>% 
      filter(name %in% input$players_coming)

    out = simulate_teams(players_data, n_sim = 500)

    team_1 = out$team_1[,"name"]
    colnames(team_1) = c("Team 1")
    
    team_2 = out$team_2[,"name"]
    colnames(team_2) = c("Team 2")
    
    teams = cbind(team_1, team_2)
    addClass("best_teams_div", "panel_background")
    return(teams)
  })
  
  output$best_teams = renderTable({
    out = calculate_best_teams()
  })
  
  output$players_selected = renderText(
    paste0("Broj izabranih igrača: ", length(input$players_coming))
  )
  
}

shinyApp(ui, server)