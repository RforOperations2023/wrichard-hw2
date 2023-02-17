# Chess Shiny Dashboard:
# An exploration of FIDE Chess Elo ratings
# Dashboard by @bristowrichards
# bristowrichards.github.io/blog

# load libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(ggplot2)

# load data --------------------------------------------------------------------
ratings <- readRDS('ratings.Rds')
federations <- readRDS('federations.Rds')

# define ui --------------------------------------------------------------------
ui <- dashboardPage(
  
  # header ---------------------------------------------------------------------
  dashboardHeader(
    title = 'Chess Dashboard v2'
  ),
  
  # sidebar --------------------------------------------------------------------
  dashboardSidebar(
    
    # menu ---------------------------------------------------------------------
    sidebarMenu(
      menuItem(
        'Elo Dashboard', 
        tabName = 'tab_dash', 
        icon = icon('pawn', lib = 'glyphicon')
      ),
      menuItem(
        'Country Scatterplot', 
        tabName = 'tab_scatter', 
        icon = icon('knight', lib = 'glyphicon')
      ),
      menuItem(
        'Top Players', 
        tabName = 'tab_players', 
        icon = icon('king', lib = 'glyphicon')
      ),
      menuItem(
        'Distributions', 
        tabName = 'tab_fed', 
        icon = icon('bishop', lib = 'glyphicon')
      )
    ),
    
    # sidebar options ----------------------------------------------------------
    
    # let user decide whether to filter
    checkboxInput(
      inputId = 'filter_checkbox', 
      label = 'Check to filter data'
    ),
    
    conditionalPanel(
      condition = "input.filter_checkbox",
      
      # birth year range
      sliderInput(
        inputId = 'Byear_range',
        label = 'Birth year:',
        min = 1910,
        max = 2020,
        value = c(1930, 2020),
        sep = ''
      ),
    
      selectizeInput(
        inputId = 'fed_selected',
        label = 'Select Federations (max 7)',
        choices = readRDS('federations.Rds'),
        selected = c('USA', 'RUS', 'IND'),
        multiple = TRUE,
        options = list(maxItems = 7)
      )
    )
    
    
  ),
  
  # dashboard body -------------------------------------------------------------
  dashboardBody(
    tabItems(
      
      # 1. Elo Dashboard
      tabItem(
        tabName = 'tab_dash',
        h2('Distributions by federation / sex / etc from hw1')
      ),
      
      # 2. Scatter Plot
      tabItem(
        tabName = 'tab_scatter',
        h2('scatter plot (bubble plot?) of all federations')
      ),
      
      # 3. Top Players
      tabItem(
        tabName = 'tab_players',
        h2('data table of top players given certain filters?')
      ),
      
      # 4. Federation Detailed Summary
      tabItem(
        tabName = 'tab_fed',
        h2('Federation details')
      )
    )
  )
)


# define server ----------------------------------------------------------------
server <- function(input, output) { }

# run app ----------------------------------------------------------------------
shinyApp(ui, server)