# Chess Shiny Dashboard:
# An exploration of FIDE Chess Elo ratings
# Dashboard by @bristowrichards
# bristowrichards.github.io/blog

# load libraries --------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(ggplot2)

# load data -------------------------------------------------------------------
ratings <- readRDS('ratings.Rds')
# federations <- readRDS('federations.Rds')

# data handling functions -----------------------------------------------------

## labels ----
pretty_labels <- list(
  'Bdecade' = 'Birth decade',
  'Sex' = 'Sex',
  'Fed' = 'Federation',
  'None' = NULL,
  'SRtng' = 'Standard Time Control Elo Score',
  'RRtng' = 'Rapid Time Control Elo Score',
  'BRtng' = 'Bullet Time Control Elo Score'
)

## distributions plots ----
make_dist_plot <- function(data, input) {
  # save options for easy access
  .time = input$time
  .plot_type <- input$plot_type
  .group <- input$group
  .bins <- input$bins
  .time <- input$time
  
  # save string of number of players
  player_count_string <- 
    scales::comma_format()(
      nrow(data)
    )
  
  # settings for all
  plot_settings <- list(
    theme_classic(),
    labs(
      subtitle = paste(
        'This plot shows data on',
        player_count_string,
        'players'
      ),
      caption = 'Data: FIDE, January 2023\nViz: @bristowrichards'
    ),
    xlab(pretty_labels[.time]),
    theme(plot.title = element_text(size=18))
  )
  
  # list options
  plot_list <- list(
    # histogram
    'hist' = list(
      geom_histogram(
        # if fill is null, defaults to ggplot grey, which is fine
        aes(fill = if(.group != 'None') {.data[[.group]]}),
        bins = .bins,
        color = 'black'
      ),
      labs(
        title = 'Chess ELO Rating Histogram Plot',
        fill = pretty_labels[.group]
      ),
      scale_fill_viridis_d(),
      ylab('Frequency')
    ),
    
    # density
    'dens' = list(
      geom_density(
        aes(color = if(.group != 'None') .data[[.group]]),
        linewidth = 1
      ),
      labs(
        title = 'Chess ELO Rating Density Plot',
        color = pretty_labels[.group]
      ),
      scale_color_viridis_d(end = 0.9),
      ylab('Density'),
      ggtitle('hello')
    ),
    
    # frequency
    'freq' = list(
      geom_freqpoly(
        aes(color = if(.group != 'None') .data[[.group]]),
        bins = .bins,
        linewidth = 1
      ),
      labs(
        title = 'Chess ELO Rating Frequency Plot',
        color = pretty_labels[.group]
      ),
      scale_color_viridis_d(end = 0.9),
      ylab('Frequency')
    )
  )
  
  plt <- 
    ggplot(
      data = data,
      aes(x = .data[[.time]])
    ) + list(
      plot_settings,
      plot_list[.plot_type]
    )
  
  return(plt)
}



# viz distribution functions --------------------------------------------------





# viz bubble functions --------------------------------------------------------


# define ui -------------------------------------------------------------------
ui <- dashboardPage(
  
  ## header ----
  dashboardHeader(
    title = 'Chess Dashboard v2'
  ),
  
  ## sidebar ----
  dashboardSidebar(
    
    ### menu ----
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
    )
  ),
  
  ## dashboard body ----
  dashboardBody(
    tabItems(
      
      ### 1. Elo Dashboard ----
      tabItem(
        tabName = 'tab_dash',
        h2('Distributions by federation / sex / etc from hw1'),
        br(),
        box(
          title = "Histogram", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          textOutput('test_text')
        ),
        
        box(
          title = "Inputs", status = "warning", solidHeader = TRUE,
          "Box content here", br(), "More box content",
          sliderInput("slider", "Slider input:", 1, 100, 50),
          textInput("text", "Text input:")
        )
      ),
      
      ### 2. Scatter Plot ----
      tabItem(
        tabName = 'tab_scatter',
        h2('scatter plot (bubble plot?) of all federations')
      ),
      
      ### 3. Top Players ----
      tabItem(
        tabName = 'tab_players',
        h2('data table of top players given certain filters?')
      ),
      
      ### 4. Federation Detailed Summary ----
      tabItem(
        tabName = 'tab_fed',
        h2('Federation details')
      )
    )
  )
)


# define server ---------------------------------------------------------------
server <- function(input, output) { 
  ## define data list ----
  data <- list()
  
  ## add external data ----
  data$ratings <- readRDS('ratings.Rds')
  data$federations <- readRDS('federations.Rds')
  
  # add plot ----
  output$distribution_plot <- renderPlot({
    ggplot(data$ratings, aes(x = SRtng, fill = 'Fed')) +
      geom_histogram()
  })
  
  # test text
  output$test_text <- renderText('testing 123')
}

# run app ---------------------------------------------------------------------
shinyApp(ui, server)