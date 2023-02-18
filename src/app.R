# Chess Shiny Dashboard:
# An exploration of FIDE Chess Elo ratings
# Dashboard by @bristowrichards
# bristowrichards.github.io/blog

# load libraries --------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(rlang)

# load data -------------------------------------------------------------------
ratings <- readRDS('ratings.Rds')
# federations <- readRDS('federations.Rds')

# data handling functions -----------------------------------------------------

# define reactive function to subset data based on Byear range
make_ratings_year_subset <- function(data, input) {
  req(input$Byear_range) # make sure selected
  data |> 
    filter(
      Byear >= input$Byear_range[[1]],
      Byear <= input$Byear_range[[2]]
    ) %>%
    return()
}

# return data filtered
make_country_subset <- function(data, input) {
  # only filter if group is Fed
  if (input$group == 'Fed') {
    data <- data |> 
      filter(
        is.element(
          Fed, input$fed_selected
        )
      ) 
  }
  
  return(data)
}

# put it together
make_data_subset <- function(data, input) {
  data <- data |> 
    make_ratings_year_subset(input = input) |>
    make_country_subset(input = input)
  
  return(data)
}

get_top_ten <- function(data, input) {
  top_ten_tbl <- data |> 
    arrange(desc(!!sym(input$time))) |> 
    head(10) 
  
  return(top_ten_tbl)
}

make_top_datatable <- function(top_ten) {
  dt <- top_ten |> 
    DT::datatable(
      options = list(lengthChange = FALSE)
    )
  
  return(dt)
}



# viz distribution functions --------------------------------------------------

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
  .plot_type <- input$plot_type
  .group <- input$group
  .bins <- 50
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





# viz bubble functions --------------------------------------------------------

make_scatter_plot <- function(data, input) {
  # save options for easy access
  .time <- input$time
  .highlight_elo <- input$highlight_elo
  
  # save string of number of players
  player_count_string <- 
    scales::comma_format()(
      nrow(data)
    )
  
  # settings for all
  plot_settings <- list(
    theme_classic(),
    labs(
      caption = 'Data: FIDE, January 2023\nViz: @bristowrichards'
    ),
    xlab(paste('Average', pretty_labels[.time])),
    theme(plot.title = element_text(size=18))
  )
  
  # data
  data <- make_ratings_year_subset(data, input) |> 
    group_by(Fed) |> 
    summarize(
      player_count = n(),
      average_elo = mean(!!sym(input$time), na.rm = TRUE),
      average_age = mean(Age, na.rm = TRUE),
      has_top_players = factor(any(!!sym(input$time) > .highlight_elo))
    ) 
  
  plt <- ggplot(
    data, 
    aes(
      Federation = Fed,
      x = average_elo, 
      y = average_age, 
      size = player_count,
      color = has_top_players
    )
  ) + 
    geom_point(alpha = 0.7) +
    scale_color_manual(
      values = c('FALSE' = "grey40",
                 'TRUE' = 'green4')
    ) +
    scale_size(range = c(1, 10), name = 'Player Count') +
    labs(
      color = 'Has Top Performer'
    ) +
    ylab('Average Age') +
    plot_settings
  
  return(plt)
    
}

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
        'Elo Distributions', 
        tabName = 'tab_dist', 
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
        'Federation Snapshot', 
        tabName = 'tab_fed', 
        icon = icon('bishop', lib = 'glyphicon')
      )
    ),
    
    # put here preference for type of ratings
    # make it input
    radioButtons(
      inputId = 'time',
      label = 'Time control:',
      choices = c(
        'Standard' = 'SRtng',
        'Rapid' = 'RRtng',
        'Bullet' = 'BRtng'
      )
    ),
    
    # birth year range
    sliderInput(
      inputId = 'Byear_range',
      label = 'Birth year:',
      min = 1910,
      max = 2020,
      value = c(1930, 2020),
      sep = ''
    )
  ),
  
  ## dashboard body ----
  dashboardBody(
    tabItems(
      
      ### 1. Elo Dashboard ----
      tabItem(
        tabName = 'tab_dist',
        box(
          title = 'Distribution Plot', status = 'primary', solidHeader = TRUE,
          plotlyOutput('distribution_plot', height = 350),
          width = 9
        ),
        
        box(
          title = 'Inputs', status = 'warning', solidHeader = TRUE,
          width = 3,
          
          # select grouping
          radioButtons(
            inputId = 'group',
            label = 'Select grouping',
            choices = c(
              'Birth decade' = 'Bdecade',
              'Sex' = 'Sex',
              'Federation' = 'Fed',
              'None' = 'None'
            )
          ),
          
          # select country if country is selected
          conditionalPanel(
            condition = "input.group == 'Fed'",
            selectizeInput(
              inputId = 'fed_selected',
              label = 'Select Federations (max 7)',
              choices = readRDS('federations.Rds'),
              selected = c('USA', 'RUS', 'IND'),
              multiple = TRUE,
              options = list(maxItems = 7)
            )
          ),
          
          # graph type
          radioButtons(
            inputId = 'plot_type',
            label = 'Select graph type',
            choices = c(
              'Histogram' = 'hist',
              'Density Curve' = 'dens',
              'Frequency Curve' = 'freq'
            )
          )
        )
      ),
      
      ### 2. Scatter Plot ----
      tabItem(
        tabName = 'tab_scatter',
        
        # show plot
        box(
          title = 'Federation Scatter Plot', 
          status = 'primary',
          solidHeader = TRUE,
          plotlyOutput('federation_scatter_plot', height = 350),
          width = 8
        ),
        
        # inputs
        box(
          title = 'Inputs', status = 'warning', solidHeader = TRUE,
          width = 4,
          
          # birth year range
          sliderInput(
            inputId = 'highlight_elo',
            label = 'Highlight federations with players above:',
            min = 1000,
            max = 2800,
            value = 2700,
            sep = ''
          )
        )
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
  
  # add filter subset data for distribution tab
  data$dist_ratings_subset <- reactive(
    make_data_subset(
      data = data$ratings,
      input = input
    )
  )
  
  # add plots ----
  output$distribution_plot <- renderPlotly({
    make_dist_plot(
      data = data$dist_ratings_subset(),
      input = input
    )
  })
  
  output$federation_scatter_plot <- renderPlotly({
    make_scatter_plot(
      data = data$ratings,
      input = input
    )
  })
  
  # test text
  output$test_text <- renderText('testing 123')
  output$text <- renderText(input$text)
}

# run app ---------------------------------------------------------------------
shinyApp(ui, server)