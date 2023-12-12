
library(ggplot2)
library(viridis)
library(shinythemes)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(sf)

# Data loading ----
chart_data <- read.csv('chart_data.csv')
map_data <- readRDS('map_data.rds')


# Define UI----
ui <- navbarPage('Pages',
  theme = shinytheme("paper"),
  #Tab1: ----
  tabPanel('Overview',
    titlePanel("Greenhouse gas emissions"),
    h3('A reflection on COP28'),
    h5(p('Climate change is an immediate and existential threat to our society that we must address within a relatively short time period. 
    With the world\'s attention currently focused on the latest climate conference, COP28, it\'s worthwhile to take a look at emission trends and
    recon with which countries and industries pollute our Earth, and based on hard numbers, ask: are we on track?'),
       br(),
       p('When looking at emissions from the last couple of years, the answer is clear: after a brief decrease in
       Carbon-Dioxide and Nitrous Oxide emissions due to the COVID-19 pandemic, emissions are on the rise again.')),
    # PLOT1: Interactive widget + formatted text for plot title + a plot 
    radioButtons("t00_variable", h5("Greenhouse gas type:"),
                 choices = list("Carbon Dioxide" = "Carbon_Dioxide",
                                "Methane" = "Methane",
                                "Nitrous oxide" = "Nitrous_oxide"),
                 selected = "Carbon_Dioxide"),
    h4(textOutput("t00_plotitle")),
    plotlyOutput("t00_plotly"),
    h6(em('Source: Climate Trace')),
    h5(p('To help understand what\'s behind this, this dashboard provides a snapshot into recent emissions trends. On this page, you can find the latest per country emissions of the top greenhouse gases
       (check out the glossary at the bottom of the page if you\'re unfamiliar with them). The next page provides an view on the Top 5, Top 10, and Top 15 emittors
       and their track record since 2017. The view is bleak: we\'re not on track to decarbonize. Finally, the last page lets you deep dive into the performance of 
       specific countries, look at different years, emissions sector-by-sector, and compare countries.')),
    # PLOT2: Interactive widget + formatted text for plot title + a plot 
    radioButtons('t0_variable',h5('Emission type:'),
                 choices = list(
                   'Total emissions' = 'log_emissions',
                   'Per capita emissions' = 'CO2_PerCapita'),
                 selected = 'log_emissions'
    ),
    h4(textOutput("t0_plotitle")),
    plotlyOutput("worldmap"),
    h6(em('Source: Climate Trace, Gapminder'),
       br(),
       em('You can zoom in and out with the plus and minus signs on the top-right corner, and move around with the diagonal arrows. To return to the world view,
          double click or click on the house icon. You can save the plot as a png file as well.'),
       br(),
       em('Note: coloring is based on logarithmic values of emissions to better visualize differences.')),
    #Glossary ----
    h6(strong('Glossary'),
      p('There are many pollutants released into the air on a constant basis, but here we are focusing on the three most important greenhouse gases.
         Greenhouse gases trap heat in the athmospehere. Their increased presence is the result of human activities, and they are responsible for
         global warming.'),
       br(),
       strong('Carbon-Dioxide '),
       p('is the greenhouse gas mostly discussed when talking about climate change. It is present in the atmosphere naturally,
              but burning fossil fuels or cutting down trees leads to increased emissions.'),
       br(),
       strong('Methane '),
       p('is another strong contributor to climate change. Emissions are mostly linked to agriculture, livestock, and transportation of fossil fuels
         and similar materials (e.g., badly constructed or old pipelines).'),
       br(),
       strong('Nitrous oxide '),
       p('is emitted during various industrial and agricultural activities.'),
      br(),
      em('Source: EPA'))
  ),
  #Tab2: ----
  tabPanel('Top emittors',
           titlePanel("Greenhouse gas emissions"),
           h3('Countries with the highest total emissions'),
           h5(
              p('When looking at the trend between 2017 and 2021, the overall view is quite depressing: overall emissions have not been decreasing in most countries, and
              after a brief decrease due to the COVID-19 pandemic in some places, they are on the rise again.'),
              br(),
         p('The biggest emittor currently is China, and emissions have been
         rising in the last couple of years, and they are expected to keep rising for quite some time in the future. The second biggest emittor is the United States,
         followed by India, Russia, and Japan. Many of the larger developed economies, like Canada, are in the top 15, but we can also find developing ones.'),
         br()
         ),
         # PLOT: Interactive widgets in columns + formatted text for plot title + a plot 
           fluidRow(
             column(3,
                    h4('Who are the top emittors?')),
             column(4, offset = 1,
                    radioButtons('t1_SelectedCountries',h5('Selected countries:'),
                                 choices = list(
                                   'Top 5' = '5',
                                   'Top 6 - 10' = '10',
                                   'Top 11 - 15' = '15'),
                                 selected = '5'
                    )),
             column(4,
                    radioButtons("t1_variable", h5("Greenhouse gas type:"),
                                 choices = list("Carbon Dioxide" = "Carbon_Dioxide",
                                                "Methane" = "Methane",
                                                "Nitrous oxide" = "Nitrous_oxide"),
                                 selected = "Carbon_Dioxide"))
           ),
           h4(textOutput('t1_plotitle')),
           plotlyOutput("t1_plotly"),
         h6(em('Source: Climate Trace'),
            br(),
            em('You can see the exact emission of a given country in a given year by hovering over the datapoint. To compare yearly emissions of all five countries,
               click on the double left-pointing arrow in the top-right corner.')),
         h5(p('When evaluating this picture, it\'s also important to keep in mind historical emissions: China or India might be one of the biggest pollutors now, 
         but large developed economies have been polluting for a longer time and historically contributed much more to global warming that developing countries.'),
         p('This is one of the critical areas where countries disagree: while it is clear developed economies have a greater historical role, they are often unwilling
         or unable to take responsibility and financially contribute to the green transition of developing economies.'),
         p('Developing economies, on the other hand, 
         wish to also advance and provide the same benefits and lifestyle to their citizens as developed economy citizens enjoy, but that would come at the expense of
         increased emissions, or expensive green technologies they either cannot afford, or have no access to.'))
  ),
  #Tab3: ----
  tabPanel('Country-level view',
           titlePanel("Greenhouse gas emissions"),
           h3('Explore and compare country-level emissions'),
           sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(
               p('On this tab, you can select and de-select countries of interest, choose a greenhouse gas, select a time period, and compare yearly emissions or
              sector by sector emissions between these countries.'),
               # Input 1: Selector for variable to plot 
               radioButtons("t2_variable", strong("Select greenhouse gas type:"),
                            choices = list("Carbion Dioxide" = "Carbon_Dioxide",
                                           "Methane" = "Methane",
                                           "Nitrous oxide" = "Nitrous_oxide"),
                            selected = "Carbon_Dioxide"),
               
               # Input 2: Selector for country 
               selectInput("t2_country", strong("Choose countries:"),
                           choices = sort(unique(na.omit(chart_data$country))),
                           selected = 'United States of America',
                           multiple = TRUE
               ),
               #Input 3: Select year
               sliderInput("t2_yearSelect", strong("Choose year(s):"),
                           min=2015, max=2021, value=c(2015,2021), sep='', step=1, round=0),
               h6(em('You can see the exact emission of a given country in a given year / sector by hovering over the datapoint. To compare emissions,
               click on the double left-pointing arrow in the top-right corner.'))
               
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   'By year',
                   # PLOT1: Formatted text for plot title + a plot 
                   h4(textOutput("plotitle")),
                   plotlyOutput("Emissions"),
                   h6(em('Source: Climate Trace'))
                 ),
                 tabPanel(
                   'By sector',
                   # PLOT2: Formatted text for plot title + a plot 
                   h3(textOutput("plotitle2")),
                   h4(textOutput("plotsubtitle2"), style = "font-weight: bold;"),
                   plotlyOutput("Emissions2"),
                   h6(em('Source: Climate Trace'))
                 )
               )
             )
           )
  )
)

# Define server logic to plot ----
server <- function(input, output) {
  #TAB1 ----
  #PLOT1: Total emissions ----
  t00_formulaText <- reactive({
    selected_variable <- switch(input$t00_variable,
                                "Carbon_Dioxide" = "Carbon Dioxide",
                                "Methane" = "Methane",
                                "Nitrous_oxide" = "Nitrous oxide"
    )
    paste("Yearly emissions of ", selected_variable, "in million tonnes")
  })
  # Generate a plot of the requested # of countries and gas type ----
  output$t00_plotitle <- renderText({
    t00_formulaText()
  })
  output$t00_plotly <- renderPlotly({
    #PREPROCESS for plot
    filtered_data <- chart_data %>%
      group_by(year, country) %>%
      summarize(
        Carbon_Dioxide = sum(Carbon_Dioxide, na.rm = TRUE),
        Methane = sum(Methane, na.rm = TRUE),
        Nitrous_oxide = sum(Nitrous_oxide, na.rm = TRUE),
      ) %>%
      ungroup()


    #PLOT
    fig00 <- ggplotly(ggplot(filtered_data, aes(x = year, y = get(input$t00_variable), fill=reorder(country, -get(input$t00_variable)),
                                                text = paste("Country:",country))) +
                      geom_area(stat = "identity") +
                      labs(y = 'Emissions (m ton)', x = "Year", caption = 'Source: Climate Trace') +
                      theme_minimal() +
                      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
                      scale_x_continuous(breaks = unique(filtered_data$year)) +
                      theme(
                        axis.text.x = element_text(color = "grey20", size = 12),
                        axis.text.y = element_text(color = "grey20", size = 12),
                        axis.title.x = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        plot.caption = element_text(size=10)
                      ), tooltip = 'text')
    fig00 <- fig00 %>% 
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
      "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", 
      "resetScale2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation", "hoverClosestCartesian", "hoverCompareCartesian"
    ))
    fig00
  })
  #PLOT2: Map ----
  #Reactive elements
  t0_formulaText <- reactive({
    t0_selected_variable <- switch(input$t0_variable,
                                "log_emissions" = "Carbon Dioxide in million tonnes",
                                "CO2_PerCapita" = "per capita Carbon Dioxide in tonnes"
    )
    paste("2021 global emissions of ", t0_selected_variable)
  })
  output$t0_plotitle <- renderText({
    t0_formulaText()
  })
  output$worldmap <- renderPlotly({
    #PLOT
    fig0 <- ggplotly(ggplot(map_data, aes(text = paste("Country:", SOVEREIGNT, "<br>Value:", round(get(input$t0_variable),2)))) +
      geom_sf(aes(fill = get(input$t0_variable)), size = 0.15) +
      scale_fill_distiller(palette = "RdYlBu", direction = -1) +
      labs(y = '', x = "Carbon Dioxide emissions in 2021", fill = 'Emissions', caption = 'Source: Climate Trace') +
      theme_minimal(), tooltip = 'text')
    fig0 <- fig0 %>% layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3))
    fig0 <- fig0 %>% config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
      "zoom2d", "select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
       "zoom3d", "pan3d", "orbitRotation", "tableRotation"
    ))
    fig0
    
  })
  #TAB2 ----
  #reactive elements
  t1_formulaText <- reactive({
    selected_variable <- switch(input$t1_variable,
                                "Carbon_Dioxide" = "Carbon Dioxide",
                                "Methane" = "Methane",
                                "Nitrous_oxide" = "Nitrous oxide"
    )
    paste("Yearly emissions of ", selected_variable, "in million tonnes")
  })
  # PLOT
  output$t1_plotitle <- renderText({
    t1_formulaText()
  })
  output$t1_plotly <- renderPlotly({
    #PREPROCESS for plot
    filter <- chart_data %>%
      group_by(country) %>%
      summarize(total_emissions = sum(!!sym(input$t1_variable), na.rm = TRUE)) %>%
      arrange(desc(total_emissions)) %>%
      select(country)
    
    selected_palette <- colorRampPalette(brewer.pal(9, "Paired"))
    interpolated_colors <- selected_palette(15)
    unique_countries <- unique(filter$country)[1:15]
    color_mapping <- setNames(interpolated_colors[1:length(unique_countries)], unique_countries)
    
    filter <- filter%>%
      slice((as.numeric(input$t1_SelectedCountries)-4):as.numeric(input$t1_SelectedCountries))
      
    #Filter our data based on selected countries
    filtered_data <- chart_data %>%
      semi_join(filter, by = "country") %>%
      group_by(country, year) %>%
      summarize(
        CO2_PerCapita = sum(CO2_PerCapita, na.rm = TRUE),
        Carbon_Dioxide = sum(Carbon_Dioxide, na.rm = TRUE),
        Methane = sum(Methane, na.rm = TRUE),
        Nitrous_oxide = sum(Nitrous_oxide, na.rm = TRUE),
      ) %>%
      ungroup()
    #PLOT itself
    fig <- ggplotly(ggplot(filtered_data, aes(x = year, y = get(input$t1_variable), group=country,
                                                     text = paste("Year:", year, "<br>Country:", country, "<br>Value:", get(input$t2_variable)))) +
                      geom_line(aes(color=country)) +
                      geom_point(aes(color=country)) +
                      scale_color_manual(values = color_mapping) +
                      labs(y = 'Emissions (m ton)', x = "Year", color = 'Country', caption = 'Source: Climate Trace') +
                      theme_minimal() +
                      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
                      scale_x_continuous(breaks = unique(filtered_data$year)) +
                      theme(
                        legend.position="bottom",
                        legend.title = element_text(size = 14),
                        legend.text = element_text(size = 12),
                        axis.text.x = element_text(color = "grey20", size = 12),
                        axis.text.y = element_text(color = "grey20", size = 12),
                        axis.title.x = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        plot.caption = element_text(size=10)
                      ), tooltip='text')
    fig <- fig %>% layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3))
    fig <- fig %>% config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
      "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", 
      "resetScale2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation"
    ))
    fig

  })
  
  #TAB3 ----
  # Reactive elements
  gasType <- reactive({
    selected_variable <- switch(input$t2_variable,
                                "Carbon_Dioxide" = "Carbon Dioxide",
                                "Methane" = "Methane",
                                "Nitrous_oxide" = "Nitrous oxide"
    )
    
    paste(selected_variable)
  })
  formulaText <- reactive({
    selected_variable <- switch(input$t2_variable,
                                "Carbon_Dioxide" = "Carbon Dioxide",
                                "Methane" = "Methane",
                                "Nitrous_oxide" = "Nitrous oxide"
    )
    paste("Yearly emissions of ", selected_variable, "in million tonnes")
  })
  
  
  # Plot title
  output$plotitle <- renderText({
    formulaText()
  })
  
  #PLOT
  output$Emissions <- renderPlotly({
    #PREPROCESS data
    filtered_data <- chart_data[chart_data$country %in% input$t2_country, ]
    year_range <- input$t2_yearSelect
    filtered_data <- filtered_data %>%
      filter(year >= year_range[1] & year <= year_range[2]) %>%
      group_by(country, year) %>%
      summarize(
        Carbon_Dioxide = sum(Carbon_Dioxide, na.rm = TRUE),
        Methane = sum(Methane, na.rm = TRUE),
        Nitrous_oxide = sum(Nitrous_oxide, na.rm = TRUE),
      ) %>%
      ungroup()
    #PLOT itself
    fig2 <- ggplotly(ggplot(filtered_data, aes(x = year, y = get(input$t2_variable), group=country,
                                                      text = paste("Year:", year, "<br>Country:", country, "<br>Value:", get(input$t2_variable)))) +
      geom_line(aes(color=country)) +
      geom_point(aes(color=country)) +
      scale_color_viridis(discrete = TRUE) +
      labs(y = gasType(), x = "Year", color = 'Country', caption = 'Source: Climate Trace') +
      theme_minimal() +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      scale_x_continuous(breaks = unique(filtered_data$year)) +
      theme(
        legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        plot.caption = element_text(size=10)
      ), tooltip='text')
    fig2 <- fig2 %>% layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3))
    fig2 <- fig2 %>% config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
      "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", 
      "resetScale2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation"
    ))
    
    fig2
  })
  output$plotitle2  <- renderText({
    'Sector-level emissions'
  })
  output$plotsubtitle2  <- renderText({
    'Agriculture, Power generation, and Fossil fuel operations are the biggest contributors for respective greenhouse gas emissions'
  })
  #PLOT
  output$Emissions2  <- renderPlotly({
    #PREPROCESS data
    selected_variable <- switch(input$t2_variable,
                                "Carbon_Dioxide" = "Carbon_Dioxide",
                                "Methane" = "Methane",
                                "Nitrous_oxide" = "Nitrous_oxide"
    )
    year_range <- input$t2_yearSelect
    sector_data <- chart_data %>%
      filter(country %in% input$t2_country) %>%
      filter(year >= year_range[1] & year <= year_range[2]) %>%
      rename(sector = `sector.subsector`)%>%
      group_by(sector, country) %>%
      summarize(
        Carbon_Dioxide = sum(Carbon_Dioxide, na.rm = TRUE),
        Methane = sum(Methane, na.rm = TRUE),
        Nitrous_oxide = sum(Nitrous_oxide, na.rm = TRUE),
      ) %>%
      ungroup()
    #Plot itself
    fig3 <- ggplotly(ggplot(sector_data, aes(x = reorder(sector, get(selected_variable)), y = get(selected_variable), fill=country,
                                             text = paste("Sector:", sector, "<br>Country:", country, "<br>Value:", get(selected_variable)))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(y = gasType(), x = '', caption= ' Source: Climate Trace') +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.text.x = element_text(color = "grey20", size = 12),
        axis.text.y = element_text(color = "grey20", size = 12),
        axis.title.y = element_text(size=14),
        plot.caption = element_text(size=10)
      ) +
      scale_fill_viridis_d(), tooltip='text')
    fig3 <- fig3 %>% layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3))
    fig3 <- fig3 %>% config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
      "zoom2d", "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d", "autoScale2d", 
      "resetScale2d", "zoom3d", "pan3d", "orbitRotation", "tableRotation"
    ))
    fig3

  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
