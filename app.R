library(shiny)
library(tidyverse)
library(janitor)
library(broom)
library(scales)

# ----------------------------
# Data load (same as your qmd)
# ----------------------------
obesity_raw <- read_csv(
  "https://data.cdc.gov/resource/hn4x-zwk7.csv?$limit=50000",
  show_col_types = FALSE
)

obesity_age <- obesity_raw %>%
  filter(
    class == "Obesity / Weight Status",
    question == "Percent of adults aged 18 years and older who have obesity",
    stratificationcategory1 == "Age (years)",
    !is.na(age_years),
    !is.na(data_value),
    locationabbr != "US"
  ) %>%
  transmute(
    state_abbr = locationabbr,
    state_name = locationdesc,
    year = yearstart,
    age_group = age_years,
    obesity_pct = data_value
  ) %>%
  mutate(
    age_low  = readr::parse_number(age_group),
    age_high = readr::parse_number(str_extract(age_group, "\\d+\\s*$")),
    age_mid  = case_when(
      !is.na(age_high) ~ (age_low + age_high) / 2,
      TRUE             ~ age_low + 5
    )
  )

target_year <- max(obesity_age$year)

obesity_year <- obesity_age %>%
  filter(year == target_year)

state_slopes <- obesity_year %>%
  group_by(state_abbr, state_name) %>%
  filter(n() >= 3) %>%
  do(tidy(lm(obesity_pct ~ age_mid, data = .))) %>%
  filter(term == "age_mid") %>%
  ungroup() %>%
  mutate(slope_per_decade = estimate * 10)

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  titlePanel("How Sharply Does Obesity Increase with Age?"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "state",
        "Select a state:",
        choices = sort(unique(obesity_year$state_name)),
        selected = "New York"
      ),
      helpText(
        "The line plot shows obesity prevalence by age group for the selected state.
         The histogram shows how that state's slope compares nationally."
      )
    ),
    
    mainPanel(
      plotOutput("age_plot", height = 350),
      plotOutput("slope_plot", height = 300)
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output) {
  
  state_data <- reactive({
    obesity_year %>%
      filter(state_name == input$state)
  })
  
  state_slope <- reactive({
    state_slopes %>%
      filter(state_name == input$state)
  })
  
  output$age_plot <- renderPlot({
    ggplot(state_data(), aes(x = age_mid, y = obesity_pct)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        title = paste("Obesity vs Age in", input$state),
        x = "Age (midpoint of group)",
        y = "Obesity prevalence (%)"
      ) +
      theme_minimal()
  })
  
  output$slope_plot <- renderPlot({
    ggplot(state_slopes, aes(x = slope_per_decade)) +
      geom_histogram(bins = 25, fill = "grey80", color = "white") +
      geom_vline(
        xintercept = state_slope()$slope_per_decade,
        color = "red",
        linewidth = 1.2
      ) +
      labs(
        title = "Distribution of age–obesity slopes across states",
        subtitle = paste(input$state, "highlighted in red"),
        x = "Slope (pp change per decade of age)",
        y = "Number of states"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)

