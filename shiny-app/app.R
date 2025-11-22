# Установка и подключение необходимых пакетов
# install.packages(c("shiny", "shinydashboard", "tidyverse", "scales", "plotly"))
library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
library(plotly)

# Загрузка и предобработка данных
hh <- read.csv("../hh_million_cities.csv", encoding = "UTF-8") %>%
  mutate(vacancy_count = 1) %>%
  group_by(name, employer, area_name, salary_from, salary_to, experience, schedule) %>%
  summarise(vacancy_count = sum(vacancy_count, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    salary_from = as.numeric(salary_from),
    salary_to = as.numeric(salary_to),
    salary_mid = case_when(
      !is.na(salary_from) & !is.na(salary_to) ~ (salary_from + salary_to)/2,
      !is.na(salary_from) ~ salary_from,
      !is.na(salary_to) ~ salary_to,
      TRUE ~ NA_real_
    ),
    experience_clean = str_to_lower(str_trim(experience)),
    experience_level = case_when(
      str_detect(experience_clean, "нет") ~ "0",
      str_detect(experience_clean, "1") ~ "1-3",
      str_detect(experience_clean, "3") & !str_detect(experience_clean, "1") ~ "3-6",
      str_detect(experience_clean, "6") ~ "6+",
      TRUE ~ NA_character_
    ),
    region_clean = case_when(
      str_detect(area_name, regex("москва", ignore_case = TRUE)) ~ "Москва",
      str_detect(area_name, regex("санкт", ignore_case = TRUE)) ~ "Санкт-Петербург",
      TRUE ~ area_name
    ),
    region_type = case_when(
      region_clean == "Москва" ~ "Москва",
      region_clean == "Санкт-Петербург" ~ "Санкт-Петербург",
      region_clean %in% c("Новосибирск", "Екатеринбург", "Казань", "Красноярск", 
                          "Нижний Новгород", "Челябинск", "Уфа", "Краснодар", 
                          "Самара", "Ростов-на-Дону", "Омск", "Воронеж", "Пермь",
                          "Волгоград") ~ "Города-миллионники",
      TRUE ~ "Другие"
    )
  ) %>%
  filter(region_type %in% c("Москва", "Санкт-Петербург", "Города-миллионники"))

# UI дашборда
ui <- dashboardPage(
  dashboardHeader(title = "Анализ вакансий HH"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Зарплаты по регионам", tabName = "salary_region", icon = icon("bar-chart")),
      menuItem("Зарплаты по опыту", tabName = "salary_exp", icon = icon("line-chart")),
      menuItem("Топ профессий", tabName = "top_jobs", icon = icon("table"))
    ),
    hr(),
    # Фильтры вне sidebarMenu, всегда видны и работают сразу
    selectInput("region_filter", "Выберите регион", 
                choices = sort(unique(hh$region_type)), 
                selected = unique(hh$region_type), 
                multiple = TRUE),
    selectInput("exp_filter", "Уровень опыта", 
                choices = sort(unique(hh$experience_level)), 
                selected = unique(hh$experience_level), 
                multiple = TRUE),
    selectInput("sched_filter", "График работы", 
                choices = sort(unique(hh$schedule)), 
                selected = unique(hh$schedule), 
                multiple = TRUE)
  ),
  
  dashboardBody(
    tabItems(
      # Вкладка: Зарплаты по регионам
      tabItem(tabName = "salary_region",
              fluidRow(
                box(title = "Распределение зарплат по регионам", width = 12, status = "primary",
                    plotlyOutput("plot_region"))
              )
      ),
      
      # Вкладка: Зарплаты по опыту
      tabItem(tabName = "salary_exp",
              fluidRow(
                box(title = "Зарплаты по уровню опыта", width = 12, status = "primary",
                    plotlyOutput("plot_exp"))
              )
      ),
      
      # Вкладка: Топ профессий
      tabItem(tabName = "top_jobs",
              fluidRow(
                box(title = "ТОП-10 профессий по количеству вакансий", width = 12, status = "primary",
                    plotlyOutput("plot_jobs"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Фильтрация данных по выбранным фильтрам
  filtered_data <- reactive({
    req(input$region_filter, input$exp_filter, input$sched_filter) # обязательно для корректной работы
    hh %>%
      filter(region_type %in% input$region_filter,
             experience_level %in% input$exp_filter,
             schedule %in% input$sched_filter)
  })
  
  # График: зарплаты по регионам
  output$plot_region <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = region_type, y = salary_mid, fill = region_type)) +
      geom_boxplot(outlier.alpha = 0.3) +
      coord_flip() +
      labs(title = "Зарплаты по регионам", x = "Регион", y = "Зарплата (руб.)") +
      scale_y_continuous(labels = comma_format(big.mark = " ", decimal.mark = ",")) +
      theme_minimal()
    ggplotly(p)
  })
  
  # График: зарплаты по опыту
  output$plot_exp <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = experience_level, y = salary_mid, fill = experience_level)) +
      geom_boxplot(outlier.alpha = 0.3) +
      labs(title = "Зарплаты по опыту", x = "Опыт работы", y = "Зарплата (руб.)") +
      scale_y_continuous(labels = comma_format(big.mark = " ", decimal.mark = ",")) +
      theme_minimal()
    ggplotly(p)
  })
  
  # График: топ профессий
  output$plot_jobs <- renderPlotly({
    top_jobs <- filtered_data() %>%
      group_by(name) %>%
      summarise(total_vacancies = sum(vacancy_count, na.rm = TRUE)) %>%
      arrange(desc(total_vacancies)) %>%
      slice_head(n = 10)
    
    p <- ggplot(top_jobs, aes(x = reorder(name, total_vacancies), y = total_vacancies)) +
      geom_col(fill = "#2E86AB") +
      coord_flip() +
      labs(title = "ТОП-10 профессий", x = "Профессия", y = "Количество вакансий") +
      theme_minimal()
    ggplotly(p)
  })
}

# Запуск приложения
shinyApp(ui = ui, server = server)
