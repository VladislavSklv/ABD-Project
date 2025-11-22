# Установка пакетов (если ещё не стоят)
install.packages(c("httr", "jsonlite", "purrr", "dplyr", "stringr"))

library(httr)
library(jsonlite)
library(purrr)
library(dplyr)
library(stringr)

# Безопасная функция для извлечения вложенных полей
safe_extract <- function(x, path) {
  cur <- x
  for (p in path) {
    if (is.null(cur) || !p %in% names(cur)) return(NA)
    cur <- cur[[p]]
  }
  if (is.list(cur) && length(cur) == 0) return(NA)
  return(cur)
}

# Универсальная функция для загрузки вакансий по коду города
get_hh_data <- function(area_id, pages = 5, per_page = 100, pause = 0.3) {
  all_items <- list()
  for (p in 0:(pages - 1)) {
    url <- paste0("https://api.hh.ru/vacancies?area=", area_id,
                  "&per_page=", per_page, "&page=", p)
    res <- GET(url, user_agent("R (learning project)"))
    if (status_code(res) != 200) break
    txt <- content(res, "text", encoding = "UTF-8")
    rjson <- fromJSON(txt, simplifyVector = FALSE)
    items <- rjson$items
    if (length(items) == 0) break
    
    page_df <- map_df(items, function(it) {
      tibble(
        id = safe_extract(it, c("id")),
        name = safe_extract(it, c("name")),
        area_name = safe_extract(it, c("area","name")),
        employer = safe_extract(it, c("employer","name")),
        salary_from = safe_extract(it, c("salary","from")),
        salary_to = safe_extract(it, c("salary","to")),
        salary_currency = safe_extract(it, c("salary","currency")),
        experience = safe_extract(it, c("experience","name")),
        schedule = safe_extract(it, c("schedule","name")),
        employment = safe_extract(it, c("employment","name")),
        address_raw = safe_extract(it, c("address","raw")),
        snippet_requirement = safe_extract(it, c("snippet","requirement")),
        snippet_responsibility = safe_extract(it, c("snippet","responsibility")),
        published_at = safe_extract(it, c("published_at"))
      )
    })
    all_items[[length(all_items)+1]] <- page_df
    Sys.sleep(pause)
  }
  bind_rows(all_items)
}

# Коды регионов HH.ru для крупных городов
# (по документации hh.ru /areas)
cities <- tribble(
  ~city,              ~area_id,
  "Москва",            1,
  "Санкт-Петербург",   2,
  "Новосибирск",       4,
  "Екатеринбург",      3,
  "Казань",            88,
  "Красноярск",        54,
  "Нижний Новгород",   66,
  "Челябинск",         104,
  "Уфа",               99,
  "Краснодар",         53,
  "Самара",            78,
  "Ростов-на-Дону",    76,
  "Омск",              68,
  "Воронеж",           26,
  "Пермь",             72,
  "Волгоград",         24
)

# Скачиваем данные для всех этих городов
all_data <- map2_dfr(cities$area_id, cities$city, function(id, nm) {
  message("Загрузка: ", nm)
  df <- get_hh_data(area_id = id, pages = 50)  # pages можно увеличить
  df$city_label <- nm
  df
})

# Обработка и добавление средней зарплаты
hh_all <- all_data %>%
  mutate(
    salary_from = as.numeric(salary_from),
    salary_to = as.numeric(salary_to),
    salary_mid = case_when(
      !is.na(salary_from) & !is.na(salary_to) ~ (salary_from + salary_to)/2,
      !is.na(salary_from) ~ salary_from,
      !is.na(salary_to) ~ salary_to,
      TRUE ~ NA_real_
    )
  )

# Сохраняем объединённый датасет
write.csv(hh_all, "hh_million_cities.csv", row.names = FALSE, fileEncoding = "UTF-8")
message("✅ Сохранено hh_million_cities.csv (строк: ", nrow(hh_all), ")")
