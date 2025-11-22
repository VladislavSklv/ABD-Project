# Установка необходимых пакетов
install.packages(c("tidyverse", "stringr", "lubridate", "scales", "rstatix", "clinfun", "lmtest", "sandwich", "coin"))

library(tidyverse)
library(stringr)
library(lubridate)
library(scales)
library(rstatix)
library(clinfun)
library(lmtest)
library(sandwich)

# загрузка датасета
hh <- read.csv("hh_million_cities.csv", encoding = "UTF-8")

str(hh)
colnames(hh)

# количество вакансий
hh <- hh %>%
  mutate(vacancy_count = 1)

# удаляем дубликаты ===
hh <- hh %>%
  group_by(name, employer, area_name, salary_from, salary_to, experience, schedule) %>%
  summarise(vacancy_count = sum(vacancy_count, na.rm = TRUE), .groups = "drop")

# перевод зп в числовой формат и расчёт mid
hh <- hh %>%
  mutate(
    salary_from = as.numeric(salary_from),
    salary_to = as.numeric(salary_to),
    salary_mid = case_when(
      !is.na(salary_from) & !is.na(salary_to) ~ (salary_from + salary_to) / 2,
      !is.na(salary_from) ~ salary_from,
      !is.na(salary_to) ~ salary_to,
      TRUE ~ NA_real_
    )
  )

# очистка и категоризация опыта
hh <- hh %>%
  mutate(experience_clean = str_to_lower(str_trim(experience))) %>%
  mutate(experience_level = case_when(
    str_detect(experience_clean, "нет") ~ "0",
    str_detect(experience_clean, "1") ~ "1-3",
    str_detect(experience_clean, "3") & !str_detect(experience_clean, "1") ~ "3-6",
    str_detect(experience_clean, "6") ~ "6+",
    TRUE ~ NA_character_
  ))

# определение региона
hh <- hh %>%
  mutate(region_clean = case_when(
    str_detect(area_name, regex("москва", ignore_case = TRUE)) ~ "Москва",
    str_detect(area_name, regex("санкт", ignore_case = TRUE)) ~ "Санкт-Петербург",
    TRUE ~ area_name
  ))

# класс регионов
million_cities <- c("Новосибирск", "Екатеринбург", "Казань", "Красноярск", 
                    "Нижний Новгород", "Челябинск", "Уфа", "Краснодар", 
                    "Самара", "Ростов-на-Дону", "Омск", "Воронеж", "Пермь",
                    "Волгоград")

hh <- hh %>%
  mutate(region_type = case_when(
    region_clean == "Москва" ~ "Москва",
    region_clean == "Санкт-Петербург" ~ "Санкт-Петербург",
    region_clean %in% million_cities ~ "Города-миллионники",
    TRUE ~ "Другие"
  ))

#только нужные регионы
hh <- hh %>%
  filter(region_type %in% c("Москва", "Санкт-Петербург", "Города-миллионники"))

# средняя зп по регионам
weighted_salary <- hh %>%
  filter(!is.na(salary_mid)) %>%
  group_by(region_type) %>%
  summarise(weighted_salary = weighted.mean(salary_mid, vacancy_count, na.rm = TRUE))

print(weighted_salary)

# по регионам
hh %>%
  filter(!is.na(salary_mid)) %>%
  ggplot(aes(x = region_type, y = salary_mid, fill = region_type)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.6) +
  coord_flip() +
  labs(
    title = "Сравнение уровня заработных плат по регионам России",
    x = "Категория региона",
    y = "Средняя зарплата (руб.)"
  ) +
  scale_y_continuous(labels = comma_format(big.mark = " ", decimal.mark = ",")) +
  theme_minimal() +
  theme(legend.position = "none")

# топ 10
top_jobs <- hh %>%
  group_by(name) %>%
  summarise(total_vacancies = sum(vacancy_count, na.rm = TRUE)) %>%
  arrange(desc(total_vacancies)) %>%
  slice_head(n = 10)

ggplot(top_jobs, aes(x = reorder(name, total_vacancies), y = total_vacancies)) +
  geom_col(fill = "#2E86AB") +
  coord_flip() +
  labs(title = "ТОП-10 профессий по количеству вакансий (с учётом кратности)",
       x = "Профессия", y = "Количество вакансий") +
  theme_minimal(base_size = 12)

# по опыту
hh %>%
  filter(!is.na(salary_mid), !is.na(experience_level)) %>%
  ggplot(aes(x = experience_level, y = salary_mid, fill = experience_level)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.6) +
  scale_y_continuous(labels = comma_format(big.mark = " ", decimal.mark = ",")) +
  labs(
    title = "Распределение заработных плат в зависимости от опыта работы",
    x = "Уровень опыта",
    y = "Средняя зарплата (руб.)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# по занятости
hh %>%
  filter(!is.na(salary_mid), !is.na(schedule), schedule != "") %>%
  ggplot(aes(x = schedule, y = salary_mid, fill = schedule)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = comma_format(big.mark = " ", decimal.mark = ",")) +
  labs(
    title = "Распределение заработных плат в зависимости от формата занятости",
    x = "График работы",
    y = "Средняя зарплата (руб.)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

#Гипотеза 1. Различаются ли уровни заработных плат между регионами?
df_reg <- hh %>% filter(!is.na(salary_mid))
kruskal_test(df_reg, salary_mid ~ region_type)
kruskal_effsize(df_reg, salary_mid ~ region_type)
df_reg %>% dunn_test(salary_mid ~ region_type, p.adjust.method = "bonferroni")

#Гипотеза 2. Отличаются ли зарплаты Москвы и Санкт-Петербурга?
df_two <- hh %>% filter(region_type %in% c("Москва","Санкт-Петербург"))
wilcox_test(df_two, salary_mid ~ region_type)
wilcox_effsize(df_two, salary_mid ~ region_type)

# Исправляем experience_level для Jonckheere теста
df_exp <- hh %>%
  filter(!is.na(salary_mid), !is.na(experience_level)) %>%
  mutate(experience_num = case_when(
    experience_level == "0"   ~ 0,
    experience_level == "1-3" ~ 1,
    experience_level == "3-6" ~ 2,
    experience_level == "6+"  ~ 3
  ))

table(df_exp$experience_num)

#Гипотеза 3. Растёт ли зарплата по мере увеличения опыта?
df_exp$experience_num <- as.numeric(df_exp$experience_num)
jonckheere.test(df_exp$salary_mid, df_exp$experience_num, alternative = "increasing")

#Гипотеза 4. Влияет ли формат занятости на уровень зарплаты?
df_sched <- hh %>% filter(!is.na(schedule), !is.na(salary_mid))
kruskal_test(df_sched, salary_mid ~ schedule)
df_sched %>% dunn_test(salary_mid ~ schedule, p.adjust.method = "bonferroni")

#Гипотеза 5. Зависит ли структура востребованных профессий от региона?
top_jobs <- hh %>% count(name) %>% slice_max(n, n = 20) %>% pull(name)
df_prof <- hh %>% filter(name %in% top_jobs)
tbl <- table(df_prof$region_type, df_prof$name)
chisq.test(tbl, simulate.p.value = TRUE, B = 2000)

#Регрессионное моделирование (линейная регрессия)
df_model <- hh %>%
  filter(!is.na(salary_mid), !is.na(experience_level)) %>%
  mutate(log_salary = log1p(salary_mid))
model <- lm(log_salary ~ region_type + experience_level + schedule, data = df_model)
coeftest(model, vcovHC(model, type = "HC3"))




