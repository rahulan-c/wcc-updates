
# READ WCC COMBINED ACPL DATA

acpl_data <- readr::read_csv("https://raw.githubusercontent.com/michael1241/wcc_analysis/master/analysis.csv")

acpl_data_rev <- acpl_data %>% 
  add_row(Year = rep(2023, 6),
          `Game Number` = c(seq(1:6)),
          `White ACPL` = c(7, 43, 8, 9, 11, 11),
          `Black ACPL` = c(8, 16, 8, 21, 18, 18),
          `White Num Moves` = c(49, 29, 30, 47, 48, 44),
          `Black Num Moves` = c(49, 29, 30, 46, 47, 43),
          )

acpl_data_rev <- acpl_data_rev %>% 
  mutate(`Combined ACPL` = case_when(
    Year == 2023 ~ ((`White ACPL` * `White Num Moves`) + (`Black ACPL` * `Black Num Moves`)) / (`White Num Moves` + `Black Num Moves`),
    TRUE ~ `Combined ACPL`
  ))

showtext::showtext_auto(enable = F)
ggplot(data = acpl_data_rev, aes(x = Year, y = `Combined ACPL`)) +
  geom_point(size = 2, alpha = 0.3) +
  cowplot::theme_half_open()


acpl_data_rev %>% 
  dplyr::filter(`Game Number` <= 6) %>% 
  group_by(Year) %>% 
  summarise(acpl = mean(`Combined ACPL`)) %>% 
  arrange(acpl)
