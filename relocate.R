####### Google Trends #########
library(tidyverse)
library(lubridate)
library(showtext)

showtext_auto()
showtext_opts(dpi = 300)
font_add_google("Playfair Display", "PD")
font_add_google("PT Sans", "PT Sans")

title <- stringr::str_wrap("СВО, мобилизация и ограничения интернета — три волны интереса к эмиграции", 45)

subtitle <- stringr::str_wrap("Google Trends фиксирует в России всплески популярности поисковых запросов «переезд из России», «релокация» и «второе гражданство» с началом СВО и частичной мобилизации в 2022 году, а новый рост в 2026 году — на фоне ограничений интернета, блокировок мессенджеров и VPN", 70)

caption <- "Данные: Google Trends, 2026\nВизуализация: Юрий Тукачев, апрель 2026 @weekly_charts"

# загрузка данных
df <- read_csv("time_series_RU_20210425-1510_20260425-1510.csv")

# tidy
df_long <- df %>%
  rename(date = 1) %>%
  mutate(date = as.Date(date)) %>%
  pivot_longer(-date, names_to = "query", values_to = "value") %>%
  mutate(
    query = recode(
      query,
      "переезд из россии" = "переезд из России"
    )
  )

# подписи линий
label_x <- max(df_long$date) + days(40)
line_labels <- df_long %>%
  group_by(query) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  mutate(
    x = label_x,
    y = value
  )

# события: СВО, мобилизация и блокровки интернета
events <- tibble(
  date = as.Date(c("2022-01-15", "2022-11-10", "2026-02-01")),
  y = c(68, 68, 40),
  label = c(
    "фев 2022\nНачало\nСВО",
    "сен 2022\nЧастичная\nмобилизация",
    "мар 2026\nНовая волна\nинтереса"
  ),
  x_text = as.Date(c("2021-09-01", "2023-05-01", "2025-09-01")),
  y_text = c(78, 78, 55),
  curvature = c(0.25, -0.25, 0.25)
)

colors <- c(
  "переезд из России" = "#1D3557",   
  "второе гражданство" = "#7A9E7E",  
  "релокация" = "#C05746"            
)

ggplot(df_long, aes(x = date, y = value, color = query)) +
  geom_segment(
    data = tibble(y = 0),
    aes(
      x = min(df_long$date),
      xend = as.Date("2026-04-30"),
      y = y,
      yend = y
    ),
    inherit.aes = FALSE,
    color = "grey30",
    linewidth = 0.8
  ) +
  geom_line(aes(size = query)) +
  scale_size_manual(values = c(
    "релокация" = 0.9,
    "переезд из России" = 0.9,
    "второе гражданство" = 0.9
  )) +
  # стрелки событий
  # geom_curve(
  #   data = events,
  #   aes(x = x_text, y = y_text, xend = date, yend = y),
  #   curvature = events$curvature,
  #   arrow = arrow(length = unit(0.02, "npc")),
  #   color = "grey30",
  #   size = 0.4,
  #   inherit.aes = FALSE
  # ) +
  lapply(seq_len(nrow(events)), function(i) {
    geom_curve(
      data = events[i, ],
      aes(x = x_text, y = y_text, xend = date, yend = y),
      curvature = events$curvature[i],
      arrow = arrow(length = unit(0.02, "npc")),
      color = "grey50",
      size = 0.4,
      inherit.aes = FALSE
    )
  }) +
  # текст событий
  geom_text(
    data = events,
    aes(x = x_text, y = y_text, label = label),
    hjust = 0.5,
    color = "gray50",
    vjust = 0,
    nudge_y = 2, 
    size = 4.5,
    lineheight = 0.9,
    family = "PT Sans",
    inherit.aes = FALSE
  ) +
  # подписи линий
  geom_text(
    data = line_labels,
    aes(x = x, y = y, label = query, color = query),
    hjust = 0,
    size = 4.5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_y_continuous(breaks = seq(0,100,20), 
                     expand = expansion(mult = c(0, 0.05))) + 
  scale_color_manual(values = colors) +
  scale_x_date(expand = expansion(mult = c(0.01, 0.3))) +
  labs(
    title = title,
    subtitle = subtitle,
    x = NULL,
    y = NULL,
    caption = caption
  ) +
  theme_minimal(base_family = "PT Sans", base_size = 16) +
  theme(
    plot.margin = margin(20, 25, 20, 25),
    plot.title = element_text(
      size = 22,
      face = "bold",
      family = "PD",
      color = "black",
      margin = margin(b = 15)
    ),
    plot.subtitle = element_text(
      size = 16,
      color = "gray40",
      margin = margin(b = 15)
    ),
    legend.position = "none",
    axis.text = element_text(color = "#4D4D4D"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(
      size = 13,
      color = "gray70",
      lineheight = 1.2,
      hjust = 0,
      margin = margin(t = 20)
    )
  )

ggsave(
  "relocate_Google_Trends_ru_2026.png",
  height = 8,
  width = 8,
  dpi = 300
)
