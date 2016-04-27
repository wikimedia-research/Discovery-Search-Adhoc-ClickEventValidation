library(magrittr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange, select)
library(ggplot2)
library(cowplot)
library(GGally)
library(purrr)

dir.create("figures")

events <- data.table::as.data.table(readr::read_rds("data/satisfaction_click_events.rds"))
events <- events[events$date > "2016-04-01" & events$date <= "2016-04-24",,]
# Let's make sure we're only using sessions that had at least one SERP
#   (sessions with clicks but not SERPs are invalid)
valid_sessions <- unique(events$session_id[events$action == "searchResultPage"])
events <- events[events$session_id %in% valid_sessions,,]
# Weird: SERPs appear multiple times! (with same num of results returned)
# Need to do some more cleaning
dupes <- duplicated(events, by = c("session_id", "action", "page_id", "result_position", "results_returned"))
events <- events[!dupes,,]
# Let's convert to a more usable format:
events %<>% as.data.frame %>% dplyr::tbl_df()
rm(dupes, valid_sessions)

nonzero_session_ids <- events %>%
  group_by(session_id) %>%
  summarize(any_results = any(results_returned > 0)) %>%
  keep_where(any_results) %>%
  { .$session_id }

# Use the page ids that SERPs and click events share

ctr_overall <- events %>%
  keep_where(session_id %in% nonzero_session_ids) %>%
  group_by(session_id) %>%
  summarize(visited_page = any(action %in% "visitPage"),
            clicked = any(action %in% "click" & !is.na(result_position))) %>%
  summarize(`clickthrough rate (via visitPage, session-wise)` = mean(visited_page),
            `clickthrough rate (via click, session-wise)` = mean(clicked)) %>%
  tidyr::gather(type, ctr) %>%
  mutate(platform = "Desktop")
ctr_overall_extra <- events %>%
  keep_where(session_id %in% nonzero_session_ids) %>%
  group_by(session_id, page_id) %>%
  summarize(clickthrough = all(c("searchResultPage", "click") %in% action)) %>%
  ungroup %>%
  summarize(`clickthrough rate (via click, SERP-wise)` = mean(clickthrough)) %>%
  tidyr::gather(type, ctr) %>%
  mutate(platform = "Desktop")
ctr_daily<- events %>%
  keep_where(session_id %in% nonzero_session_ids) %>%
  group_by(date, session_id) %>%
  summarize(visited_page = any(action %in% "visitPage"),
            clicked = any(action %in% "click" & !is.na(result_position))) %>%
  group_by(date) %>%
  summarize(`clickthrough rate (via visitPage, session-wise)` = mean(visited_page),
            `clickthrough rate (via click, session-wise)` = mean(clicked)) %>%
  tidyr::gather(type, ctr, -date) %>%
  mutate(platform = "Desktop")
ctr_daily_extra <- events %>%
  keep_where(session_id %in% nonzero_session_ids) %>%
  group_by(date, session_id, page_id) %>%
  summarize(clickthrough = all(c("searchResultPage", "click") %in% action)) %>%
  group_by(date) %>%
  summarize(`clickthrough rate (via click, SERP-wise)` = mean(clickthrough)) %>%
  tidyr::gather(type, ctr, -date) %>%
  mutate(platform = "Desktop")

dash_data <- readr::read_rds("data/dash_data.rds")
dash_overall <- dash_data %>%
  tidyr::spread(action, events) %>%
  group_by(platform) %>%
  summarize(clickthroughs = sum(clickthroughs),
            SERPs = sum(`Result pages opened`)) %>%
  mutate(`clickthrough rate (via Schema:Search)` = clickthroughs/SERPs) %>%
  select(c(platform, `clickthrough rate (via Schema:Search)`)) %>%
  tidyr::gather(type, ctr, -platform)
dash_daily <- dash_data %>%
  tidyr::spread(action, events) %>%
  mutate(`clickthrough rate (via Schema:Search)` = clickthroughs/`Result pages opened`) %>%
  select(c(date, platform, `clickthrough rate (via Schema:Search)`)) %>%
  tidyr::gather(type, ctr, -c(date, platform))

p <- ggplot(data = dplyr::bind_rows(ctr_daily, ctr_daily_extra,
                                    keep_where(dash_daily, platform != "Desktop")),
            aes(x = date, y = ctr, color = paste(type, "on", platform))) +
  geom_hline(data = dplyr::bind_rows(ctr_overall, ctr_overall_extra,
                                     keep_where(dash_overall, platform != "Desktop")),
             aes(yintercept = ctr, color = paste(type, "on", platform)), linetype = "dashed") +
  geom_line(size = 1.5) + geom_point(size = 2) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%a (%d %b)") +
  scale_y_continuous("Clickthrough rate", labels = scales::percent_format()) +
  labs(title = "Proportions of sessions or searches where user clicked on a result") +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 12) +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black", size = 0.1)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(7, "Set1")[-6],
                     guide = guide_legend(title = "Clickthrough & Platform", nrow = 3))
print(p)
ggsave("daily_ctr.png", p, path = "figures", width = 10, height = 6, dpi = 150)

png("figures/ctr_matrix.png", width = 18, height = 10, res = 150, units = "in", pointsize = 14)
ctr_daily %>%
  dplyr::bind_rows(ctr_daily_extra, keep_where(dash_daily, platform != "Desktop")) %>%
  mutate(ctr = 100*ctr, type = paste(type, "on", platform)) %>%
  select(-platform) %>%
  tidyr::spread(type, ctr) %>%
  select(-date) %>%
  pairs(pch = 16)
dev.off()

# Let's see how visitPage and click events are different:
click_visits <- readr::read_rds("data/click_visits.rds")
click_visits %>%
  summarize(`sessions with valid clicks only` = sum(valid_clicks > 0)/n(),
            `sessions with valid visits only` = sum(valid_visits > 0)/n(),
            `sessions with valid clicks AND visits` = sum(valid_clicks > 0 & valid_visits > 0)/n(),
            `sessions with more valid clicks than valid visits` = sum(valid_clicks > valid_visits)/n(),
            `sessions with more valid visits than valid clicks` = sum(valid_visits > valid_clicks)/n(),
            `sessions with valid clicks AND visits, AND clicks match visits 100%` = sum(valid_clicks > 0 & valid_visits > 0 & `clicks not accounted for` == 0 & `visits not accounted for` == 0)/n(),
            `sessions with valid clicks AND visits, AND clicks don't match visits at all` = sum(valid_clicks > 0 & valid_visits > 0 & matches == 0)/n(),
            `sessions with clicks but not valid clicks` = sum(clicks > 0 & valid_clicks == 0)/n(),
            `sessions with visits but not valid visits` = sum(visits > 0 & valid_visits == 0)/n(),
            `sessions with valid clicks that couldn't be matched with valid visits` = sum(valid_clicks > 0 & valid_visits > 0 & `clicks not accounted for` > 0)/n(),
            `sessions with valid visits that couldn't be matched with valid clicks` = sum(valid_clicks > 0 & valid_visits > 0 & `visits not accounted for` > 0)/n()) %>%
  tidyr::gather(" ", `proportion of sessions`) %>%
  mutate(`proportion of sessions` = sprintf("%.3f%%", 100*`proportion of sessions`)) %>%
  knitr::kable(align = c("l", "r"), format = "latex", caption = "...")
