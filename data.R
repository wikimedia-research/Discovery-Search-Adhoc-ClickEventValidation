# SELECT * FROM TestSearchSatisfaction2_15357244 WHERE event_action = 'click' LIMIT 10;

end_date <- Sys.Date()-1
start_date <- end_date - 30
# This test does not effect autocomplete (prefix or comp suggest)
#   and does not have any effect on zero results rate.
cat("Fetching EL data from", as.character(start_date), "to", as.character(end_date), "...\n")
events <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching EL data from", as.character(date), "\n")
  data <- wmf::build_query("SELECT LEFT(timestamp, 8) AS date,
                            timestamp AS ts,
                            userAgent AS user_agent,
                            event_mwSessionId AS session_id,
                            event_pageViewId AS page_id,
                            event_action AS action,
                            event_checkin AS checkin,
                            event_position AS result_position,
                            event_hitsReturned AS results_returned",
                           date = date,
                           table = "TestSearchSatisfaction2_15357244",
                           conditionals = "event_action IN('searchResultPage', 'visitPage', 'click') AND event_subTest IS NULL")
  return(data)
}))
cat("...done! Doing some date/datetime post-processing...")
library(magrittr)
events$date %<>% lubridate::ymd()
events$ts %<>% lubridate::ymd_hms()
events <- events[events$session_id != "", ]
cat("done.\n")

cat("Processing user agents to do spider filtering...")
events$device <- uaparser::parse_agents(events$user_agent, fields = "device")
events <- events[events$device != "Spider", ]
events <- events[, setdiff(names(events), c("user_agent", "device"))]
events$test_group[is.na(events$test_group)] <- "baseline"
cat("done.\n")

cat("Writing data (", nrow(events), " events) to disk...", sep = "")
readr::write_rds(events, "~/satisfaction_click_events.rds", "gz")
cat("done.\n")

q(ask = "no")

## Locally:
dir.create("data")
system("scp stat2:/home/bearloga/satisfaction_click_events.rds data/")

## Dashboard Data
# dash_data_desktop <- polloi::read_dataset("search/desktop_event_counts.tsv",
#                                           col_types = "Tci", skip = 1,
#                                           col_names = c("date", "action", "events"))
# dash_data_desktop$platform <- "Desktop"
dash_data_apps <- polloi::read_dataset("search/app_event_counts",
                                       col_types = "Tcci", skip = 1,
                                       col_names = c("date", "action", "platform", "events"))
dash_data_mobile <- polloi::read_dataset("search/mobile_event_counts",
                                         col_types = "Tci", skip = 1,
                                         col_names = c("date", "action", "events"))
dash_data_mobile$platform <- "Mobile Web"
dash_data <- dplyr::bind_rows(dash_data_apps, dash_data_mobile)
dash_data %<>% keep_where(action %in% c("Result pages opened", "clickthroughs"))
# dash_data <- dash_data[dash_data$date > "2016-04-01" & dash_data$date <= "2016-04-24", ]
dash_data <- dash_data[dash_data$date > "2016-03-25" & dash_data$date <= "2016-04-24", ]
readr::write_rds(dash_data, "data/dash_data.rds")

events <- data.table::as.data.table(readr::read_rds("data/satisfaction_click_events.rds"))
# events <- events[events$date > "2016-04-01" & events$date <= "2016-04-24",,]
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

ctr_overall <- events %>%
  keep_where(session_id %in% nonzero_session_ids) %>%
  group_by(session_id) %>%
  summarize(visited_page = any(action %in% "visitPage"),
            clicked = any(action %in% "click" & !is.na(result_position))) %>%
  summarize(`clickthrough rate (via visitPage, session-wise)` = mean(visited_page),
            `clickthrough rate (via click, session-wise)` = mean(clicked)) %>%
  tidyr::gather(type, ctr) %>%
  mutate(platform = "Desktop")
# Use the page ids that SERPs and click events share
ctr_overall_extra <- events %>%
  keep_where(session_id %in% nonzero_session_ids) %>%
  group_by(session_id, page_id) %>%
  summarize(clickthrough = all(c("searchResultPage", "click") %in% action)) %>%
  ungroup %>%
  summarize(`clickthrough rate (via click, SERP-wise)` = mean(clickthrough)) %>%
  tidyr::gather(type, ctr) %>%
  mutate(platform = "Desktop")
readr::write_rds(dplyr::bind_rows(ctr_overall, ctr_overall_extra), "data/ctr_overall.rds")
ctr_daily <- events %>%
  keep_where(session_id %in% nonzero_session_ids) %>%
  group_by(date, session_id) %>%
  summarize(visited_page = any(action %in% "visitPage"),
            clicked = any(action %in% "click" & !is.na(result_position))) %>%
  group_by(date) %>%
  summarize(`clickthrough rate (via visitPage, session-wise)` = mean(visited_page),
            `clickthrough rate (via click, session-wise)` = mean(clicked)) %>%
  tidyr::gather(type, ctr, -date) %>%
  mutate(platform = "Desktop")
# Use the page ids that SERPs and click events share
ctr_daily_extra <- events %>%
  keep_where(session_id %in% nonzero_session_ids) %>%
  group_by(date, session_id, page_id) %>%
  summarize(clickthrough = all(c("searchResultPage", "click") %in% action)) %>%
  group_by(date) %>%
  summarize(`clickthrough rate (via click, SERP-wise)` = mean(clickthrough)) %>%
  tidyr::gather(type, ctr, -date) %>%
  mutate(platform = "Desktop")
readr::write_rds(dplyr::bind_rows(ctr_daily, ctr_daily_extra), "data/ctr_daily.rds")

click_visits <- events %>%
  # head(1000) %>% # for prototyping
  keep_where(session_id %in% nonzero_session_ids, action != "searchResultPage") %>%
  slice_rows(c("date", "session_id")) %>%
  by_slice(function(slice) {
    n_clicks <- sum(slice$action == "click" & !is.na(slice$result_position))
    n_visits <- sum(slice$action == "visitPage" & !is.na(slice$result_position))
    if (n_clicks == 0 || n_visits == 0) {
      return(data.frame(clicks = sum(slice$action == "click"),
                        visits = sum(slice$action == "visitPage"),
                        valid_clicks = n_clicks,
                        valid_visits = n_visits,
                        matches = 0L))
    }
    clicks <- slice[slice$action == "click" & !is.na(slice$result_position), ]
    clicks$accounted_for <- FALSE
    visits <- slice[slice$action == "visitPage" & !is.na(slice$result_position), ]
    visits$accounted_for <- FALSE
    for (i in 1:nrow(clicks)) {
      if (sum(visits$accounted_for) == n_visits) {
        break
      } else {
        for (j in which(!visits$accounted_for)) {
          if (clicks$result_position[i] == visits$result_position[j]) {
            clicks$accounted_for[i] <- TRUE
            visits$accounted_for[j] <- TRUE
          }
        }
      }
    }
    return(data.frame(clicks = sum(slice$action == "click"),
                      visits = sum(slice$action == "visitPage"),
                      valid_clicks = n_clicks,
                      valid_visits = n_visits,
                      matches = sum(clicks$accounted_for)))
  }, .collate = "rows") %>%
  mutate(`clicks not accounted for` = valid_clicks - matches,
         `visits not accounted for` = valid_visits - matches)
readr::write_rds(click_visits, "data/click_visits.rds")
