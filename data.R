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
dash_data <- dash_data[dash_data$date > "2016-04-01" & dash_data$date <= "2016-04-24", ]
readr::write_rds(dash_data, "data/dash_data.rds")

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
