library(magrittr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange, select)
library(ggplot2)
library(cowplot)
library(GGally)
library(purrr)

colorblind_friendly_colors <- c("#D55E00", "#CC79A7", "#E69F00", "#56B4E9", "#2B9F78", "#0072B2", "#F0E442")
# ^ from the set of colors unambiguous to color vision deficients
# source: http://www.archimedes-lab.org/colorblindnesstest.html

dir.create("figures")

dash_data <- readr::read_rds("data/dash_data.rds")
dash_overall <- dash_data %>%
  tidyr::spread(action, events) %>%
  group_by(platform) %>%
  summarize(clickthroughs = sum(clickthroughs),
            SERPs = sum(`Result pages opened`)) %>%
  mutate(`clickthrough rate` = clickthroughs/SERPs) %>%
  select(c(platform, `clickthrough rate`)) %>%
  tidyr::gather(type, ctr, -platform) %>%
  mutate(type = ifelse(platform == "Desktop", paste(type, "(via Schema:Search)"), type)) %>%
  mutate(type = ifelse(platform == "Mobile Web", paste(type, "(via Schema:MobileWebSearch)"), type)) %>%
  mutate(type = ifelse(platform %in% c("Android", "iOS"), paste(type, "(via Schema:MobileWikiAppSearch)"), type))
dash_daily <- dash_data %>%
  tidyr::spread(action, events) %>%
  mutate(`clickthrough rate` = clickthroughs/`Result pages opened`) %>%
  select(c(date, platform, `clickthrough rate`)) %>%
  tidyr::gather(type, ctr, -c(date, platform)) %>%
  mutate(type = ifelse(platform == "Desktop", paste(type, "(via Schema:Search)"), type)) %>%
  mutate(type = ifelse(platform == "Mobile Web", paste(type, "(via Schema:MobileWebSearch)"), type)) %>%
  mutate(type = ifelse(platform %in% c("Android", "iOS"), paste(type, "(via Schema:MobileWikiAppSearch)"), type))

ctr_daily <- readr::read_rds("data/ctr_daily.rds")

p <- ggplot(data = dplyr::bind_rows(ctr_daily, keep_where(dash_daily, platform != "Desktop")),
            aes(x = date, y = ctr, color = paste(type, "on", platform))) +
  # geom_hline(data = dplyr::bind_rows(ctr_overall, ctr_overall_extra,
  #                                    keep_where(dash_overall, platform != "Desktop")),
             # aes(yintercept = ctr, color = paste(type, "on", platform)), linetype = "dashed") +
  geom_line(size = 1.5) + geom_point(size = 2) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%a (%d %b)") +
  scale_y_continuous("Clickthrough rate", labels = scales::percent_format()) +
  labs(title = "Proportions of sessions or searches where user clicked on a result") +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 12) +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black", size = 0.1)) +
  scale_color_manual(values = colorblind_friendly_colors,
                     # values = RColorBrewer::brewer.pal(7, "Set1")[-6],
                     guide = guide_legend(title = "Method & Platform", nrow = 3))
print(p)
ggsave("daily_ctr.png", p, path = "figures", width = 10, height = 6, dpi = 150)
# ggsave("daily_ctr_mini.png", p, path = "figures", width = 10, height = 6, dpi = 100)

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
            `sessions with more valid clicks than valid visits` = sum(valid_clicks > 0 & valid_visits > 0 & valid_clicks > valid_visits)/n(),
            `sessions with more valid visits than valid clicks` = sum(valid_clicks > 0 & valid_visits > 0 & valid_visits > valid_clicks)/n(),
            `sessions with more valid clicks than valid visits (2)` = sum(valid_clicks > 0 & valid_visits > 0 & valid_clicks > valid_visits)/sum(valid_clicks > 0 & valid_visits > 0),
            `sessions with more valid visits than valid clicks (2)` = sum(valid_clicks > 0 & valid_visits > 0 & valid_visits > valid_clicks)/sum(valid_clicks > 0 & valid_visits > 0),
            `sessions with valid clicks AND visits, AND clicks match visits 100%` = sum(valid_clicks > 0 & valid_visits > 0 & `clicks not accounted for` == 0 & `visits not accounted for` == 0)/n(),
            `sessions with valid clicks AND visits, AND clicks match visits 100% (2)` = sum(valid_clicks > 0 & valid_visits > 0 & `clicks not accounted for` == 0 & `visits not accounted for` == 0)/sum(valid_clicks > 0 & valid_visits > 0),
            `sessions with valid clicks AND visits, AND clicks don't match visits at all` = sum(valid_clicks > 0 & valid_visits > 0 & matches == 0)/n(),
            `sessions with valid clicks AND visits, AND clicks don't match visits at all (2)` = sum(valid_clicks > 0 & valid_visits > 0 & matches == 0)/sum(valid_clicks > 0 & valid_visits > 0),
            `sessions with clicks but not valid clicks` = sum(clicks > 0 & valid_clicks == 0)/n(),
            `sessions with visits but not valid visits` = sum(visits > 0 & valid_visits == 0)/n(),
            `sessions with valid clicks that couldn't be matched with valid visits` = sum(valid_clicks > 0 & valid_visits > 0 & `clicks not accounted for` > 0)/n(),
            `sessions with valid visits that couldn't be matched with valid clicks` = sum(valid_clicks > 0 & valid_visits > 0 & `visits not accounted for` > 0)/n(),
            `sessions with valid clicks that couldn't be matched with valid visits (2)` = sum(valid_clicks > 0 & valid_visits > 0 & `clicks not accounted for` > 0)/sum(valid_clicks > 0 & valid_visits > 0),
            `sessions with valid visits that couldn't be matched with valid clicks (2)` = sum(valid_clicks > 0 & valid_visits > 0 & `visits not accounted for` > 0)/sum(valid_clicks > 0 & valid_visits > 0)) %>%
  tidyr::gather(" ", `proportion of sessions`) %>%
  mutate(`proportion of sessions` = sprintf("%.3f%%", 100*`proportion of sessions`)) %>%
  # knitr::kable(align = c("l", "r"), format = "markdown") %>%
  knitr::kable(align = c("l", "r"), format = "latex", caption = "...")

p <- click_visits %>%
  keep_where((valid_clicks == 0 & valid_visits == 0) | (`clicks not accounted for` == 0 & `visits not accounted for` == 0)) %>%
  group_by(date) %>%
  summarize(`total sessions` = n(),
            `abandoned sessions` = sum(valid_clicks == 0),
            `clickthrough'd sessions` = sum(matches > 0),
            `clickthrough rate (via click-visit matching)` = mean(matches > 0)) %>%
  select(c(date, `clickthrough rate (via click-visit matching)`)) %>%
  tidyr::gather(type, ctr, -date) %>%
  mutate(platform = "Desktop") %>%
  dplyr::bind_rows(ctr_daily) %>%
  ggplot(aes(x = date, y = ctr, color = paste(type, "on", platform))) +
  geom_line(size = 1.5) + geom_point(size = 2) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%a (%d %b)") +
  scale_y_continuous("Clickthrough rate", labels = scales::percent_format()) +
  labs(title = "Proportions of sessions or searches where user clicked on a result") +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 12) +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black", size = 0.1)) +
  scale_color_manual(values = c("#000000", colorblind_friendly_colors[c(1, 2, 6)]),
                     guide = guide_legend(title = "Method & Platform", nrow = 2))
print(p)
ggsave("daily_ctr_2.png", p, path = "figures", width = 10, height = 6, dpi = 150)
ggsave("daily_ctr_2_mini.png", p, path = "figures", width = 10, height = 6, dpi = 100)

ctrs <- click_visits %>%
  keep_where((valid_clicks == 0 & valid_visits == 0) | (`clicks not accounted for` == 0 & `visits not accounted for` == 0)) %>%
  group_by(date) %>%
  summarize(`total sessions` = n(),
            `abandoned sessions` = sum(valid_clicks == 0),
            `clickthrough'd sessions` = sum(matches > 0),
            `clickthrough rate (via click-visit matching)` = mean(matches > 0)) %>%
  select(c(date, `clickthrough rate (via click-visit matching)`)) %>%
  tidyr::gather(type, ctr, -date) %>%
  mutate(platform = "Desktop") %>%
  dplyr::bind_rows(ctr_daily) %>%
  mutate(ctr = 100*ctr, type = paste(type, "on", platform)) %>%
  select(-platform) %>%
  tidyr::spread(type, ctr) %>%
  select(-date)

click_visits %>%
  keep_where((valid_clicks == 0 & valid_visits == 0) | (`clicks not accounted for` == 0 & `visits not accounted for` == 0)) %>%
  summarize(`total sessions` = n(),
            `abandoned sessions` = sum(valid_clicks == 0),
            `clickthrough'd sessions` = sum(matches > 0),
            `clickthrough rate (via click-visit matching)` = mean(matches > 0)) %>%
  select(`clickthrough rate (via click-visit matching)`) %>%
  tidyr::gather(type, ctr) %>%
  mutate(platform = "Desktop") %>%
  dplyr::bind_rows(ctr_overall, ctr_overall_extra) %>%
  select(-platform) %>%
  mutate(ctr = 100*ctr) %>%
  knitr::kable(format = "latex", caption = "...")

foo <- function(x, y, ...) {
  fit <- lm(y ~ x)
  xx <- seq(min(x), max(x), length.out = 1000)
  predicted <- predict(fit, newdata = data.frame(x = xx), interval = "confidence", level = 0.95)
  # abline(a = 0, b = 1, lty = "dashed", ...)
  polygon(c(xx, rev(xx)), c(predicted[, 'upr'], rev(predicted[, 'lwr'])), col = "gray80", border = NA)
  abline(fit, lwd = 1)
  points(x, y, ...)
}
png("figures/ctr_matrix_2.png", width = 14, height = 8, res = 300, units = "in", pointsize = 16)
pairs(ctrs, pch = 16, upper.panel = foo)
dev.off()

# A stats test of Granger causality (whether x can be used to forecast y)
plot(ctrs$`clickthrough rate (via click, session-wise) on Desktop`, type = "l", lwd = 2)
lines(ctrs$`clickthrough rate (via click-visit matching) on Desktop`, col = "red", lwd = 2)
lmtest::grangertest(ctrs$`clickthrough rate (via click, session-wise) on Desktop`[-nrow(ctrs)],
                    ctrs$`clickthrough rate (via click-visit matching) on Desktop`[-1], order = 1)
# p-val 0.2488
lmtest::grangertest(ctrs$`clickthrough rate (via click, session-wise) on Desktop`,
                    ctrs$`clickthrough rate (via click-visit matching) on Desktop`, order = 1)
# p-val 0.4876

library(forecast)
fit <- auto.arima(x = ctrs$`clickthrough rate (via click-visit matching) on Desktop`,
                  xreg = as.matrix(ctrs[, c(2, 3)]), seasonal = TRUE, stationary = TRUE,
                  stepwise = FALSE, start.P = 1)
plot(ctrs$`clickthrough rate (via click-visit matching) on Desktop`, type = "l", lwd = 2)
lines(as.numeric(fitted(fit)), col = "red")

# > broom::tidy(fit)
# Model 1:
#                                                     term   estimate std.error
# 1                                              intercept 12.1001089 6.3221226
# 2    clickthrough rate (via click, SERP-wise) on Desktop -0.7665743 0.1909358
# 3 clickthrough rate (via click, session-wise) on Desktop  1.3211061 0.2353843
# Model 2:
#                                                     term   estimate std.error
# 1    clickthrough rate (via click, SERP-wise) on Desktop -0.7735988 0.1529176
# 2 clickthrough rate (via click, session-wise) on Desktop  1.5576939 0.1125968

# Version 1
ctrs$`clickthrough rate (via model 1) on Desktop` <- 12.1001 +
  -0.7666 * ctrs$`clickthrough rate (via click, SERP-wise) on Desktop` +
  1.3211 * ctrs$`clickthrough rate (via click, session-wise) on Desktop`
# Version 2
ctrs$`clickthrough rate (via model 2) on Desktop` <- -0.7735988 * ctrs$`clickthrough rate (via click, SERP-wise) on Desktop` +
  1.5576939 * ctrs$`clickthrough rate (via click, session-wise) on Desktop`
# Version 3
ctrs$`clickthrough rate (via avg model) on Desktop` <- (ctrs$`clickthrough rate (via model 1) on Desktop` + ctrs$`clickthrough rate (via model 2) on Desktop`)/2
# Plot it!
plot(ctrs$`clickthrough rate (via click-visit matching) on Desktop`, type = "l", lwd = 2)
lines(ctrs$`clickthrough rate (via model 1) on Desktop`, col = "red", lwd = 2)
lines(ctrs$`clickthrough rate (via model 2) on Desktop`, col = "blue", lwd = 2)
lines(ctrs$`clickthrough rate (via avg model) on Desktop`, col = "green", lwd = 2)

mae <- function(actual, fitted) { # mean absolute error
  return(sum(abs(actual-fitted))/length(actual))
}
mae(ctrs$`clickthrough rate (via click-visit matching) on Desktop`, ctrs$`clickthrough rate (via model 1) on Desktop`)
# = 0.69578
mae(ctrs$`clickthrough rate (via click-visit matching) on Desktop`, ctrs$`clickthrough rate (via model 2) on Desktop`)
# = 0.6916616
mae(ctrs$`clickthrough rate (via click-visit matching) on Desktop`, ctrs$`clickthrough rate (via avg model) on Desktop`)
# = 0.6799207

ctrs_new <- click_visits_new %>%
  keep_where((valid_clicks == 0 & valid_visits == 0) | (`clicks not accounted for` == 0 & `visits not accounted for` == 0)) %>%
  group_by(date) %>%
  summarize(`total sessions` = n(),
            `abandoned sessions` = sum(valid_clicks == 0),
            `clickthrough'd sessions` = sum(matches > 0),
            `clickthrough rate (via click-visit matching)` = mean(matches > 0)) %>%
  select(c(date, `clickthrough rate (via click-visit matching)`)) %>%
  tidyr::gather(type, ctr, -date) %>%
  mutate(platform = "Desktop") %>%
  dplyr::bind_rows(ctr_daily_new, ctr_daily_extra_new) %>%
  mutate(ctr = 100*ctr, type = paste(type, "on", platform)) %>%
  select(-platform) %>%
  tidyr::spread(type, ctr) %>%
  select(-date)

# Version 1
ctrs_new$`clickthrough rate (via model 1) on Desktop` <- 12.1001 +
  -0.7666 * ctrs_new$`clickthrough rate (via click, SERP-wise) on Desktop` +
  1.3211 * ctrs_new$`clickthrough rate (via click, session-wise) on Desktop`
# Version 2
ctrs_new$`clickthrough rate (via model 2) on Desktop` <- -0.7735988 * ctrs_new$`clickthrough rate (via click, SERP-wise) on Desktop` +
  1.5576939 * ctrs_new$`clickthrough rate (via click, session-wise) on Desktop`
# Version 3
ctrs_new$`clickthrough rate (via avg model) on Desktop` <- (ctrs_new$`clickthrough rate (via model 1) on Desktop` + ctrs_new$`clickthrough rate (via model 2) on Desktop`)/2
# Visualize!
plot(ctrs_new$`clickthrough rate (via click-visit matching) on Desktop`, type = "l", lwd = 1, col = "gray30", xlim = c(1, nrow(ctrs_new)))
abline(v = 1:nrow(ctrs_new), col = "gray90")
lines(head(ctrs_new$`clickthrough rate (via click-visit matching) on Desktop`, nrow(ctrs_new) - 3), col = "black", lwd = 2, lty = "solid")
lines(head(ctrs_new$`clickthrough rate (via model 1) on Desktop`, nrow(ctrs_new) - 3), col = "red", lwd = 2, lty = "solid")
lines(head(ctrs_new$`clickthrough rate (via model 2) on Desktop`, nrow(ctrs_new) - 3), col = "blue", lwd = 2, lty = "solid")
lines(head(ctrs_new$`clickthrough rate (via avg model) on Desktop`, nrow(ctrs_new) - 3), col = "green", lwd = 2, lty = "solid")
lines(nrow(ctrs_new) - c(2:0), tail(ctrs_new$`clickthrough rate (via click-visit matching) on Desktop`, 3), type = "l", lwd = 2, lty = "dashed")
lines(nrow(ctrs_new) - c(2:0), tail(ctrs_new$`clickthrough rate (via model 1) on Desktop`, 3), col = "red", lwd = 2, lty = "dashed")
lines(nrow(ctrs_new) - c(2:0), tail(ctrs_new$`clickthrough rate (via model 2) on Desktop`, 3), col = "blue", lwd = 2, lty = "dashed")
lines(nrow(ctrs_new) - c(2:0), tail(ctrs_new$`clickthrough rate (via avg model) on Desktop`, 3), col = "green", lwd = 2, lty = "dashed")

mae(tail(ctrs_new$`clickthrough rate (via click-visit matching) on Desktop`, 3),
    tail(ctrs_new$`clickthrough rate (via model 1) on Desktop`, 3))
# = 0.754515
mae(tail(ctrs_new$`clickthrough rate (via click-visit matching) on Desktop`, 3),
    tail(ctrs_new$`clickthrough rate (via model 2) on Desktop`, 3))
# = 0.5991338
mae(tail(ctrs_new$`clickthrough rate (via click-visit matching) on Desktop`, 3),
    tail(ctrs_new$`clickthrough rate (via avg model) on Desktop`, 3))
# = 0.6640269

# Conclusion: we can use a very well trained model to estimate "clickthrough rate via click-visit matching"
#             in the absense of visitPage events, using only click events as a proxy, or more specifically:
#             the session-wise and SERP-wise clickthrough rates. To compare test vs controls, we compare
#             the estimated clickthrough, preferably in a probabilistic, Bayesian framework because we're
#             comparing indirectly observed (well, unobserved) random variable quantities.
