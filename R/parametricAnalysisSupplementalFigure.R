library(cowplot)
library(egg)
library(ggnewscale)
library(ggplot2)
library(ggrastr)
library(grid)
library(gtable)
library(scales)
library(zoo)
theme_set(
  theme_bw() +
    theme(
      axis.text = element_text(size = unit(8, "pt")),
      axis.title = element_text(size = unit(8, "pt")),
      panel.grid.minor = element_blank(),
    )
)
dpi <- 600
mv_groups <- bind_rows(
  list(male = malecohort, female = femalecohort),
  .id = "sex"
) %>%
  tibble(
    decade = cut(birth, as.Date(c("1929-12-31", "1939-12-31", "1949-12-31", "1959-12-31", "1969-12-31", "1979-12-31"))),
    age_tens_place = factor(floor(age / 10)),
    job_distance_group = cut(job_distance, c(-1, 5, 10 * (1:10), 200)),
  ) %>%
  mutate(Y = pmin(Y, 20)) %>%
  group_by(
    sex, decade, age_tens_place, job_distance_group, centre
  )
yearly <- mv_groups %>%
  mutate(age = floor(age)) %>%
  group_by(sex, age) %>%
  summarise(age_mean = mean(age), across(Y, list(count=length, mean=mean, var=var, sparsity=\(v) mean(v == 0))), .groups = "drop") %>%
  group_by(sex) %>%
  mutate(
    across(
      c(Y_mean, Y_var, Y_sparsity),
      \(v) rollapply(v, 3, mean, partial = TRUE)
    )
  ) %>%
  ungroup() %>%
  subset(Y_count >= 1000)
groups <- cohort %>%
  tibble(
    decade = cut(birth, as.Date(c("1929-12-31", "1939-12-31", "1949-12-31", "1959-12-31", "1969-12-31", "1979-12-31"))),
    job_distance_group = cut(job_distance, c(-1, 5, 10 * (1:10), 200)),
  ) %>%
  mutate(Y = pmin(Y, 12)) %>%
  group_by(
    decade, job_distance_group, centre
  )
mv <- mv_groups %>% summarise(age_mean = mean(age), across(Y, list(count=length, mean=mean, var=var, sparsity=\(v) mean(v == 0))), .groups = "drop")
mv <- mv %>% subset(Y_count >= 3)
ggplot(subset(mv, sex == "female"), aes(Y_mean, Y_var)) + geom_point(stroke = NA, size = 1.5) + annotate("segment", 0, 0, xend=4, yend=4, color="darkred") + geom_smooth(data = \(data) data %>% subset(Y_count >= 5 & Y_var <= 6)) + geom_path(data=subset(yearly, sex == "female"), color="forestgreen", linewidth = 4*25.4/72) + coord_cartesian(c(0, 4), c(0, 10), expand=FALSE)
p <- set_panel_size(
  ggplot(subset(mv, sex == "female"), aes(ppois(0, Y_mean), Y_sparsity)) + annotate("segment", 0, 0, xend = 1, yend = 1, color = "darkred") + rasterise(geom_point(stroke = NA, size = 1.5, alpha = 0.1), dpi = dpi) + coord_cartesian(expand = FALSE) + labs(title = "B"),
  w = unit(2, "in"),
  h = unit(1.5, "in")
)
p$layout$l[match("title", p$layout$name)] <- 1
grid.newpage()
grid.draw(p)

leftitle <- function(p) {
  p$layout$l[match("title", p$layout$name)] <- 1
  p
}

male_color <- "#77BFFF"
female_color <- "#F19AF1"
report <- rbind(
  cbind(
    set_panel_size(
      ggplot(subset(mv, sex == "female"), aes(Y_mean, Y_var)) +
        rasterise(geom_point(stroke = NA, size = 1, alpha = 0.2, color = female_color), dpi=dpi) +
        annotate("segment", 0, 0, xend=4, yend=4, color="darkred") +
        geom_smooth(data = \(data) data %>% subset(Y_count >= 5 & Y_var <= 6), method = "loess", color = muted(female_color, l = 40, c = 60)) +
        geom_path(data=subset(yearly, sex == "female"), color="forestgreen", linewidth = 4*25.4/72) +
        coord_cartesian(c(0, 4), c(0, 10), expand=FALSE, clip="off") +
        labs(title = "A", x = "Mean (Narrow Cohort)", y = "Variance (Narrow Cohort)"),
      w = unit(2, "in"),
      h = unit(1.5, "in")
    ) %>%
      leftitle(),
    set_panel_size(
      ggplot(subset(mv, sex == "female"), aes(ppois(0, Y_mean), Y_sparsity)) +
        annotate("segment", 0, 0, xend = 1, yend = 1, color = "darkred") +
        rasterise(geom_point(stroke = NA, size = 1, alpha = 0.2, color = muted(female_color, l = 40, c = 60)), dpi = dpi) +
        scale_x_continuous(labels = percent) +
        scale_y_continuous(labels = percent) +
        coord_cartesian(expand = FALSE, clip="off") +
        labs(title = "B", x = "P(0 | Poisson)", y = "P(0) Empirical"),
      w = unit(2, "in"),
      h = unit(1.5, "in")
    ) %>%
      leftitle()
  ),
  cbind(
    set_panel_size(
      ggplot(subset(mv, sex == "male"), aes(Y_mean, Y_var)) +
        rasterise(geom_point(stroke = NA, size = 1, alpha = 0.2, color = male_color), dpi=dpi) +
        annotate("segment", 0, 0, xend=4, yend=4, color="darkred") +
        geom_smooth(data = \(data) data %>% subset(Y_count >= 5 & Y_var <= 6), method = "loess", color = muted(male_color, l = 40, c = 60)) +
        geom_path(data=subset(yearly, sex == "male"), color="forestgreen", linewidth = 4*25.4/72) +
        coord_cartesian(c(0, 4), c(0, 10), expand=FALSE, clip="off") +
        labs(title = "C", x = "Mean (Narrow Cohort)", y = "Variance (Narrow Cohort)"),
      w = unit(2, "in"),
      h = unit(1.5, "in")
    ) %>%
      leftitle(),
    set_panel_size(
      ggplot(subset(mv, sex == "male"), aes(ppois(0, Y_mean), Y_sparsity)) +
        annotate("segment", 0, 0, xend = 1, yend = 1, color = "darkred") +
        rasterise(geom_point(stroke = NA, size = 1, alpha = 0.2, color = muted(male_color, l = 40, c = 60)), dpi = dpi) +
        scale_x_continuous(labels = percent) +
        scale_y_continuous(labels = percent) +
        coord_cartesian(expand = FALSE, clip="off") +
        labs(title = "D", x = "P(0 | Poisson)", y = "P(0) Empirical"),
      w = unit(2, "in"),
      h = unit(1.5, "in")
    ) %>%
      leftitle()
  )
)
grid.draw(
  lgd <- rbind(
    get_legend(
      ggplot() +
        geom_point(aes(x, y, color=Sex), tibble(x = 0, y = 0, Sex = c("Female", "Male") %>% factor(., .))) +
        scale_color_manual(values = c(female_color, male_color))
    ),
    get_legend(
      ggplot() +
        geom_path(aes(x, y, color=`Trend Line`), tibble(x = 0:1, y = 0, `Trend Line` = factor("Age Cohort-Wide"))) +
        scale_color_manual(values = "forestgreen")
    )
  )
)
report <- gtable(
  unit.c(sum(convertUnit(report$widths, "in")), sum(convertUnit(lgd$widths, "in"))),
  unit(1, "null")
) %>%
  gtable_add_grob(
    list(report, lgd),
    t = 1,
    l = 1:2
  )
grid.newpage()
grid.draw(report)
ggsave("parametricAnalysis.pdf", report, w = 7.5, h = 5)
