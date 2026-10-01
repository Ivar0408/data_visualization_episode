library(tidyverse)
penguins <- read.csv('data/penguins.csv')
setwd("~/personal/MDA/data-vis-tutorial/data_visualization_episode")
head(penguins)
str(penguins)
penguins %>% filter(year == 2007)
penguins_summary <- penguins %>% 
  group_by(species) %>% 
  summarize(mean = mean(body_mass_g, na.rm = TRUE),
            se = sd(body_mass_g, na.rm = TRUE) / sqrt(n()))
head(penguins_summary)
penguins_summary %>% 
  ggplot(aes(x = species, y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2)
penguins %>% 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_boxplot()
library(ggbeeswarm)
p <- penguins %>% 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_violin(draw_quantiles = 0.5, quantile.linetype=1) +
  geom_beeswarm(side = -1, alpha = 0.5) +
  scale_y_continuous(limits = c(0, NA)) +
  stat_summary(fun = mean, geom = "point", size = 3, colour = "steelblue", alpha = 1)
p
p2 <- p + 
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(linewidth = 0.3, colour = "grey70")) +
  labs(
    x = "Penguin species",
    y = "Body mass (grams)")

p2
ggsave('results/body_mass_plot.png', p2, width = 5, height = 4, units = "in", create.dir = TRUE)
ggsave('results/body_mass_plot.pdf', p2, width = 5, height = 4, units = "in", create.dir = TRUE)
list.files("results")
p <- penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, colour = sex)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(island ~ species)

p
p2 <- p +
  scale_colour_manual(values = c(
    "female" = "#E69F00",
    "male" = "#56B4E9"
  )) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom") +
  labs(
    x = "Bill length (mm)",
    y = "Bill depth (mm)"
  )

p2
ggsave('results/small_multiples.png', p2, width = 13, height = 14, units = "cm")
