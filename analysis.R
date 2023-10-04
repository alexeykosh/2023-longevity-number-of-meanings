library(tidyverse)
library(ggpubr)
library(DescTools)
library(scales)
library(ppcor)

theme_set(theme_bw())

### Longevity distribution
age_estimation <- read.csv("data/age_estimations.csv")
nrow(age_estimation)
a <- age_estimation %>%
  ggplot(aes(x = etymology, fill = pos)) +
  geom_histogram(bins = 20) +
  facet_wrap(~factor(pos, levels = c("ADV", "VERB", "ADJ", "NOUN")), 
  nrow = 1, ncol = 4) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none") +
  ylab("Count") +
  geom_vline(xintercept = 1800, color = "red", linetype = "dashed") +
  xlab("")
b <- age_estimation  %>%
  distinct(lemma, .keep_all = TRUE) %>%
  mutate(old_new = if_else(etymology > 1800, "after", "before")) %>%
  group_by(pos, old_new) %>%
  summarize(count = n(), old_new = old_new) %>%
  ggplot(aes(x = reorder(pos, +count), y = count, fill = old_new)) +
  geom_bar(stat = "identity", position = position_dodge(),
           width = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  xlab("") +
  ylab("Count") +
  theme(legend.position = "bottom", 
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, -10, 10, 10)) +
  labs(fill = "Year 1800:")
plt <- ggarrange(a, b,
          labels = c("A", "B"),
          widths = c(7, 5),  nrow = 2)
ggsave("figures/longevity_distribution.pdf", width = 10, height = 4)

### POS counts
initial_counts <- read.csv('data/lemma_freq_.csv') %>%
  filter(pos %in% c("NOUN", "VERB", "ADJ", "ADV")) %>%
  group_by(pos) %>%
  summarise(count = n(), type = 'All data') %>%
  mutate(percentage = count / sum(count) * 100)
post_counts <- age_estimation %>%
  group_by(pos) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100, type = 'Pre-processed data')
rbind(initial_counts, post_counts) %>%
  ggplot(aes(x = reorder(pos, +percentage), y = percentage, 
        fill = type)) +
  geom_bar(stat = "identity", position = position_dodge2()) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  xlab("") +
  ylab("Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'top') +
  scale_fill_brewer(palette = "Set2") +
  labs(fill = "Data type:") +
  ylim(NA, 70)
ggsave("figures/pos_counts.pdf", width = 10, height = 4)
