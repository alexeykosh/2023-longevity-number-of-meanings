library(tidyverse)
library(ggpubr)
library(DescTools)
library(scales)
library(ppcor)
library(brms)
library(tidybayes)
library(modelr)
library(ggpubr)
library(bayesplot)
library(sjPlot)
library(broom)
library(marginaleffects)

theme_set(theme_bw())

set.seed(42)

# function to save models

run_model <- function(expr, path, reuse = TRUE) {
  path <- paste0(path, ".Rds")
  if (reuse) {
    fit <- suppressWarnings(try(readRDS(path), silent = TRUE))
  }
  if (is(fit, "try-error")) {
    fit <- eval(expr)
    saveRDS(fit, file = path)
  }
  fit
}

### Pre-reg plot ### 
# Getting the data
age_estimation <- read.csv("data/age_estimations.csv")
# POS counts
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
# Save the plot
ggsave("figures/pos_counts.pdf", width = 10, height = 4)

# Distribution of age:

ggplot(aes(x = etymology), data = age_estimation) +
  geom_histogram(bins = 50, fill = 'grey') +
  labs(x = 'Estimated date of appearance', y = '') +
  annotate(
    geom = "curve", x = 1650, y = 600, xend = 1560, yend = 760, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 1655, y = 605, label = "16th century as 1550", hjust = "left")
ggsave("figures/age_distibution.pdf", width = 10, height = 6)

age_estimation %>%
  mutate(etymology = etymology %/% 10) %>%
  group_by(etymology, pos) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = etymology, y = count, fill = pos)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_colour_brewer(palette = "Spectral")

age_estimation %>%
  ggplot(aes(x = factor(number_of_meanings))) +
  geom_bar(stat = "count", fill = "#08519C", alpha = 1) 

## Share of words that have one than more meaning at time t 

age_estimation %>%
  mutate(etymology = etymology %/% 20) %>%
  mutate(bin_add_meaning = if_else(number_of_meanings - 1 >= 1, 
                                   'at least 1', 
                                   'zero')) %>%
  group_by(etymology, bin_add_meaning) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = etymology * 20, y = count, fill = bin_add_meaning)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_brewer(palette = "Spectral") +
  labs(x = 'Year of appearance', 
       y = 'Share', 
       fill = ' Number of \n additional \n meanings')
  


  ### Statistical analysis ### 

df <- read.csv('data/age_estimations.csv') %>%
  mutate(z_freq = (log(freq) - mean(log(freq))) / sd(log(freq)),
         z_age = (age - mean(age))/ sd(age),
         meanings = number_of_meanings - 1)


# Spearman correlation coefficient (longevity ~ frequency)

cor.test(df$age, df$freq, method=c("kendall"))

model_full <- brm(meanings ~ 1 + z_freq + z_age + (1 | pos),
                    data = df, 
                    family = zero_inflated_poisson(),
                    prior = c(prior(normal(0, 0.5), class = 'b'), #beta 
                              prior(beta(2, 2), class = 'zi'), #pi or zi
                              prior(normal(0, 1), class = Intercept), #alpha bar
                              prior(exponential(1), class = sd)), # sigma
                    cores = 16,
                    backend = "cmdstanr",
                    adapt_delta = 0.95,
                    iter = 5000,
                    save_pars=save_pars(all=TRUE))
model_full <- add_criterion(model_full, criterion = 'loo')

model_freq <- brm(meanings ~ 1 + z_freq + (1 | pos),
                  data = df, 
                  family = zero_inflated_poisson(),
                  prior = c(prior(normal(0, 0.5), class = 'b'), #beta 
                            prior(beta(2, 2), class = 'zi'), #pi or zi
                            prior(normal(0, 1), class = Intercept), #alpha bar
                            prior(exponential(1), class = sd)), # sigma
                  cores = 16,
                  backend = "cmdstanr",
                  adapt_delta = 0.95,
                  iter = 5000, 
                  save_pars=save_pars(all=TRUE))
model_freq <- add_criterion(model_freq, criterion = 'loo')

model_age <- brm(meanings ~ 1 + z_age + (1 | pos),
                  data = df, 
                  family = zero_inflated_poisson(),
                  prior = c(prior(normal(0, 0.5), class = 'b'), #beta 
                            prior(beta(2, 2), class = 'zi'), #pi or zi
                            prior(normal(0, 1), class = Intercept), #alpha bar
                            prior(exponential(1), class = sd)), # sigma
                  cores = 16,
                  backend = "cmdstanr",
                  adapt_delta = 0.95,
                  iter = 5000)
model_age <- add_criterion(model_age, criterion = 'loo')
summary(model_age)

loo_compare(model_full, model_freq, criterion = 'loo') %>% 
  print(simplify = F)

# brms::bayes_factor(model_full, model_freq)

summary(model_full)

pp_check(model_full, ndraws = 100, type = "bars")

pp_check(model_freq, ndraws = 100, type = "bars")

pp_check(model_age, ndraws = 100, type = "bars")

hypothesis(x = model_full, 'z_age > 0')
hypothesis(x = model_full, 'z_age > z_freq')

# Plotting predictions
mcmc_neff(neff_ratio(model_full))+ 
  theme_bw()

re_model_only <- crossing(z_freq = seq(min(df$z_freq), 
                                        max(df$z_freq), length.out=100),
                          z_age = seq(min(df$z_age), 
                                       max(df$z_age), length.out=100),
                          pos = unique(df$pos)) %>%
  epred_draws(newdata = ., object = model_full,
                   scale = "response", ndraws = 1e3)
ggplot(re_model_only,
       aes(x = z_age, y = .epred)) +
  facet_wrap(~pos) +
  geom_count(data = df, 
             aes(x = z_age, y = meanings),
             alpha = 0.1) +
  stat_lineribbon(.width = c(.95, 0.8, 0.5), 
                  color = "#08519C", 
                  alpha = 0.5) +
  scale_fill_brewer('CI') +
  theme(strip.background=element_rect(colour="black",
                                      fill=NA)) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(0, 2, 5, 8, 10, 12)
  ) +
  scale_x_continuous(
    labels = round(seq(min(df$age), max(df$age), length.out = 5)),
    breaks = seq(min(df$z_age), max(df$z_age), length.out = 5)
  ) +
  xlab('Longevity') +
  ylab('Number of additional meanings')

full <- avg_comparisons(model_full) %>%
  posterior_draws(shape = "rvar") %>%
  ggplot(aes(y = term, xdist = rvar)) + 
  stat_slabinterval() +
  xlim(0, 0.5) +
  ggtitle('Full dataset') +
  ylab('') + 
  xlab('') +
  geom_vline(xintercept = 0, 
             linetype="dotted",
             color = 'red')

only_freq <- avg_comparisons(model_freq) %>%
  posterior_draws(shape = "rvar") %>%
  ggplot(aes(y = term, xdist = rvar)) + 
  stat_slabinterval() +
  xlim(0, 0.5) +
  ggtitle('Frequency') +
  ylab('') + 
  xlab('') +
  geom_vline(xintercept = 0, 
             linetype="dotted",
             color = 'red')

only_age <- avg_comparisons(model_age) %>%
  posterior_draws(shape = "rvar") %>%
  ggplot(aes(y = term, xdist = rvar)) + 
  stat_slabinterval() +
  xlim(0, 0.5) +
  ggtitle('Longevity') +
  ylab('') + 
  xlab('') +
  geom_vline(xintercept = 0, 
             linetype="dotted",
             color = 'red')

ggarrange(full, only_freq, nrow=2, ncol=1)

### Exploratory analysis ### 

## Subset predictions

df_1800 <- read.csv('data/age_estimations_1800.csv') %>%
  mutate(z_freq = (log(freq) - mean(log(freq))) / sd(log(freq)),
         z_age = (age - mean(age))/ sd(age),
         meanings = number_of_meanings - 1)

cor.test(df_1800$age, df_1800$freq, method=c("kendall"))

model_full_1800 <- brm(meanings ~ 1 + z_freq + z_age + (1 | pos),
                       data = df_1800, 
                       family = zero_inflated_poisson(),
                       prior = c(prior(normal(0, 0.5), class = 'b'), #beta 
                                 prior(beta(2, 2), class = 'zi'), #pi or zi
                                 prior(normal(0, 1), class = Intercept), #alpha bar
                                 prior(exponential(1), class = sd)), # sigma
                       cores = 16,
                       backend = "cmdstanr",
                       adapt_delta = 0.95,
                       iter = 5000)
model_full_1800 <- add_criterion(model_full_1800, criterion = 'loo')
summary(model_full_1800)

model_freq_1800 <- brm(meanings ~ 1 + z_freq + (1 | pos),
                       data = df_1800, 
                       family = zero_inflated_poisson(),
                       prior = c(prior(normal(0, 0.5), class = 'b'), #beta 
                                 prior(beta(2, 2), class = 'zi'), #pi or zi
                                 prior(normal(0, 1), class = Intercept), #alpha bar
                                 prior(exponential(1), class = sd)), # sigma
                       cores = 16,
                       backend = "cmdstanr",
                       adapt_delta = 0.95,
                       iter = 5000)
model_freq_1800 <- add_criterion(model_freq_1800, criterion = 'loo')
summary(model_freq_1800)

model_age_1800 <- brm(meanings ~ 1 + z_age + (1 | pos),
                       data = df_1800, 
                       family = zero_inflated_poisson(),
                       prior = c(prior(normal(0, 0.5), class = 'b'), #beta 
                                 prior(beta(2, 2), class = 'zi'), #pi or zi
                                 prior(normal(0, 1), class = Intercept), #alpha bar
                                 prior(exponential(1), class = sd)), # sigma
                       cores = 16,
                       backend = "cmdstanr",
                       adapt_delta = 0.95,
                       iter = 5000)
summary(model_age_1800)

## LOO comparison
loo_compare(model_full_1800, model_freq_1800, criterion = 'loo') %>% 
  print(simplify = F)

### Plotting clipped results 

plot_avg_comparisons <- function(model, title) {
  plot <- avg_comparisons(model) %>%
    posterior_draws(shape = "rvar") %>%
    ggplot(aes(y = term, xdist = rvar)) + 
    stat_slabinterval() +
    xlim(0, 0.5) +
    ggtitle(title) +
    ylab('') + 
    xlab('') +
    geom_vline(xintercept = 0, 
               linetype="dotted",
               color = 'red')
  return(plot)
}
  
plot_full <- plot_avg_comparisons(model_full_1800, 'Cross-validated dataset')
plot_freq <- plot_avg_comparisons(model_freq_1800, 'Frequency')

ggarrange(plot_full, plot_freq, nrow=2, ncol=1)

# for both datasets 

posterior_beta_both <- ggarrange(full, 
                                pp_check(model_full, ndraws = 100, type = "bars") + 
                                  labs(x = 'N of additional meanings'), 
                                plot_full, 
                                pp_check(model_full_1800, ndraws = 100, type = "bars") + 
                                  labs(x = 'N of additional meanings'), 
                                nrow=2, ncol=2,
                                common.legend = TRUE,
                                align = 'hv',
                                legend = 'right')
ggsave("figures/posterior_beta.pdf", plot = posterior_beta_both, width = 10, height = 4)

hypothesis(model_full, 'z_age > z_freq')
hypothesis(model_full_1800, 'z_age < z_freq')


re_model_only <- crossing(z_freq = seq(min(df_1800$z_freq), 
                                       max(df_1800$z_freq), length.out=100),
                          z_age = seq(min(df_1800$z_age), 
                                      max(df$z_age), length.out=100),
                          pos = unique(df_1800$pos)) %>%
  epred_draws(newdata = ., object = model_full_1800,
              scale = "response", ndraws = 1e3)
ggplot(re_model_only,
       aes(x = z_age, y = .epred)) +
  facet_wrap(~pos) +
  geom_count(data = df_1800, 
             aes(x = z_age, y = meanings),
             alpha = 0.1) +
  stat_lineribbon(.width = c(.95, 0.8, 0.5), 
                  color = "#08519C", 
                  alpha = 0.5) +
  scale_fill_brewer('CI') +
  theme(strip.background=element_rect(colour="black",
                                      fill=NA)) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(0, 2, 5, 8, 10, 12)
  )

# Average number of meanings per decade
pos_year <- df %>%
  mutate(decade = etymology %/% 50) %>%
  group_by(decade, pos) %>%
  do(tidy(t.test(.$number_of_meanings - 1, conf.level = 0.95))) %>%
  group_by(pos) %>%
  mutate(pos_label = paste(pos, "(", sum(parameter), "observations)")) %>%
  ggplot(aes(x = decade * 50, y = estimate, color = pos_label)) +  
  geom_point(size = 2) +
  geom_path(alpha = 0.3, linewidth = 1) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  ylim(0, 2) +
  theme(strip.background = element_rect(colour = "black", fill = NA)) +
  labs(x = 'Year of appearance (grouped by 50 year periods)', y = 'Mean number of additional meanings') +
  theme(legend.position = 'none') +
  scale_colour_brewer(palette = "Spectral") +
  facet_wrap(~pos_label) +
  geom_segment(aes(x = 1500, y = 1.8, xend = 1950, yend = 1.8),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "grey", size = 0.3, alpha = 0.3) +
  geom_segment(aes(x = 1800, y = 1.6, xend = 1950, yend = 1.6),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "grey", size = 0.3, alpha = 0.3) +
  geom_text(aes(x = 1750, y = 1.9, label = "Full"), color = "grey", size = 2.5, alpha = 0.3) +
  geom_text(aes(x = 1875, y = 1.7, label = "Cross-validated"), color = "grey", 
            size = 2.5, alpha = 0.3)

# Average number of meanings per pos
avg_pos <- df %>%
  group_by(pos) %>%
  summarize(conf_int = 1.96 * sd(meanings)/sqrt(length(meanings)),
            count = n(),
            meanings = mean(meanings)) %>%
  ggplot(aes(x = reorder(pos, meanings), y = meanings, color = pos)) +
  geom_point(aes(size = 2)) +
  geom_errorbar(aes(ymin = meanings - conf_int, 
                    ymax = meanings + conf_int), 
                width = 0, linewidth = 1, alpha = 1) +
  labs(x = '', y = 'Mean number of additional meanings') +
  scale_colour_brewer(palette = "Spectral") +
  theme(legend.position = 'none')

combined_plot <- ggarrange(pos_year, avg_pos, labels = c("A", "B"), 
                           widths = c(1.5, 1))
combined_plot
ggsave("figures/combined_plot.pdf", plot = combined_plot, width = 10, height = 4)
 
df %>% 
  filter(pos == 'ADJ', etymology > 1950)
