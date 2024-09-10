library(tidyverse)
library(ggpubr)
library(DescTools)
library(scales)
library(ppcor)
library(brms)
library(tidybayes)
library(modelr)
library(bayesplot)
library(sjPlot)
library(broom)
library(marginaleffects)
library(distributions3)
library(latex2exp)
library(modelsummary)
library(ggridges)

theme_set(theme_bw())

set.seed(42)

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

## Combined descriptive figure
# Distribution of age:
fig_age <- ggplot(aes(x = etymology), data = age_estimation) +
  geom_histogram(bins = 50, fill = '#08519C') +
  labs(x = 'Estimated date of appearance', y = 'Count') +
  annotate(
    geom = "curve", x = 1650, y = 600, xend = 1560, yend = 760, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 1655, y = 605, label = "16th century as 1550", 
           hjust = "left")
# Distribution of POS per 50 year periods
pos_distr <- age_estimation %>%
  mutate(etymology = etymology %/% 40) %>%
  group_by(etymology, pos) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = etymology * 40, y = count, fill = pos)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_brewer() +
  labs(x = 'Year of appearance', y = 'Share', fill = 'POS') +
  scale_y_continuous(labels = scales::percent)
# Distribution of the number of additional meanings
num_additional <- age_estimation %>%
  ggplot(aes(x = factor(number_of_meanings - 1))) +
  geom_bar(stat = "count", fill = "#08519C", alpha = 1) +
  xlab('Number of additional meanings')
# Share of additional meanings
additional_share <- age_estimation %>%
  mutate(etymology = etymology %/% 40) %>%
  mutate(bin_add_meaning = if_else(number_of_meanings - 1 >= 1, 
                                   'At least 1', 
                                   'None')) %>%
  group_by(etymology, bin_add_meaning) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = etymology * 40, y = count, fill = bin_add_meaning)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_brewer() +
  labs(x = 'Year of appearance', 
       y = 'Share', 
       fill = ' Number of \n additional \n meanings') +
  scale_y_continuous(labels = scales::percent)
arr_descr <- ggarrange(fig_age, pos_distr, num_additional, additional_share, 
          labels = c("A", "B", "C", "D"))
ggsave("figures/summary_figures.pdf", arr_descr, width = 10, height = 4)

### Statistical analysis ### 
## Create a dataset
df <- read.csv('data/age_estimations.csv') %>%
  mutate(z_freq = (log(freq) - mean(log(freq))) / sd(log(freq)),
         z_age = (age - mean(age))/ sd(age),
         meanings = number_of_meanings - 1,
         z_length = (nchar(lemma) - mean(nchar(lemma)))/sd(nchar(lemma)))
## Spearman correlation coefficient (longevity ~ frequency)
cor.test(df$z_age, df$z_freq, method=c("kendall"))
## Spearman correlation coefficient (longevity ~ length)
cor.test(df$z_age, df$z_length, method=c("kendall"))
## Full model
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
summary(model_full)
## Frequency only model
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
summary(model_freq)
## LOOIC comparison
loo_compare(model_full, model_freq, criterion = 'loo') %>% 
  print(simplify = F)
## Posterior predictive checks
pp_check(model_full, ndraws = 100, type = "bars")
pp_check(model_freq, ndraws = 100, type = "bars")
## Hypotheses
hypothesis(x = model_full, 'z_age > 0')
hypothesis(x = model_full, 'z_age > z_freq')
## N eff plots:
mcmc_neff(neff_ratio(model_full))+ 
  theme_bw()
## Predictions of the model
model_1500_pred <- crossing(
  z_freq = seq(min(df$z_freq), max(df$z_freq), length.out = 30),
  z_age = seq(min(df$z_age), max(df$z_age), length.out = 30),
  pos = unique(df$pos)) %>%
  tidybayes::epred_draws(
    newdata = ., object = model_full,
    scale = "response", ndraws = 1e1
  ) %>%
  rename('prediction' = '.epred' ) %>%
  group_by(z_age, pos) %>%
  mutate(min = bayestestR::ci(prediction)$CI_low, 
         max = bayestestR::ci(prediction)$CI_high, 
         mean = mean(prediction)) %>%
  ggplot(aes(x = z_age, y = mean)) +
  facet_wrap(~pos) +
  geom_line(size = 2, color = '#08519C') +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "#08519C", alpha = 0.2) +
  geom_point(data = df,
             aes(x = z_age, y = meanings),
             alpha = 0.1) +
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
  ylab('Number of additional meanings') +
  ggtitle('1500-2020')
## Combined frequency + full model
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
ggarrange(full, only_freq, nrow=3, ncol=1)


### Fitting the subset models ###
df_1800 <- read.csv('data/age_estimations_1800.csv') %>%
  mutate(z_freq = (log(freq) - mean(log(freq))) / sd(log(freq)),
         z_age = (age - mean(age))/ sd(age),
         meanings = number_of_meanings - 1)
## Combined distribution of pos longevity for two datasets
age_estimation$dataset <- "1500-2020"
df_1800$dataset <- "1800-2020"
combined_data <- bind_rows(age_estimation, df_1800)
combined_hist <- combined_data %>%
  group_by(pos, dataset) %>%
  mutate(avg_long = mean(age)) %>%
  ggplot(aes(x = age, y = pos, fill = pos)) +
  geom_density_ridges(stat = "binline", bins = 40,) +
  # quantile_lines = TRUE, quantiles = 0.5,
  scale_fill_brewer() +
  ylab('') +
  xlab('Longevity') +
  theme(legend.position = 'none') +
  facet_wrap(~dataset, nrow = 2, ncol = 1) +
  theme(strip.background = element_rect(colour = "black", fill = NA))
ggsave("figures/combined_hist.pdf", plot = combined_hist, width = 10, height = 4)

## Age frequency correlation
cor.test(df_1800$age, df_1800$freq, method=c("kendall"))
## Full model
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
## Only frequency model
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
## Combined results (Figure 5)
plot_full <- plot_avg_comparisons(model_full, 'Full dataset') +
  scale_y_discrete(labels=c('Longevity', 'Frequency'))
plot_freq <- plot_avg_comparisons(model_full_1800, 'Cross-validated dataset') +
  scale_y_discrete(labels=c('Longevity', 'Frequency'))
ggarrange(plot_full, plot_freq, nrow=2, ncol=1)
posterior_beta_both <- ggarrange(plot_full, 
                                pp_check(model_full, ndraws = 100, type = "bars") + 
                                  labs(x = 'N. of additional meanings'), 
                                plot_freq, 
                                pp_check(model_full_1800, ndraws = 100, type = "bars") + 
                                  labs(x = 'N. of additional meanings'), 
                                nrow=2, ncol=2,
                                common.legend = TRUE,
                                align = 'hv',
                                legend = 'right')
ggsave("figures/posterior_beta.pdf", plot = posterior_beta_both, width = 10, height = 4)
## Hypothesis testing
hypothesis(model_full, 'z_age > z_freq')
hypothesis(model_full_1800, 'z_age < z_freq')
## Plotting model predictions
model_1800_pred <- crossing(z_freq = seq(min(df_1800$z_freq), 
                      max(df_1800$z_freq), length.out=20),
         z_age = seq(min(df_1800$z_age), 
                     max(df_1800$z_age), length.out=20),
         pos = unique(df_1800$pos)) %>%
  tidybayes::epred_draws(newdata = ., object = model_full_1800,
                         scale = "response", ndraws = 1e3) %>%
  rename('prediction' = '.epred' ) %>%
  group_by(z_age, pos) %>%
  mutate(min = bayestestR::ci(prediction)$CI_low, 
         max = bayestestR::ci(prediction)$CI_high, 
         mean = mean(prediction)) %>%
  ggplot(aes(x = z_age, y = mean)) +
  facet_wrap(~pos) +
  geom_line(size = 2, color = '#08519C') +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "#08519C", alpha = 0.2) +
  geom_point(data = df_1800,
             aes(x = z_age, y = meanings),
             alpha = 0.1) +
  theme(strip.background=element_rect(colour="black",
                                      fill=NA)) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(0, 2, 5, 8, 10, 12)
  ) +
  scale_x_continuous(
    labels = round(seq(min(df_1800$age), max(df_1800$age), length.out = 5)),
    breaks = seq(min(df_1800$z_age), max(df_1800$z_age), length.out = 5)
  ) +
  xlab('Longevity') +
  ylab('Number of additional meanings') +
  ggtitle('1800-2020')
arr_1800_1500 <- ggarrange(model_1500_pred, model_1800_pred, nrow=1, ncol=2, labels = c("A", "B"))
ggsave("figures/model_18500_pred.pdf", arr_1800_1500, width = 10, height = 6)

## Figure 6
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


## Idividual PoS analysis
ranef_long <- as.data.frame(ranef(model_full)) %>%
  rownames_to_column(var = "POS") %>%
  rename_with(~c("POS", "Estimate", "Est.Error", 
                 "Q2.5", "Q97.5"), everything())
random_int <- ranef_long %>% ggplot(aes(x = Estimate, 
                          y = reorder(POS, Estimate),
                          color = POS)) +
  geom_point() +
  geom_pointrange(aes(xmin=Q2.5, xmax=Q97.5)) +
  xlim(-2, 2) +
  scale_colour_brewer(palette = "Spectral") +
  theme(legend.position = 'none') +
  labs(y = '', x = 'Intercept')
new_pred <- crossing(
  z_freq = seq(min(df$z_freq), max(df$z_freq), length.out = 30),
  z_age = seq(min(df$z_age), max(df$z_age), length.out = 30),
  pos = unique(df$pos)) %>%
  tidybayes::epred_draws(
    newdata = ., object = model_full,
    scale = "response", ndraws = 1e1
  ) %>%
  rename('prediction' = '.epred' ) %>%
  group_by(z_age, pos) %>%
  mutate(min = bayestestR::ci(prediction)$CI_low, 
         max = bayestestR::ci(prediction)$CI_high, 
         mean = mean(prediction)) %>%
  ggplot(aes(x = z_age, y = mean, color = pos)) +
  facet_wrap(~pos) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2,
              colour = NA) +
  theme(strip.background=element_rect(colour="black",
                                      fill=NA),
        legend.position = 'none') +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(0, 2, 5, 8, 10, 12)
  ) +
  scale_x_continuous(
    labels = round(seq(min(df$age), max(df$age), length.out = 5)),
    breaks = seq(min(df$z_age), max(df$z_age), length.out = 5)
  ) +
  xlab('Longevity') +
  ylab('Number of additional meanings') +
  scale_colour_brewer(palette = "Spectral")
  # geom_count(data = df,
  #            aes(x = z_age, y = meanings),
  #            alpha = 0.05, color='black')
combined_pred_ranef <- ggarrange(random_int, new_pred, labels = c("A", "B"), 
                           widths = c(1, 1.5))
ggsave("figures/1500_pred_ranef.pdf", combined_pred_ranef, 
       width = 10, height = 4)

