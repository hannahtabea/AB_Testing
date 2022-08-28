#-------------------------------------------------------------------------------
# Bayesian vs. Frequentist A/B Testing on social media data
# Author: Hannah Roos
#-------------------------------------------------------------------------------

# load data 
# use here package to set up the right paths while making it less hard-wired for other users
path_dat <- here::here('Bayesian','dataset_Facebook.csv')

# get data
library(data.table)
facebook_dat <- fread(path_dat, stringsAsFactors = T)


# feature engineering - calculate engagement rate
library(dplyr)

facebook_dat %>% 
  group_by(Type) %>%
  count()

# how many engaged users/reach per type? - number of success
facebook_dat %>% 
  group_by(Type) %>%
  summarise(Engagement = sum(`Lifetime Engaged Users`))

# how many reached users per type? - number of trials
facebook_dat %>% 
  group_by(Type) %>%
  summarise(Reach = sum(`Lifetime Post Total Reach`))

#-----------------------------------------------------------
# Experimental Design and planning
#------------------------------------------------------------

# F: power analysis for testing videos against status
# Load package to run power analysis
library(pwr)

# Run power analysis for t-test
sample_size <- pwr.t.test(d = 1,
                          sig.level = 0.05,
                          power = 0.8,
                          alternative = "greater")
sample_size

#------------------------------------------------------------
# B: prior specification (read paper)
library("abtest")
prior_par <- elicit_prior(q = c(0.02, 0.05, 0.08),
                             prob = c(.025, .5, .975),
                             what = "arisk")
plot_prior(prior_par, what = "arisk")
plot_prior(prior_par, what = "logor")

#------------------------------------------------------------
# Hypothesis Testing
#------------------------------------------------------------
# F: run t-test
ab_experiment_results <- t.test(ER ~ Type,
                                data = facebook_dat_ER,
                                alternative = "less")
ab_experiment_results

#------------------------------------------------------------
# B: Bayes Factor
set.seed(1)

# how many engaged users/reach per type? - number of success
facebook_dat_ER %>% 
                  group_by(Type) %>%
                  summarise(Engagement = sum(`Lifetime Engaged Users`))

# how many reached users per type? - number of trials
facebook_dat_ER %>% 
                  group_by(Type) %>%
                  summarise(Reach = sum(`Lifetime Post Total Reach`))


data <- list(y1 = 91810, n1 = 588550, y2 = 11949, n2 = 358440)
ab <- ab_test(data = data, prior_par = prior_par)
print(ab)

#plot posterior
png(filename="posterior_p1p2.png", width= 800, height= 550)
plot_posterior(ab, what = "p1p2")
dev.off()

png(filename="posterior_risk.png", width= 800, height= 550)
plot_posterior(ab, what = "arisk")
dev.off()


#------------------------------------------------------------
# Magnitude of evidence
#------------------------------------------------------------
# F: eyeball difference with boxplot
facebook_dat_ER <- facebook_dat %>% 
  filter(Type == 'Video' | Type == 'Status') %>%
  group_by(Type) %>%
  mutate(ER = `Lifetime Engaged Users`/`Lifetime Post Total Reach`,)

# F: Boxplot of user engagement A vs B
library(ggplot2)

png(filename="boxplot.png", width= 800, height= 550)
ggplot(facebook_dat_ER,
                  aes(x = Type,
                      y = ER)) +
                  geom_boxplot()+
                  ggtitle("User engagement Rate by Content Type")
dev.off()
------------------------------------------------------------
# B: Magnitude of success with probability wheel
prob_wheel(ab)

#-------------------------------------------------------------
# Accumulation of evidence
#------------------------------------------------------------

# F: sequential analysis
library(gsDesign)
seq_analysis <- gsDesign(k = 3,
                         test.type = 1,
                         alpha = 0.05,
                         beta = 0.2,
                         sfu = "Pocock")
# we have a total of 52 observations
max_n <- 52
max_n_per_group <- max_n / 2
stopping_points <- max_n_per_group * seq_analysis$timing
stopping_points

# B: emergence of evidence 
png(filename="seq_plot.png", width= 800, height= 550)
plot_sequential(ab)
dev.off()
