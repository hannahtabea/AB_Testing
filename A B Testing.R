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

# F: power analysis for testing videos against photos
# given that we would downsample photo group
library(powerMediation)
total_sample_size <- SSizeLogisticBin (p1 = 0.01,
                                       p2 = 0.06,
                                       B = 0.15,
                                       alpha = 0.05,
                                       power = 0.8)
total_sample_size 

total_sample_size / 2

# which power do we get with a setup like ours?
library(pwr)

# unequal sample size
n1 = 45
n2 = 7
p1 = 0.01
p2 = 0.06
h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2)))

pwr.2p2n.test(h, n1=n1, n2=n2, sig.level=0.10)



# B: prior specification (read paper)
library("abtest")
prior_par <- elicit_prior(q = c(2, 6, 8),
                             prob = c(.025, .5, .975),
                             what = "arisk")
plot_prior(prior_par, what = "logor")
plot_prior(prior_par, what = "p1p2")

# F: Plot Frequencies A vs B success and run logistic regression



# B: Posterior success probabilities
set.seed(1)

# how many engaged users/reach per type? - number of success
success_video <- facebook_dat %>% 
                  filter(Type == 'Video') %>%
                  select(`Lifetime Engaged Users`)
success_status <- facebook_dat %>% 
                    filter(Type == 'Status') %>%
                    select(`Lifetime Engaged Users`)

# how many reached users per type? - number of trials
trials_video <- facebook_dat %>% 
                  filter(Type=='Video') %>%
                  select(`Lifetime Post Total Reach`)
trials_status <- facebook_dat %>% 
                  filter(Type=='Status') %>%
                  select(`Lifetime Post Total Reach`)

data <- list(y1 = success_status, n1 = trials_status, y2 = success_video, n2 = trials_video)
ab <- ab_test(data = data, prior_par = prior_par)
print(ab)

# F: Boxplot of user engagement A vs B
# B: Magnitude of success with probability wheel

# F: sequential analysis
# B: emergence of evidence 

# F: no way
# B: robustness of evidence with heatmap
