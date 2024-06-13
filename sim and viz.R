library(tidyverse) 
library(MVN) 

# Filter four-seam fastballs 
# Data from 2022 MLB season 
lefty_ff_league <- data_all %>% 
  filter(pitch_type == 'FF' & p_throws == 'L') %>% 
  filter(!is.na(plate_x), !is.na(plate_z)) %>% 
  select(plate_x, plate_z) 

righty_ff_league <- data_all %>% 
  filter(pitch_type == 'FF' & p_throws == 'R') %>% 
  filter(!is.na(plate_x), !is.na(plate_z)) %>% 
  select(plate_x, plate_z) 

set.seed(65) 

# Take random samples for ease of computing
lefty_sample <- lefty_ff[sample(nrow(lefty_ff), size=1000),]
righty_sample <- righty_ff[sample(nrow(righty_ff), size=1000),]

# Verify normality 
mvn(lefty_sample, mvnTest = 'mardia', multivariatePlot = 'qq') 
mvn(righty_sample, mvnTest = 'mardia', multivariatePlot = 'qq')

lefty_ff %>% 
  summarize(xloc = mean(plate_x), yloc = mean(plate_z), 
            cov = cov(plate_x, plate_z)) 
righty_ff %>% 
  summarize(xloc = mean(plate_x), yloc = mean(plate_z), 
            cov = cov(plate_x, plate_z)) 
# Both lefty and righty four-seamers have nearly identical mean 
# Variances are also identical but signs are flipped 
# i.e. locations are essentially mirrored which makes sense 

# List of lefties with min. 50 four-seamers thrown 
lefty_ff_data <- data_all %>% 
  filter(pitch_type == 'FF' & p_throws == 'L') %>% 
  filter(!is.na(plate_x), !is.na(plate_z)) %>% 
  group_by(player_name) %>% 
  reframe(pitches = n(), 
          loc_mean = list(colMeans(cbind(plate_x, plate_z))), 
          loc_var = list(cov(cbind(plate_x, plate_z)))) %>% 
  filter(pitches >= 50) 

# List of righties with min. 50 four-seamers thrown 
righty_ff_data <- data_all %>% 
  filter(pitch_type == 'FF' & p_throws == 'R') %>% 
  filter(!is.na(plate_x), !is.na(plate_z)) %>% 
  group_by(player_name) %>% 
  reframe(pitches = n(), 
          loc_mean = list(colMeans(cbind(plate_x, plate_z))), 
          loc_var = list(cov(cbind(plate_x, plate_z)))) %>% 
  filter(pitches >= 50) 

library(MASS) 
library(LaplacesDemon) 
set.seed(66) 

# Function that calculates posterior covariance matrix 
# Player data (likelihood) and league data (prior) as inputs 
ff_posterior <- function(players, league) {
  result <- tibble() 
  for (i in 1:nrow(players)) {
    player_name <- players[[1]][i]
    prior_nu <- 1000 
    prior_sigma <- prior_nu*cov(league)
    n <- players[[2]][i]
    mu <- unlist(players[[3]][i]) 
    sample_cov <- matrix(unlist(players[[4]][i]),2,2) 
    posterior_nu <- n + prior_nu 
    posterior_sigma <- n*sample_cov + prior_sigma 
    cov_new <- matrix(0,2,2)
    for (i in 1:1000) {
      cov_new <- cov_new + rinvwishart(posterior_nu, posterior_sigma)
    } 
    player_data <- mvrnorm(n, mu, cov_new/1000) 
    mean_vector <- list(colMeans(player_data))  
    cov_matrix <- list(cov(player_data)) 
    entry <- tibble(player_name, mean_vector, cov_matrix) 
    result <- bind_rows(result, entry)
  }
  return(result)
} 

# Store results by handedness 
lefty_posterior <- ff_posterior(lefty_ff_data, lefty_ff_league) 
righty_posterior <- ff_posterior(righty_ff_data, righty_ff_league)

# Given a player, handedness, and desired sample size, generates random sample 
# of n four-seam fastball locations 
ff_sampler <- function(name, hand, n) {
  if (hand == 'L') {
    player_data <- 
      lefty_posterior %>% 
      filter(player_name == name) 
    mu <- unlist(player_data[[2]]) 
    sigma <- matrix(unlist(player_data[[3]]),2,2) 
    sample <- as.tibble(mvrnorm(n, mu, sigma)) 
  } else {
    player_data <- 
      righty_posterior %>% 
      filter(player_name == name) 
    mu <- unlist(player_data[[2]]) 
    sigma <- matrix(unlist(player_data[[3]]),2,2) 
    sample <- as.tibble(mvrnorm(n, mu, sigma)) 
  }
  return(sample)
}

# Example players 
wheeler_loc <- data_all %>% 
  filter(pitch_type == 'FF' & p_throws == 'R') %>% 
  filter(pitcher == 554430) %>% 
  filter(!is.na(plate_x), !is.na(plate_z)) %>% 
  dplyr::select(plate_x, plate_z)
kikuchi_loc <- data_all %>% 
  filter(pitch_type == 'FF' & p_throws == 'L') %>%
  filter(pitcher == 579328) %>% 
  filter(!is.na(plate_x), !is.na(plate_z)) %>% 
  dplyr::select(plate_x, plate_z) 

set.seed(67) 

wheeler_sim <- ff_sampler("Wheeler, Zack", 'R', nrow(wheeler_loc))
kikuchi_sim <- ff_sampler("Kikuchi, Yusei", 'L', nrow(kikuchi_loc))

# Indicate where the data is from then aggregate 
wheeler_sim$source <- as.factor(c("Simulated"))
wheeler_loc$source <- as.factor(c("Actual")) 
wheeler_data <- bind_rows(wheeler_loc, wheeler_sim) 
kikuchi_sim$source <- as.factor(c("Simulated"))
kikuchi_loc$source <- as.factor(c("Actual")) 
kikuchi_data <- bind_rows(kikuchi_loc, kikuchi_sim)  

# Visualization 
library(scales)
topKzone <- 3.5
botKzone <- 1.6
inKzone <- -0.95
outKzone <- 0.95
kZone <- data.frame(
  x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
  y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

wheeler_plot <- 
  ggplot(kZone, aes(x, y)) + 
  stat_density_2d(data=wheeler_data, 
                  aes(x=plate_x, y=plate_z, fill=after_stat(density)), 
                  geom="raster", contour=F) + 
  scale_fill_distiller(palette = "Spectral") + 
  geom_path(lwd=1.5, col="black") + 
  facet_wrap(~source) + 
  scale_x_continuous(breaks=breaks_pretty(n=5)) + 
  scale_y_continuous(breaks=breaks_pretty(n=5)) + 
  theme_bw() + 
  ggtitle("Zack Wheeler Pitch Location Heatmap") + 
  theme(plot.title = element_text(face = 'bold')) + 
  labs(x = "Horizontal Pitch Location", y = "Vertical Pitch Location") 

kikuchi_plot <- 
  ggplot(kZone, aes(x, y)) + 
  stat_density_2d(data=kikuchi_data, 
                  aes(x=plate_x, y=plate_z, fill=after_stat(density)), 
                  geom="raster", contour=F) + 
  scale_fill_distiller(palette = "Spectral") + 
  geom_path(lwd=1.5, col="black") + 
  facet_wrap(~source) + 
  scale_x_continuous(breaks=breaks_pretty(n=5)) + 
  scale_y_continuous(breaks=breaks_pretty(n=5)) + 
  theme_bw() + 
  ggtitle("Yusei Kikuchi FF Location Heatmaps, 2022") + 
  theme(plot.title = element_text(face = 'bold')) + 
  labs(x = "Horizontal Pitch Location", y = "Vertical Pitch Location")  


