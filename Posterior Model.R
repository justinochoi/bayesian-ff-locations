library(baseballr)
library(tidyverse) 
library(MVN) 

# Data consists of all pitches thrown in 2022 regular season
righty_ff <- data_all %>% 
  filter(pitch_type == 'FF' & p_throws == 'R') %>% 
  filter(!is.na(plate_x), !is.na(plate_z)) %>% 
  select(plate_x, plate_z) 
lefty_ff <- data_all %>% 
  filter(pitch_type == 'FF' & p_throws == 'L') %>% 
  filter(!is.na(plate_x), !is.na(plate_z)) %>% 
  select(plate_x, plate_z) 

# Take random samples for ease of computing 
righty_sample <- righty_ff[sample(nrow(righty_ff), size=1000),]
lefty_sample <- lefty_ff[sample(nrow(lefty_ff), size=1000),]

# Giving data the ol' eye test 
righty_sample %>% 
  ggplot(aes(plate_x,plate_z)) + geom_point() 
lefty_sample %>% 
  ggplot(aes(plate_x,plate_z)) + geom_point() 

# Verify normality 
mvn(lefty_sample, mvnTest = 'mardia', multivariatePlot = 'qq') 
mvn(righty_sample, mvnTest = 'mardia', multivariatePlot = 'qq') 

righty_ff %>% 
  summarize(xloc = mean(plate_x), yloc = mean(plate_z), 
            cov = cov(plate_x, plate_z)) 
lefty_ff %>% 
  summarize(xloc = mean(plate_x), yloc = mean(plate_z), 
            cov = cov(plate_x, plate_z)) 
# Both lefty and righty ff have mean location (0.0, 2.8)
# Righty ff have covariance -0.164 while lefty ff have covariance +0.159
# Makes sense, essentially pitch locations are mirrored 

library(LaplacesDemon) 
library(MASS)
set.seed(486580) 

# Takes in player and league-wide data and calculates posterior  
# Player data is likelihood; league data is prior 
# Assume mean and variance of player data are unknown 
# Then the posterior follows normal-inverse-wishart distribution 
ff_posterior <- function(player, league) {
  mu <- (nrow(league)*colMeans(league) + nrow(player)*colMeans(player)) / 
        (nrow(league) + nrow(player))
  k <- nrow(league) + nrow(player)
  v <- nrow(league) + nrow(player)
  psi <- nrow(league)*cov(league) 
  s <- matrix(0,2,2) 
  for(i in 1:nrow(player)) {
    s_vector <- as.vector(t(player[i,] - colMeans(player))) 
    s <- s + outer(s_vector, s_vector) 
  }
  mean_vector <- as.vector(t(colMeans(player) - colMeans(league)))  
  c <- (nrow(league)*nrow(player)) / (nrow(league) + nrow(player))
  psi_new <- psi + s + c*outer(mean_vector, mean_vector)
  means <- mvrnorm(1000, mu, cov(player)/k) 
  vars <- matrix(0,2,2)
  for (i in 1:1000) {
    vars <- vars + rinvwishart(v, psi_new)
  }
  data <- mvrnorm(nrow(player), colMeans(means), vars/1000) 
  return(data)
} 

# Example players
wheeler_loc <- righty_ff %>% 
  filter(pitcher == 554430)
wheeler_posterior <- as.data.frame(ff_posterior(wheeler_loc, righty_sample))
kikuchi_loc <- lefty_ff %>% 
  filter(pitcher == 579328)
kikuchi_posterior <- as.data.frame(ff_posterior(kikuchi_loc, lefty_sample)) 

wheeler_posterior$source <- as.factor(c("posterior"))
wheeler_loc$source <- as.factor(c("likelihood")) 
wheeler_data <- bind_rows(wheeler_loc, wheeler_posterior) 
kikuchi_posterior$source <- as.factor(c("posterior"))
kikuchi_loc$source <- as.factor(c("likelihood")) 
kikuchi_data <- bind_rows(kikuchi_loc, kikuchi_posterior) 

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

wheeler_plot <- ggplot(kZone, aes(x, y)) + 
  stat_density_2d(data=wheeler_data, 
                  aes(x=plate_x, y=plate_z, fill=after_stat(density)), 
                  geom="raster", contour=F) + 
  scale_fill_distiller(palette = "Spectral") + 
  geom_path(lwd=1.5, col="black") + 
  scale_x_continuous(breaks=pretty_breaks(n=5)) + 
  scale_y_continuous(breaks=pretty_breaks(n=5)) + 
  theme_bw() + 
  ggtitle("Zack Wheeler Pitch Location Heatmap") + 
  theme(plot.title = element_text(face = 'bold')) + 
  labs(x = "Horizontal", y = "Vertical")

wheeler_plot + facet_wrap(vars(source)) 
# More variance in pitch locations 

kikuchi_plot <- ggplot(kZone, aes(x, y)) + 
  stat_density_2d(data=kikuchi_data, 
                  aes(x=plate_x, y=plate_z, fill=after_stat(density)), 
                  geom="raster", contour=F) + 
  scale_fill_distiller(palette = "Spectral") + 
  geom_path(lwd=1.5, col="black") + 
  scale_x_continuous(breaks=pretty_breaks(n=5)) + 
  scale_y_continuous(breaks=pretty_breaks(n=5)) + 
  theme_bw() + 
  ggtitle("Yusei Kikuchi Pitch Location Heatmap") + 
  theme(plot.title = element_text(face = 'bold')) + 
  labs(x = "Horizontal", y = "Vertical")

kikuchi_plot + facet_wrap(vars(source))
# Less variance in pitch locations 





