kikuchi_stan <- stan_model("inverse-wishart.stan") 

N <- 932; y <- as.matrix(kikuchi_loc); mu <- as.vector(colMeans(kikuchi_loc));  
prior_sigma <- 1000*cov(lefty_ff); prior_nu <- 1000
kikuchi_list <- list(N=N, y=y, mu=mu, prior_sigma=prior_sigma, prior_nu=prior_nu)

kikuchi_fit <- sampling(kikuchi_stan, data = kikuchi_list)
kikuchi_result <- extract(kikuchi_fit, permuted = T) 
kikuchi_sim <- as.data.frame(mvrnorm(N, mu, matrix(c(0.68,0.12,0.12,0.72),2,2)))  

kikuchi_sim <- kikuchi_sim %>% 
  rename(plate_x = 'V1', 
         plate_z = 'V2') 
kikuchi_sim$source <- as.factor(c("posterior")) 
kikuchi_loc$source <- as.factor(c("likelihood")) 
kikuchi_all <- bind_rows(kikuchi_loc, kikuchi_sim) 