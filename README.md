# bayesian-ff-locations
This is a straightforward attempt four-seam fastball locations using bayesian inference for multivariate data. I place a normal-inverse-wishart prior on the unknown mean and variance of location data from pitchers with initial parameters determined by league-wide data. The posterior also follows a normal-inverse-wishart distribution whose parameters are a blend of the observed and prior data; the exact ratio depends on the sample size chosen. 

There's no empirical measure of what an ideal ratio should be, but perhaps there's one that can best predict rest-of-season or even next-season pitcher command. For now, though, this is simply a framework for future applications. 
