######################################################################
######################################################################
###                                                                ###
###                 ******************************                 ###
###                 * Created by: SHANNON LOOMIS *                 ###
###                 ******************************                 ###
###                                                                ###
###    Code used to test data creation, wrangling, and plotting    ### 
###         before including them in the accompanying app          ###
###    The Shiny app was designed to create a map and a chart      ###
###         based on an input value selection                      ###
###                                                                ###
######################################################################
######################################################################




######################
# WRANGLING THE DATA #
######################


# Define number of data points
n = 23   # Because Michael Jordan and Ryne Sandberg

# Set seed for reproducibility
set.seed(n)

# Select normally distributed random numbers for location
df = data.frame(x = rnorm(n),
                y = rnorm(n))

# Add these to University of Chicago lat/long for mapping
df$x = -87.600730 + df$x/100
df$y =  41.791076 + df$y/100

# Add columns of uniform random values for the charts
df$a = runif(n,min = -2, max = 2)
df$b = runif(n,min = -2, max = 2)



######################
# EXPLORING THE DATA #
######################

# install.packages("ggplot2")
library(ggplot2)



### HISTOGRAMS ###

# Standard histogram of a
png("hist1.png",width = 600,height = 400)
ggplot() + 
  geom_histogram(data = df, aes(x = a))
dev.off()

# Choose different bin based on low number of datapoints
png("hist2.png",width = 600,height = 400)
ggplot() + 
  geom_histogram(data = df, aes(x = a),
                 bins=round(log(n,2)))
dev.off()

# Change to U of C colors and a title
png("hist3.png",width = 600,height = 400)
ggplot() + 
  geom_histogram(data = df, aes(x = a),
                 bins=round(log(n,2)),
                 color="black", fill="darkred") + 
  labs(title="Histogram of a")
dev.off()

# Remove superfluous background formatting
png("hist4.png",width = 600,height = 400)
ggplot() + 
  geom_histogram(data = df, aes(x = a),
                 bins=round(log(n,2)),
                 color="black", fill="darkred") + 
  labs(title="Histogram of a") + 
  theme_classic()
dev.off()

# Plot b with the same formatting
png("hist5.png",width = 600,height = 400)
ggplot() + 
  geom_histogram(data = df, aes(x = b),
                 bins=round(log(n,2)),
                 color="black", fill="darkred") + 
  labs(title="Histogram of b") + 
  theme_classic()
dev.off()




### SCATTER PLOTS ###

# Scatter plot of location (x & y)
png("scat1.png",width = 600,height = 400)
ggplot() + 
  geom_point(data = df, aes(x = x, y = y))
dev.off()

# Scatter plot of location colored by a
png("scat2.png",width = 600,height = 400)
ggplot() + 
  geom_point(data = df, aes(x = x, y = y, color = a, size = 12))
dev.off()

# Add labels and reformat
png("scat3.png",width = 600,height = 400)
ggplot() + 
  geom_point(data = df, aes(x = x, y = y, color = a, size = 12)) + 
  labs(title="Lat/Long Colored by a",
       x="Longitude", y = "Latitude") + 
  theme_classic()
dev.off()

# Scatter plot of location colored by b
png("scat4.png",width = 600,height = 400)
ggplot() + 
  geom_point(data = df, aes(x = x, y = y, color = b, size = 12)) + 
  labs(title="Lat/Long Colored by b",
       x="Longitude", y = "Latitude") + 
  theme_classic()
dev.off()




