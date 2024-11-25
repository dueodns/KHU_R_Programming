# Installing packages
install.packages('ggplot2')

# Import packages
library(ggplot2)

# Get current working directory
getwd()

# Set working directory to use relative path
# Replace the path below with your local path
setwd('/Users/jparkgeo/Documents/GitHub/KHU_R_Programming/Week9')

# Read CSV file
seoul.temp <- read.csv('./data/seoul_temp_precip.csv')
head(seoul.temp)

# Slice August Temperature
temp_aug <- seoul.temp[seoul.temp$month == 8,
                       c('year', 'temperature')]
head(temp_aug)

par(mar=c(1,1,1,1))

### Line Graph ###
# Conventional way of plotting
plot(temp_aug, 
     type='b', 
     main='August Temperature (1954-2023)',
     xlab='month',
     ylab='temperature'
     )

# You can specify x and y axes. 
plot(x=temp_aug$year, 
     y=temp_aug$temperature, 
     type='b', 
     main='August Tempeature (1954-2023)',
     xlab='month',
     ylab='temperature'
     )

# Same plot using ggplot
ggplot(data=temp_aug, mapping=aes(x=year, y=temperature)) + 
  geom_line() +
  geom_point() +
  ggtitle('August Tempeature (1954-2023)')


### Boxplot ###
# Conventional Boxplot
boxplot(temperature~month, 
  data=seoul.temp,
  main='Monthly Temperature (1954-2023)',
  xlab='Month', 
  ylab='Tempearture (C)'
)

# Boxplot using ggplot2
ggplot(data=seoul.temp, aes(x=month, y=temperature, group=month, color=as.factor(month))) + 
  geom_boxplot() + 
  ggtitle('Monthly Temperature (1954-2023)') +
  labs(x='Month', y='Tempearture (C)')


# Conventional way of multiple plots
par(mfrow=c(2, 1))

plot(temp_aug, 
     type='b', 
     main='August Tempeature (1954-2023)'
)

boxplot(temperature~month, 
        data=seoul.temp,
        main='Monthly Temperature (1954-2023)',
        xlab='Month', 
        ylab='Tempearture (C)'
)

par(mfrow=c(1, 1))

# gridExtra is a package to enter multiple plots into a figure
install.packages("gridExtra")
library(gridExtra)

# Multiple plots in a figure
p1 <- ggplot(data=temp_aug, mapping=aes(x=year, y=temperature)) + 
  geom_line() +
  theme_minimal() + 
  ggtitle('August Tempeature (1954-2023)')

p2 <- ggplot(data=seoul.temp, aes(x=month, y=temperature, group=month, color=as.factor(month))) + 
  geom_boxplot() + 
  theme_minimal() + 
  ggtitle('Monthly Temperature (1954-2023)')

# Combine the plots with gridExtra
grid.arrange(p1, p2) 

### Handling CSV Files ###
# Get the average temperature per year
mean.temp <- aggregate(temperature~year, data=seoul.temp, FUN='mean')
head(mean.temp)

# Plot the average temperature over the years
ggplot(data=mean.temp, mapping=aes(x=year, y=temperature)) + 
  geom_point() + 
  geom_line() + 
  labs(title='Temperature Change over the Years',
       x='Year',
       y='Temperature (C)') 

# Export to a CSV file
write.csv(mean.temp, file='./data/mean_teperature.csv', row.names=FALSE)

### Plot Maps ###
install.packages('sf')
library(sf)

# Read GeoJSON file (geometry file)
states <- st_read("./data/us_states.geojson")
states

ggplot(data=states)+
  geom_sf() +
  theme_minimal()

ggplot(data=states)+
  geom_sf(aes(fill=total_pop_10)) +
  theme_minimal()

ggplot(states)+
  geom_sf(aes(fill=REGION)) +
  theme_minimal()

# Treemap could be attractive to non-geography major students.
install.packages('treemap')
library('treemap')

colnames(states)

states_df <- as.data.frame(states)
treemap(states_df,
        index=c('REGION', 'NAME'), # Set hierarchy of the treemap (region & state)
        vSize='AREA',              # Size of each tile
        vColor='total_pop_10',     # Color of each tile
        type='value'               # Define how to color the tiles
        )




