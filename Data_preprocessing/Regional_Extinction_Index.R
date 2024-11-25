library(ggplot2)
library(data.table)
library(sf)

# Creating a sample data.frame
df <- data.frame(
  A = c(1, 2, 3, 4),
  B = c(5, 6, 7, 8),
  C = c(9, 10, 11, 12),
  stringsAsFactors = FALSE
)

print(df)

# apply: Apply a function to rows/columns of a dataframe
# MARGIN = 1 means applying function by rows, 
# MARGIN = 2 means by columns

# By row (MARGIN = 1)
apply_result_row <- apply(X=df, 
                          MARGIN=1, 
                          FUN=function(x) sum(x))  # Calculating the sum of each row
print(apply_result_row)
class(apply_result_row)
# By column (MARGIN = 2)
apply_result_col <- apply(X=df, 
                          MARGIN=2, 
                          FUN=function(x) sum(x))  # Calculating the sum of each column
print(apply_result_col)

# lapply: Apply a function to a list or vector and return the result as a list
lapply_result <- lapply(df$A, function(x) sum(x))  # Calculating the sum of column A
print(lapply_result) # Return: List
print(class(lapply_result))

# sapply: Apply a function to a list or vector and return the result as a vector
sapply_result <- sapply(df$A, function(x) sum(x))  # Calculating the sum of column A
print(sapply_result)  # Return Named Vector
print(class(sapply_result))


#### Actual Code Starts Here ####
# Load Population Data (주민등록 연앙인구 (2023년))
# https://jumin.mois.go.kr/ageStatMonth.do#none
census <- read.csv("./data/2023_populations.csv", fileEncoding='CP949')
head(census)

# Load Geometry Data
sgg_geom <- read_sf("./data/sgg_geom.geojson")
sgg_geom

# FYI: Manipulate Geometry Data
# sgg_geom <- read_sf("./data/sig_20230729/sig.shp", options='ENCODING=CP949')
# sgg_geom <- st_set_crs(sgg_geom, 5179) # Define CRS
# sgg_geom <- st_simplify(sgg_geom, dTolerance = 50, preserveTopology = TRUE) # Simplify Geometry
# sgg_geom_4326 <- st_transform(sgg_geom, 4326) # Change CRS
# st_crs(sgg_geom_4326) # Check CRS
# write_sf(sgg_geom_4326, './data/sgg_geom.geojson')

# Plot maps 
plot(sgg_geom)

# Manipulate Census data
head(census$행정구역)

# Need to split the following text
temp_text <- "서울특별시  (1100000000)"
strsplit(temp_text, "  ") # Function to split text

# Two regular expressions may be joined by the infix operator ‘⁠|’;
# the resulting regular expression matches any string matching either subexpression.
strsplit(temp_text, "\\(|\\)")[[1]]

# Use a function to automatically split text
my_split <- function(x){
  split_parts <- strsplit(x, "\\(|\\)")[[1]]  # Split by " (" and ")"
  return(list(admin_name=split_parts[1], admin_code=split_parts[2]))
}

# Check the performance of the function
my_split("서울특별시  (1100000000)")

# Apply the function to a data.frame
split_results <- apply(X=census, 
                       MARGIN=1, 
                       FUN=function(row) my_split(row['행정구역']))
split_results

# Same results with lapply
split_results_2 <- lapply(X=census$행정구역, FUN=function(row) my_split(row))
split_results_2[1:3]
class(split_results_2)

# Same results with sapply
split_results_3 <- sapply(X=census$행정구역, FUN=function(row) my_split(row))
split_results_3[1:3]
class(split_results_3)

# Remove names associated with the vector
admin_names <- split_results_3['admin_name', ]
admin_names[1:3]
names(admin_names) <- NULL
admin_names[1:3]

admin_codes <- split_results_3['admin_code', ]
names(admin_codes) <- NULL
admin_codes[1:3]

# Enter the admin names and admin codes back to the census data.frame
census$admin_names <- admin_names
census$admin_codes <- admin_codes

#### Cleaning Rows in the Census Data.Frame#### 
# Extract the first two digits (sido code)
two_digits <- substr(x=admin_codes, 
                     start=1, 
                     stop=2)
two_digits

# Code of Seoul 
paste0(two_digits[1], "00000000")

# Apply 8-leading zeros to the `two-digits` vector
sido_codes = sapply(X=two_digits, FUN=function(x) paste0(x, "00000000"))
sido_codes <- unique(sido_codes)
sido_codes[1:5]

# Extract the first five digits (sgg code)
five_digits <- substr(admin_codes, 1, 5)
five_digits

# Apply 5-leading zeros to the `five-digits` vector
sgg_codes <- sapply(X=five_digits, FUN=function(x) paste0(x, "00000"))
sgg_codes <- unique(sgg_codes)
sgg_codes[1:5]

# Extract rows matches with sido_codes
# Needs to import 'data.table' package
sido_census <- census[census$admin_codes %in% sido_codes, ]
dim(sido_census)

# Extract rows matches with sgg_coes but not matches with sido_codes
sgg_census <- census[(census$admin_codes %in% sgg_codes) &
                       !(census$admin_codes %in% sido_codes),  ]
dim(sgg_census)

# Extract the first-five digits for merge purpose
sgg_census$sgg_codes <- apply(X=sgg_census, 
                              MARGIN=1, 
                              FUN=function(x) substr(x['admin_codes'], 1, 5))
sgg_census


# Risk of extinction index is calculated by dividing 
# the female population aged 20-39 by the population aged 65 and over
# The index are classified as follows:
## < 0.2 : high risk
## 0.2 - 0.5 : moderate risk
## 0.5 - 1.0 : need attention
## 1.0 - 1.5 : normal
## > 1.5 : no risk

#### Cleaning columns ####
colnames(sgg_census)

library(glue)
# Creating column names with age ranges (0, 5, 10, 15, 20)
young_female_range <- seq(20, 39, by = 5)
young_female_range

# Dynamically create column names
# Female 20-39 years old
young_female_cols <- glue("X2023년_여_{young_female_range}.{young_female_range+4}세")
young_female_cols

# Elderly 65 years and older (both male and female)
old_people_range <- seq(65, 99, by=5)
old_people_range

old_female_cols <- glue("X2023년_여_{old_people_range}.{old_people_range+4}세")
old_female_cols

old_male_cols <- glue("X2023년_남_{old_people_range}.{old_people_range+4}세")
old_male_cols

# Appending columns of interest
old_people_cols <- c(old_female_cols, old_male_cols, 
                     "X2023년_여_100세.이상", "X2023년_남_100세.이상")

# Cleaned Census data.frame
cleaned_df <- sgg_census[ , c("admin_names", 
                              "sgg_codes", 
                              young_female_cols, 
                              old_people_cols)
                          ]
cleaned_df

# Wanted to get the sum of young female population.. but,
# Error in FUN(X[[i]], ...) : 
# only defined on a data frame with all numeric-alike variables
# sum(cleaned_df[2, young_female_cols])
typeof(cleaned_df[, 'X2023년_여_20.24세']) # Strings, so can't be summed. 

# Let's change the data type to numeric. but.. 
# Warning messages:
#   1: In lapply(X = X, FUN = FUN, ...) : NAs introduced by coercion
# Comma (,) prevents the datatype conversion. 
sapply(X=cleaned_df[young_female_cols], FUN=as.numeric)

# Remove the comma in the columns of interest
cleaned_df[old_people_cols] <- lapply(X=cleaned_df[old_people_cols], 
                                      FUN=function(x) gsub(",", "", x)
                                      )
cleaned_df[young_female_cols] <- lapply(X=cleaned_df[young_female_cols], 
                                        FUN=function(x) gsub(",", "", x)
                                        )

# Change the data type again. 
cleaned_df[old_people_cols] <- lapply(X=cleaned_df[old_people_cols], 
                                      FUN=as.numeric)

cleaned_df[young_female_cols] <- lapply(X=cleaned_df[young_female_cols], 
                                        FUN=as.numeric)

# Get the some of the age groups in interest (young female pops and old pops)
for (i in 1:dim(cleaned_df)[1]){
  cleaned_df[i, 'young_females'] <- sum(cleaned_df[i, young_female_cols])
  cleaned_df[i, 'old_people'] <- sum(cleaned_df[i, old_people_cols])
}

# Get Risk of extinction index
cleaned_df$ratio <- cleaned_df$young_females / cleaned_df$old_people
cleaned_df$ratio

# Clean the data.frame one more time
cleaned_df <- cleaned_df[c('admin_names', 'sgg_codes', 'ratio')]
cleaned_df

#### Making Maps ####
# Merge geometry and census data
colnames(sgg_geom)
colnames(cleaned_df)

# Example of an outer join
# merged_df <- merge(sgg_geom, cleaned_df, by.x='SIG_CD', by.y='sgg_codes', all.x = TRUE, all.y = TRUE)

# Example of an inner join
merged_df <- merge(sgg_geom, cleaned_df, by.x='SIG_CD', by.y='sgg_codes')

# Final results to be plotted
merged_df$ratio

# Creating a map
ggplot(data = merged_df) +
  geom_sf(aes(fill=ratio)) +
  scale_fill_gradient(low = "red", high = "blue") +  # Continuous color scale
  theme_minimal()

# install.packages('tmap')
# install.packages('classInt')
library(tmap) # Thematic Map
library(tmaptools)
library(classInt) # Package for classification

# Histogram of the variable
ggplot(merged_df, aes(x=ratio)) +
  geom_histogram() 

# Equal Distance Intervals
equal_distance_intervals <- classIntervals(var=merged_df$ratio, 
                                           n = 5, 
                                           style = "equal")  # Equal Intervals
equal_distance_intervals

# Plot Intervals
ggplot(merged_df, aes(x=ratio)) +
  geom_histogram() + 
  geom_vline(xintercept = equal_distance_intervals$brks, color = "red", linetype = "dashed") + 
  labs(title = 'Equal Intervals')


# Quantiles Intervals (Same Numbers in the classes)
quantile_intervals <- classIntervals(var=merged_df$ratio, 
                                     n = 5, 
                                     style = "quantile")  # Quantile Intervals (Same Number)
quantile_intervals

# Plot Intervals
ggplot(merged_df, aes(x=ratio)) +
  geom_histogram() + 
  geom_vline(xintercept = quantile_intervals$brks, color = "red", linetype = "dashed") + 
  labs(title = 'Quantiles')

# Fisher-Jenks Algorithm
jenks_intervals <- classIntervals(var=merged_df$ratio, 
                            n = 5, 
                            style = "jenks")  # Jenks breaks (natural breaks)
jenks_intervals

# Plot Intervals
ggplot(merged_df, aes(x=ratio)) +
  geom_histogram() + 
  geom_vline(xintercept = jenks_intervals$brks, color = "red", linetype = "dashed")  + 
  labs(title = 'Fisher-Jenks')


# Automatically fix geometry if error is detected. 
tmap_options(check.and.fix = TRUE)

# Map visualization: Fisher Jenks
tm_jenks <- tm_shape(merged_df) +
  tm_borders(alpha = 0.2) +
  tm_fill("ratio", 
          palette = "RdYlGn", 
          style = "fixed",
          breaks = jenks_intervals$brks) +
  tm_layout(legend.position = c("left", "bottom"),
            main.title = 'Fisher Jenks')
tm_jenks

# Map visualization: Defined Intervals
tm_manual <- tm_shape(merged_df) +
  tm_borders(alpha=0.2) +
  tm_fill("ratio", 
          breaks=c(0, 0.2, 0.5, 1.0, 1.5, 3),
          palette=c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'),
          # palette=c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')
          ) +
  tm_layout(legend.position = c("left", "bottom"),
            main.title='Manual Interval'
            )
tm_manual

## Interactive Map
library(leaflet)

# Define the breaks and colors
breaks <- c(0, 0.2, 0.5, 1.0, 1.5, 3)
colors <- c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')

# Create a color bin function based on the breaks
color_bins <- colorBin(palette = colors, 
                       domain = merged_df$ratio, 
                       bins = breaks, 
                       na.color = "transparent")

merged_df_4326 <- st_transform(merged_df, 4326)

# Create a leaflet map
leaflet_map <- leaflet(merged_df_4326) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~color_bins(ratio),  # Assign colors based on the ratio column
    weight = 1,  # Set border width
    color = "grey",  # Border color
    fillOpacity = 0.7,  # Fill opacity
    popup = ~glue("{admin_names}: {ratio}") # Optional: display the ratio value in a popup
  ) %>%
  addLegend(
    "bottomright",  # Position of the legend
    pal = color_bins,  # Use the color bin function
    values = ~ratio,  # Map to the ratio column
    title = "Ratio",  # Legend title
    opacity = 1
  )

# Show the map
leaflet_map