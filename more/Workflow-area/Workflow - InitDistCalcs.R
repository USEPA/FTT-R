## Function for the uniform initial distribution in FTT
## Nate Pollesch - Sept 3rd, 2025

## number of individuals in initial population
n0<-100

## number of meshpoints (size classes)
noMeshPts<-100

## Our standard is 100 size classes and 100 individuals, so this comes back as a very simple vector of 100 1's.
initDist<-rep(n0/noMeshPts,noMeshPts)
initDist

## However, since you don't have the size classes, we can calculate the average size of the
## individuals within the size classes (this corresponds to the meshpoints directly when we have 100 individuals)
upperBound<-74
lowerBound<-5.6

h <- (upperBound - lowerBound)/noMeshPts
meshpts <- lowerBound + ((1:noMeshPts) - 1/2) * h

meshpts

length(meshpts)

write.csv(meshpts,file="initDistUnif100.csv")

#### Going from sizes to distributions for initial distibutions ####
## based on the data shared by C. Accolla, I am going to convert frequencies in size classes to an initial distribution for the IPM
library(readr)
ABM_init_dist <- read_csv("workflow-data/ABM_init_dist.csv")

# Define the range and number of bins
range_min <- 5.6
range_max <- 74
num_bins <- 100

# Create breaks for the bins
breaks <- seq(range_min, range_max, length.out = num_bins + 1)

# Categorize the data into bins
binned_data <- cut(ABM_init_dist$mm, breaks = breaks, include.lowest = TRUE, right = FALSE)

# Display the binned data
print(binned_data)

# Optionally, create a frequency table
freq_table <- table(binned_data)
print(freq_table)
str(freq_table)

names(freq_table)
names(freq_table)<-as.character(1:100)
freq_table
length(freq_table)

ABMDist<-freq_table
ABMDistVec<-as.vector(ABMDist)

write.csv(ABMDistVec, file="ABM_initial_distribution.csv")
# Convert the frequency table to a data frame, including all bins
plot_data <- data.frame(
  binned_data = levels(binned_data),
  frequency = as.numeric(freq_table)
)

# Ensure the bins are in order of magnitude
plot_data$binned_data <- factor(plot_data$binned_data, levels = levels(binned_data), ordered = TRUE)

# Create a bar plot using ggplot2
ggplot(plot_data, aes(x = binned_data, y = frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency Distribution of Binned Data",
       x = "Bins",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#### Repeat visualization with meshpts data from IPM, then compare plots
# Sample datasets
data1 <- ABM_init_dist$mm
data2 <- meshpts

# Define the range and number of bins
range_min <- 5.6
range_max <- 74
num_bins <- 100

# Create breaks for the bins
breaks <- seq(range_min, range_max, length.out = num_bins + 1)

# Categorize the data into bins
binned_data1 <- cut(data1, breaks = breaks, include.lowest = TRUE, right = FALSE)
binned_data2 <- cut(data2, breaks = breaks, include.lowest = TRUE, right = FALSE)

# Create frequency tables
freq_table1 <- table(binned_data1)
freq_table2 <- table(binned_data2)

# Convert frequency tables to data frames
plot_data1 <- data.frame(
  binned_data = levels(binned_data1),
  frequency = as.numeric(freq_table1)
)

plot_data2 <- data.frame(
  binned_data = levels(binned_data2),
  frequency = as.numeric(freq_table2)
)

# Ensure the bins are in order of magnitude
plot_data1$binned_data <- factor(plot_data1$binned_data, levels = levels(binned_data1), ordered = TRUE)
plot_data2$binned_data <- factor(plot_data2$binned_data, levels = levels(binned_data2), ordered = TRUE)

# Combine the data for plotting
combined_data <- merge(plot_data1, plot_data2, by = "binned_data", suffixes = c("_data1", "_data2"))

# Plot using base R
barplot(
  height = rbind(combined_data$frequency_data1, combined_data$frequency_data2),
  beside = TRUE,
  names.arg = combined_data$binned_data,
  col = c("skyblue", "orange"),
  legend.text = c("Data 1", "Data 2"),
  args.legend = list(x = "topright"),
  las = 2,
  cex.names = 0.7,
  main = "Comparison of Binned Data",
  xlab = "Bins",
  ylab = "Frequency"
)
