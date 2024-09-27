# Correlation-Diagrams
# Load necessary libraries
library(readxl)
library(writexl)

# Read the Excel file
data <- read_excel("C://.xlsx", sheet = "Sheet1")

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)

# Read the Excel file


# Replace 'None' with NA
data[data == "None"] <- NA

# Convert all data to numeric (this will coerce non-numeric values to NA)
data <- data %>% mutate(across(everything(), as.numeric))

# Remove rows with NA values
data <- na.omit(data)

# Convert data to a matrix where rows are observations and columns are stations
data_matrix <- as.matrix(data)

# Function to calculate Coefficient of Divergence between two distributions
calculate_cd <- function(p, q) {
  # Ensure that there are no zeros in the distributions
  if (any(p == 0) | any(q == 0)) {
    stop("Distributions should not contain zeros.")
  }
  
  # Calculate CD using the formula
  cd <- sum(p * log(p / q), na.rm = TRUE)
  return(cd)
}

# Initialize a matrix to store CD values
num_stations <- ncol(data_matrix)
cd_matrix <- matrix(0, nrow = num_stations, ncol = num_stations)
rownames(cd_matrix) <- colnames(cd_matrix) <- colnames(data_matrix)

# Calculate Coefficient of Divergence for each pair of stations
for (i in 1:(num_stations - 1)) {
  for (j in (i + 1):num_stations) {
    p <- data_matrix[, i]
    q <- data_matrix[, j]
    
    # Ensure vectors are of the same length and remove NA values
    common_indices <- complete.cases(p, q)
    p <- p[common_indices]
    q <- q[common_indices]
    
    # Check if vectors are empty after filtering
    if (length(p) == 0 || length(q) == 0) {
      next
    }
    
    # Normalize to get probability distributions
    p <- p / sum(p)
    q <- q / sum(q)
    
    # Calculate CD
    cd_value <- tryCatch({
      calculate_cd(p, q)
    }, error = function(e) {
      NA
    })
    
    cd_matrix[i, j] <- cd_value
    cd_matrix[j, i] <- cd_value
  }
}

# Print the Coefficient of Divergence matrix
print(cd_matrix)
# Convert matrix to long format for ggplot2
cd_melted <- melt(cd_matrix)
install.packages("reshape2")
library(reshape2)

# Plot heatmap
ggplot(cd_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "grey", high = "blue") +
  theme_minimal() +
  labs(x = "Station", y = "Station", fill = "Coefficient of Divergence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
cd_df <- as.data.frame(cd_matrix)
cd_df$Station <- rownames(cd_df)
cd_long <- cd_df %>%
  pivot_longer(-Station, names_to = "Station2", values_to = "Coefficient_of_Divergence") %>%
  rename(Station1 = Station)

# Plot heatmap with highlighted values
ggplot(cd_long, aes(x = Station1, y = Station2, fill = Coefficient_of_Divergence)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red", midpoint = 0.5, na.value = "grey") +
  geom_text(aes(label = ifelse(Coefficient_of_Divergence > 0.5, round(Coefficient_of_Divergence, 2), "")),
            color = "black", size = 3) +
  theme_minimal() +
  labs(x = "Station", y = "Station", fill = "Coefficient of Divergence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(cd_long, aes(x = Station1, y = Station2, fill = Coefficient_of_Divergence)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", na.value = "grey", limits = c(0, 1)) +
  geom_text(aes(label = ifelse(Coefficient_of_Divergence > 0.05, round(Coefficient_of_Divergence, 2), "")),
            color = "white", size = 3) +
  theme_minimal() +
  labs(x = "Station", y = "Station", fill = "Coefficient of Divergence") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
