# Load the necessary libraries
library(dplyr)
library(readr)

wide_data <- read_csv("C:/Users/amurg/Downloads/tile_exp24_somemods - Sheet1.csv")
View(wide_data)

# Define the start date
start_date <- as.Date("2024-03-27")

# Define the time points (t0 to t17)
time_points <- paste0("t", 0:17)
n_time_points <- length(time_points)

# Get the number of wells in wide_data (assuming each well has its own row)
n_wells <- nrow(wide_data)

# Create a long dataset with repeated wells and time points
long_data <- expand.grid(
  well_id = wide_data$well_id,  # Repeat well_id for each time point
  t = time_points,
  stringsAsFactors = FALSE
) %>%
  mutate(
    date = rep(start_date + 0:17, times = n_wells),  # Repeat the dates across all wells
    age_days = rep(1:18, times = n_wells),  # Repeat age_days across all wells
    plate = rep(wide_data$plate_id, each = n_time_points),  # Plate repeated for each well
    treatment = rep(wide_data$treatment, each = n_time_points),  # Treatment repeated for each well
    n_larvae = 10  # Constant larvae per time point
  )

# Ensure that the columns exist in wide_data before proceeding
expected_columns <- c(paste0("t", 0:17, "_swimming"),
                      paste0("t", 0:17, "_round"),
                      paste0("t", 0:17, "_settled_tile"),
                      paste0("t", 0:17, "_settled_plastic"),
                      paste0("t", 0:17, "_detached_spat"),
                      paste0("t", 0:17, "_dead"))

missing_columns <- setdiff(expected_columns, colnames(wide_data))

if (length(missing_columns) > 0) {
  stop("The following columns are missing in wide_data: ", paste(missing_columns, collapse = ", "))
}
#Error: The following columns are missing in wide_data: t0_detached_spat, t0_dead

# Now, map the wide_data columns into long_data
for (i in 0:17) {
  # Logical index for rows in long_data corresponding to the current time point
  long_data_idx <- long_data$t == paste0("t", i)
  
  # Map columns from wide_data to long_data for each time point
  long_data[long_data_idx, "swim"] <- wide_data[[paste0("t", i, "_swimming")]]
  long_data[long_data_idx, "round"] <- wide_data[[paste0("t", i, "_round")]]
  long_data[long_data_idx, "settled_tile"] <- wide_data[[paste0("t", i, "_settled_tile")]]
  long_data[long_data_idx, "settled_plastic"] <- wide_data[[paste0("t", i, "_settled_plastic")]]
  long_data[long_data_idx, "detached"] <- wide_data[[paste0("t", i, "_detached_spat")]]
  long_data[long_data_idx, "dead"] <- wide_data[[paste0("t", i, "_dead")]]
  
  # Calculate the total for the current time point
  long_data[long_data_idx, "total"] <- long_data[long_data_idx, "swim"] +
    long_data[long_data_idx, "round"] +
    long_data[long_data_idx, "settled_tile"] +
    long_data[long_data_idx, "settled_plastic"] +
    long_data[long_data_idx, "detached"] +
    long_data[long_data_idx, "dead"]
}

setwd("C:/Users/amurg/OneDrive/Desktop/PhD Haifa/Haifa projects/Recruitment_experiments")
write.csv(long_data, "long_data.csv", row.names = FALSE)

