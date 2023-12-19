# Load necessary libraries
library(ggplot2)
library(readr)  # For reading CSV data

# Define parameters
epsilon_p_demand <- 0.5  # Price elasticity of demand for biodegradable plastics
epsilon_Y_demand <- 1  # Income elasticity of demand for biodegradable plastics
Psi <- 1               # Shift parameter
tau_bio <- 0.05         # Learning rate for biodegradable plastics production
theta_fossil <- 0.75    # Conversion factor for fossil-based plastics
theta_bio <- 0.58       # Conversion factor for biodegradable plastics
epsilon_feedstock_demand <- 0.1  # Inverse feedstock price elasticity of demand for biodegradable plastics

# Assuming the time_series_data has columns like 'Year', 'GDP', 'Price_Fossil', 'Price_Bio', etc., along with the newly added time series data
time_series_data <- data.frame(
  Year = 1:10,                                                                # 2021 -2022 data
  GDP = runif(2, min=850000000000000000, max=1000000000000000000),          # Random GDP values for demonstration
  Price_Fossil = runif(2, min=0.63, max=0.63),                 # Random Fossil Price values
  Price_Bio = runif(2, min=13, max=17),                   # Random Bio Price values
  PC_bio_t = runif(2, min=1792000, max=2217000),                 # Time series for production capacity (STATISTA biodegradable + bio-bsed non biodegradable)
  c_process_bio_t = runif(2, min=2.82, max=2.82),           # Time series for process costs (bio)
  c_feedstock_bio_t = runif(2, min=3.20, max=3.45),       # Time series for feedstock costs (bio)
  c_process_fossil_t = runif(2, min=1.70, max=1.70),      # Time series for process costs (fossil)
  c_feedstock_fossil_t = runif(2, min=0.63, max=0.63)   # Time series for feedstock costs (fossil)
)



# Function to calculate the model's variables
calculate_model <- function(year, GDP_t, p_fossil_t, p_bio_t, PC_bio_t, c_process_bio_t, c_feedstock_bio_t, c_process_fossil_t, c_feedstock_fossil_t, epsilon_p_demand, epsilon_Y_demand) {
  # Model calculations
  D_bio_t <- ((p_fossil_t / p_bio_t) ^ epsilon_p_demand) * (GDP_t ^ epsilon_Y_demand) * Psi
  PC_bio_t_updated <- D_bio_t
  delta_c_process_bio <- ifelse(year > min(time_series_data$Year), max((PC_bio_t_updated - PC_bio_t) / PC_bio_t * tau_bio, 0), 0)
  c_process_bio_t <- c_process_bio_t + delta_c_process_bio
  p_bio_t <- c_feedstock_bio_t + c_process_bio_t
  p_fossil_t <- c_feedstock_fossil_t + c_process_fossil_t
  return(list(PC_bio_t_updated = PC_bio_t_updated, D_bio_t = D_bio_t, p_bio_t = p_bio_t, p_fossil_t = p_fossil_t))
}

# Function to run the model for different scenarios
run_model <- function(epsilon_p_demand, epsilon_Y_demand, GDP_growth_rate) {
  local_time_series_data <- time_series_data
  local_time_series_data$GDP <- local_time_series_data$GDP * (1 + GDP_growth_rate)
  results <- data.frame(Time = integer(), Demand_Bio = numeric(), Price_Bio = numeric(), Price_Fossil = numeric())
  PC_bio_previous <- local_time_series_data$PC_bio_t[1]
  
  for (year in local_time_series_data$Year) {
    year_data <- local_time_series_data[local_time_series_data$Year == year,]
    model_output <- calculate_model(year, year_data$GDP, year_data$Price_Fossil, year_data$Price_Bio, PC_bio_previous, year_data$c_process_bio_t, year_data$c_feedstock_bio_t, year_data$c_process_fossil_t, year_data$c_feedstock_fossil_t, epsilon_p_demand, epsilon_Y_demand)
    PC_bio_previous <- model_output$PC_bio_t_updated
    results <- rbind(results, data.frame(Time = year, Demand_Bio = model_output$D_bio_t, Price_Bio = model_output$p_bio_t, Price_Fossil = model_output$p_fossil_t))
  }
  
  return(results)
}

# Running scenarios with  epsilon p demand, epsilon Y demand and GDP growth
#Creating a scenario analysis for your model involves running the model under different sets of assumptions or parameters to understand how 
#changes in these inputs might affect the outcomes. For your system dynamics model of biodegradable and fossil-based plastics, 
#you might be interested in scenarios like changes in price elasticity, GDP growth rates, or shifts in production costs due to technological advancements.

base_results <- run_model(epsilon_p_demand, epsilon_Y_demand, 0.03)
scenario1_results <- run_model(1.5, epsilon_Y_demand, 0.03)
scenario2_results <- run_model(epsilon_p_demand, epsilon_Y_demand, 0.05)


# Combine the results from all scenarios into one data frame
base_results$Scenario <- "Base Scenario"
scenario1_results$Scenario <- "Scenario 1: Higher Price Elasticity"
scenario2_results$Scenario <- "Scenario 2: Higher GDP Growth Rate"

combined_results <- rbind(base_results, scenario1_results, scenario2_results)

# Plotting function for combined scenarios
plot_combined_scenarios <- function(combined_results) {
  ggplot(combined_results, aes(x = Time, y = Demand_Bio, color = Scenario)) +
    geom_line() +
    labs(title = "Demand for Biodegradable Plastics Under Different Scenarios", x = "Time", y = "Demand for Biodegradable Plastics") +
    theme_minimal() +
    scale_color_manual(values = c("Base Scenario" = "blue",
                                  "Scenario 1: Higher Price Elasticity" = "green",
                                  "Scenario 2: Higher GDP Growth Rate" = "red")) +
    scale_y_continuous(labels = scales::comma)
}

# Plot the combined scenarios
plot_combined_scenarios(combined_results)
