library(tidyverse)  
library(dplyr) 
library(lubridate) 
library(moments) 
library(ggplot2) 


#Question 2 
alpha <- 0.4
deltas <- c(0.3, 0.1, 0, -0.3)
gammas <- c(0.01, 0.1, 1)
x <- seq(-3, 3, length.out = 500)

# Create a data frame to hold all combinations
plot_data <- expand.grid(x = x, delta = deltas, gamma = gammas)

# Calculate the News-Impact Curve values
plot_data <- plot_data %>%
  mutate(
    nic = (alpha + delta * tanh(-gamma * x)) * x^2,
    delta_label = paste0("δ = ", delta),
    gamma_label = paste0("γ = ", gamma)
  )

# Generate the 2x2 grid plot
nic <- ggplot(plot_data, aes(x = x, y = nic, color = as.factor(gamma))) +
  geom_line(linewidth = 1) +
  facet_wrap(~ delta_label, scales = "free_y", ncol = 2) +
  labs(
    title = "News-Impact Curves for GARCH-M-L Model",
    subtitle = expression(paste("Holding ", sigma[t-1]^2, " = 1, ", mu, " = 0, ", lambda, " = 0, ", alpha, " = 0.4")),
    x = expression(x[t-1] ~ "(Shock)"),
    y = expression(sigma[t]^2 ~ "(Conditional Volatility)"),
    color = expression(gamma)
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("nic.png", nic, width = 10, height = 8, dpi =300)

#Question 3 
tic_data<-read_csv("ticker_data.csv", show_col_types = FALSE) 
tic_data <- tic_data |> 
  mutate(RET = RET*100)|>
  arrange(date) |> 
  filter(!is.na(RET)) 

summary = tic_data |> 
  group_by(TICKER) |> 
  summarise( 
    Length = n(),
    Mean = mean(RET), 
    Med = median(RET),  
    SD = sd(RET), 
    Skew = skewness(RET), 
    Excess_Kurt = kurtosis(RET) - 3,  
    Min = min(RET), 
    Max = max(RET)
    )

plot <-ggplot(data = tic_data, aes(x=date, y=RET)) + 
  geom_line(linewidth = 0.5) + 
  facet_wrap(~ TICKER, ncol = 2, scales = "free")+ 
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  labs( 
     
    x = NULL, 
    y = "% per day"
  ) +
  theme_bw() + 
  theme( 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold") 
  )
ggsave("plot_ret.png", plot, width = 10, height = 8, dpi =300)
