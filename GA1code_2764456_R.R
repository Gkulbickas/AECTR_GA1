library(tidyverse)  
library(dplyr) 
library(lubridate) 
library(moments) 
library(ggplot2)
tic_data<-read_csv("input/ticker_data.csv", show_col_types = FALSE) 
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

appl_ret <-tic_data[tic_data$TICKER=="AAPL",]
pfe_ret <-tic_data[tic_data$TICKER=="PFE",] 
jnj_ret <-tic_data[tic_data$TICKER=="JNJ",] 
mrk_ret <-tic_data[tic_data$TICKER=="MRK",]

 
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
ggsave("output/plot_ret.png", plot, width = 10, height = 8, dpi =300)
