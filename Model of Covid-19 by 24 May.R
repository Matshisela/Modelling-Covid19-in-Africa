#Loading data
cdata <- read.csv(file.choose())

#Loading Packages
library(tidyverse)
library(gganimate)
library(gapminder)
library(plotly)
library(tibble)
library(lubridate)
library(maps)
library(ggthemes)
library(babynames)
library(hrbrthemes)
library(viridis)
library(gridExtra)
library(cowplot)

cdata$Date <- as.Date(cdata$Date)


#Reshape data = Combining Cases and Deaths same column
df <- cdata %>%
  filter(Region=="AFRO") %>%
  select(Date, Country, Cases, Deaths) %>%
  pivot_longer(., cols = c(Cases, Deaths), names_to = "Variable", 
               values_to = "Cases")

daily_data <- df %>%
  group_by(Date, Variable) %>%
  summarise(Cases = sum(Cases))

ggplot(daily_data, aes(x=Date, y=Cases)) +
  geom_line(aes(linetype=Variable, color = Variable)) +
  geom_point(aes(linetype=Variable, color = Variable)) +
  ggtitle("Daily Corona Cases & Deaths in Africa") +
  theme_ipsum() +
  xlab('Date') +
  ylab("Number of cases") +
  transition_reveal(Date)

# Save at gif:
anim_save("Daily corona cases & Deaths in Africa by 24 May 2020.gif")


#Adding up the cases per day
cum_cases <- cdata %>%
  filter(Region=="AFRO") %>%
  group_by(Date) %>%
  summarise(Cases = sum(Cases))

#Plotting the Cummulative plots
c <- ggplot(cum_cases, aes(x=Date, y=cumsum(Cases)))+
  geom_line(color='blue') +
  ggtitle('Total Corona Cases  in Africa') +
  ylab("Number of cases")

#Cumulative deaths per day
cum_deaths <- cdata %>%
  filter(Region=="AFRO") %>%
  group_by(Date) %>%
  summarise(Deaths = sum(Deaths))

#Plotting the Cummulative plots
d <- ggplot(cum_deaths, aes(x=Date, y=cumsum(Deaths)))+
  geom_line(color='red') +
  ggtitle('Total Deaths  in Africa due to Corona') +
  ylab('Number of Deaths')

#Combining the plots
plot_grid(c, d)


#Selecting Nigeria, Southh Africa etc
target <- c('Nigeria', 'South Africa', 'Ethiopia', 'Tanzania', 'Sudan', 'Kenya')
fdata <- cdata %>%
  filter(Region=='AFRO', Country %in% target)

#Cumulative deaths per day and by country
fcum_deaths <- fdata %>%
  group_by(Date, Country) %>%
  summarise(Deaths = sum(Deaths))

#Plotting the Cummulative plots for selected countries
ggplot(fcum_deaths, aes(x=Date, y=cumsum(Deaths))) +
  geom_line(aes(linetype=Country, color = Country)) +
  ggtitle('Total Deaths in some selected African countries due to Corona') +
  ylab('Number of Deaths')





