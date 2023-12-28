# libraries
library(shiny)
library(tidyverse)
# library(gapminder)
library(ggplot2)
library(plotly)
library(ggiraph)
library(scales)
library(dplyr)
library(RColorBrewer)
library(shinydashboard)
library(countrycode)

### Path setup ------------------------------------------------------------
#setwd("C:/Users/chiar/Desktop/DataVisualization_code/data_visualisation")

### Data --------------------------------------------------------------------
dataframe <- read_csv("Forest_and_Carbon.csv", show_col_types = FALSE) # readr version

### Clear data --------------------------------------------------------------------
# Remove the objectID column
data <- dataframe %>% select(-(1))

# Add units in the indicator names
data$Indicator <- paste(data$Indicator, data$Unit)

# Remove the non interesting columns, keeping the ISOs, the Countries, the Indicators and the values for each year
data <- data %>% select(-(5:9))

## Change the names of the years: remove the "F" before each year
# Select the names of the columns corresponding to the years
years <- data %>% select(-(1:4)) # remove all the first columns until the year ones
years <- colnames(years)
years <- as.character(as.numeric(sub("F", "", years))) # keep the correct names of the years for later on
names(data)[5:33] <- years # change the names

# Switch the year columns into lines
long_data <- pivot_longer(data, cols = years, names_to = "year", values_to ="value")

# Switch the indicators lines into columns
wide_data <- pivot_wider(long_data, names_from = "Indicator", values_from = "value")


### Getting a column of continents corresponding to the country -----------------------------
# getting the countries in a new data
df <- data.frame(country = wide_data$Country)

# find the correspondent continent for each country
df$Continent <- countrycode(sourcevar = df[, "country"],
                            origin = "country.name",
                            destination = "continent")

# create a new column with the corresponding continents in the clear data
wide_data$Continent <- df$Continent
wide_data <- wide_data %>% relocate(Continent, .before=ISO2) # replace it next to the country column
colnames(wide_data) = gsub(" ", "_", colnames(wide_data))    # replace the spaces by _

### Create the normalized data and adding it to the dataset -----------------------------
# add a new column with normalized data (carbon stocks/forest area, in tonnes/Ha)
wide_data <- mutate(wide_data, Normalized_tonnes_per_Ha = (wide_data$Carbon_stocks_in_forests_Million_tonnes)*1000000/((wide_data$Forest_area_1000_HA)*1000))



### Function to plot the maps of 2020 -------------------------------------------
# Getting the map from ggplot 
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
# head(world_data)

# renaming the unmatched countries between the dataset and the ggplot library
old_names <- c("Afghanistan, Islamic Rep. of","Andorra, Principality of","Armenia, Rep. of",
               "Aruba, Kingdom of the Netherlands","Azerbaijan, Rep. of","Bahamas, The","	Bahrain, Kingdom of",
               "Belarus, Rep. of","British Virgin Islands","China, P.R.: Mainland",
               "Comoros, Union of the", "Congo, Dem. Rep. of the","Croatia, Rep. of","Czech Rep.","Dominican Rep.","Egypt, Arab Rep. of","Equatorial Guinea, Rep. of",
               "Eritrea, The State of","Estonia, Rep. of","Eswatini, Kingdom of","Ethiopia, The Federal Dem. Rep. of","Fiji, Rep. of","Gambia, The",
               "Guiana, French","Iran, Islamic Rep. of","Kazakhstan, Rep. of","Kyrgyz Rep.","Lesotho, Kingdom of","Madagascar, Rep. of","Marshall Islands, Rep. of the",
               "Mauritania, Islamic Rep. of","Micronesia, Federated States of",
               "Moldova, Rep. of","Mozambique, Rep. of","Netherlands, The","North Macedonia, Republic of","Palau, Rep. of",
               "Poland, Rep. of","Russian Federation","San Marino, Rep. of","São Tomé and Príncipe, Dem. Rep. of",
               "Serbia, Rep. of","Slovak Rep.","Slovenia, Rep. of", "Syrian Arab Rep.","Tajikistan, Rep. of","Tanzania, United Rep. of","Timor-Leste, Dem. Rep. of",
               "Türkiye, Rep. of","United States","Uzbekistan, Rep. of","Venezuela, Rep. Bolivariana de","Wallis and Futuna Islands","West Bank and Gaza",
               "Yemen, Rep. of", "United Kingdom", "Congo", "Congo, Rep. of", "Central African Rep.", "South Sudan, Rep. of", "Côte d'Ivoire", "Lao People's Dem. Rep.", 
               "Korea, Dem. People's Rep. of", "Korea, Rep. of")


new_names <- c("Afghanistan","Andorra","Armenia","Aruba","Azerbaijan","Bahamas","Bahrain","Belarus","Virgin Island","China","Comoros",
               "Congo","Croatia","Czech Republic","Dominican Republic","Egypt","Guinea","Eritrea","Estonia","Eswatini","Ethiopia","Fiji","Gambia",
               "French Guiana","Iran","Kazakhstan","Kyrgyzstan","Lesotho","Madagascar","Marshall Islands","Mauritania","Micronesia",
               "Moldova","Mozambique","Netherlands","North Macedonia","Palau","Poland","Russia","San Marino","Sao Tome and Principe",
               "Serbia","Slovakia","Slovenia","Syria","Tajikistan","Tanzania","Timor-Leste","Turkey","USA","Uzbekistan","Venezuela",
               "Wallis and Futuna","Gaza","Yemen", "UK", "Democratic Republic of the Congo", "Republic of Congo", "Central African Republic", 
               "South Sudan", "Ivory Coast", "Laos", "North Korea", "South Korea")

# Change the names of countries in the datset
for (i in 1:length(old_names)){
  wide_data$Country[wide_data$Country == old_names[i]] <- new_names[i]
}


# Adding the ISO3 code to the ggplot library
world_data["ISO3"] <- wide_data$ISO3[match(world_data$region, wide_data$Country)]

# Getting info for year 2020
df_2020 <-  wide_data[wide_data$year == "2020", ]
df <- df_2020 # changing the name for fitting
df <- df[!is.na(df$Continent), ] # removing continents

# Function to print maps for the 2 indicators wanted in 2020
worldMaps <- function(df, world_data, indicator){
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.text = element_text(size = 0),
                       axis.title = element_text(size = 0),
                       strip.text = element_text(size = 14),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Select for each Country the value of carbon stocks and the ISO3
  plotdf <- df[, c("Country", indicator, "ISO3")]
  plotdf <- plotdf[!is.na(plotdf[[indicator]]), ]
  plotdf[[indicator]][plotdf[[indicator]]==0] <- 0.00001
  
  # Create a color palette for each unique country
  country_palette <- scales::hue_pal()(length(unique(plotdf$Country)))
  
  # Map each country to a unique color using scale_fill_manual
  color_mapping <- scale_fill_manual(values = setNames(country_palette, unique(plotdf$Country)))
  
  # Add the indicator to the geographical world data
  world_data["Index_to_plot"] <- plotdf[[indicator]][match(world_data$ISO3, plotdf$ISO3)]
  
  # Create caption with the data source to show underneath the map
  if (indicator == "Carbon_stocks_in_forests_Million_tonnes"){
    colours <- brewer.pal(6, "Greens")
    breaks <- scales::trans_breaks("log10", function(x) 10^x)(c(1, 1e5))
    trans <- "log10"
    title <- "Carbon stocks (million Tonnes)"
    unit <- "million tonnes"
    labels <- trans_format("log10", math_format(10^.x))
    limits <- c(1, max(breaks))
  }
  else{
    colours <- brewer.pal(9, "Greens")
    breaks <- c(min(plotdf[[indicator]], na.rm=TRUE), max(plotdf[[indicator]], na.rm=TRUE))
    trans <- "identity"
    title <- "Carbon stocks per forest area (tonnes/Ha)"
    unit <- "tonnes/Ha"
    labels <- scales::label_number(scale = 1)
    limits <- c(min(breaks), max(breaks))
  }
  
  # Plot
  g <- ggplot() + 
    geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Index_to_plot, group=group,
                                 tooltip = sprintf("Country: %s<br/>Index: %s %s", region, round(Index_to_plot), unit))) + 
    scale_fill_gradientn(colours = colours, 
                         na.value = 'grey' , 
                         breaks = breaks,
                         trans = trans,
                         labels = labels,
                         limits= limits
    ) + 
    scale_y_continuous(limits = c(-115, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) + 
    labs(fill = "Legend: Description") +
    guides(fill = guide_colorbar(
      title = title,
      label = TRUE,                # view of the values in the legend
      label.position = "bottom",   # position of the labels
    )) +
    my_theme()
  
  return(g)
}

# To plot the maps outside of the dashboard
# mappa <- worldMaps(df, world_data, "Carbon_stocks_in_forests_Million_tonnes")
# mappa2 <- worldMaps(df, world_data, "Normalized_tonnes_per_Ha")
# print(mappa)
# ggsave(filename = "map_2020_carbon_stocks.png", path = "../Graphs/")
# print(mappa2)
# ggsave(filename = "map_2020_carbon_stocks_normalised.png", path = "../Graphs/")

### Function to plot the continent line charts -----------------------------
continent_linechart <- function(continent, i){
  continent_name <- wide_data %>% filter(Continent==continent)
  continent_name <- subset(continent_name,!is.na(Carbon_stocks_in_forests_Million_tonnes))
  # continent_name <- subset(continent_name,!is.na(Normalized_tonnes_per_Ha))
  continent_above_8000 <- subset(continent_name,!Carbon_stocks_in_forests_Million_tonnes<8000)
  
  plots <- list()
  
  if (continent == "Americas"){
    plots[[i]] <- ggplot(continent_name, aes(x = year, y = Carbon_stocks_in_forests_Million_tonnes, group = Country)) +
      geom_line(aes(color = ifelse(Carbon_stocks_in_forests_Million_tonnes < 8000, "Below 8000", Country)), alpha = 0.5, size = 1) +
      geom_hline(yintercept = 8000, linetype = "dashed", color = "black", alpha = 0.7) +  # add the dotted line of threshold
      scale_color_manual(name = "", values = c("#B2B2B2", "#FF9933", "#C9A0DC", "#990066", "#99CBFF"),
                         labels = c("Other countries (below 8000)", 
                                    unique(continent_above_8000$Country)[1], 
                                    unique(continent_above_8000$Country)[2], 
                                    unique(continent_above_8000$Country)[3], 
                                    unique(continent_above_8000$Country)[4])) +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(),
            axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(breaks = seq(1995, 2020, 5)) +
      scale_y_continuous(limits = c(0, 60000)) +
      labs(title = "Carbon stocks in American countries through the years", x = "Years", y = "Carbon stocks (million tonnes)") +
      theme(legend.position = "right")
  }
  else if (continent == "Africa"){
    plots[[i]] <- ggplot(continent_name, aes(x = year, y = Carbon_stocks_in_forests_Million_tonnes, group = Country)) + 
      geom_line(aes(color = ifelse(Carbon_stocks_in_forests_Million_tonnes < 8000, "Below 8000", Country)), alpha = 0.5 , size = 1) +
      geom_hline(yintercept = 8000, linetype = "dashed", color = "black", alpha = 0.7) +
      scale_color_manual(name = "", values = c("#B2B2B2", "#D1E231"),
                         labels = c("Other countries (below 8000)", 
                                    unique(continent_above_8000$Country)[1], 
                                    unique(continent_above_8000$Country)[2], 
                                    unique(continent_above_8000$Country)[3], 
                                    unique(continent_above_8000$Country)[4])) +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
            axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(breaks = seq(1995, 2020, 5)) +
      scale_y_continuous(limits = c(0, 60000)) +
      labs(title = "Carbon stocks in African countries through the years", x = "Years", y = "Carbon stocks (million tonnes)") +
      theme(legend.position = "right")
  }
  else if (continent == "Oceania"){
    plots[[i]] <- ggplot(continent_name, aes(x = year, y = Carbon_stocks_in_forests_Million_tonnes, group = Country)) + 
      geom_line(aes(color = ifelse(Carbon_stocks_in_forests_Million_tonnes < 8000, "Below 8000", Country)), alpha = 0.5, size = 1) +
      geom_hline(yintercept = 8000, linetype = "dashed", color = "black", alpha = 0.7) +
      scale_color_manual(name = "", values = c("#B20000" ,"gray"),
                         labels = c(unique(continent_above_8000$Country)[1],"Other countries (below 8000)")) +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
            axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(breaks = seq(1995, 2020, 5)) +
      scale_y_continuous(limits = c(0, 60000)) +
      labs(title = "Carbon stocks in Oceanian countries through the years", x = "Years", y = "Carbon stocks (million tonnes)") +
      theme(legend.position = "right")
  }
  else if (continent == "Europe"){
    plots[[i]] <- ggplot(continent_name, aes(x = year, y = Carbon_stocks_in_forests_Million_tonnes, group = Country)) + 
      geom_line(aes(color = ifelse(Carbon_stocks_in_forests_Million_tonnes < 8000, "Below 8000", Country)), alpha = 0.5, size = 1) +
      geom_hline(yintercept = 8000, linetype = "dashed", color = "black", alpha = 0.7) +
      scale_color_manual(name = "", values = c("#B2B2B2", "#6495ED"),
                         labels = c("Other countries (below 8000)", 
                                    unique(continent_above_8000$Country)[1], 
                                    unique(continent_above_8000$Country)[2], 
                                    unique(continent_above_8000$Country)[3], 
                                    unique(continent_above_8000$Country)[4])) +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
            axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(breaks = seq(1995, 2020, 5)) +
      scale_y_continuous(limits = c(0, 60000)) +
      labs(title = "Carbon stocks in European countries through the years", x = "Years", y = "Carbon stocks (million tonnes)") +
      theme(legend.position = "right")
  }
  else if (continent == "Asia"){
    plots[[i]] <- ggplot(continent_name, aes(x = year, y = Carbon_stocks_in_forests_Million_tonnes, group = Country)) + 
      geom_line(aes(color = ifelse(Carbon_stocks_in_forests_Million_tonnes < 8000, "Below 8000", Country)), alpha = 0.5, size = 1) +
      geom_hline(yintercept = 8000, linetype = "dashed", color = "black", alpha = 0.7) +
      scale_color_manual(name = "", values = c("gray", "#B20000","#0F52BA"),
                         labels = c("Other countries (below 8000)", 
                                    unique(continent_above_8000$Country)[1], 
                                    unique(continent_above_8000$Country)[2], 
                                    unique(continent_above_8000$Country)[3], 
                                    unique(continent_above_8000$Country)[4])) +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
            axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(breaks = seq(1995, 2020, 5)) +
      scale_y_continuous(limits = c(0, 60000)) +
      labs(title = "Carbon stocks in Asian countries through the years", x = "Years", y = "Carbon stocks (million tonnes)") +
      theme(legend.position = "right")
  }
}


### Function to plot the continent line charts with normalized data -----------------------------
continent_linechart_normalized <- function(continent, i){
  continent_name <- wide_data %>% filter(Continent==continent)
  # continent_name <- subset(continent_name,!is.na(Carbon_stocks_in_forests_Million_tonnes))
  continent_name <- subset(continent_name,!is.na(Normalized_tonnes_per_Ha))
  
  #select the threshold
  continent_above_perc <- subset(continent_name,!Normalized_tonnes_per_Ha<140)
  
  plots <- list()
  
  if (continent == "Americas"){
    plots[[i]] <- ggplot(continent_name, aes(x = year, y = Normalized_tonnes_per_Ha, group = Country)) + 
      geom_line(aes(color = ifelse(Normalized_tonnes_per_Ha < 140, "Below 140", Country)), alpha = 0.5, size = 1) +
      geom_hline(yintercept = 140, linetype = "dashed", color = "black", alpha = 0.7) +
      scale_color_manual(name = "", 
                         values = c("gray", scales::hue_pal()(length(unique(continent_above_perc$Country)))),
                         labels = c("Other countries (below 140)", unique(continent_above_perc$Country))) +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
            axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(breaks = seq(1995, 2020, 5)) +
      scale_y_continuous(limits = c(0, 320)) +
      labs(title = "Carbon stocks per forest area in American countries", x = "Years", y = "Carbon stocks per forest area (tonnes/Ha)") +
      theme(legend.position = "right") 
      # ggsave(filename="normalized_america_throught_years.png", path="../Graphs/")
  }
  else if (continent == "Africa"){
    plots[[i]] <- ggplot(continent_name, aes(x = year, y = Normalized_tonnes_per_Ha, group = Country)) + 
      geom_line(aes(color = ifelse(Normalized_tonnes_per_Ha < 140, "Below 140", Country)), alpha = 0.5, size = 1) +
      geom_hline(yintercept = 140, linetype = "dashed", color = "black", alpha = 0.7) +
      scale_color_manual(name = "", 
                         values = c("gray", scales::hue_pal()(length(unique(continent_above_perc$Country)))),
                         labels = c("Other countries (below 140)", unique(continent_above_perc$Country))) +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
            axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(breaks = seq(1995, 2020, 5)) +
      scale_y_continuous(limits = c(0, 320)) +
      labs(title = "Carbon stocks per forest area in African countries", x = "Years", y = "Carbon stocks per forest area (tonnes/Ha)") +
      theme(legend.position = "right")
    # ggsave(filename="normalized_africa_throught_years.png", path="../Graphs/")
  }
  else if (continent == "Oceania"){
    plots[[i]] <- ggplot(continent_name, aes(x = year, y = Normalized_tonnes_per_Ha, group = Country)) + 
      geom_line(aes(color = ifelse(Normalized_tonnes_per_Ha < 140, "Below 140", Country)), alpha = 0.5, size = 1) +
      geom_hline(yintercept = 140, linetype = "dashed", color = "black", alpha = 0.7) +
      scale_color_manual(name = "", 
                         values = c("gray", scales::hue_pal()(length(unique(continent_above_perc$Country)))),
                         labels = c("Other countries (below 140)", unique(continent_above_perc$Country))) +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
            axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(breaks = seq(1995, 2020, 5)) +
      scale_y_continuous(limits = c(0, 320)) +
      labs(title = "Carbon stocks per forest area in Oceanian countries", x = "Years", y = "Carbon stocks per forest area (tonnes/Ha)") +
      theme(legend.position = "right")
    # ggsave(filename="normalized_oceania_throught_years.png", path="../Graphs/")
  }
  else if (continent == "Europe"){
    plots[[i]] <- ggplot(continent_name, aes(x = year, y = Normalized_tonnes_per_Ha, group = Country)) + 
      geom_line(aes(color = ifelse(Normalized_tonnes_per_Ha < 140, "Below 140", Country)), alpha = 0.5, size = 1) +
      geom_hline(yintercept = 140, linetype = "dashed", color = "black", alpha = 0.7) +
      scale_color_manual(name = "", 
                         values = c("gray", scales::hue_pal()(length(unique(continent_above_perc$Country)))),
                         labels = c("Other countries (below 140)", unique(continent_above_perc$Country))) +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
            axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(breaks = seq(1995, 2020, 5)) +
      scale_y_continuous(limits = c(0, 320)) +
      labs(title = "Carbon stocks per forest area in European Countries", x = "Years", y = "Carbon stocks per forest area (tonnes/Ha)") +
      theme(legend.position = "right")
    # ggsave(filename="normalized_europe_throught_years.png", path="../Graphs/")
  }
  else if (continent == "Asia"){
    plots[[i]] <- ggplot(continent_name, aes(x = year, y = Normalized_tonnes_per_Ha, group = Country)) + 
      geom_line(aes(color = ifelse(Normalized_tonnes_per_Ha < 140, "Below 140", Country)), alpha = 0.5, size = 1) +
      geom_hline(yintercept = 140, linetype = "dashed", color = "black", alpha = 0.7) +
      scale_color_manual(name = "", 
                         values = c("gray", scales::hue_pal()(length(unique(continent_above_perc$Country)))),
                         labels = c("Other countries (below 140)", unique(continent_above_perc$Country))) +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), 
            axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(breaks = seq(1995, 2020, 5)) +
      scale_y_continuous(limits = c(0, 320)) +
      labs(title = "Carbon stocks per forest area in Asian countries", x = "Years", y = "Carbon stocks per forest area (tonnes/Ha)") +
      theme(legend.position = "right")
    # ggsave(filename="normalized_asia_throught_years.png", path="../Graphs/")
  }
}

### Function to plot the average of carbon stocks for each continent ---------------------
### Function to plot the pie chart ----------------------
pie_chart <- function(){
  pie_data <- wide_data[!is.na(wide_data$Continent),] # remove continents
  pie_data <- pie_data[!is.na(pie_data$Carbon_stocks_in_forests_Million_tonnes),] # remove the NA values from carbon stocks
  
  avarage_carbon_stock_world <- pie_data %>% group_by(Country) %>% summarize(Avg_Carbon_stock = mean(Carbon_stocks_in_forests_Million_tonnes))
  avarage_carbon_stock_world <- avarage_carbon_stock_world %>% arrange(desc(Avg_Carbon_stock))

  # summing all average of the not relevant countries (i.e. from Peru to above)
  sum_not_relevant_country <- avarage_carbon_stock_world %>%
    filter(Avg_Carbon_stock < avarage_carbon_stock_world$Avg_Carbon_stock[5]) %>%
    summarise(Avg_Carbon_stock = sum(Avg_Carbon_stock)) %>%
    mutate(Country = "All Others Countries") %>%
    select(Country, Avg_Carbon_stock)
  
  # adding to the new dataset 
  avarage_carbon_stock_world <- head(avarage_carbon_stock_world, 5)
  avarage_carbon_stock_world <- rbind(avarage_carbon_stock_world, sum_not_relevant_country)
  
  # Plot
  ggplot(avarage_carbon_stock_world, aes(x = "", y = Avg_Carbon_stock, fill = Country)) +
    geom_bar(stat = "identity", width = 1, color = "white") + coord_polar(theta = "y") +
    geom_text(aes(label = percent(Avg_Carbon_stock/sum(Avg_Carbon_stock))), position = position_stack(vjust = 0.5)) +
    labs(title = "Proportion of carbon stocks in average by the 5 best countries") + # old title: Average of carbon stocks through the years
    scale_fill_manual(values = c("#B2B2B2","#99CBFF", "#C9A0DC", "#D1E231", "#6495ED","#FF9933"),
                      breaks = rev(unique(avarage_carbon_stock_world$Country))) +
    guides(fill = guide_legend(reverse = TRUE)) +  # Invert the order of the legend
    theme_void()
}


### Function to plot the pie chart with normalized data ----------------------
pie_chart_normalized <- function(){
  pie_data<-wide_data[!is.na(wide_data$Continent),]
  pie_data<-pie_data[!is.na(pie_data$Carbon_stocks_in_forests_Million_tonnes),] # !!! indicator?
  
  #Find the best 5 countries according to the normalization
  avarage_percentage_world <- pie_data %>%
    group_by(Country) %>%
    summarize(Avg_percentage = mean(Normalized_tonnes_per_Ha))

  avarage_percentage_world <- avarage_percentage_world %>% arrange(desc(Avg_percentage))

  Best_5_normalized <- head(avarage_percentage_world, 5)

  #Calculate the average carbon stocks of all countries
  avarage_carbon_stock_world <- pie_data %>%
    group_by(Country) %>%
    summarize(Avg_Carbon_stock = mean(Carbon_stocks_in_forests_Million_tonnes))

  # Create the dataset to plot the 5 best countries
  to_plot <- avarage_carbon_stock_world %>% filter(Country %in% Best_5_normalized$Country)
  
  # Sum all the average carbon stocks of the best 5 countries
  sum_relevant_country <- avarage_carbon_stock_world %>%
    filter((Country %in% Best_5_normalized$Country)) %>%
    summarise(Avg_Carbon_stock = sum(Avg_Carbon_stock)) %>%
    mutate(Country = "Guadeloupe, Guyana, Micronesia, Palau and Suriname") %>%
    select(Country, Avg_Carbon_stock)
  
  # Sum all the average carbon stocks for countries not in the best normalized 5
  sum_not_relevant_country <- avarage_carbon_stock_world %>%
    filter(!(Country %in% Best_5_normalized$Country)) %>%
    summarise(Avg_Carbon_stock = sum(Avg_Carbon_stock)) %>%
    mutate(Country = "All Others Countries") %>%
    select(Country, Avg_Carbon_stock)
  
  to_plot <- rbind(sum_relevant_country, sum_not_relevant_country)

  # Plot
  ggplot(to_plot, aes(x = "", y = Avg_Carbon_stock, fill = Country)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(aes(label = percent(Avg_Carbon_stock/sum(Avg_Carbon_stock))), position = position_stack(vjust = 0.5)) +
    labs(title = "Proportion of carbon stocks in average by the 5 best countries") + # old title: Ratio of carbon stocks of countries with higher carbon stock density on forest area to all others
    scale_fill_manual(values = c("#B2B2B2","#FF9933"), breaks = rev(unique(to_plot$Country))) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_void()
  
  # ggsave(filename = "pie_chart_of_normalized_best_5.png", path = "../Graphs/")
}

### Function to plot the bar chart ----------------------------------
bar_chart <- function(){
  # For indicator carbon stocks in forests --------------------------------------------------------
  filtered_data <- wide_data[wide_data$year %in% c(1992, 2020) & wide_data$Country %in% c("Russia", "Brazil", "Canada", "USA","Democratic Republic of the Congo"), ]
  
  # Create a new column 'country_year' using interaction
  filtered_data$country_year <- interaction(filtered_data$Country, factor(filtered_data$year))
  filtered_data$Country[filtered_data$Country == 'Democratic Republic of the Congo'] <- 'Dem. Rep. of Congo'
  
  # Colors depending on increase (green) or decrease (red) of carbon stocks between 1992 and 2020
  custom_colors <- c("Brazil.1992" = "#800000", "Canada.1992" = "#800000", "Russia.1992" = "#5aa17f", "USA.1992" = "#5aa17f",
                     "Brazil.2020" = "red", "Canada.2020" = "red", "Russia.2020" = "green", "USA.2020" = "green","Democratic Republic of the Congo.1992" = "#800000","Democratic Republic of the Congo.2020" = "red")
  
  # Plot
  ggplot(filtered_data, aes(x = Country, y = Carbon_stocks_in_forests_Million_tonnes, fill = country_year)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_text(aes(label=year), position=position_dodge(width = 0.7), vjust=-0.25)+
    scale_fill_manual(values=custom_colors) +  # Specify custom colors
    
    labs(title = "1992 vs 2020 carbon stocks for the 5 best countries", # old title: Carbon stocks comparison (1992 vs 2020)
         x = "Country",
         y = "Carbon stocks (million tonnes)",
         fill = "year") +
    theme(panel.background = element_blank(), 
          axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust=1),
          legend.position = "none", axis.line.x =  element_blank())
}


### Function to plot the bar chart with normalized data ----------------------------------
bar_chart_normalized <- function(indic){
  # For indicator carbon stocks in forests --------------------------------------------------------
  filtered_data <- wide_data[wide_data$year %in% c(1992, 2020) & wide_data$Country %in% c("Guadeloupe", "Guyana", "Micronesia", "Suriname", "Palau"), ]
  
  # Create a new column 'country_year' using interaction
  filtered_data$country_year <- interaction(filtered_data$Country, factor(filtered_data$year))
  # filtered_data$Country[filtered_data$Country == 'Democratic Republic of the Congo'] <- 'Dem. Rep. of Congo'
  
  # Colors depending on increase (green) or decrease (red) of carbon stocks between 1992 and 2020
  # To change order and colours for the "neutral" ones
  custom_colors <- c("Guadeloupe.1992" = "#5aa17f", "Guyana.1992" = "#5aa17f", "Suriname.1992" = "#5aa17f", "Palau.1992" = "#5aa17f", "Micronesia.1992" = "#800000",
                     "Guadeloupe.2020" = "green", "Guyana.2020" = "green", "Suriname.2020" = "green", "Palau.2020" = "green", "Micronesia.2020" = "red")
  
  plots <- list()
  
  ### Plots
  # Carbon stocks per forest area 
  if (indic == "Carbon"){
    plots[[1]] <- ggplot(filtered_data, aes(x = Country, y = Normalized_tonnes_per_Ha, fill = country_year)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_text(aes(label=year), position=position_dodge(width = 0.7), vjust=-0.25)+
      scale_fill_manual(values=custom_colors) +  # Specify custom colors
      
      labs(title = "1992 vs 2020 carbon stocks per forest area for the 5 best countries", # old title: Carbon stocks comparison (1992 vs 2020)
           x = "Country",
           y = "Carbon stocks per forest area (tonnes/Ha)",
           fill = "year") +
      theme(panel.background = element_blank(),
            axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust=1),
            legend.position = "none", axis.line.x =  element_blank())
  }
  # Carbon stocks with the scale of the 5 other best countries
  else if (indic == "Normalized"){
    plots[[2]] <- ggplot(filtered_data, aes(x = Country, y = Carbon_stocks_in_forests_Million_tonnes, fill = country_year)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_text(aes(label=year), position=position_dodge(width = 0.7), vjust=-0.25)+
      scale_fill_manual(values=custom_colors) +  # Specify custom colors
      ylim(0,60000)+
      labs(title = "1992 vs 2020 carbon stocks for the 5 best countries", # old title: Carbon stocks comparison (1992 vs 2020)
           x = "Country",
           y = "Carbon stocks (million tonnes)",
           fill = "year") +
      theme(panel.background = element_blank(),
            axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust=1),
            legend.position = "none", axis.line.x =  element_blank())
  }
}

# To plot outside of the dashboard
# bar_chart_normalized("Carbon")
# ggsave(filename = "bar_chart_of_best_5.png", path = "../Graphs/")
# 
# bar_chart_normalized("Normalized")
# ggsave(filename = "bar_chart_of_normalized_best_5.png", path = "../Graphs/")

### Dashboard introduction ---------------------------------
# Create a new data without coninents values
wide_data_2 <- subset(wide_data,!is.na(Continent)) # remove the continents

# Create a data with the names of variables and their corresponding units (useful in some functions)
indicators <- c("Carbon_stocks_in_forests_Million_tonnes",
                "Forest_area_1000_HA",
                "Index_of_carbon_stocks_in_forests_Index",
                "Index_of_forest_extent_Index", 
                "Land_area_1000_HA", 
                "Share_of_forest_area_Percent",
                "Normalized_tonnes_per_Ha")
units <- c("(Million tonnes)", "(1000Ha)", "", "", "(1000Ha)", "%", "(Million tonnes/1000Ha)")
table_unit <- data.frame(indicators, units)

### Define UI for the world global line chart -------------------------------
global_line_ui <- fluidPage(
  # Application title
  titlePanel("Evolution of the world carbon stocks"),
  # Main panel with line chart
  mainPanel(
    plotOutput(outputId = "globalLine"),
    htmlOutput(outputId = "globalLineDescr")
  )
)

### Define UI for the maps --------------------------------
map_ui <- fluidPage(
  titlePanel("World status in 2020"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "data_type",
                  label = "Choose the type Indicator:",
                  choices = c("Carbon stocks in forest (Million tonnes)",
                              "Carbon stocks per forest area (Tonnes/Ha)"
                              # "Forest area",
                              # "Index of carbon stocks in forests",
                              # "Index of forest extent",
                              # "Land area",
                              # "Share of forest area"
                  )),
    ),
    mainPanel(
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      girafeOutput("distPlot", width = "100%", height = "100%"),
     
    )
  ),
  fluidRow(
    column(12,
           htmlOutput(outputId = "WorldMapDescr")  # Moved htmlOutput outside of mainPanel
    )
  )
)


### Define UI for the continent line charts -----------------------
# choice of indicator on the top horizontally
continents_line_ui <- fluidPage(
  titlePanel("Carbon stocks by continents among time"),
  fluidRow(
    column(12,
           tabsetPanel(
             tabPanel("Carbon stocks",
                      fluidPage(
                        mainPanel(
                          uiOutput("plots1"),
                            htmlOutput(outputId = "textNotNormalized")
  
                        )
                      )
             ),
             tabPanel("Carbon stocks per forest area",
                      fluidPage(
                        mainPanel(
                          uiOutput("plots2"),
                            htmlOutput(outputId = "textNormalized")
                          
                        )
                      )
             )
             
           )
    )
  )
)



### Define UI for the pie chart -------------------------
pie_chart_ui <- fluidPage(
  titlePanel("Proportional carbon stock dominance of the best 5 countries"),
  fluidRow(
    column(12,
           tabsetPanel(
             tabPanel("Carbon stocks",
                      fluidPage(
                          mainPanel(
                            plotOutput("pie1"),
                            htmlOutput(outputId = "pieChart1Description")
                          )
                        
                      )),
             tabPanel("Carbon stocks per forest area",
                      fluidPage(
                          mainPanel(
                            plotOutput("pie2"),
                            htmlOutput(outputId = "pieChart2Description")
                          )
                        
                      ))
           )
    )
  )
)

### Define UI for the bar chart ----------------------
compere_1992_2020_ui <- fluidPage(
  titlePanel = "1992 vs 2020 carbon stocks in 5 best countries",
  fluidRow(
    column(12,
           tabsetPanel(
             tabPanel("Carbon stocks",
                      fluidPage(
                       
                          mainPanel(
                            plotOutput("bar1"),
                            htmlOutput(outputId = "BarNotNormalized")
                          )
                        
                      )),
             tabPanel("Carbon stocks per forest area",
                      fluidPage(
                        
                          mainPanel(
                            uiOutput("bar2"),
                            htmlOutput(outputId = "BarNormalized")
                          )
                        
                      ))
           )
    )
  )
)

### Define UI for the bubble chart -----------------------------
bubble_ui <- fluidPage(
  # Application title
  titlePanel("Bubble Chart"),
  
  # Lateral panel with controls to select the variables
  sidebarLayout(
    sidebarPanel(
      # Selector for x variable
      selectInput(inputId = "xvar",
                  label = "Variable for x-axis",
                  choices = c("Carbon stocks (million tonnes)", 
                              "Forest area (1000 Ha)", 
                              "Index of carbon stocks", 
                              "Index of forest extent", 
                              "Land area (1000 Ha)", 
                              "Share of forest area (%)"),
                  selected = "Forest area (1000 Ha)"),
      
      # Selector for y variable
      selectInput(inputId = "yvar",
                  label = "Variable for y-axis",
                  choices = c("Carbon stocks (million tonnes)", 
                              "Forest area (1000 Ha)", 
                              "Index of carbon stocks", 
                              "Index of forest extent", 
                              "Land area (1000 Ha)", 
                              "Share of forest area (%)"),
                  selected = "Carbon stocks (million tonnes)"),
      
      # Selector for size
      selectInput(inputId = "bsize",
                  label = "Size",
                  choices = c("Carbon stocks (million tonnes)", 
                              "Forest area (1000 Ha)", 
                              "Index of carbon stocks", 
                              "Index of forest extent", 
                              "Land area (1000 Ha)", 
                              "Share of forest area (%)"),
                  selected = "Share of forest area (%)"),
    ),
    
    # Main panel with bubble chart and description
    mainPanel(
      plotlyOutput(outputId = "bubbleplot"),
      htmlOutput(outputId = "BubbleDescription")
    )
  )
)




### Define UI for home tab -----------------------
home_ui <- function() {
  fluidPage(
    titlePanel("Home Page"),
    
    fluidRow(
      column(12, h3("Welcome to the world of forests!")),
      column(12, p("We are Chiara, Marie, Andrea and Mateusz and we are group 7 of the Data Visualization class.")),
      column(12, p("In this dashboard, we will tell you a story about forests in the world and their capacity to absorb CO2 (carbon dioxide)."))
    ),
    
    fluidRow(
      column(12, h3("Introduction")),
      column(12, p("If you did not know it before, trees have an amazing property: they are breathing CO2 while releasing O2 (oxygen). In other words, 
                   the exact opposite of humans and animals. Isn’t it beautiful? Therefore, forest ecosystems are extremely important to keep stability 
                   in the atmosphere of planet Earth. Not to mention the current global warming due to human activity these last decades, you can guess 
                   how urgent it is to preserve forests in order to build a sustainable world for tomorrow. More than global warming, we can talk about 
                   climate disruption, leading to more natural disasters than there were in the past. Among them, you have probably heard about the 
                   repeated wildfires happening everywhere in the world, but mostly in the biggest well-known forest, such as Canada, Brazil, Australia, 
                   Russia. But wildfires are also happening in smaller countries everywhere in the world, like in Greece or Spain.")),
      column(12, p("All of this has led to a feedback loop. While CO2 is increasing due to human activity, the new drier conditions are creating more 
                   fragile forests that are fire-prone. The fire season is becoming longer, releasing all the more CO2 and so maintaining global warming
                   (J. MacCarthy, 2023).")),
      column(12, p("One of the many challenges this century is facing is to control this increase of temperature to not jeopardize life on earth. 
                   To do so, there are a lot of actions to take. Regarding the role of forests in the world, the issue to maintain the balance between O2 
                   and CO2 is to protect forests and to plant new trees.")),
      column(12, p("In this project, the aim is to study the capacity of forests from all over the world to stock CO2. We want to find the most ‘efficient’ 
                   forests to protect them or even try to use their properties (climate, leaves etc) to create new ones and protect other ones which can 
                   have similar properties."))
    ),
    
    fluidRow(
      column(12, h3("Our dataset")),
      column(12, p("Our dataset is taken from the ‘Food and Agriculture Organization of the United Nations (FAO), 2022’:", 
                  a("https://climatedata.imf.org/datasets/66dad9817da847b385d3b2323ce1be57_0/about"))),
      column(12, p("This data set includes six indicators, each provided with its respective unit, for all countries. It also provides mean 
                   values for continents and a mean world value. Useful information in this dataset is also the ISO2 and 
                   ISO3 of the countries and continents. The 6 indicators are mentioned below:")),
      column(12, p("- Carbon stocks in forests, in Million tonnes")),
      column(12, p("- Forest area, in 1000 Ha")),
      column(12, p("- Share of forest area, in %")),
      column(12, p("- Land area, in 1000 Ha")),
      column(12, p("- Index of forest extent")),
      column(12, p("- Index of carbon stocks in forests"))
   ),
    
    fluidRow(
      column(12, h3("About the story-telling")),
      column(12, p("This story is mostly using the first three indicators. The ‘share of forest area’ is a part of the 15th 
                   SDG (Sustainable Development Goal) 'Life on land' given by the United Nations. This indicator is 
                   exactly the sub-goal 15.1.1 which is 'Forest area as a proportion of total land area' and is measured in a percentage.")),
      column(12, p("The story telling of this dashboard is about the efficiency of forests in the world. We define 2 ways to look at efficiency. 
                   First, the efficiency can be the quantity of carbon stocks in forest. But with that definition, the bigger forests are obviously winning. 
                   In order to compare all the forests equally, we wanted to normalize the data. The efficiency therefore can be seen as the quantity of 
                   carbon stocks per forest area. For each definition of efficiency, we are trying to find the best forests in the world."))
    ),
   
   fluidRow(
     column(12, h3("What to find inside the dashboard")),
     column(12, p("A small description will also be provided inside each page, as well as a short analysis of the graphs coming with a part of the story telling.")),
     column(12, h4("Mean world carbon stocks from 1992 to 2020")),
     column(12, p("This is a time-line showing the evolution of the average carbon stocks in the whole world between 1992 and 2020.
                  The idea is to get an overview of the global carbon stocks, in order to see if it has increased or decreased in 28 years.
                  We provided explanations about the changes at some precise points.")),
     column(12, h4("Carbon stocks in the world in 2020")),
     column(12, p("This is a map of the world showing the carbon stocks by countries in 2020.
                  There is the option to select the normalized carbon stocks (carbon stocks per forest area).
                  The idea is to have an easy view at the most efficient countries in terms of carbon stocks (normalized and not normalized).
                  The maps are interactive, you can see the values of each country by hovering on it with the mouse pointer.")),
     column(12, h4("Carbon stocks by continent among time")),
     column(12, p("These are line charts representing the evolution of carbon stocks continent by continent.
                  On this page, you have the choice between normalized and not normalized data. 
                  For each case, each graph contains one line per country. As there are many countries, we decided to apply a threshold 
                  in order to highlight only the relevant one and show how less the other countries are in comparison.
                  From these graphs we selected the 5 best countries (with the higher values) in either case.")),
     column(12, h4("Proportional carbon stock dominance of the best 5 countries")),
     column(12, p("This is a pie-chart hilighting the quantity of carbon stocks by each one of the 5 best countries selected in the previous section.
                  In both case (normalized and non normalized data), the quantity of carbon stock by the forest is represented (and not carbon stocks per forest area). 
                  The idea is to highlight how much carbon stocks the most efficient forests
                  as defined before are representing compare to the whold world carbon stocks.")),
     column(12, h4("Firsts forests carbon stocks un 1992 vs 2020")),
     column(12, p("This is a bar chart comparing the carbon stocks by the 5 best countries in 1992 and 2020.
                  There is the choice between the best countries coming from non normalized data or normalized ones.
                  The idea is to look at the global evolution of the most efficient forests over the period of time of the data.
                  The wanted evolution is an increase of carbon stocks.")),
     column(12, h4("Correlation between carbon stocks and forest area")),
     column(12, p("This is a bubble chart trying to find a link between carbon stocks and forest area.
                  There is the choice of indicator for each axe and for the size of the bubbles.
                  But in the story telling we are focusing on the carbon stocks over the forest area with the share of forest area as the bubble sizes.
                  This is also an animated graph. By playing it, you can see the evolution of the indicators over from 1992 to 2020.
                  This is also an interactive graph. Put the mouse on the bubbles to get the values from indicators."))
    ),
    
    fluidRow(
      column(12, h3("Come with us, let’s walk inside the forest story!")),
      # column(12, p("Conclude the home page with any closing remarks or summary."))
    ),
   
   fluidRow(
     column(12, h3("Link to the resources")),
     column(6, p("Link to the dashboard webpage:", a("https://mf2r1h-andrea0munari.shinyapps.io/data_visualisation/"))),
     column(6, p("Link to the report: "), a("https://github.com/latilmarie/data_visualisation/blob/main/DataVisualization_report.pdf"))
   ),
   
   fluidRow(
     column(12, h3("References of this page")),
     column(12, p("James MacCarthy, Jessica Richter et al., 2023. 
                  The Latest Data Confirms: Forest Fires Are Getting Worse.
                  World Resources Institude", a("https://www.wri.org/insights/global-trends-forest-fires")))
   ),
    
    mainPanel(
      plotOutput("introduction")
    )
  )
}

# home_ui <- fluidPage(
#   titlePanel("Home Page"),
#   fluidPage(
#     fluidRow(
#       column(width = 12, 
#              verbatimTextOutput("scrolling_text")
#       )
#     )
#   )
# )

### Define the server -----------------------------
server <- function(input, output, session) {
  # ### Render the scrolling text
  output$introduction <- renderImage({
    list(src = "forest-design.jpg", width = "600px", height = "312px")
  }, deleteFile = FALSE)
  
  ### Create the global world line chart
  output$globalLine <- renderPlot({
    world <- wide_data %>% filter(Country=="World")
    
    ggplot(world, aes(x=year, y=Carbon_stocks_in_forests_Million_tonnes, group=Country)) + 
      geom_line() +
      annotate("text", x = 17, y = 295100, label = 1, color="blue", size=7, hjust="left") +
      annotate("segment", x = 18, xend = 19, y = 295000, yend = 293700, colour = "blue", linewidth=2, alpha=0.6, arrow=arrow()) +
      annotate("text", x = 23, y = 296100, label = 2, color="red", size = 7, hjust ="left") +
      annotate("segment", x = 24, xend = 25, y = 296000, yend = 294800, colour = "red", linewidth=2, alpha=0.6, arrow=arrow()) +
      # theme_bw() +
      theme(panel.background = element_blank(), panel.grid.major = element_line("grey", linewidth = 0.1), 
            axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust=1))+ #, legend.background = element_blank()) +
      scale_x_discrete(breaks=seq(1995, 2020, 5)) +
      labs(title="World carbon stocks", x="Years", y="Carbon stocks (million tonnes)"
      )
  })
  
  output$globalLineDescr <- renderUI({
    newText <- "<h3>Our idea: </h3>
                Use the global average of carbon stocks to figure out what has been in recent years (ie. 1992 - 2020)<br/>
                <h3>What we got: </h3>"
    # Label data for the global line plot
    twentyten <- "<span style='color:blue;'>1. Relevant events of 2010:</span><br />
    <ul>
      <li>Brazil registers lowest forest loss since record keeping (1988)</li>
      <li>Worst drought on record for the Brazilian Amazon</li>
      <li>Indonesia and Norway signed a billion-dollar agreement to protect forests in the Southeast Asian nation</li>
      <li>Nestle partners with the Forest Trust to rid its supply chain of any involvement with the destruction of rainforests</li>
    </ul>
    "
    
    twentysixteen <- "<span style='color:red;'>2. Relevant events of 2016:</span><br />
    <ul>
      <li>Brazil registers highest forest loss since 2008</li>
      <li>Massive fire swept through logged forests in northern Congo</li>
      <li>Bolivian Amazon overrun by informal gold miners, causing water pollution and deforestation</li>
      <li>Congo issued at least seven permits that allow companies to prospect or begin mining for gold inside its largest national park</li>
      <li>An agribusiness company based in Singapore continued to clear thousands of hectares of rainforest for oil palm in Gabon</li>
    </ul>
    "
    
    #HTML(paste(twentyten, twentysixteen, sep = '<br/>'))
    combinedText <- paste(newText, twentyten, twentysixteen, sep = '<br/>')
    HTML(combinedText)
    
  })
  
  ### Create the interactive world maps
  output$distPlot <- renderGirafe({
    data_type <- switch(input$data_type,
                        "Carbon stocks in forest (Million tonnes)" = "Carbon_stocks_in_forests_Million_tonnes",
                        "Carbon stocks per forest area (Tonnes/Ha)" = "Normalized_tonnes_per_Ha")
    ggiraph(code = print(worldMaps(df, world_data, data_type))) 
  })
  output$WorldMapDescr <- renderUI({
    newText <- "<h3>Our idea: </h3> 
    <p>Aim for a straightforward and immediate overview of the current situation in each state.
    <h3> What we got: </h3>"
    notNormalized <- "<span> <strong>1. If we don't normalize the data:</strong></span>
    <p>The highest carbon stocks are found in Brazil and Russia, closely followed by Canada and the USA.
    This aligns with expectations, as the territorial extent is directly proportional to the quantity of reserved carbon. 
    Therefore, the largest countries naturally possess the most substantial carbon stocks. </p>"
    normalized<-"<span><strong>2. If we normalize the data:</strong></span>
    <p>Almost all countries exhibit similar efficiency in terms of normalized carbon stocks, resulting in minimal contrast 
    on the map. However, an exception stands out: Guyana. </p>
    <p>What happened in Guyana? The remarkable result in Guyana is attributed to a purposeful government 
    initiative spanning multiple administrations, aimed at maintaining the country's deforestation rates among the lowest in the world.</p>
    "
  
    combinedText <- paste(newText, notNormalized, normalized)
    HTML(combinedText)
  })
  
  ### Create the continent line charts
  plots1 <- list()
  plots1[[1]] <- continent_linechart("Americas", 1)
  plots1[[2]] <- continent_linechart("Africa", 2)
  plots1[[3]] <- continent_linechart("Asia", 3)
  plots1[[4]] <- continent_linechart("Europe", 4)
  plots1[[5]] <- continent_linechart("Oceania", 5)
  
  # Display plots one by one based on scrolling
  output$plots1 <- renderUI({
    plot_list <- lapply(seq_along(plots1), function(i) {
      plotOutput(paste0("plot1_", i))
    })
    tagList(plot_list)
  })
  
  # Render the plots
  lapply(seq_along(plots1), function(i) {
    output[[paste0("plot1_", i)]] <- renderPlot({
      plots1[[i]]
    })
  })
  
  output$textNotNormalized <- renderUI({
    text <- "<h3>OUR IDEA:</h3>
    <p>Illustrate the continent-wise progression of carbon stocks over the years to have a more detailed global view of how we've arrived at our current environmental state.</p>
    <p>To highlight the biggest contributors for each continent, we've set a minimum threshold. Continents below this threshold are shaded in gray, indicating their relatively lower significance.</p>

    <h3>WHAT WE GOT:</h3>

    <span><strong>If we don't normalize the data:</strong></span>
    <p>For every continent, the more substantial territories with well-known forests take the spotlight. The states that appeared significant on the world map maintain their prominence, experiencing minimal changes over time.</p>
    
"
    HTML(text)
  })
  output$textNormalized <- renderUI({
    text <- "<h3>OUR IDEA:</h3>
    <p>Illustrate the continent-wise progression of carbon stocks over the years to have a more detailed global view of how we've arrived at our current environmental state.</p>
    <p>To highlight the major contributors for each continent, we've set a minimum threshold. Continents below this threshold are shaded in gray, indicating their relatively lower significance.</p>

    <h3>WHAT WE GOT:</h3>

    <span><strong>If we normalize the data:</strong></span>
    <p>The notable performers are presently situated in Central America and Oceania. Guadeloupe, Guyana, Micronesia, Palau, and Suriname take center stage as exemplars. Their commonality lies in being tropical rainforests, strategically located between specific latitudes in close proximity to the equator. It is noteworthy that rainforests emerge as exceptional contributors to biomass production, demonstrating an efficiency up to three times greater than that observed in the old-growth forests of the Pacific Northwest.</p>
"
    HTML(text)
    
  })
  
  ### Create the continent line charts normalized
  plots2 <- list()
  plots2[[1]] <- continent_linechart_normalized("Americas", 1)
  plots2[[2]] <- continent_linechart_normalized("Africa", 2)
  plots2[[3]] <- continent_linechart_normalized("Asia", 3)
  plots2[[4]] <- continent_linechart_normalized("Europe", 4)
  plots2[[5]] <- continent_linechart_normalized("Oceania", 5)
  
  # Display plots one by one based on scrolling
  output$plots2 <- renderUI({
    plot_list <- lapply(seq_along(plots2), function(i) {
      plotOutput(paste0("plot2_", i))
    })
    tagList(plot_list)
  })
  
  # Render the plots
  lapply(seq_along(plots2), function(i) {
    output[[paste0("plot2_", i)]] <- renderPlot({
      plots2[[i]]
    })
  })
  
  
  ### Create the line chart with all the continent average
  # #Prepere data for line chart continents
  # cat <- dataframe %>% filter(Indicator=="Carbon stocks in forests") %>% filter(Country %in% c("Africa","Americas","Asia","Australia","Euro Area","Oceania")) 
  # continents <- dataframe %>% filter(Country %in% c("Africa","Americas","Asia","Australia","Euro Area","Oceania"))
  # years <- cat[, -c(1:10)]
  # names(years) <- gsub("F", "", names(years))
  # dataT <- t(years)
  # df_dataT <- dataT
  # colnames(df_dataT)<- c('Africa','Americas','Asia','Australia','Europe', 'Oceania')
  # df_dataT <- as.data.frame(df_dataT)
  # df_dataT$Oceania <- df_dataT$Oceania + df_dataT$Australia
  # df_dataT <- subset(df_dataT, select = -Australia)
  # 
  # df_carbon <- data.frame(x = 1992:2020,
  #                         y1 = df_dataT$Africa,
  #                         y2 = df_dataT$Americas,
  #                         y3 = df_dataT$Asia,
  #                         y5 = df_dataT$Europe,
  #                         y6 = df_dataT$Oceania
  # )
  # 
  # # Reshape data frame
  # df_reshaped <- data.frame(x = df_carbon$x,                            
  #                           y = c(df_carbon$y1, df_carbon$y2, df_carbon$y3,  df_carbon$y5, df_carbon$y6),
  #                           group = c(rep("Africa", nrow(df_carbon)),
  #                                     rep("America", nrow(df_carbon)),
  #                                     rep("Asia", nrow(df_carbon)),
  #                                     rep("Europe", nrow(df_carbon)),
  #                                     rep("Oceania", nrow(df_carbon))))
  # rm(cat,continents,years,dataT,df_dataT,df_carbon)
  # 
  # output$plots3 <- renderPlot({
  #   line_colors<-c("Asia" = "#FFEC57", "Europe" = "#5CDDEE", "Africa" = "#FF7B8F", "America" = "#9AF054", "Oceania" = "#FFA500")
  #   ggplot(df_reshaped, aes(x, y, col = group)) +  geom_line() +geom_point(data = df_reshaped[seq(1, nrow(df_reshaped), by = 2),])+
  #     scale_x_continuous(breaks = round(seq(min(df_reshaped$x), max(df_reshaped$x), by = 2),1))+
  #     scale_y_continuous(breaks = round(seq(min(df_reshaped$y), max(df_reshaped$y), by = 10000),1))+
  #     scale_color_manual(values = line_colors)+
  #     # theme_ipsum(axis_title_size=15)+
  #     theme(axis.text.x = element_text(angle=45))+
  #     ylab("Carbon stocks in forests (Million tonnes)")+
  #     xlab("Years")
  #   # ggsave(filename="Line chart continents.png", path="../Graphs/")
  # })
  
  ### Create the pie charts
  output$pie1 <- renderPlot({
    pie_chart()
  })
  
  output$pieChart1Description<- renderUI({
    text <- "<h3>OUR IDEA:</h3>
    <p>As only a select few countries emerged as significant contributors to large carbon stocks, it became intriguing to examine the actual ratio between the carbon stocked by these nations and those held by the rest of the world collectively.</p>
    <p>The objective was to assess the impact of the top countries on the total carbon stock of all the world's forests.</p>

    <h3>WHAT WE GOT:</h3>

    <span><strong>If we don't normalize the data:</strong></span>
    <p>Brazil, Canada, Russia, the Americas, and the Democratic Republic of the Congo emerge as substantial contributors to the enrichment of global carbon stocks. The chart reveals that these five countries alone account for more than half of the world's carbon reserves.</p>
"
    HTML(text)
  })
  
  output$pie2 <- renderPlot({
    pie_chart_normalized()
  })
  
  output$pieChart2Description<- renderUI({
    text <- "<h3>OUR IDEA:</h3>
    <p>As only a select few countries emerged as significant contributors to large carbon stocks, it became intriguing to examine the actual ratio between the carbon stocked by these nations and those held by the rest of the world collectively. </p>
    <p>The objective was to assess the impact of the top countries on the total carbon stock of all the world's forests.</p>

    <h3>WHAT WE GOT:</h3>

    <span><strong>If we normalize the data:</strong></span>
    <p>The best countries are Guadeloupe, Guyana, Micronesia, Palau and Suriname, which are very small countries. It means that they are representing only a small part of the all carbon stocks.  These 5 countries are representing only 3% of the whole carbon stocks in the world per year, while the 5 best countries of non normalized carbon stocks were more than a half. </p>
    <p>The efficiency per area is relevant, but the global impact on planet Earth might be more important at the end.</p>
    
"
    HTML(text)
    
  })
  
  
  ### Create the bar charts 1992 vs 2020
  output$bar1 <- renderPlot({
    bar_chart()
  })
  
  output$BarNotNormalized <- renderUI({
    text <- "<h3>OUR IDEA:</h3>
    <p>Illustrating the fluctuations in carbon stocks among the five largest contributors by comparing the quantities stored in 1992 and 2020.</p>
    <p> This analysis aims to determine whether these nations have consistently maintained their status as major contributors over the years.</p>

    <h3>WHAT WE GOT:</h3>

    <span><strong>If we don't normalize the data:</strong></span>
    <p>Carbon reserves in Brazil, Canada, and the Democratic Republic of the Congo experienced a decrease, while Russia and the USA saw an increase. However, these changes were not significant, and the top five countries remained dominant both in 1992 and 2020.</p>
    
"
    HTML(text)
  })
  output$BarNormalized <- renderUI({
    text <- "<h3>OUR IDEA:</h3>
    <p>Illustrating the fluctuations in carbon stocks among the five largest contributors by comparing the quantities stored in 1992 and 2020.</p>
    <p> This analysis aims to determine whether these nations have consistently maintained their status as major contributors over the years.</p>

    <h3>WHAT WE GOT:</h3>

    <span><strong>If we normalize the data:</strong></span>
    <p>Carbon reserves in Guadeloupe, Guyana, Palau, and Suriname, when observed on the same scale as the previous non-normalized carbon stocks countries, are nearly invisible due to the scale being too small. </p>
    <p>Examining carbon stocks per forest area reveals minimal differences between 1992 and 2020. For some countries, there is no discernible difference at all, primarily because the unit scale (tonnes/Ha) is too large to visually detect changes, especially for smaller nations.  </p>
    <p>The differences might become apparent with a smaller scale, such as kg/m^2.</p>
    <p>While these forests demonstrate efficiency per area, their impact on a global scale is relatively small.</p>
    
"
    HTML(text)
  })
  
  
  ### Create the continent line charts
  plots <- list()
  plots[[1]] <- bar_chart_normalized("Carbon")
  plots[[2]] <- bar_chart_normalized("Normalized")
  
  # Display plots one by one based on scrolling
  output$bar2 <- renderUI({
    plot_list <- lapply(seq_along(plots), function(i) {
      plotOutput(paste0("plot_", i))
    })
    tagList(plot_list)

  })
  
  # Render the plots
  lapply(seq_along(plots), function(i) {
    output[[paste0("plot_", i)]] <- renderPlot({
      plots[[i]]
    })
  })
  
  
  ### Create the bubble chart
  output$bubbleplot <- renderPlotly({
    xvar <- switch(input$xvar,
                   "Carbon stocks (million tonnes)" = "Carbon_stocks_in_forests_Million_tonnes",
                   "Forest area (1000 Ha)" = "Forest_area_1000_HA",
                   "Index of carbon stocks" = "Index_of_carbon_stocks_in_forests_Index",
                   "Index of forest extent" = "Index_of_forest_extent_Index", 
                   "Land area (1000 Ha)" = "Land_area_1000_HA", 
                   "Share of forest area (%)" = "Share_of_forest_area_Percent"
    )
    yvar <- switch(input$yvar,
                   "Carbon stocks (million tonnes)" = "Carbon_stocks_in_forests_Million_tonnes",
                   "Forest area (1000 Ha)" = "Forest_area_1000_HA",
                   "Index of carbon stocks" = "Index_of_carbon_stocks_in_forests_Index",
                   "Index of forest extent" = "Index_of_forest_extent_Index", 
                   "Land area (1000 Ha)" = "Land_area_1000_HA", 
                   "Share of forest area (%)" = "Share_of_forest_area_Percent"
    )
    bsize <- switch(input$bsize,
                    "Carbon stocks (million tonnes)" = "Carbon_stocks_in_forests_Million_tonnes",
                    "Forest area (1000 Ha)" = "Forest_area_1000_HA",
                    "Index of carbon stocks" = "Index_of_carbon_stocks_in_forests_Index",
                    "Index of forest extent" = "Index_of_forest_extent_Index", 
                    "Land area (1000 Ha)" = "Land_area_1000_HA", 
                    "Share of forest area (%)" = "Share_of_forest_area_Percent"
    )
    colorvar <- "Continent"
    xunit <- table_unit$units[table_unit$indicators == xvar]
    yunit <- table_unit$units[table_unit$indicators == yvar]
    sunit <- table_unit$units[table_unit$indicators == bsize]
    selected_data <- wide_data_2[complete.cases(wide_data_2[, c(xvar, yvar, bsize)]), ]
    # selected_data <- wide_data_2[order(wide_data_2$Share_of_forest_area_Percent),]
    # print(max(selected_data[[bsize]]))
    # print(min(selected_data[[bsize]]))
    # print(min(selected_data[[xvar]])-1000)
    # print(max(selected_data[[xvar]])+1000)
    
    # Plot
    plot_ly(selected_data, x = ~get(xvar), y = ~get(yvar), size = ~get(bsize),
            frame = ~year, color = ~get(colorvar), 
            type = 'scatter', mode = 'markers',
            marker = list(alpha=0.7, line = list(width = 0.5, color = "black")),
            sizes = c(1, 500),  # size of markers (bubbles)
            colors = c("#FF7B8F", "#9AF054", "#FFEC57", "#5CDDEE", "orange"),
            text = ~paste("Country: ", Country,
                          sprintf("<br>%s: %s %s", sub(" \\(.*", "", input$xvar), round(get(xvar)), xunit),
                          sprintf("<br>%s: %s %s", sub(" \\(.*", "", input$yvar), round(get(yvar)), yunit),
                          sprintf("<br>%s: %s %s", sub(" \\(.*", "", input$bsize), round(get(bsize)), sunit)),
            hoverinfo = "text") %>%
      layout(
        xaxis = list(title=input$xvar, range = c(min(selected_data[[xvar]])-100000, max(selected_data[[xvar]])*1.2)),
        yaxis = list(title=input$yvar, range = c(min(selected_data[[yvar]])-20000, max(selected_data[[yvar]])*1.3)),
        legend = list(title=colorvar)
      )
  })
  
  output$BubbleDescription <- renderUI({
    text <- "<h3>OUR IDEA:</h3>
    <p>To visualize the correlation between carbon stocks and forest area over the years.</p>

    <h3>WHAT WE GOT:</h3>

    <p>The relationship between carbon stock and forest area generally follows an intuitive pattern for most countries, where a reduction in forest area correlates with a decrease in carbon stock. However, this correlation is not always linear, and exceptions to the rule exist.</p>
    <p>One intriguing example is the comparison between China and the Democratic Republic of the Congo. Despite having a significantly smaller forest area, the Democratic Republic of the Congo boasts over twice the carbon stock of China. This deviation from the expected pattern can be attributed to the unique characteristics of the rainforests in the Democratic Republic of the Congo. These forests exhibit exceptional plant diversity, and their trees can reach heights of up to 50 meters.
It is essential to note that China's forests, in contrast, have undergone transformations and are no longer considered primary forests. </p>
    <p>This nuanced interplay between forest characteristics and carbon stock highlights the complexity of the relationship and emphasizes the need for a thorough understanding of individual ecosystems.</p>
"
    HTML(text)
  })
  # Function to download the dashboard
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dashboard-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # Save the entire UI to an HTML file
      htmltools::save_html(ui, file)
    }
  )
  
}

### Define the pages on the dashboard -----------------------------
# ui=fluidPage(
#   title = "Forest-Carbon App",
#   # tags$head(
#   #   tags$style(HTML("
#   #     /* Adjust the width of the tabPanel */
#   #     .tab-content {
#   #       width: 80%; /* Adjust the desired width */
#   #       margin-left: auto;
#   #       margin-right: auto;
#   #     }
#   #   "))
#   # ),
#   tags$head(
#     tags$style(HTML("
#       /* Use CSS Grid to define layout */
#       .shiny-split-layout {
#         display: grid;
#         grid-template-columns: 100px 1fr; /* Adjust the width of the left panel */
#         grid-gap: 10px; /* Adjust the gap between the panels */
#       }
#     "))
#   ),
#   navlistPanel(
#     id = "tabset",
#     tabPanel(title = "Home", "Introduction to the dashboard and the dataset"),
#     tabPanel(title = "Mean world carbon stocks from 1992 to 2020", global_line_ui),
#     tabPanel(title = "Carbon stocks in the world in 2020", map_ui),
#     tabPanel(title = "Correlation between carbon stocks and forest area", bubble_ui),
#     tabPanel(title = "Firsts forests carbon stocks in 1992 vs 2020", compere_1992_2020_ui),
#     tabPanel(title = "Carbon stocks by continent among time", continents_line_ui),
#     # tabPanel(title = "Test", test),
#     tabPanel(title = "Test", test)
#   )
# )



ui <- dashboardPage(
  dashboardHeader(title = "Forest-Carbon App"),
  dashboardSidebar(
    width = 150, # Adjust the width of the sidebar
    tags$head(
      tags$style(HTML(".sidebar-menu li a { white-space: normal !important; }"))
    ),
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Mean world carbon stocks from 1992 to 2020", tabName = "global_line"),
      menuItem("Carbon stocks in the world in 2020", tabName = "map"),
      menuItem("Carbon stocks by continent among time", tabName = "continents_line"),
      menuItem("Proportional carbon stock dominance of the best 5 countries", tabName = "pie_chart"),
      menuItem("Firsts forests carbon stocks in 1992 vs 2020", tabName = "compere_1992_2020"),
      menuItem("Correlation between carbon stocks and forest area", tabName = "bubble")
    ),
    # Add a download button
    downloadButton("downloadData", "Download Dashboard", class = "btn-primary btn-sm")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home", home_ui()),
      tabItem(tabName = "global_line", global_line_ui),
      tabItem(tabName = "map", map_ui),
      tabItem(tabName = "continents_line", continents_line_ui),
      tabItem(tabName = "pie_chart", pie_chart_ui),
      tabItem(tabName = "compere_1992_2020", compere_1992_2020_ui),
      tabItem(tabName = "bubble", bubble_ui)
    )
  )
)

### Running the dashboard -----------------------------
shinyApp(ui = ui, server = server)

