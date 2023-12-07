# rm(list=ls())
# cmd+enter to run line
# cmd+option+T to run section
# cmd+shift+C to comment/uncomment

library(tidyverse)
setwd("/Users/marie/Documents/ENSE3/3A/Courses/Data Visualization/Datasets/")
# setwd("/Users/marie/Downloads/")

### Data --------------------------------------------------------------------
dataframe <- read_csv("Forest_and_Carbon.csv", show_col_types = FALSE) # readr version

### Clear data --------------------------------------------------------------------
# Remove the objectID column
data <- dataframe %>% select(-(1))

# Add the units in the indicator lines
# data$Indicator <- paste(data$Indicator, "")
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


### Getting continents from countries ---------------------------------------
library(countrycode)

# getting the countries in a new data
df <- data.frame(country = wide_data$Country)

# find the correspondant continent for each country
df$continent <- countrycode(sourcevar = df[, "country"],
                            origin = "country.name",
                            destination = "continent")

# create a new column with the corresponding continents in the clear data
wide_data$continent <- df$continent
wide_data <- wide_data %>% relocate(continent, .before=ISO2) # replace it next to the country column
colnames(wide_data) = gsub(" ", "_", colnames(wide_data))    # replace the spaces by _

wide_data <- mutate(wide_data, Percentage=wide_data$Carbon_stocks_in_forests_Million_tonnes/wide_data$Forest_area_1000_HA)

### World carbon stocks --------------------------------------------------------------------

world <- wide_data %>% filter(Country=="World")
# asia <- asia %>% drop_na()
# world <- subset(world,!is.na(Carbon_stocks_in_forests_Million_tonnes))
# asia <- subset(asia,!is.na(Percentage))

#asia <- subset(asia,!Carbon_stocks_in_forests_Million_tonnes<2000)
# asia <- asia %>% arrange(-Carbon_stocks_in_forests_Million_tonnes)

#can <- wide_data %>% filter(Country=="Canada")

# ggplot(asia, aes(x=year, y=Carbon_stocks_in_forests_Million_tonnes)) + 
#   geom_point(aes(color=Country)) +
#   labs(title="Most carbon stocks in forests in Asia", x="Years", y="Carbon stocks (million tonnes)") +
#   theme_bw() +
#   scale_x_discrete(breaks=seq(1995, 2020, 5)) +
#   theme(axis.text.x = element_text(angle = 45))

ggplot(world, aes(x=year, y=Carbon_stocks_in_forests_Million_tonnes, group=Country)) + 
  geom_line() +
  # theme_bw() +
  theme(panel.background = element_blank(), panel.grid.major = element_line("grey", linewidth = 0.1), 
        axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust=1))+ #, legend.background = element_blank()) +
  scale_x_discrete(breaks=seq(1995, 2020, 5)) +
  labs(title="World carbon stocks", x="Years", y="Carbon stocks (million tonnes)")
# scale_fill_discrete()

ggsave(filename="world.png", path="/Users/marie/Documents/ENSE3/3A/Courses/Data Visualization/Graphs/")


### Asia carbon stocks --------------------------------------------------------------------

asia <- wide_data %>% filter(continent=="Asia")
# asia <- asia %>% drop_na()
asia <- subset(asia,!is.na(Carbon_stocks_in_forests_Million_tonnes))
asia <- subset(asia,!is.na(Percentage))

#asia <- subset(asia,!Carbon_stocks_in_forests_Million_tonnes<2000)
# asia <- asia %>% arrange(-Carbon_stocks_in_forests_Million_tonnes)

#can <- wide_data %>% filter(Country=="Canada")

# ggplot(asia, aes(x=year, y=Carbon_stocks_in_forests_Million_tonnes)) + 
#   geom_point(aes(color=Country)) +
#   labs(title="Most carbon stocks in forests in Asia", x="Years", y="Carbon stocks (million tonnes)") +
#   theme_bw() +
#   scale_x_discrete(breaks=seq(1995, 2020, 5)) +
#   theme(axis.text.x = element_text(angle = 45))

ggplot(asia, aes(x=year, y=Percentage, group=Country)) + 
  geom_line(aes(color = Country)) +
  # theme_bw() +
  theme(panel.background = element_blank(), panel.grid.major = element_line("grey", linewidth = 0.1), 
        axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust=1))+ #, legend.background = element_blank()) +
  scale_x_discrete(breaks=seq(1995, 2020, 5)) +
  labs(title="Most carbon stocks in forests in Asia", x="Years", y="Carbon stocks (million tonnes)")
# scale_fill_discrete()

#ggsave(filename="asia_l.png", path="/Users/marie/Documents/ENSE3/3A/Courses/Data Visualization/Graphs/")


### America carbon stocks --------------
america <- wide_data %>% filter(continent=="Americas")
#america <- subset(america,!is.na(Carbon_stocks_in_forests_Million_tonnes))
# america <- subset(america,!is.na(Percentage))
america <- subset(america,!Carbon_stocks_in_forests_Million_tonnes<8000)

ggplot(america, aes(x=year, y=Carbon_stocks_in_forests_Million_tonnes, group=Country)) + 
  geom_line(aes(color = Country)) +
  theme(panel.background = element_blank(), panel.grid.major = element_line("grey", linewidth = 0.1), 
        axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust=1))+ #, legend.background = element_blank()) +
  scale_x_discrete(breaks=seq(1995, 2020, 5)) +
  labs(title="Most carbon stocks in forests in America", x="Years", y="Carbon stocks (million tonnes)")

ggsave(filename="america.png", path="/Users/marie/Documents/ENSE3/3A/Courses/Data Visualization/Graphs/")


### Oceania carbon stocks --------------
oceania <- wide_data %>% filter(continent=="Oceania")
oceania <- subset(oceania,!is.na(Carbon_stocks_in_forests_Million_tonnes))
# oceania <- subset(oceania,!Carbon_stocks_in_forests_Million_tonnes<7000)
oceania <- subset(oceania,!is.na(Percentage))

ggplot(oceania, aes(x=year, y=Percentage, group=Country)) + 
  geom_line(aes(color = Country)) +
  theme(panel.background = element_blank(), panel.grid.major = element_line("grey", linewidth = 0.1), 
        axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust=1))+ #, legend.background = element_blank()) +
  scale_x_discrete(breaks=seq(1995, 2020, 5)) +
  labs(title="Most carbon stocks in forests in America", x="Years", y="Carbon stocks (million tonnes)")

#ggsave(filename="oceania.png", path="/Users/marie/Documents/ENSE3/3A/Courses/Data Visualization/Graphs/")

### Europe carbon stocks --------------
europe <- wide_data %>% filter(continent=="Europe")
europe <- subset(europe,!is.na(Carbon_stocks_in_forests_Million_tonnes))
europe <- subset(europe,!Carbon_stocks_in_forests_Million_tonnes<1000)

ggplot(europe, aes(x=year, y=Carbon_stocks_in_forests_Million_tonnes, group=Country)) + 
  geom_line(aes(color = Country)) +
  theme(panel.background = element_blank(), panel.grid.major = element_line("grey", linewidth = 0.1), 
        axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust=1))+ #, legend.background = element_blank()) +
  scale_x_discrete(breaks=seq(1995, 2020, 5)) +
  labs(title="Most carbon stocks in forests in America", x="Years", y="Carbon stocks (million tonnes)")

#ggsave(filename="europe.png", path="/Users/marie/Documents/ENSE3/3A/Courses/Data Visualization/Graphs/")

### Africa carbon stocks --------------
africa <- wide_data %>% filter(continent=="Africa")
africa <- subset(africa,!is.na(Carbon_stocks_in_forests_Million_tonnes))
africa <- subset(africa,!Carbon_stocks_in_forests_Million_tonnes<1000)

ggplot(africa, aes(x=year, y=Carbon_stocks_in_forests_Million_tonnes, group=Country)) + 
  geom_line(aes(color = Country)) +
  theme(panel.background = element_blank(), panel.grid.major = element_line("grey", linewidth = 0.1), 
        axis.line = element_line("black", linewidth = 0.2), axis.text.x = element_text(angle = 45, hjust=1))+ #, legend.background = element_blank()) +
  scale_x_discrete(breaks=seq(1995, 2020, 5)) +
  labs(title="Most carbon stocks in forests in America", x="Years", y="Carbon stocks (million tonnes)")

ggsave(filename="africa.png", path="/Users/marie/Documents/ENSE3/3A/Courses/Data Visualization/Graphs/")




### Print map for 2020 carbon stock -----------------------------------------------------------------

#plotting the map 
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
head(world_data)

#renaming
old_names <- c("Afghanistan, Islamic Rep. of","Andorra, Principality of","Armenia, Rep. of",
               "Aruba, Kingdom of the Netherlands","Azerbaijan, Rep. of","Bahamas, The","	Bahrain, Kingdom of",
               "Belarus, Rep. of","British Virgin Islands","China, P.R.: Mainland",
               "Comoros, Union of the",
               "Congo, Dem. Rep. of the","Croatia, Rep. of","Czech Rep.","Dominican Rep.","Egypt, Arab Rep. of","Equatorial Guinea, Rep. of",
               "Eritrea, The State of","Estonia, Rep. of","Eswatini, Kingdom of","Ethiopia, The Federal Dem. Rep. of","Fiji, Rep. of","Gambia, The",
               "Guiana, French","Iran, Islamic Rep. of","Kazakhstan, Rep. of","Kyrgyz Rep.","Lesotho, Kingdom of","Madagascar, Rep. of","Marshall Islands, Rep. of the",
               "Mauritania, Islamic Rep. of","Micronesia, Federated States of",
               "Moldova, Rep. of","Mozambique, Rep. of","Netherlands, The","North Macedonia, Republic of","Palau, Rep. of",
               "Poland, Rep. of","Russian Federation","San Marino, Rep. of","São Tomé and Príncipe, Dem. Rep. of",
               
               "Serbia, Rep. of","Slovak Rep.","Slovenia, Rep. of",
               "Syrian Arab Rep.","Tajikistan, Rep. of","Tanzania, United Rep. of","Timor-Leste, Dem. Rep. of",
               "Türkiye, Rep. of","United States","Uzbekistan, Rep. of","Venezuela, Rep. Bolivariana de","Wallis and Futuna Islands","West Bank and Gaza",
               "Yemen, Rep. of", "United Kingdom", "Congo", "Congo, Rep. of", "Central African Rep.", "South Sudan, Rep. of", "Côte d'Ivoire", "Lao People's Dem. Rep.", "Korea, Dem. People's Rep. of", "Korea, Rep. of")


new_names <- c("Afghanistan","Andorra","Armenia","Aruba","Azerbaijan","Bahamas","Bahrain","Belarus","Virgin Island","China","Comoros",
               "Congo","Croatia","Czech Republic","Dominican Republic","Egypt","Guinea","Eritrea","Estonia","Eswatini","Ethiopia","Fiji","Gambia",
               "French Guiana","Iran","Kazakhstan","Kyrgyzstan","Lesotho","Madagascar","Marshall Islands","Mauritania","Micronesia",
               "Moldova","Mozambique","Netherlands","North Macedonia","Palau","Poland","Russia","San Marino","Sao Tome and Principe",
               "Serbia","Slovakia","Slovenia","Syria","Tajikistan","Tanzania","Timor-Leste","Turkey","USA","Uzbekistan","Venezuela",
               "Wallis and Futuna","Gaza","Yemen", "UK", "Democratic Republic of the Congo", "Republic of Congo", "Central African Republic", "South Sudan", "Ivory Coast", "Laos", "North Korea", "South Korea")

for (i in 1:length(old_names)){
  wide_data$Country[wide_data$Country == old_names[i]] <- new_names[i]
}


#adding the ISO3 code
world_data["ISO3"] <- wide_data$ISO3[match(world_data$region, wide_data$Country)]

#getting info for year 2020
df_2020 <-  wide_data[wide_data$year == "2020", ]

df <-df_2020 #changing the name for fitting


##plot stuff just for year=2020
worldMaps <- function(df, world_data, column_name){
  print(paste(column_name))
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
  plotdf <- df[, c("Country", column_name, "ISO3")]
  plotdf <- df[!is.na(plotdf$column_name), ]
  
  # Create a color palette for each unique country
  country_palette <- scales::hue_pal()(length(unique(plotdf$Country)))
  
  # Map each country to a unique color using scale_fill_manual
  color_mapping <- scale_fill_manual(values = setNames(country_palette, unique(plotdf$Country)))
  
  
  # Add the Carbon stocks in forest Milion tonnes to the geographical world data
  world_data["Index_to_plot"] <- plotdf$column_name[match(world_data$ISO3, plotdf$ISO3)]
  
  
  # Create caption with the data source to show underneath the map
  #capt <- paste0("Source: ", ifelse(data_type == "Carbon stocks in forests", "United Nations" , "www.climatedata.imf.org"))
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Index_to_plot, group= group,
                                 tooltip = sprintf("%s<br/>%s", ISO3, Index_to_plot))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "Greens"), na.value = 'grey' , breaks = c(0.1,50000),trans = "log10") + 
    scale_y_continuous(limits = c(-115, 90), breaks = c()) + 
    scale_x_continuous(breaks = c()) + 
    #labs(fill = Index_to_plot, color = Index_to_plot, title = NULL, x = NULL, y = NULL, caption = capt) + 
    labs(fill = "Legenda: Descrizione") +
    guides(fill = guide_colorbar(
      title = "Milion Tonnes Carbon stock",
      label = TRUE,                # Visualizza i valori della legenda
      label.position = "bottom",   # Posizione delle etichette
      label.theme = element_text(size = 10),  # Dimensione del testo delle etichette
      keywidth = 10,                # Larghezza della barra colorata
      keyheight = 10               # Altezza della barra colorata
    )) +
    my_theme()
  
  return(g)
}
# Chiamare la funzione e assegnare il risultato a un oggetto
mappa <- worldMaps(df, world_data)

# Visualizzare la mappa direttamente
print(mappa)
# ggsave(filename="world_map.png", path="./Graphs/") 







### Clear data fastidious way --------------------------------------------------------------------
### Create 6 different dataset for each indicator with the correct shape and then combine them together
# data1 <- data %>% select(-(5:9)) %>% filter(Indicator=="Carbon stocks in forests")
# long_data1 <- pivot_longer(data1,
#                          cols = row,
#                          names_to = "year", 
#                          values_to ="value")
# wide_data1 <- pivot_wider(long_data1,
#                           names_from = "Indicator",
#                           values_from = "value")
# 
# 
# data2 <- data %>% select(-(5:9)) %>% filter(Indicator=="Forest area")
# long_data2 <- pivot_longer(data2,
#                           cols = row,
#                           names_to = "year", 
#                           values_to ="value")
# wide_data2 <- pivot_wider(long_data2,
#                          names_from = "Indicator",
#                          values_from = "value")
# 
# 
# data3 <- data %>% select(-(5:9)) %>% filter(Indicator=="Index of carbon stocks in forests")
# long_data3 <- pivot_longer(data3,
#                            cols = row,
#                            names_to = "year", 
#                            values_to ="value")
# wide_data3 <- pivot_wider(long_data3,
#                           names_from = "Indicator",
#                           values_from = "value")
# 
# 
# data4 <- data %>% select(-(5:9)) %>% filter(Indicator=="Index of forest extent")
# long_data4 <- pivot_longer(data4,
#                            cols = row,
#                            names_to = "year", 
#                            values_to ="value")
# wide_data4 <- pivot_wider(long_data4,
#                           names_from = "Indicator",
#                           values_from = "value")
# 
# 
# data5 <- data %>% select(-(5:9)) %>% filter(Indicator=="Land area")
# long_data5 <- pivot_longer(data5,
#                            cols = row,
#                            names_to = "year", 
#                            values_to ="value")
# wide_data5 <- pivot_wider(long_data5,
#                           names_from = "Indicator",
#                           values_from = "value")
# 
# 
# data6 <- data %>% select(-(5:9)) %>% filter(Indicator=="Share of forest area")
# long_data6 <- pivot_longer(data6,
#                            cols = row,
#                            names_to = "year", 
#                            values_to ="value")
# wide_data6 <- pivot_wider(long_data6,
#                           names_from = "Indicator",
#                           values_from = "value")
# 
# # Combine all the wide_data to get the final right shape
# data_clear <- full_join(wide_data1,wide_data2)
# data_clear <- full_join(data_clear,wide_data3)
# data_clear <- full_join(data_clear,wide_data4)
# data_clear <- full_join(data_clear,wide_data5)
# data_clear <- full_join(data_clear,wide_data6)

### One indicator --------------------------------------------------------------------
# One indicator
cat <- data %>% filter(Indicator == "Carbon stocks in forests") %>% select(-(3:10))
row <- colnames(cat)
row <- as.numeric( sub("F", "", row ))
cat <- rbind(row, cat)
cat <- t(cat)
cat0<- as.data.frame(cat)

ggplot(cat0, aes(x=V1, V2)) 

ggplot(cat0, aes(x=V1)) + for (i in 1:10){
  geom_line(aes(y=as.name(paste("V","i", sep="")))) +
}

# data <- read_csv("Forest_and_Carbon.csv", col_names=FALSE) # readr version


# Plot one indicator for one country
cat <- data %>% filter(Indicator == "Share of forest area", Country=="Canada") %>% select(-(1:10))
row <- colnames(cat)
row <- as.numeric( sub("F", "", row ))
cat <- rbind(row, cat)
cat <- t(cat)
cat0<- as.data.frame(cat)
ggplot(cat0, aes(V1, V2)) + geom_line()

### Old first try --------------------------------------------------------------------
cat <- data %>% filter(Indicator == "Carbon stocks in forests")
catT <- t(cat)

col1 <- cat %>% select("F1992")
col2 <- cat %>% select("F1992", "F1993")
#col_names <- names(col)
a1 <- c(rep(1992,each=208))
a2 <- c(rep(1992,each=104), rep(1993, each=104))

ggplot(cat, aes(a1, t(col1))) + geom_point(colour="red") #+ labs(x="horsepower", y="km per liter")
ggplot(cat, aes(a2, col2$F1992)) + geom_point(colour="red") #+ labs(x="horsepower", y="km per liter")


col3 <- cat %>% select(-(1:10))
col_names <- as.numeric( sub("F", "", names(col3) ))
#a1 <- c(rep(1992,each=208))
#a2 <- c(rep(1992,each=104), rep(1993, each=104))

ggplot(cat, aes(col_names, t(col3))) + geom_point(colour="red") #+ labs(x="horsepower", y="km per liter")
col3T <- t(col3)

group <- group_by(data, Country, Indicator) %>% select(-1, -3, -4, -(6:10))
group <- select(group, -1, -2)
group <- t(group)


fil <- filter(data, Country == "Australia", Indicator == "Carbon stocks in forests") %>% select(-(1:10))
fil <- t(fil)
ggplot(fil, aes(x=)) + geom_point(colour="red") 