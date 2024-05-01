#uttarakhand map 2022 performance
library(rio)
library(leaflet)
library(sf)
library(RColorBrewer)
pa_data <- rio::import("/Users/yashi/Downloads/data visualization/2022/MAP20222.csv")
pa_geo <- sf::st_read("/Users/yashi/Downloads/data visualization/maps-master/assembly-constituencies/India_AC.shp", 
                      stringsAsFactors = FALSE)
pa_map_data <- merge(pa_geo, pa_data, by = "AC_NAME")
pa_map_data <- st_transform(pa_map_data, "+proj=longlat +datum=WGS84")
BJP_df <- pa_map_data[pa_map_data$PARTY == "BJP",]
INC_df <- pa_map_data[pa_map_data$PARTY == "INC",]
BSP_df <- pa_map_data[pa_map_data$PARTY == "BSP",]
IND_df <- pa_map_data[pa_map_data$PARTY == "IND",]
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = BJP_df,
    fillColor = "#FFA500",  
    stroke = TRUE,
    smoothFactor = 1,
    fillOpacity = 0.75,
    color = "#666",
    weight = 1,
    label = ~paste0("<strong>", AC_NAME, "</strong>")
  ) %>%
  addPolygons(
    data = INC_df,
    fillColor = "#0000FF",  
    stroke = TRUE,
    smoothFactor = 1,
    fillOpacity = 0.75,
    color = "#666",
    weight = 1,
    label = ~paste0("<strong>", AC_NAME, "</strong>")
  ) %>%
  addPolygons(
    data = IND_df,
    fillColor = "#808080",  
    stroke = TRUE,
    smoothFactor = 1,
    fillOpacity = 0.75,
    color = "#666",
    weight = 1,
    label = ~paste0("<strong>", AC_NAME, "</strong>")
  )%>%
  addPolygons(
    data = BSP_df,
    fillColor = "#FC0202",  
    stroke = TRUE,
    smoothFactor = 1,
    fillOpacity = 0.75,
    color = "#666",
    weight = 1,
    label = ~paste0("<strong>", AC_NAME, "</strong>")
  )

#parliament seat code 2022
library(ggplot2) 
library(ggparliament)
library(readr)
library(dplyr)
library(readxl)
getwd()
Results <- read_excel("/Users/yashi/Downloads/data visualization/2022/performas.xlsx")
cols.num <- c("CONTESTED","WON","FD")
Results[cols.num] <- sapply(Results[cols.num],as.numeric)
sapply(Results, class)
head(Results)
Parlia_semicircle <- parliament_data(election_data = Results,
                                     type = "semicircle",
                                     parl_rows = 5,
                                     party_seats = Results$WON)
ind <- ggplot(Parlia_semicircle, aes(x = x, y = y, colour = ABBREVIATION)) +
  geom_parliament_seats(stat = "identity", position = "identity", size = 7) +
  theme_ggparliament() +
  labs(colour = "Parties",  
       title = "Uttarakhand Legislative 2022 Parliament", legend.position = "center") +
  scale_color_manual(values = c("Bharatiya Janata Party" = "#FF8C00",
                                "Indian National Congress" = "#0000FF",
                                "Bahujan Samaj Party" = "#B8860B",
                                "Independent" = "#ADFF2F")) +
  theme(legend.position = "bottom") +
  draw_totalseats(n = 70, type = "semicircle") + 
  draw_majoritythreshold(n = 36, label = TRUE, type = 'semicircle') +
  theme(plot.title = element_text(hjust = 0.5)) 
ind

#voteshare treemap
library(treemap)


group <- c("Bharatiya Janata Party", "Indian National Congress", "Bahujan Samaj Party", "Independent Candidate", "NOTA+Other")
value <- c(44.33, 37.91, 4.82, 5.58, 7.36)
data <- data.frame(group,value)


treemap(data,
        index="group",
        vSize="value",
        type="index",
        title = "Vote share of Political Parties in 2022")

#margin bar chart
library(ggplot2)

data2 <- data.frame(constituency = as.factor(c("Purola", "Yamunotri", "Gangotri", "Badrinath", "Tharali", "Karnprayag", "Kedarnath", "Rudraprayag", "Ghanshali", "Devprayag", "Narendranagar", "Pratapnagar", "Tehri", "Dhanolti", "Chakrata", "Vikasnagar", "Sahaspur", "Dharampur", "Raipur", "Rajpur Road","Dehradun Cantt.", "Mussoorie", "Doiwala", "Rishikesh", "Hardwar", "B.H.E.L. Ranipur", "Jwalapur", "Bhagwanpur", "Jhabrera", "Pirankaliyar", "Roorkee", "Khanpur", "Manglore", "Laksar", "Hardwar Rural", "Yamkeshwar", "Pauri", "Srinagar", "Chaubattakhal", "Lansdowne", "Kotdwar", "Dharchula", "Didihat", "Pithoragarh", "Gangolihat", "Kapkot", "Bageshwar", "Dwarahat", "Salt", "Ranikhet", "Someshwar (S.C.)", "Almora", "Jageshwar", "Lohaghat", "Champawat", "Lalkuwa", "Bhimtal", "Nainital", "Haldwani", "Kaladhungi", "Ramnagar", "Jaspur", "Kashipur", "Bajpur", "Gadarpur", "Rudrapur", "Kichha", "Sitarganj", "Nanak Matta", "Khatima")),
                    margin = as.numeric(c(6296, -12332, 8029, -2066, 8302, 6715, 8463, 9802, 10285, 2588, 1798, -2341, 951, 4684, -9436, 5193, 8355, 10090, 30052, 11163, 20938, 15325, 29021, 19057, 15237, 13862, -13343, -32741, -8216, -15743, 2277, -7933, -13897, -10440, -4472, 10410, 5738, 587, 11430, 9868, 3687, -1118, 3226, -6054, 10053, 4046, 12141, -182, 3688, 2584, 5293, -127, 5883, -6038, 5304, 17527, 9844, 7881, -7814, 23931, 4745, -4172, 16335, -1611, 1120, 19750, -10077, 10938, -13020, -6579)))

ggplot(data2, aes(x = constituency, y = margin, fill = margin < 0)) +
  geom_col(stat = "identity") +
  scale_fill_manual(values = c("orange", "blue")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



