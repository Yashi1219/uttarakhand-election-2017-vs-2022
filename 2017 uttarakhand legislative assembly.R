library(rio)
library(leaflet)
library(sf)
library(RColorBrewer)  
# map of uttarakhand
pa_data <- rio::import("/Users/yashi/Downloads/data visualization/2017/MAP20172.csv")
pa_geo <- sf::st_read("/Users/yashi/Downloads/data visualization/maps-master/assembly-constituencies/India_AC.shp",  stringsAsFactors = FALSE)
pa_map_data <- merge(pa_geo, pa_data, by = "AC_NAME")
pa_map_data <- st_transform(pa_map_data, "+proj=longlat +datum=WGS84")

BJP_df <- pa_map_data[pa_map_data$Party == "BJP",]
INC_df <- pa_map_data[pa_map_data$Party == "INC",]
IND_df <- pa_map_data[pa_map_data$Party == "IND",]

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = BJP_df,  fillColor = "#FFA500",  stroke = TRUE, smoothFactor = 1,fillOpacity = 0.75,  color = "#666",   weight = 1,
    label = ~paste0("<strong>", AC_NAME, "</strong>")
  ) %>%
  addPolygons(
    data = INC_df, fillColor = "#0000FF",  stroke = TRUE, smoothFactor = 1,  fillOpacity = 0.75,  color = "#666",  weight = 1,
    label = ~paste0("<strong>", AC_NAME, "</strong>")
  ) %>%
  addPolygons(
    data = IND_df, fillColor = "#808080",   stroke = TRUE,   smoothFactor = 1, fillOpacity = 0.75,  color = "#666",  weight = 1,
    label = ~paste0("<strong>", AC_NAME, "</strong>")
  )



#parliament seat code
library(ggplot2) 
library(ggparliament)
library(readr)
library(dplyr)
library(readxl)
getwd()
Results <- read_excel("/Users/yashi/Downloads/data visualization/2017/performas.xlsx")
head(Results)
Parlia_semicircle <- parliament_data(election_data = Results,
                                     type = "semicircle",
                                     parl_rows = 5,
                                     party_seats = Results$Won)
ind <- ggplot(Parlia_semicircle, aes(x = x, y = y, colour = Paartname)) +
  geom_parliament_seats(stat = "identity", position = "identity", size = 7) +
  theme_ggparliament() +
  labs(colour = "Parties",  
       title = "Uttarakhand Legislative 2017 Parliament", legend.position = "center") +
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
install.packages("treemap")
library(treemap)


group <- c("Bharatiya Janata Party", "Indian National Congress", "Bahujan Samaj Party", "Independent Candidate", "NOTA+Other")
value <- c(46.51, 33.49, 6.98, 10.04, 2.98)
data <- data.frame(group,value)


treemap(data,
        index="group",
        vSize="value",
        type="index",
        title = "Vote share of Political Parties in 2017")

#margin bar chart
library(ggplot2)

data2 <- data.frame(constituency = as.factor(c("Purola", "Yamunotri", "Gangotri", "Badrinath", "Tharali", "Karnprayag",  "Kedarnath", "Rudraprayag", "Ghanshali", "Devprayag", "Narendranagar",  "Pratapnagar", "Tehri", "Dhanolti", "Chakrata", "Vikasnagar", "Sahaspur",   "Dharampur", "Raipur", "Rajpur Road","Dehradun Cantt.", "Mussoorie", "Doiwala",   "Rishikesh", "Hardwar", "B.H.E.L. Ranipur", "Jwalapur", "Bhagwanpur", "Jhabrera",  "Pirankaliyar", "Roorkee", "Khanpur", "Manglore", "Laksar", "Hardwar Rural",  "Yamkeshwar", "Pauri", "Srinagar", "Chaubattakhal", "Lansdowne", "Kotdwar",  "Dharchula", "Didihat", "Pithoragarh", "Gangolihat", "Kapkot", "Bageshwar",  "Dwarahat", "Salt", "Ranikhet", "Someshwar (S.C.)", "Almora", "Jageshwar", "Lohaghat",  "Champawat", "Lalkuwa", "Bhimtal", "Nainital", "Haldwani", "Kaladhungi", "Ramnagar","Jaspur", "Kashipur", "Bajpur", "Gadarpur", "Rudrapur", "Kichha", "Sitarganj",  "Nanak Matta", "Khatima")),
                    margin = as.numeric(c(-1013, 5960, 9610, 5634, 4858, 7549, -2434, 14632, 11653, 3499,   4972, 1939, 6840, -1615, -1543, 6418, 18863, 10953, 36771, 8632, 16670, 12077, 24869, 14801, 35927, 22240, 4788, -2513, 2253, -1349, 12542, 13735, -14388, 1604, 12278, 8982, 7030, 8698, 7354, 6475, 11318, -3085, 2368, 2684, 805, 5982, 14567, 6593, 2904, -4981, 710, 5379,  -399, 834, 17360, 27108, -3446, 7247, -6557, 20597, 8611,-4204,  20114, 12636, 14106, 24771, 2127, 28450, 9531, 2709)))

ggplot(data2, aes(x = constituency, y = margin, fill = margin < 0)) +
  geom_col(stat = "identity") +
  scale_fill_manual(values = c("orange", "blue")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



