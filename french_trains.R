library(igraph)
library(devtools)
# library(network)
# library(sna)
# library(visNetwork)
# library(threejs)
# library(networkD3)
# library(ndtv)
library(arcdiagram)

full_trains <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")

gps <- readr::read_csv("referentiel-gares-voyageurs.csv")
g1 <- gps %>%
  select(`Intitulé gare`, `Région SNCF`) %>%
  rename(dept = `Région SNCF`,
         d_s = `Intitulé gare`) %>%
  mutate(d_s = gsub("-", " ", d_s),
         d_s = stringi::stri_trans_general(d_s, "Latin-ASCII"),
         d_s = gsub("   ", " ", d_s),
         d_s = gsub(" CHR", "", d_s),
         d_s = gsub(" TGV", "", d_s),
         d_s = ifelse(d_s == "Paris Montparnasse Vaugirard", "Paris Montparnasse", d_s),
         d_s = ifelse(d_s == "Paris Gare du Nord", "Paris Nord", d_s),
         d_s = ifelse(d_s == "Paris Gare de Lyon Souterrain", "Paris Lyon", d_s),
         d_s = ifelse(d_s == "Metz Ville", "Metz", d_s)) %>%
  arrange(d_s)

# Selecting the necessary variables ----
ft <- full_trains %>%
  filter(service == "National") %>%
  select(departure_station, arrival_station, 
         #num_late_at_departure, 
         #num_arriving_late, 
         #num_of_canceled_trains, 
         avg_delay_all_arriving,
         avg_delay_all_departing,
         #total_num_trips, 
         year, month) %>%
  mutate(avg_delay = avg_delay_all_departing + avg_delay_all_arriving) %>%
  mutate(a_s = tools::toTitleCase(tolower(arrival_station)),
         d_s = tools::toTitleCase(tolower(departure_station))) %>%
  group_by(year, a_s, d_s) %>%
  summarise_if(is.numeric, funs(mean)) %>%
  arrange(year, d_s, a_s) %>%
  ungroup() %>%
  filter(year == 2017) %>%
  select(d_s, a_s, avg_delay) %>%
  mutate(d_s = gsub(" Tgv", "", d_s),
         d_s = ifelse(d_s == "Bellegarde (Ain)", "Bellegarde", d_s),
         d_s = gsub("St ", "Saint ", d_s),
         d_s = gsub("Les", "les", d_s),
         d_s = gsub(" Ville", "", d_s),
         d_s = ifelse(d_s == "Le Creusot Montceau Montchanin", "Le Creusot", d_s),
         d_s = ifelse(d_s == "Montpellier", "Montpellier Saint Roch", d_s),
         d_s = ifelse(d_s == "Valence Alixan", "Valence", d_s),
         d_s = gsub("Des", "des", d_s),
         d_s = gsub("En", "en", d_s),
         a_s = gsub(" Tgv", "", a_s),
         a_s = ifelse(a_s == "Bellegarde (Ain)", "Bellegarde", a_s),
         a_s = gsub("St ", "Saint ", a_s),
         a_s = gsub("Les", "les", a_s),
         a_s = gsub(" Ville", "", a_s),
         a_s = ifelse(a_s == "Le Creusot Montceau Montchanin", "Le Creusot", a_s),
         a_s = ifelse(a_s == "Montpellier", "Montpellier Saint Roch", a_s),
         a_s = ifelse(a_s == "Valence Alixan", "Valence", a_s),
         a_s = gsub("Des", "des", a_s),
         a_s = gsub("En", "en", a_s)) %>%
  arrange(d_s) 

fg <- inner_join(ft, g1, by = "d_s") 
fg <- fg[-1,]

rm(ft, full_trains, g1, gps)


fg <- fg %>%
  arrange(dept) %>%
  mutate(avg_delay = case_when(avg_delay <= 0 ~ avg_delay *0,
                               avg_delay>0 ~ avg_delay))

# Creating an edge list
ft_m <- as.matrix(fg[,c("d_s", "a_s")])
ft_lab <- graph.edgelist(ft_m, directed = T)

# Weight of arcs and color
E(ft_lab)$weight = fg$avg_delay
w2 = E(ft_lab)$weight
cols = hsv(h=0, s=w2/max(w2), v=0, alpha=0.5*w2/max(w2))
lwds = sqrt(w2/20)

# Color of nodes
library(viridis)
a <- viridis(11)
fg_dept <- fg %>%
  select(dept, d_s)
fg_dept <- unique(fg_dept)
fg_dept <- fg_dept[c(1,29, 2, 3, 4, 5, 27, 46, 36, 14, 6, 7, 8, 9, 10, 11, 12, 30, 13, 15, 16, 17, 18, 19, 28, 20, 21, 22, 23, 43,24, 31, 25, 26, 44, 45, 47, 37, 38, 48, 40, 32, 33, 34, 41, 42, 35, 39 ),]
d2 = as.data.frame(as.numeric(as.factor(fg_dept$dept)))
names(d2) <- "dept"
ax <- as.data.frame(cbind(seq(1,11, by = 1), a))
names(ax) <- c("dept", "a")
ax$dept <- as.numeric(ax$dept)

nwd <- full_join(d2, ax) 
nwds <- nwd$a

nwds
# Ordering
fg_ord <- as.vector(unique(fg[,c("d_s")]))

avec <- as.vector(fg_ord[['d_s']])
 

# # Bands
# fg1 <- fg %>%
#   select(d_s, dept, num_late_at_departure, num_of_canceled_trains) %>%
#   mutate(num_time = 100 - num_late_at_departure - num_of_canceled_trains) %>%
#   group_by(d_s, dept) %>%
#   summarise_if(is.numeric, funs(mean)) %>%
#   mutate_if(is.numeric, round) 
# 
# names(fg1) <- c("a_s", "dept", "Late", "Cancelled", "Punctual")
# fg_m <- as.matrix(fg1[,3:5])
# rownames(fg_m) <- fg1$a_s


arcplot(ft_m, 
        sorted = F,
        ordering = avec,
        
        lwd.arcs = E(ft_lab)$weight, 
        col.arcs = cols,
        
        show.nodes = T,
        cex.nodes = 3,
        col.nodes = nwds,
        #pch.nodes = 1,
        
        lend = 0,
        
        cex.labels = 0.8,
        mar=c(4,4,4,4) # margins
        )




# # arc-diagram with bands
# arcBands(ft_m, fg_m,
#          lwd = E(ft_lab)$weight*3, 
#          col = cols,
#          sorted = T,
#          
#          col.bands=col.bands
#          )
# title("SNCF", cex.main=0.9)
# legend(x=0.9, y=0.5, title="", text.col="gray65", cex=0.8,
#        legend=c("Late","Cancelled","Punctual"), pch=19, col=col.bands, bty="n")
