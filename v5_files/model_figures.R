
chyp_data <- chyp_data |>
  mutate(month = mon_names()[month])

cfin_data <- data |>
  mutate(month = mon_names()[month])


ggplot(cfin_data, aes(x = lon, y = lat)) +
  geom_polygon(data = ggplot2::map_data("world"),
               aes(long, lat, group = group),
               fill = "lightgray", col = "gray",
               linewidth = .2) +
  geom_point(aes(col = patch), alpha = .7, size = .3) +
  scale_color_manual(values = c("yellow", "blue")) +
  coord_quickmap(xlim = c(-76, -40), ylim = c(35, 60), expand = TRUE) +
  theme_bw() + 
  theme(axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank()) +
  ggtitle("a) C. finmarchicus") +
  facet_wrap(~month, nrow = 2, ncol = 6) +
  theme(legend.position = "none")

# combined effect
Bathy_test <- 0:1000
b <- as.data.frame(x = Bathy_test)
plot_data <- 
  rbind(mutate(b, type = 'Prey Density Correction',
               val = 0.147235074555841 + 
                 0.980070557219308 * exp(-0.0101656880852439 * Bathy_test)),
        mutate(b, type = 'Foraging Time Correction',
               val = get_foraging_table("rest")[round(pmin(1000, pmax(10, Bathy_test))/10, 0), 2]),
        mutate(b, type = 'Combined Effect',
               val = stephane_final()(Bathy_test))) |>
  mutate(type = factor(type, levels = c('Foraging Time Correction',
                                        'Prey Density Correction',
                                        'Combined Effect')))

as.data.frame(x = Bathy_test) |>
  mutate(`Combined Effect` = stephane_final()(Bathy_test),
         `Prey Density Correction` = 0.147235074555841 + 
           0.980070557219308 * exp(-0.0101656880852439 * Bathy_test),
         `Foraging Time Correction` = 
           get_foraging_table("rest")[round(pmin(1000, pmax(10, Bathy_test))/10, 0), 2]) |> 
  ggplot(aes(x = Bathy_test)) +
  geom_line(aes(y = `Prey Density Correction`), color = 'grey50') +
  geom_line(aes(y = `Foraging Time Correction`), color = 'grey50') +
  geom_line(aes(y = `Combined Effect`), color = 'red', linewidth = 1) + 
  theme_bw() +
  labs(y = "Combined effect", x = "Bathymetry")  


ggplot(plot_data, aes(x = Bathy_test, y = val, col = type)) +
  scale_color_manual(values = c("grey50", "grey50", "red")) +
  geom_line(linewidth = 1) +
  facet_wrap(~type) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Correction Factor", x = "Bathymetry")  
