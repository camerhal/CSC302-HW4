cars93 <- MASS::Cars93
# 
# # LOESS
# ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
#   geom_point(color = "grey60") +
#   geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "#0072B2") +
#   scale_x_continuous(
#     name = "price (USD)",
#     breaks = c(20, 40, 60),
#     labels = c("$20,000", "$40,000", "$60,000")
#   ) +
#   scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")
#   



# LM
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "lm", color = "#8fe388") +
  labs(title = "LM Method") +
  theme(plot.title = (text = element_text(color='#8fe388', size = 14))) +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")


# GLM
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "glm", color = "#fe8d6d") +
  labs(title = "GLM Method") +
  theme(plot.title = (text = element_text(color='#fe8d6d', size = 14))) +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")


# GAM
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "gam", color = "#7c6bea") +
  labs(title = "GAM Method") +
  theme(plot.title = (text = element_text(color='#7c6bea', size = 14))) +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")






# 5. Please inspect the following code which can be also found in TimeSeries_Trends.R and try to run how it generates
# three time series in a single plot. Then, perform the steps in the following bullet points:

load("G:/.shortcut-targets-by-id/1ehWwunuAo7CE1Vk2JYkUnQMmxh5pph3C/DATA/preprint_growth.rda") #please change the path if needed
head(preprint_growth)

preprint_growth %>% filter(archive == "bioRxiv") %>%
  filter(count > 0) -> biorxiv_growth

preprints<-preprint_growth %>% filter(archive %in%
                                        c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%filter(count > 0) %>% 
                                         mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))

preprints_final <- filter(preprints, date == ymd("2017-01-01"))

ggplot(preprints) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis( #this part is for the second y axis
      breaks = preprints_final$count, #and we use the counts to position our labels
      labels = c("arXivq-bio", "PeerJPreprints", "bioRxiv"),
      name = NULL)
  ) +
  scale_x_date(name = "year",
               limits = c(min(biorxiv_growth$date), ymd("2017-01-01"))) +
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                     name = NULL) +
  theme(legend.position = "none")

#===================================================================================================================

# (a) By using drop_na( ) and filter( ) on preprint_growth data frame, get the rows which have count greater
# than 0 and year later than 2004, and output it to another data frame called preprint_full.
# (b) Use the filter function again to select the rows that have "bioRxiv", "F1000Research" in it only by looking at the
# example in the code above.
# (c) Draw line graphs for these two time series, "bioRxiv" and "F1000Research", by coloring them with "#7c6bea" and
# "#fe8d6d".
# (d) Put the legend to the right of the figure.
# (e) For the x-axis, start the values from Feb 2014.
# (f) Add a title "Preprint Counts" to the figure.

preprint_growth %>% drop_na()

preprint_full<-preprint_growth %>% filter(archive %in% c("bioRxiv", "F1000Research")) %>% filter(count > 0) %>% filter(date > ymd("2014-02-01"))

ggplot(preprint_full) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) + 
  scale_color_manual(values = c("#7c6bea", "#fe8d6d"), name = NULL) +
  scale_x_date(name = "year",
               limits = c(min(preprint_full$date), ymd("2018-01-01"))) +
  theme(legend.position = "right") +
  scale_x_date(name = "year") 
  labs(title = "Preprint Counts")

  
  
  

