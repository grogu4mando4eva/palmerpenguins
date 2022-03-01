##number of NA values
penguins %>% 
  apply(.,2,is.na) %>%
  apply(.,2,sum)
##drop NA values
penguins %>% 
  drop_na()
## create NA-free data subset
penguins_nafree <- penguins %>% drop_na()
## learning functions for separating and analysing data
filter(penguins_nafree, island == "Torgersen" )
summarise(penguins_nafree, average_bill_length = mean(bill_length_mm))
group_by(penguins_nafree, species)
penguins_nafree %>% 
  group_by(species) %>% 
  summarise(average_bill_length = mean(bill_length_mm))
penguins_nafree %>% 
  group_by(island,species) %>% 
  summarise(average_bill_length = mean(bill_length_mm))
## create a plot to analyse different groups of data
penguins_nafree %>%
  filter(., sex != "male") %>%
  dplyr::select(c("species", "island", "body_mass_g")) %>%
  group_by(species, island) %>%
  summarise(total_mass_g = sum(body_mass_g)) %>%
  pivot_wider(names_from = c("island"), values_from = total_mass_g)
## creating ggplot for pengs
ggplot(penguins,aes(x = body_mass_g, y = flipper_length_mm)) + ## data & aesthetics
  geom_point() + ## geom
  geom_smooth(method = 'lm', se = FALSE) ## statistics (linear regression line) 
## creating box plot
ggplot(penguins,aes(x = species, y = flipper_length_mm)) + ## data & aesthetics
  geom_boxplot()  + ## geom
  ggtitle("Flipper length (mm) by species") +
  ylab("Flipper length (mm)") +
  xlab("Species") +
  theme_dark() ## theme
## creating scatter plot with colour
ggplot(penguins,aes(x = body_mass_g, y = flipper_length_mm, color = species)) + ## data and aesthetics
  geom_point() + ## geom
  geom_smooth(method = 'lm', se = FALSE) ## statistic (linear regression line without intervals)
## box
box <- ggplot(penguins,aes(x = species, y = flipper_length_mm)) + ## data & aesthetics
  geom_boxplot()  + ## geom
  ggtitle("Flipper length (mm) by species") +
  ylab("Flipper length (mm)") +
  xlab("Species") +
  theme_dark() ## theme
box
## jitter
jitter <- ggplot(penguins,aes(x = species, y = flipper_length_mm)) + ## data & aesthetics
  geom_jitter()  + ## geom
  ggtitle("Flipper length (mm) by species") +
  ylab("Flipper length (mm)") +
  xlab("Species") +
  theme_dark() ## theme
jitter
## violin
violin <- ggplot(penguins,aes(x = species, y = flipper_length_mm)) + ## data & aesthetics
  geom_violin()  + ## geom
  ggtitle("Flipper length (mm) by species") +
  ylab("Flipper length (mm)") +
  xlab("Species") +
  theme_dark() ## theme
violin
## 
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue", se = FALSE)
## creating multiple scatter plots with title and legend
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm, 
                     col = species)) +
  geom_point(size = 2, alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(~ sex) +
  theme_bw() + 
  labs(title = "Flipper Length and Body Mass, by Sex & Species",
       subtitle = paste0(nrow(penguins), " of the Palmer Penguins"),
       x = "Body Mass (g)", 
       y = "Flipper Length (mm)")
## creating multiple scatter plots with title and legend with penguins_nafree
penguins_nafree <- penguins %>% drop_na()
ggplot(penguins_nafree, aes(x = body_mass_g, y = flipper_length_mm, 
                            col = species)) +
  geom_point(size = 2, alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(~ sex) +
  theme_bw() + 
  labs(title = "Flipper Length and Body Mass, by Sex & Species",
       subtitle = paste0(nrow(penguins_nafree), " of the Palmer Penguins"),
       x = "Body Mass (g)", 
       y = "Flipper Length (mm)")
