# To load rayshader you need to have xQuartz downloaded on the Mac. See
# https://www.xquartz.org/ and
# https://github.com/tylermorganwall/rayshader/issues/5 etc.

library(rayshader)
library(animation)
library(gifski)
library(tidyverse)

# Key trick is the use of render_depth() to create the png, at least until we
# try to allow for interactive use. Or maybe render_movie()?

# Code for the first two models rayshader plot

city_pop <- 100000
has_disease <- 0.01 * city_pop

test_positive_true <- 0.95 * has_disease
test_negative_false <- 0.05 * has_disease

test_positive_false <- (city_pop - has_disease) * 0.05  
test_negative_true <- (city_pop - has_disease) * 0.95  

pop <- tibble(patient_id = 1:100000, 
              have_disease = c(rep(1, has_disease),
                               rep(0, city_pop - has_disease)),         
              positive_test = c(rep(1, test_positive_true),
                                rep(0, test_negative_false),
                                rep(0, test_negative_true),
                                rep(1, test_positive_false)))

pop_sum <- pop  %>%
  group_by(positive_test, have_disease) %>%
  summarize(total = n())

pos_factor <- as.factor(pop_sum$positive_test)
disease_factor <- as.factor(pop_sum$have_disease)

mtplot = ggplot(pop_sum) +
  geom_point(aes(x = pos_factor, y = disease_factor, color = total)) +
  theme(legend.position = "none") +
  labs(x = "Test Result", y = "Disease Status")

plot_gg(mtplot, width = 3.5, multicore = F, windowsize = c(800, 800), 
        zoom = 0.85, phi = 30, theta = 10, sunangle = 225, soliddepth = -100,
        reduce_size = TRUE, raytrace = FALSE)

# render_snapshot(file = "05-probability/images/rayshader_disease.png")


# Code for the second two models rayshader plot where we isolate a slice

pop <- tibble(patient_id = 1:1980, positive_test = c(rep(1, 990),
                                                     rep(1, 990)),
              have_disease = c(rep(1, 990),
                               rep(0, 990))) %>%
  group_by(positive_test, have_disease) %>%
  summarize(total = n())

pos_factor <- as.factor(pop$positive_test)
disease_factor <- as.factor(pop$have_disease)

mtplot = ggplot(pop) +
  geom_point(aes(x = pos_factor, y = disease_factor, color = total)) +
  theme(legend.position = "none") +
  labs(x = "Test Result", y = "Disease Status")

plot_gg(mtplot, width = 3.5, multicore = F, windowsize = c(800, 800), 
        zoom = 0.85, phi = 30, theta = 10, sunangle = 225, soliddepth = -100,
        reduce_size = TRUE, raytrace = FALSE)

# Code for joint distributions rayshader plot

mydata <- tibble(die.1 = sample(1:6, size = 1000, prob =
                                  c(0.1,0.1,0.1,0.1,0.1,0.5), replace = TRUE),
                 die.2 = sample(1:6, size = 1000, prob =
                                  c(0.1,0.1,0.1,0.1,0.5,0.1), replace = TRUE)) %>%
  group_by(die.1, die.2) %>%
  summarize(total = n())

die1_factor <- as.factor(mydata$die.1)
die2_factor <- as.factor(mydata$die.2)

mtplot = ggplot(mydata) +
  geom_point(aes(x=die1_factor,y=die2_factor,color=total)) +
  scale_color_continuous(limits=c(0,100)) +
  theme(legend.position = "none") +
  labs(x = "Die 1", y = "Die 2")

plot_gg(mtplot, width = 3.5, multicore = F, windowsize = c(800, 800), 
        zoom = 0.85, phi = 30, theta = 10, sunangle = 225, soliddepth = -100,
        reduce_size = TRUE, raytrace = FALSE)

# Code for 3 models rayshader

x <- tibble(p = rep(seq(0, 1, 0.5), 1000)) %>%
  mutate(white_marbles = map_int(p, ~ rbinom(n = 1, size = 3, p = .))) %>%
  group_by(p,white_marbles) %>%
  summarize(total = n())

p_factor <- as.factor(x$p)
x_factor <- as.factor(x$white_marbles)

mtplot = ggplot(x) +
  geom_point(aes(x=x_factor,y=p_factor,color=total)) +
  scale_color_continuous() +
  theme(legend.position = "none") +
  labs(x = "Number of White Marbles Out of 3 Samples",
       y = "Number of White Marbles in the Bag")

plot_gg(mtplot, width = 3.5, multicore = F, windowsize = c(800, 800), 
        zoom = 0.85, phi = 30, theta = 10, sunangle = 225, soliddepth = -100,
        reduce_size = TRUE, raytrace = FALSE)

# Code for N models plot

set.seed(10)

x <- tibble(p = rep(seq(0, 1, 0.1), 1000)) %>%
  mutate(heads = map_int(p, ~ rbinom(n = 1, size = 20, p = .))) %>%
  group_by(p,heads) %>%
  summarize(total = n())

p_factor <- as.factor(x$p)
x_factor <- as.factor(x$heads)

mtplot = ggplot(x) +
  geom_point(aes(x = x_factor, y = p_factor, color = total)) +
  theme(legend.position = "none") +
  labs(x = "Number of Heads out of 20 Tosses",
       y = "Value of p")

plot_gg(mtplot, width = 3.5, multicore = F, windowsize = c(800, 800), 
        zoom = 0.85, phi = 30, theta = 10, sunangle = 225, soliddepth = -100,
        reduce_size = TRUE, raytrace = FALSE)


