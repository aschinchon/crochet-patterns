# Load the tidyverse
library(tidyverse)

# This function creates an individual pattern
pattern <- function(a, b, c = pi/2, d = pi/5, e = 4) {
  data_frame(
    x = accumulate(1:40, ~.x-sin((.y %% e)*c-ceiling((.y-1)/e)*d), .init = a),
    y = accumulate(1:40, ~.x+cos((.y %% e)*c-ceiling((.y-1)/e)*d), .init = b))
}

# I arrange patterns on a circle
t <- seq(0, 2*pi, length.out = 120)
centers <- data_frame(x = sin(t), y = cos(t))

# To initialize parameters of function pattern
c  <- sample(seq(from = 0, to = 2*pi, by = pi/32), 1)
d  <- sample(seq(from = 0, to = 2*pi, by = pi/32), 1)
e  <- sample(1:20, 1)
  
# This creates the plot
apply(centers, 1, function(r) pattern(a = r[1], 
                                      b = r[2],
                                      c = c,
                                      d = d,
                                      e = e)) %>% 
  bind_rows(.id="df") %>% 
  ggplot() +
  geom_path(aes(x, y, group = df), alpha = 0.15) + 
  coord_fixed()+
  theme_void()

# Do you like the drawing? Save it!
ggsave("choose_a_name.png", height=5, width=5, units='in', dpi=800)
  
