library(ggplot2)

ggplot(mtcars, aes(mpg, hp) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "blue")



# with a legend
ggplot(mtcars, aes(mpg, hp) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(color = "black") ) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = "red") ) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(color = "blue") ) +
  scale_color_identity(guide = "legend")

# customized legend
ggplot(mtcars, aes(mpg, hp) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(color = "black") ) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = "red") ) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(color = "blue") ) +
  scale_color_identity(name = "Model fit",
                       breaks = c("black", "red", "blue"),
                       labels = c("Linear", "Quadratic", "Cubic"),
                       guide = "legend")

# with descriptive names
ggplot(mtcars, aes(mpg, hp) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(color = "Linear") ) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = "Quadratic") ) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(color = "Cubic") )

# use named vector to map to the colors
ggplot(mtcars, aes(mpg, hp) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(color = "Linear") ) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = "Quadratic") ) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, aes(color = "Cubic") ) +
  scale_color_manual(name = "Model fit",
                     breaks = c("Linear", "Quadratic", "Cubic"),
                     values = c("Cubic" = "blue", "Quadratic" = "red", "Linear" = "black") )
