### Load packages
# install.packages('ggplot2')
# install.packages('RColorBrewer')
#install.packages('ggpubr')
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
#### Generate Dummy Dataset
ndf <- data.frame(
  value     = rep(runif(100)), #Random numbers between 0 and 1; repeat 100 times
  factor1   = as.character(rep(sample(1:2, replace = TRUE, 100)))) # Random selection of either 1 or 2 as an independent variable

ndf$factor1 = factor(ndf$factor1, levels = c('1', '2')) #We have categorical variables. Here we set the order that they will be plotted on the x-axis.

### It's Plotting Time!

plot_data <- ggplot(ndf, aes(y = value, x = factor1, color = factor1)) +
  #Specify dataset followed by x and y variable names EXACTLY as they appear in your dataset
  scale_color_manual(values = c('1' = 'blue', '2' = 'firebrick4')) + # Set Manual Colors 
  stat_boxplot( aes(y = value, x = factor1 ), 
                geom='errorbar', width = 0.5) + #Add in a boxplot with error bars, half the width of the box
  geom_boxplot(aes(y = value, x =  factor1))  + #Actual boxplot
  theme_bw() + #Set theme to black and white to get rid of all the grey in the plot
  geom_dotplot(binaxis='y', #add dots representing the y variable
               stackdir = "center",  #position them centered above one another
               binwidth = 1/20, #specify how close together the dots themselves will be
               dotsize = .5,     #specify the size of the dots
               aes(fill = factor1)) + #specify inside color of the dots is dependent on 'factor1' 
  scale_fill_manual(values = c('1' = '#0571b0', '2' = '#ca0020')) + #Add a fill color using RColorBrewer to make sure its colour-blind friendly
  xlab("Independent\nVariable") + ylab("Dependent\nVariable") + #Make some labels! \n lets you start the rest of the label on a separate line
  ggtitle("Box-Dot Plot") + #Let's give the plot a title because plots love titles!
  theme(  panel.grid.major = element_blank(), #Let's get rid of the grid inside the plot
          panel.grid.minor = element_blank(), #Let's get rid of the grid lines
          axis.line = element_line(colour = "black"),  #Change the color of the axes from default grey to black
          panel.border = element_blank(), #get rid of the border 
          plot.margin=unit(c(0.5,0.5, 0.5,0.5),"cm"), #Set the empty space outside of the plot
          legend.position="none", #Let's you pick a position for the legend 
          axis.title.x= element_text(size=14, color = 'black', vjust = 0.5),
          axis.title.y= element_text(size=14, color = 'black', vjust = 0.5),
          axis.text.x = element_text(size=12, color = 'black'),
          axis.text.y = element_text(size=14, color = 'black'),
          plot.title  = element_text(hjust = 0.5, size=16, face = 'bold', vjust = 0.5)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,1,0.2), #Set the way the y axis looks and how far breaks are
                     limits = c(0,1.1)) + #Start the plot at 0,0 and leave some room at the top so our title isn't too close to the plot itself
  scale_x_discrete(labels = c('Nicolas Cage', 'Danny DeVito')) + #Customize the names of your variables
  stat_compare_means(method = 't.test', hide.ns = FALSE, size = 4, vjust = 2) #Test the means and position the output on the plot 


plot_data 
