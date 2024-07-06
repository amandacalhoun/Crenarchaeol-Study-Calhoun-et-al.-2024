#Code for plots and statistics for Calhoun et al. 2024
#First section includes code for figures and second section has code for statistics and models 

#Uses pacman package to open libraries with necessary functions 
#  note that not all of these packages may be necessary as the code has evolved over time
pacman::p_load("data.table", "dplyr", "ggplot2", "ggpubr", "ggtext", "lubridate", "tidyverse", 
               "devtools", "ggcorrplot", "FactoMineR", "factoextra", "vegan", "adespatial"
               , "ggfortify","grid", "plotrix", "ggbreak", "ggExtra", "olsrr", "ggsci", "patchwork", "cowplot", "aplot")

#allows you to find the working directory for your file
#file.choose()

#Set working directory to folder with the data file(s) of interest - change for your computer
#Make sure to download the data files from the GitHub repository and 
#change the working directory to where you downloaded the files
setwd("C:\\Users\\amand\\OneDrive\\Desktop\\Crenarchaeol Senior Thesis Dartmouth\\CORRECT DATA")

################################################################################
#GET DATA INTO TABLES FOR LATER USE
################################################################################

#Import all data into central table - this is compilation data
allData = fread("allData.csv")
#Make a datatable of only environmental hot spring data (excluding culture)
springData = allData[Source != "Culture"]
#Import only new Yellowstone data from Calhoun et al., 2024
yellowstone = fread("yellowstone_data.csv")


#Create a table of only compilation samples with cren and only those without 
allDataWithCren = allData[(cren != 0) & (cren != 0)]
allDataWithoutCren = allData[cren == 0]

#Create a table of only environmental samples with cren and only those without 
springDataWithCren = springData[(cren != 0) & (cren != 0)]
springDataWithoutCren = springData[cren == 0]


#Create a table of only Yellowstone samples with CL and IPL cren and only those without
yellowstonewoCLcren = yellowstone[(cren_CL == 0)]
yellowstonewCLcren = yellowstone[(cren_CL != 0) ]
yellowstonewoIPLcren = yellowstone[(cren_IPL == 0)]
yellowstonewIPLcren = yellowstone[(cren_IPL != 0) ]
yellowstonewcren = yellowstone[(cren_CL != 0)&(cren_IPL != 0)]

#Make data table with only non-NA Do values of Yellowstone dataset for optional use later
doData = yellowstone[!(is.na(do_mg))]


#Set general theme applied to all plots - edit base plot aesthetics if desired
season_facet = theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(size = 0.5),
    strip.background = element_rect(fill = 'white'),
    text = element_text(size=12),
    axis.text = element_text(size = 12), 
    plot.title = element_text(size = 13))


#See paper for citation on neutral pH line varying with temperature - fit for plots 
neutral_pH_line <- data.frame(
  pH = c(7.4692,7.2692,7.0852,6.9172,6.7652,6.6292,6.5092,6.4052,6.3172,6.2452,
         6.1892,6.1492),  # Example pH values
  temp = c(0,10,20,30,40,50,60,70,80,90,100,110)  # Example temperature values
)


################################################################################
################################################################################
#FIGURES
################################################################################
################################################################################



################################################################################
#BUBBLE PLOTS
################################################################################
####Main text plot Figure 3
ggplot() +
  geom_point(data = allDataWithoutCren, aes(x = pH, y = temp, shape = "Without Cren"), color = "black", size = 1) +
  geom_point(data = allDataWithCren, aes(x = pH, y = temp, size = cren^2, color = Source), shape = 16, alpha = 1.0) +
  geom_point(data = allDataWithCren, aes(x = pH, y = temp, size = cren^2), color = "black",  alpha = 0.5, shape = 1) +  
  scale_size_continuous(range = c(0, 15), 
                        breaks = c(0.01, 0.25, 0.5, 0.75, 1.00),
                        guide = guide_legend(title = "Crenarchaeol\nRelative Abundance",
                                             override.aes = list(shape = 1),order = 1)) +
  geom_line(data = neutral_pH_line, aes(x = pH, y = temp), color = "black") +
  season_facet +
  labs(
    x = 'pH',
    y = 'Temperature (\u00B0C)',
    size = 'Crenarchaeol\nRelative Abundance'
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18)
  ) +
  coord_cartesian(xlim = c(1.3, 10.2), ylim = c(16, 96)) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = seq(10, 90, by = 10)) +
  scale_color_manual(values = c("lightblue", "orange", "#ff073a"), guide = guide_legend(title = "Source", 
                                                                                        override.aes = list(size = 3), order=3)) +
  scale_shape_manual(values = c(0), labels = c( "Below Detection Limit"), 
                     guide = guide_legend(title = NULL, override.aes = list(size = 3), order=2))



#Bubble plot without culture data - not in manuscript
#Uncomment the below code to save the figure to computer working directory - use dev.off() to turn off later 
#jpeg(
# filename="MainFigureR.jpeg",
#width=8,
#height=5.5,
#units="in",
#res=500)
ggplot() +
  geom_point(data = springDataWithoutCren, aes(x = pH, y = temp, shape = "Without Cren"), color = "black", size = 1) +
  geom_point(data = springDataWithCren, aes(x = pH, y = temp, size = cren^2, color = Source), shape = 16, alpha = 1.0) +
  geom_point(data = springDataWithCren, aes(x = pH, y = temp, size = cren^2), color = "black",  alpha = 0.5, shape = 1) +  
  scale_size_continuous(range = c(0, 15), 
                        breaks = c(0.01, 0.25, 0.5, 0.75, 1.00),
                        guide = guide_legend(title = "Crenarchaeol\nRelative Abundance",
                                             override.aes = list(shape = 1),order = 1)) +
  geom_line(data = neutral_pH_line, aes(x = pH, y = temp), color = "black") +
  season_facet +
  labs(
    x = 'pH',
    y = 'Temperature (\u00B0C)',
    size = 'Crenarchaeol\nRelative Abundance'
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18)
  ) +
  coord_cartesian(xlim = c(1.3, 10.2), ylim = c(16, 96)) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = seq(10, 90, by = 10)) +
  scale_color_manual(values = c("orange", "#ff073a"), guide = guide_legend(title = "Source", 
                                                                           override.aes = list(size = 3), order=3)) +
  scale_shape_manual(values = c(0), labels = c( "Below Detection Limit"), 
                     guide = guide_legend(title = NULL, override.aes = list(size = 3), order=2))
#dev.off()



#Bubble for yellowstone CL data - not in manuscript
ggplot() +
  geom_point(data = yellowstonewoCLcren, aes(x = pH, y = temp), color = "black", size = 1.5, pch = 0) +
  geom_point(data = yellowstonewCLcren, aes(x = pH, y = temp, size = cren_CL^2, color = "red"), alpha = 1.0) +
  geom_point(data = yellowstonewCLcren, aes(x = pH, y = temp, size = cren_CL^2), color = "black",  alpha = 0.5, pch = 1) +  # Outline points
  scale_size_continuous(range = c(0, 15), 
                        breaks = c(0.001, 0.01, 0.02),
                        guide = guide_legend(title = "CL Crenarchaeol\nRelative Abundance",
                                             override.aes = list(shape = 1))) +
  geom_line(data = neutral_pH_line, aes(x = pH, y = temp), color = "black") +
  season_facet +
  labs(
    x = 'pH',
    y = 'Temperature (\u00B0C)',
    size = 'Crenarchaeol\nRelative Abundance'
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18)
  ) +
  annotate("text",
           x = 1.2,
           y = 100,
           label = "A",
           fontface = "bold",
           size = 8,
           col = "black",
           vjust = 1.5, hjust = 0.5) +
  coord_cartesian(xlim = c(1.3, 10.2), ylim = c(16, 96))+
  scale_x_continuous(breaks = 1:10)+
  scale_y_continuous(breaks = seq(10, 90, by = 10))+
  scale_color_manual(values = "red", guide = 'none')+                                                              
  scale_shape_manual(values = c(1, 0), labels = c("With Cren", "Without Cren"))  # Specify the shapes and labels


#Bubble for yellowstone IPL data - not in manuscript
ggplot() +
  geom_point(data = yellowstonewoIPLcren, aes(x = pH, y = temp), color = "black", size = 1.5, pch = 0) +
  geom_point(data = yellowstonewIPLcren, aes(x = pH, y = temp, size = cren_IPL^2, color = "red"), alpha = 1.0) +
  geom_point(data = yellowstonewIPLcren, aes(x = pH, y = temp, size = cren_IPL^2), color = "black",  alpha = 0.5, pch = 1) +  # Outline points
  scale_size_continuous(range = c(0, 15), 
                        breaks = c(0.001, 0.01, 0.02),
                        guide = guide_legend(title = "IPL Crenarchaeol\nRelative Abundance",
                                             override.aes = list(shape = 1))) +
  geom_line(data = neutral_pH_line, aes(x = pH, y = temp), color = "black") +
  season_facet +
  labs(
    x = 'pH',
    y = 'Temperature (\u00B0C)',
    size = 'Crenarchaeol\nRelative Abundance'
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18)
  ) +
  annotate("text",
           x = 1.2,
           y = 100,
           label = "B",
           fontface = "bold",
           size = 8,
           col = "black",
           vjust = 1.5, hjust = 0.5) +
  coord_cartesian(xlim = c(1.3, 10.2), ylim = c(16, 96))+
  scale_x_continuous(breaks = 1:10)+
  scale_y_continuous(breaks = seq(10, 90, by = 10))+
  scale_color_manual(values = "red", guide = 'none')+                                                              
  scale_shape_manual(values = c(1, 0), labels = c("With Cren", "Without Cren"))  # Specify the shapes and labels




################################################################################
#PLOTS OF CREN AND RI V PH AND TEMP WITH GRADIENT FILL BY OTHER VARIABLE
################################################################################

# Create custom gradient fill legends for Temperature
legend_temp <- guide_colorbar(
  title = "Temperature (\u00B0C)",
  #title.position = "top", 
  barwidth = 15,
  barheight = 1.2,
  label.position = "bottom",
  title.theme = element_text(size = 12),  # Adjust title size
  label.theme = element_text(size = 12)  # Adjust label size
)

# Create custom gradient fill legends for pH
legend_pH <- guide_colorbar(
  title = "pH",
  barwidth = 15,
  barheight = 1.2,
  label.position = "bottom",
  title.theme = element_text(size = 12),  # Adjust title size
  label.theme = element_text(size = 12)  # Adjust label size
)


####Create subplots for four-panel Figure 4 of cren and RI verss pH and temp
##Panel 1
plot1 <- ggplot(data = springData, aes(x = pH, y = cren, fill = temp)) +
  geom_point(pch = 21, size = 5, stroke = 0.5, color = 'black', alpha = 0.7) +
  scale_fill_gradientn(values = c(1, .5, 0), colours = c("red", "blue", "cyan")) +
  season_facet +
  labs(
    x = NULL,
    y = 'Cren Relative Abundance',
    fill = '**Spring Temperature**'
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    axis.text.x = element_blank()
  ) +
  annotate("text",
           x = 1,
           y = 1,
           label = "A",
           fontface = "bold",
           size = 8,
           col = "black",
           vjust = 1.5, hjust = 0.5) +
  guides(fill = legend_temp) +
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(breaks = seq(2, 10, by = 2), labels = scales::number_format(accuracy = 1))# Adjust the breaks and labels as needed

##Panel 2
plot2 <- ggplot(data = springData, aes(x = temp, y = cren, fill = pH)) +
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', alpha = 0.7) +
  scale_fill_gradient(low = "lavender", high = "darkmagenta") +
  season_facet +
  labs(
    x = NULL,
    y = NULL,
    fill = '**Spring pH**'
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  annotate("text",
           x = 1,
           y = 1,
           label = "B",
           fontface = "bold",
           size = 8,
           col = "black",
           vjust = 1.5, hjust = 0.5) +
  guides(fill = legend_pH) +
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.1))+  # Adjust the breaks and labels as needed
  scale_x_continuous(breaks = seq(20, 80, by = 20), labels = scales::number_format(accuracy = 10))# Adjust the breaks and labels as needed

##Panel 3
plot3 <- ggplot(data = springData, aes(x = pH, y = RI, fill = temp)) +
  geom_point(pch = 21, size = 5, stroke = 0.5, color = 'black', alpha = 0.7) +
  scale_fill_gradientn(values = c(1, .5, 0), colours = c("red", "blue", "cyan")) +
  season_facet +
  labs(
    x = 'pH',
    y = 'Ring Index',
    fill = '**Spring Temperature**'
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13)
  ) +
  annotate("text",
           x = 1,
           y = 5.5,
           label = "C",
           fontface = "bold",
           size = 8,
           col = "black",
           vjust = 1.5, hjust = 0.5) +
  guides(fill = legend_temp) +
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 5, by = 1), labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(breaks = seq(2, 10, by = 2), labels = scales::number_format(accuracy = 1))# Adjust the breaks and labels as needed
# Adjust the breaks and labels as needed

##Panel 4
plot4 <- ggplot(data = springData, aes(x = temp, y = RI, fill = pH)) +
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', alpha = 0.7) +
  scale_fill_gradient(low = "lavender", high = "darkmagenta") +
  season_facet +
  labs(
    x = 'Temperature (\u00B0C)',
    y = NULL,
    fill = '**Spring pH**'
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    axis.text.y = element_blank()
  ) +
  annotate("text",
           x = 1,
           y = 5.5,
           label = "D",
           fontface = "bold",
           size = 8,
           col = "black",
           vjust = 1.5, hjust = 0.5) +
  guides(fill = legend_pH) +
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 5, by = 1), labels = scales::number_format(accuracy = 0.1))+  # Adjust the breaks and labels as needed  coord_equal()
  scale_x_continuous(breaks = seq(20, 80, by = 20), labels = scales::number_format(accuracy = 10))# Adjust the breaks and labels as needed

#Arrange the plots with cowplot
plot_grid(
  plot1 + theme(legend.position = "top"),
  plot2 + theme(legend.position = "top"),
  plot3 + theme(legend.position = "none"),
  plot4 + theme(legend.position = "none"),
  nrow = 2,
  rel_heights = c(1, 0.95),
  rel_widths = c(1, 0.9)  # Adjust the width of the second column
)




################################################################################
#BOX AND WHISKER PLOTS 
################################################################################
#Make new table to mess with
table = springData

#Make pH bins
binTable = mutate(table, bin=cut_width(pH, width=.5, boundary=0) )
stat_box_data <- function(y, upper_limit = max(iris$Sepal.Length) * 1.15) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}

binmax = list(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)

#Make box and whisker plot with pH bins 
custom_labels <- c("1", "", "2","", "3","", "4", "","5", "", "6", 
                   "", "7", "", "8", "", "9", "", "10")
plot5 = ggplot(binTable, aes(x=bin, y=cren, fill = bin) ) +
  geom_boxplot(alpha = 0.3) +
  scale_y_break(c(0.4, 0.35), scales = 0.2)+
  season_facet+
  labs(x = NULL, y = "Crenarchaeol Relative Abundance") +
  ylab("Crenarchaeol Relative Abundance")+
  annotate("text",
           x = 1,
           y = max(binTable$cren),
           label = "A",
           fontface = "bold",
           size = 8,
           col = "black",
           vjust = 1.5, hjust = 0.5) +
  annotate("text",
           x = 1:length(table(binTable$bin)),
           y = aggregate(cren ~ bin, binTable, median)[ , 2],
           label = table(binTable$bin),
           col = "black",
           vjust = -.3)+
  theme(legend.position = "none", axis.title = element_text(size = 15),axis.text = element_text(size = 13), axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank())
plot5 <- plot5 + scale_x_discrete(labels = custom_labels)
plot5



#Make temperature bins
binTable = mutate(table, bin=cut_width(temp, width=5, boundary=0) )
stat_box_data <- function(y, upper_limit = max(iris$Sepal.Length) * 1.15) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}
binTable <- mutate(binTable, binlast = last(bin))

custom_labels2 <- c("15", "20", "25","30", "35","40", "45", "50","55", "60", "65", 
                    "70", "75", "80", "85", "90")
#Make box and whisker plot in Temp bins
plot6 = ggplot(binTable, aes(x=bin, y=cren, fill = bin) ) +
  geom_boxplot(alpha = 0.3) +
  scale_y_break(c(0.4, 0.35), scales = 0.2)+
  season_facet+
  # annotate("text", x = 2, y = 0.8, label = 'N = 299')+
  #ylab("Cren Relative Abundance")+
  #xlab("Temperature (\u00B0C)")+
  annotate("text",
           x = 1,
           y = max(binTable$cren),
           label = "B",
           fontface = "bold",
           size = 8,
           col = "black",
           vjust = 1.5, hjust = 0.5) +
  labs(x = NULL, y = NULL) +
  annotate("text",
           x = 1:length(table(binTable$bin)),
           y = aggregate(cren ~ bin, binTable, median)[ , 2],
           label = table(binTable$bin),
           col = "black",
           vjust = -1.3)+
  theme(legend.position = "none", axis.title = element_text(size = 15),axis.text = element_text(size = 13), axis.text.x.top = element_blank(),axis.text.y.left = element_blank(), axis.ticks.x.top = element_blank())

plot6 <- plot6 + scale_x_discrete(labels = custom_labels2)
plot6


################################################################################
#LOESS Smoothing Curves
################################################################################

#pH loess smooth 
lo = loess(cren ~ pH, data=springData, span=.9)

f = function(x){
  p = predict(lo, newdata = data.frame(pH=x))
  (p-7)^2
}

opt2 <- optimize(f, c(0,11))
min2 = opt2$minimum
plot7 = ggplot(data = springData, aes(x = pH, y = cren)) +
  geom_point() +
  season_facet +
  annotate("text",
           x = 1,
           y = max(binTable$cren),
           label = "C",
           fontface = "bold",
           size = 8,
           col = "black",
           vjust = 1.5, hjust = 0.5) +
  scale_y_break(c(0.3, 0.31), scales = 0.5)+
  geom_smooth(method = "loess", size = 1.5, span = 0.9, se = TRUE) +
  geom_vline(xintercept = min2, linetype = "dashed") + 
  scale_x_continuous(breaks = seq(0, 11, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1.0, by = .10), limits = c(-0.1, 1)) +
  labs(x="pH", y="Crenarchaeol Relative Abundance")+
  geom_hline(yintercept = .117, linetype = "dashed")+
  theme(axis.text.y.right = element_blank(),axis.ticks.y.right = element_blank(), axis.title = element_text(size = 15),axis.text = element_text(size = 13)  # Hide right-side y-axis labels
  )
plot7


#95% confidence interval band by default 
#Make curve for cren v temp
lo = loess(cren ~ temp, data=springData, span=.9)

f = function(x){
  p = predict(lo, newdata = data.frame(temp=x))
  (p-40)^2
}

opt1 <- optimize(f, c(0,100))
min1 = opt1$minimum

plot8 = ggplot(data = springData, aes(x = temp, y = cren)) +
  geom_point() +
  season_facet +
  annotate("text",
           x = 16,
           y = 1,
           label = "D",
           fontface = "bold",
           size = 8,
           col = "black",
           vjust = 1.5, hjust = 0.5) +
  scale_y_break(c(0.3, 0.31), scales = 0.5)+
  geom_smooth(method = "loess", size = 1.5, span = 0.9, se = TRUE) +
  geom_vline(xintercept = min1, linetype = "dashed") + 
  scale_x_continuous(breaks = seq(0, 100, by = 20), limits = c(15, 95)) +
  scale_y_continuous(breaks = seq(0, 1.0, by = .10), limits = c(-0.1, 1)) +
  labs(x="Temperature (\u00B0C)", y=NULL)+
  geom_hline(yintercept = .15, linetype = "dashed")+
  theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank(), axis.text.y.left = element_blank(), axis.title = element_text(size = 15), axis.text = element_text(size = 13)  # Hide right-side y-axis labels
  )
plot8

row1 <- plot5 + plot6

row2 <- plot7 + plot8

combined_plot <- row1 / row2

# Add "A" and "B" labels back to the combined plot
combined_plot <- combined_plot +
  annotation_custom(
    grob = textGrob(label = "A", gp = gpar(fontsize = 15, fontface = "bold")),
    xmin = 1.5, xmax = 2, ymin = Inf, ymax = Inf
  ) +
  annotation_custom(
    grob = textGrob(label = "B", gp = gpar(fontsize = 15, fontface = "bold")),
    xmin = 1.5, xmax = 2, ymin = Inf, ymax = Inf
    
  )+ 
  annotation_custom(
    grob = textGrob(label = "C", gp = gpar(fontsize = 15, fontface = "bold")),
    xmin = 1.5, xmax = 2, ymin = Inf, ymax = Inf
    
  )+
  annotation_custom(
    grob = textGrob(label = "D", gp = gpar(fontsize = 15, fontface = "bold")),
    xmin = 1.5, xmax = 2, ymin = Inf, ymax = Inf
    
  )+
  
  
  # Print or save the combined plot
  print(combined_plot)



################################################################################
#GEOCHEMISTRY ONLY PLOTS
################################################################################

######## Temp v pH plot
ggplot(data = yellowstone, aes(x = pH, y = temp)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=3.8, label.y=62, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=4.0, label.y=67, color = "blue", size = 5)+
  season_facet +
  labs(x="pH", y="Temp", title="Temp v. pH")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))

#### DO mg/L v pH Plot
ggplot(data = yellowstone, aes(x = pH, y = do_mg)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=4, label.y=0.7, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=4.3, label.y=0.9, color = "blue", size = 5)+
  season_facet +
  labs(x="pH", y="DO (mg/L)", title="DO (mg/L) v. pH")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))

#### DO mg/L v temp Plot
ggplot(data = yellowstone, aes(x = temp, y = do_mg)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=50, label.y=1.05, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=55, label.y=1.2, color = "blue", size = 5)+
  season_facet +
  labs(x="Temp", y="DO (mg/L)", title="DO (mg/L) v. Temp")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))

#### pH v ORP Plot
ggplot(data = yellowstone, aes(x = orp, y = pH)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=170, label.y=4.6, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=210, label.y=5.2, color = "blue", size = 5)+
  season_facet +
  labs(x="ORP", y="pH", title="pH v. ORP")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))

#### pH v SPC Plot
ggplot(data = yellowstone, aes(x = spc, y = pH)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=300, label.y=4.6, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=650, label.y=5.2, color = "blue", size = 5)+
  season_facet +
  labs(x="SPC", y="pH", title="pH v. SPC")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))


#### temp v ORP Plot
ggplot(data = yellowstone, aes(x = orp, y = temp)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=-300, label.y=70, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=-280, label.y=75, color = "blue", size = 5)+
  season_facet +
  labs(x="ORP", y="Temp", title="Temp v. ORP")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))


#### temp v SPC Plot
ggplot(data = yellowstone, aes(x = spc, y = temp)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=-300, label.y=65, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=-280, label.y=70, color = "blue", size = 5)+
  season_facet +
  labs(x="SPC", y="Temp", title="Temp v. SPC")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))

ironTable = yellowstone[(is.na(fe) == FALSE)]
ironTable = ironTable[,logfe := log(fe)]
#### Fe2+ v. pH Plot
ggplot(data = ironTable, aes(x = pH, y = logfe)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=5, label.y=-4, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=5.3, label.y=-3.5, color = "blue", size = 5)+
  season_facet +
  labs(x="pH", y="log(Iron (II) mg/L)", title="Fe 2+ v. pH")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))







################################################################################
#CL V. IPL AND RI WITH V. WITHOUT CREN COMPARISONS
################################################################################

#### IPL Cren v CL Cren Plot
ggplot(data = yellowstone, aes(x = cren_CL, y = cren_IPL)) + 
  geom_point(pch = 21, size = 5, stroke = 1.0, color = 'black') + 
  geom_smooth(method = lm, se = TRUE, size = 1.0) +
  #stat_regline_equation(label.x=0.07, label.y=0.2, color = "blue", size = 5)+
  annotate("text", x = 0.095, y = 0.2, label = 'y = 1.5x - 0.0058', size = 5, color = "blue")+
  annotate("text", x = 0.095, y = 0.22, label = 'p-value = 9.669E-16 ', size = 5, color = "blue")+
  annotate("text", x = 0.095, y = 0.24, label = expression('adj-R'^'2'*' = 0.81'), size = 5, color = "blue")+
  #annotate("text", x = 0.095, y = 0.24, label = 'adj-R^2*', size = 5, color = "blue")+
  #stat_cor(aes(label=..rr.label..), label.x=0.08, label.y=0.24, color = "blue", size = 5)+
  season_facet +
  annotate("text", x = 0.01, y = 0.35, label = 'N = 41', size = 6)+
  #annotate("text", x = 0.03, y = 0.33, label = 'adj-R^2 = %')+
  #annotate("text", x = 0.03, y = 0.30, label = "Spearman's p-value = ")+
  labs(x="CL Cren", y="IPL Cren", title="IPL v. CL Cren")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))

#### CL v IPL cren for springs with only CL and IPL cren present
ggplot(data = yellowstonewcren,aes(x = cren_CL, y = cren_IPL)) + 
  geom_point(pch = 21, size = 5, stroke = 1.0, color = 'black') + 
  geom_smooth(method = lm, se = TRUE, size = 1) +
  #stat_regline_equation(label.x=.03, label.y=.15, color = "blue", size = 5)+
  annotate("text", x = 0.095, y = 0.2, label = 'y = 1.6x - 0.0081', size = 5, color = "blue")+
  annotate("text", x = 0.095, y = 0.22, label = 'p-value = 2.032E-8 ', size = 5, color = "blue")+
  annotate("text", x = 0.095, y = 0.24, label = expression('adj-R'^'2'*' = 0.79'), size = 5, color = "blue")+
  #stat_cor(aes(label=..rr.label..), label.x=0.08, label.y=0.24, color = "blue", size = 5)+
  season_facet +
  annotate("text", x = 0.01, y = 0.35, label = 'N = 22', size = 6)+
  labs(x="CL Cren", y="IPL Cren", title="CL v IPL Cren (only springs with cren)")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))



#### CL RI with and without cren comparison
ggplot(data = yellowstonewcren,aes(x = RI_CL_cren, y = RI_CL_nocren)) + 
  geom_point(size = 5, stroke = 0.2, fill = 'black') + 
  geom_smooth(method = lm, se = TRUE, size = 2) +
  stat_regline_equation(label.x=2.5, label.y=3.3, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=2.7, label.y=3.5, color = "blue", size = 5)+
  season_facet +
  labs(x="CL RI with Cren", y="CL RI without Cren", title="CL RI with and without Cren")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))


#### IPL RI with and without cren comparison
ggplot(data = yellowstonewcren, aes(x = RI_IPL_cren, y = RI_IPL_nocren)) + 
  geom_point(size = 5, stroke = 0.2, fill = 'black') + 
  geom_smooth(method = lm, se = TRUE, size = 2) +
  stat_regline_equation(label.x=2.5, label.y=3.3, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=2.7, label.y=3.5, color = "blue", size = 5)+
  season_facet +
  labs(x="IPL RI with Cren", y="IPL RI without Cren", title="IPL RI with and without Cren")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))






















################################################################################
################################################################################
#STATISTICS
################################################################################
################################################################################






################################################################################
#NORMALITY TESTING - all variables non-normal
################################################################################

# Shapiro-Wilk normality test for pH
     #Null hypothesis is data is normal, so p-value <0.05 is a non-normal distribution
     #Plug in different variables from "yellowstone" and "springData" as desired
shapiro.test(yellowstone$pH) 
shapiro.test(springData$pH) 




################################################################################
#Spearman's rho correlations
################################################################################
#Plug in different variables from "yellowstone" and "springData" as desired

cor.test(yellowstone$pH, yellowstone$cren_CL,  method = "spearman", exact = FALSE)

cor.test(springData$pH, springData$cren,  method = "spearman", exact = FALSE)





################################################################################
#SIMPLE LINEAR REGRESSIONS
################################################################################
#Plug in different variables from "yellowstone" and "springData" as desired
lm(cren ~ pH, data = springData)

#temp and RI
lm(cren_CL ~ pH, data = yellowstone)



################################################################################
#MULTIPLE LINEAR REGRESSIONS
################################################################################
#Plug in different variables from "yellowstone" and "springData" as desired

###########COMPILATION DATA
lm(cren ~ scale(pH) + scale(temp), data = springData)

###########YELLOWSTONE DATA
mult = lm(cren_CL ~ scale(pH) + scale(spc) + scale(orp) + scale(temp) + scale(s_ug), data = yellowstone)
summary(mult)


#GET ALL YELLOWSTONE MODELS AND SELECT WHICH BEST AND WHICH TOP 10
allmodels = ols_step_all_possible(mult)
allmodels

bestmodel = allmodels[which.max(allmodels$adjr),]
bestmodel

#####Use the following lines to adjust the cutoff to find the best 10 models 
   #in terms of adj-R^2
#Will have to change the >= "VALUE" based on modeled parameter 
  #0.067 for cren_CL model  
top10models = allmodels[which(allmodels$adjr >= 0.067),]
top10models










################################################################################
#MODELS NOT IN THE PAPER THAT TEST INTERACTION TERMS AND QUADRATIC TERMS
################################################################################
#Interaction term
summary(lm(formula = cren ~ pH*temp, data = springData))

#Quadratic term
summary(quadratic_model <- lm(cren ~ pH + I(pH^2), data = springDataWithCren))

#Interaction and quadratic terms 
summary(quadratic_model2 <- lm(cren ~  (pH+I(pH^2))*(temp + I(temp^2)),
                              data = springData))


################################################################################
#CORRELATION PLOTS NOT IN THE PAPER
################################################################################

library(corrplot)
library(datasets)

yellowstoneCorr <- yellowstone[, -c(1:6, 15, 16, 19:21, 24)]
yellowstoneCorr <- yellowstoneCorr %>% mutate_all(~ifelse(is.nan(.), NA, .))
# Calculate Spearman correlation coefficients
cor_matrix <- cor(yellowstoneCorr, method = "spearman", use = 'pairwise.complete.obs')

corrplot(cor_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Create a correlation heatmap
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.cex = 0.7,
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  title = "Spearman Correlation Heatmap"
)





