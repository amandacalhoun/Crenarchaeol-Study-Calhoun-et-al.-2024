#Code for plots and statistics for Calhoun et al. 2023

#Uses pacman package to open libraries with necessary functions
pacman::p_load("data.table", "dplyr", "ggplot2", "ggpubr", "ggtext", "lubridate", "tidyverse", 
               "devtools", "ggcorrplot", "FactoMineR", "factoextra", "vegan", "adespatial"
               , "ggfortify","grid", "plotrix", "ggbreak", "ggExtra", "olsrr", "ggsci", "patchwork", "cowplot", "aplot")

#allows you to find the working directory for your file
#file.choose()
#Set working directory to folder with the data file(s) of interest - change for your computer
setwd("C:\\Users\\amand\\OneDrive\\Desktop\\Crenarchaeol Senior Thesis Dartmouth\\CORRECT DATA")


#Import all data into central table - this is compilation data
allData = fread("allData.csv")
springData = allData[Source != "Culture"]
#Import only data from Calhoun et al., 2023
yellowstone = fread("yellowstone_data.csv")


#Create a table of only compilation samples with cren and only those without for chosen sample set
allDataWithCren = allData[(cren != 0) & (cren != 0)]
allDataWithoutCren = allData[cren == 0]

#Create a table of only Yellowstone samples with CL and IPL cren and only those without for chosen sample set
yellowstonewoCLcren = yellowstone[(cren_CL == 0)]
yellowstonewCLcren = yellowstone[(cren_CL != 0) ]
yellowstonewoIPLcren = yellowstone[(cren_IPL == 0)]
yellowstonewIPLcren = yellowstone[(cren_IPL != 0) ]

#Make data table with only non-NA Do values of Yellowstone dataset  
doData = yellowstone[!(is.na(do_mg))]


#Set general theme applied to all plots  
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


neutral_pH_line <- data.frame(
  pH = c(7.4692,7.2692,7.0852,6.9172,6.7652,6.6292,6.5092,6.4052,6.3172,6.2452,
         6.1892,6.1492),  # Example pH values
  temp = c(0,10,20,30,40,50,60,70,80,90,100,110)  # Example temperature values
)

#Bubble plot (main figure)
#jpeg(
 # filename="MainFigureR.jpeg",
  #width=8,
  #height=5.5,
  #units="in",
  #res=500)
ggplot() +
  geom_point(data = allDataWithoutCren, aes(x = pH, y = temp), color = "black", size = 1, pch = 0) +
  geom_point(data = allDataWithCren, aes(x = pH, y = temp, size = cren^2, color = Source), alpha = 1.0) +
  geom_point(data = allDataWithCren, aes(x = pH, y = temp, size = cren^2), color = "black",  alpha = 0.5, pch = 1) +  # Outline points
  scale_size_continuous(range = c(0, 15), 
                        breaks = c(0.01, 0.25, 0.5, 0.75, 1.00),
                        guide = guide_legend(title = "Crenarchaeol\nRelative Abundance",
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
  coord_cartesian(xlim = c(1.3, 10.2), ylim = c(16, 96))+
  scale_x_continuous(breaks = 1:10)+
  scale_y_continuous(breaks = seq(10, 90, by = 10))+
  scale_color_manual(values = c("#ff073a", "lightblue", "orange"), guide = guide_legend(title = "Source", 
    override.aes = list(size = 3)))+
  scale_shape_manual(values = c(1, 0), labels = c("With Cren", "Without Cren"))  # Specify the shapes and labels




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



ggplot() +
  geom_point(data = yellowstonewoIPLcren, aes(x = pH, y = temp), color = "black", size = 1.5, pch = 0) +
  geom_point(data = yellowstonewIPLcren, aes(x = pH, y = temp, size = cren_CL^2, color = "red"), alpha = 1.0) +
  geom_point(data = yellowstonewIPLcren, aes(x = pH, y = temp, size = cren_CL^2), color = "black",  alpha = 0.5, pch = 1) +  # Outline points
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










#Run again to show plot with added histograms
q = ggMarginal(p, data = allDataWithoutCren, aes(x = pH, y = temp), type="histogram", size = 5, xparams = list(bins=18), yparams = list(bins = 16))
#dev.off()
 


jpeg(
filename="Fig2withlegend.jpeg",
width=8,
height=5.5,
units="in",
res=500)

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
dev.off()
         










# Create custom legends for Temperature and pH

legend_temp <- guide_colorbar(
  title = "Temperature (\u00B0C)",
  #title.position = "top", 
  barwidth = 15,
  barheight = 1.2,
  label.position = "bottom",
  title.theme = element_text(size = 12),  # Adjust title size
  label.theme = element_text(size = 12)  # Adjust label size
)
legend_pH <- guide_colorbar(
  title = "pH",
  barwidth = 15,
  barheight = 1.2,
  label.position = "bottom",
  title.theme = element_text(size = 12),  # Adjust title size
  label.theme = element_text(size = 12)  # Adjust label size
)





# Create separate plots for each subplot
plot1 <- ggplot(data = allData, aes(x = pH, y = cren, fill = temp)) +
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


plot2 <- ggplot(data = allData, aes(x = temp, y = cren, fill = pH)) +
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


plot3 <- ggplot(data = allData, aes(x = pH, y = RI, fill = temp)) +
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



plot4 <- ggplot(data = allData, aes(x = temp, y = RI, fill = pH)) +
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

jpeg(
filename="FourpanelFig.jpeg",
width=8,
height=8,
units="in",
res=500)
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
dev.off()






ggMarginal(
  ggplot() +
    geom_point(data = allDataWithoutCren, aes(x = pH, y = temp), color = "black", size = 1, pch = 0) +
    geom_point(data = allDataWithCren, aes(x = pH, y = temp, size = cren^2, color = Source), alpha = 1.0) +
    geom_point(data = allDataWithCren, aes(x = pH, y = temp, size = cren^2), color = "black",  alpha = 0.5, pch = 1) +  # Outline points
    scale_size_continuous(range = c(0, 15), 
                          breaks = c(0.01, 0.25, 0.5, 0.75, 1.00),
                          guide = guide_legend(title = "Crenarchaeol\nRelative Abundance",
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
    coord_cartesian(xlim = c(1.3, 10.2), ylim = c(16, 96)) +
    scale_x_continuous(breaks = 1:10) +
    scale_y_continuous(breaks = seq(10, 90, by = 10)) +
    scale_color_manual(values = c("#FF073a", "lightblue", "orange"), guide = guide_legend(title = "Source", 
                                                                                          override.aes = list(size = 3))),
  data = allDataWithoutCren,
  type = "histogram",
  size = 5,
  xparams = list(bins = 18),
  yparams = list(bins = 16)
)












































################################################################################
#BOX AND WHISKER PLOTS 
################################################################################
#Make new table to mess with
table = springData

#Repeat process for pH bins
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
plot5
plot5 <- plot5 + scale_x_discrete(labels = custom_labels)




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

################################################################################
#LOESS Smoothing Curves
################################################################################


#Repeat process for pH

########pH loess smooth 
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
  scale_y_continuous(breaks = seq(0, 1.0, by = .10), limits = c(0, 1)) +
  labs(x="pH", y="Crenarchaeol Relative Abundance")+
  geom_hline(yintercept = .117, linetype = "dashed")+
  theme(axis.text.y.right = element_blank(),axis.ticks.y.right = element_blank(), axis.title = element_text(size = 15),axis.text = element_text(size = 13)  # Hide right-side y-axis labels
        )



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
  scale_y_continuous(breaks = seq(0, 1.0, by = .10), limits = c(0, 1)) +
  labs(x="Temperature (\u00B0C)", y=NULL)+
  geom_hline(yintercept = .15, linetype = "dashed")+
  theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank(), axis.text.y.left = element_blank(), axis.title = element_text(size = 15), axis.text = element_text(size = 13)  # Hide right-side y-axis labels
  )
plot8
















row1 <- plot5 + plot6

row2 <- plot7 + plot8


jpeg(
  filename="statsplottest.jpeg",
  width=10,
  height=10,
  units="in",
  res=500)


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
dev.off()




















plot9 = ggplot() +
  geom_point(data = yellowstonewoCLcren, aes(x = pH, y = temp), color = "black", size = 1, shape = 0) +
  geom_point(data = yellowstonewCLcren, aes(x = pH, y = temp, size = cren_CL^2), alpha = 1.0, color = "pink") +
  geom_point(data = yellowstonewCLcren, aes(x = pH, y = temp, size = cren_CL^2), color = "black",  alpha = 0.5, pch = 1) +  # Outline points
  scale_size_continuous(range = c(0, 10), 
                        breaks = c(0.01, 0.02, 0.03, 0.04, 0.05),
                        guide = guide_legend(title = "Crenarchaeol\nRelative Abundance",
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
  scale_x_continuous(breaks = 1:10)+
  scale_y_continuous(breaks = seq(60, 100, by = 10), limits = c(60, 100))+
  scale_shape_manual(values = c(0), labels = c("Without Cren"))  # Specify the shapes and labels



plot10 = ggplot() +
  geom_point(data = yellowstonewoIPLcren, aes(x = pH, y = temp), color = "black", size = 1, shape = 0) +
  geom_point(data = yellowstonewIPLcren, aes(x = pH, y = temp, size = cren_IPL^2), alpha = 1.0, color = "lightgreen") +
  geom_point(data = yellowstonewIPLcren, aes(x = pH, y = temp, size = cren_IPL^2), color = "black",  alpha = 0.5, pch = 1) +  # Outline points
  scale_size_continuous(range = c(0, 20), 
                        breaks = c(0.01, 0.02, 0.03, 0.04, 0.05),
                        guide = guide_legend(title = "Crenarchaeol\nRelative Abundance",
                                             override.aes = list(shape = 1))) +
  geom_line(data = neutral_pH_line, aes(x = pH, y = temp), color = "black") +
  season_facet +
  labs(
    x = 'pH',
    y = NA,
    size = 'Crenarchaeol\nRelative Abundance'
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18)
  ) +
  scale_x_continuous(breaks = 1:10)+
  scale_y_continuous(breaks = seq(60, 100, by = 10), limits = c(60, 100))+
  scale_shape_manual(values = c(0), labels = c("Without Cren"))  # Specify the shapes and labels


plot_grid(
  plot9,
  plot10# Adjust the width of the second column
)


jpeg(
  filename="FourpanelFig.jpeg",
  width=8,
  height=8,
  units="in",
  res=500)
dev.off()



