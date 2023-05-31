#Code for plots and statistics for Calhoun et al. 2023


#Uses pacman package to open libraries with necessary functions
pacman::p_load("data.table", "ggplot2", "ggpubr", "ggtext", "lubridate", "tidyverse", 
               "devtools", "ggcorrplot", "FactoMineR", "factoextra", "vegan", "adespatial"
               , "ggfortify","plotrix", "ggbreak", "ggExtra")


#allows you to find the working directory for your file
#file.choose()
#Set working directory to folder with the data file(s) of interest - change for your computer
setwd("C:\\Users\\amand\\OneDrive\\Desktop\\Senior Thesis\\CORRECT DATA")


#Import all data into central table 
allData = fread("allData.csv")

#Import only data from Calhoun et al., 2023
allData = fread("yellowstone_data.csv")

#Create a table of only samples with CL and IPL cren and only those without for chosen sample set
allDataWithCren = allData[(cren_CL != 0) & (cren_IPL != 0)]
allDataWithoutCren = allData[(cren_CL == 0) & (cren_IPL == 0)]


#Theme for plots  
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


################################################################################
#GEOCHEMISTRY ONLY PLOTS
################################################################################

######## Temp v pH plot
ggplot(data = allData, aes(x = pH, y = temp)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=3.8, label.y=62, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=4.0, label.y=67, color = "blue", size = 5)+
  season_facet +
  labs(x="pH", y="Temp", title="Temp v. pH")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))

#### DO mg/L v pH Plot
ggplot(data = allData, aes(x = pH, y = do_mg)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=4, label.y=0.7, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=4.3, label.y=0.9, color = "blue", size = 5)+
  season_facet +
  labs(x="pH", y="DO (mg/L)", title="DO (mg/L) v. pH")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))

#### DO mg/L v temp Plot
ggplot(data = allData, aes(x = temp, y = do_mg)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=50, label.y=1.05, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=55, label.y=1.2, color = "blue", size = 5)+
  season_facet +
  labs(x="Temp", y="DO (mg/L)", title="DO (mg/L) v. Temp")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))

#### pH v ORP Plot
ggplot(data = allData, aes(x = orp, y = pH)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=170, label.y=4.6, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=210, label.y=5.2, color = "blue", size = 5)+
  season_facet +
  labs(x="ORP", y="pH", title="pH v. ORP")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))

#### pH v SPC Plot
ggplot(data = allData, aes(x = spc, y = pH)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=300, label.y=4.6, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=650, label.y=5.2, color = "blue", size = 5)+
  season_facet +
  labs(x="SPC", y="pH", title="pH v. SPC")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))


#### temp v ORP Plot
ggplot(data = allData, aes(x = orp, y = temp)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=-300, label.y=70, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=-280, label.y=75, color = "blue", size = 5)+
  season_facet +
  labs(x="ORP", y="Temp", title="Temp v. ORP")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))


#### temp v SPC Plot
ggplot(data = allData, aes(x = spc, y = temp)) + 
  geom_point(pch = 21, size = 5, stroke = 0.2, color = 'black', fill = 'black') + 
  geom_smooth(method = lm, se = FALSE, size = 2) +
  stat_regline_equation(label.x=-300, label.y=65, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=-280, label.y=70, color = "blue", size = 5)+
  season_facet +
  labs(x="SPC", y="Temp", title="Temp v. SPC")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))


#Create a table of only samples with measured Fe concentrations
ironTable = allData[(is.na(fe) == FALSE)]
#Create a column with the log of Fe concentrations
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
ggplot(data = allData, aes(x = cren_CL, y = cren_IPL)) + 
  geom_point(pch = 21, size = 5, stroke = 1.0, color = 'black') + 
  geom_smooth(method = lm, se = TRUE, size = 1.0) +
  annotate("text", x = 0.095, y = 0.2, label = 'y = 1.5x - 0.0058', size = 5, color = "blue")+
  annotate("text", x = 0.095, y = 0.22, label = 'p-value = 9.669E-16 ', size = 5, color = "blue")+
  annotate("text", x = 0.095, y = 0.24, label = expression('adj-R'^'2'*' = 0.81'), size = 5, color = "blue")+
  season_facet +
  annotate("text", x = 0.01, y = 0.35, label = 'N = 41', size = 6)+
  labs(x="CL Cren", y="IPL Cren", title="IPL v. CL Cren")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))

#### CL v IPL cren for springs with only CL and IPL cren present
ggplot(data = allDataWithCren,aes(x = cren_CL, y = cren_IPL)) + 
  geom_point(pch = 21, size = 5, stroke = 1.0, color = 'black') + 
  geom_smooth(method = lm, se = TRUE, size = 1) +
  annotate("text", x = 0.095, y = 0.2, label = 'y = 1.6x - 0.0081', size = 5, color = "blue")+
  annotate("text", x = 0.095, y = 0.22, label = 'p-value = 2.032E-8 ', size = 5, color = "blue")+
  annotate("text", x = 0.095, y = 0.24, label = expression('adj-R'^'2'*' = 0.79'), size = 5, color = "blue")+
  season_facet +
  annotate("text", x = 0.01, y = 0.35, label = 'N = 22', size = 6)+
  labs(x="CL Cren", y="IPL Cren", title="CL v IPL Cren (only springs with cren)")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))



#### CL RI with and without cren comparison
ggplot(data = allDataWithCren,aes(x = RI_CL_cren, y = RI_CL_nocren)) + 
  geom_point(size = 5, stroke = 0.2, fill = 'black') + 
  geom_smooth(method = lm, se = TRUE, size = 2) +
  stat_regline_equation(label.x=2.5, label.y=3.3, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=2.7, label.y=3.5, color = "blue", size = 5)+
  season_facet +
  labs(x="CL RI with Cren", y="CL RI without Cren", title="CL RI with and without Cren")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))


#### IPL RI with and without cren comparison
ggplot(data = allDataWithCren, aes(x = RI_IPL_cren, y = RI_IPL_nocren)) + 
  geom_point(size = 5, stroke = 0.2, fill = 'black') + 
  geom_smooth(method = lm, se = TRUE, size = 2) +
  stat_regline_equation(label.x=2.5, label.y=3.3, color = "blue", size = 5)+
  stat_cor(aes(label=..rr.label..), label.x=2.7, label.y=3.5, color = "blue", size = 5)+
  season_facet +
  labs(x="IPL RI with Cren", y="IPL RI without Cren", title="IPL RI with and without Cren")+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))



################################################################################
#CREN and RI V GEOCHEM (CL AND IPL SPLIT) Yellowstone Samples
################################################################################

#### CL Cren v pH Plot without axis break
ggplot(data = allData, aes(x = pH, y = cren_CL, fill = temp)) +
  geom_point(pch = 21, size = 7, stroke = 0.5, color = 'black', alpha = 0.7, 
  ) +
  scale_fill_gradient(low = "deepskyblue3", high = "red") +
  geom_smooth(method = 'lm', se=FALSE, size = 2, color = "black")+
  stat_regline_equation(label.x=3.5, label.y=0.05, color = "black", size = 4)+
  stat_cor(aes(label=..rr.label..), label.x=4.5, label.y=0.07, color = "black", size = 4)+
  annotate("text", x = 9, y = 0.35, label = 'p-value = ')+
  annotate("text", x = 9, y = 0.33, label = 'adj-R^2 = %')+
  annotate("text", x = 8.4, y = 0.30, label = "Spearman's p-value = ")+
  season_facet +
  xlim(0, 10)+
  ylim(0, .35)+
  labs(
    x = 'pH',
    y = 'CL Rel Abundance Cren',
    title = "CL Cren v. pH",
    fill = '**Spring Temperature**<br>(*measured at site*)'
  ) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))+
  theme(legend.position = c(.1,.6),
        legend.justification = 'left',
        legend.key.width = unit(1.5,"cm"),
        legend.title = element_markdown(),
        legend.key.size = unit(1.5, "cm"))


#### CL Cren v pH Plot with axis break
ggplot(data = allData, aes(x = pH, y = cren_CL, fill = temp)) +
  geom_point(pch = 21, size = 7, stroke = 0.5, color = 'black', alpha = 0.7, 
  ) +
  scale_y_break(c(0.04, 0.07), scales = 0.5)+
  scale_fill_gradientn(values=c(1, .5, 0), colours=c("red", "blue", "cyan"))+
  geom_smooth(method = 'lm', se=TRUE, size = 1, color = "black")+
  annotate("text", x = 4.3, y = 0.034, label = 'y = 0.0045x - 0.0072', size = 5)+
  annotate("text", x = 0.4, y = 0.19, label = 'N = 41', size = 5)+
  annotate("text", x = 4.3, y = 0.037, label = 'p-value = 5.98E-2', size = 5)+
  annotate("text", x = 4.3, y = 0.040, label = expression('adj-R'^'2'*' = 0.065'), size = 5)+
  season_facet +
  labs(
    x = 'pH',
    y = 'CL Rel Abundance Cren',
    title = "CL Cren v. pH",
    fill = '**Spring Temperature**<br>(*measured at site*)'
  ) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))+
  theme(legend.position = c(.1,.6),
        legend.justification = 'left',
        legend.key.width = unit(1.5,"cm"),
        legend.title = element_markdown(),
        legend.key.size = unit(1.5, "cm"))


#### CL Cren v temp Plot without axis break 
ggplot(data = allData, aes(x = temp, y = cren_CL, fill = pH)) +
  geom_point( 
    pch = 21, size = 7, stroke = 0.2, color = 'black', alpha = 0.7, 
  ) +
  scale_fill_gradient(low = "darkmagenta", high = "lavender") +
  season_facet +
  geom_smooth(method = 'lm', se=TRUE, size = 2, color = "black")+
  stat_regline_equation(label.x=50, label.y=0.05, color = "black", size = 4)+
  stat_cor(aes(label=..rr.label..), label.x=55, label.y=0.07, color = "black", size = 4)+
  annotate("text", x = 85, y = 0.35, label = 'p-value = 0.7268')+
  annotate("text", x = 85, y = 0.33, label = 'adj-R^2 = -2.24%')+
  annotate("text", x = 82, y = 0.30, label = "Spearman's p-value = 0.5804")+
  ylim(0, .35)+
  labs(
    x = 'Temperature',
    y = 'CL Rel Abundance Cren',
    title = "CL Cren v. Temp",
    fill = '**Spring pH**<br>(*measured at site*)'
  ) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))+
  theme(legend.position = c(.1,.6),
        legend.justification = 'left',
        legend.key.width = unit(1.5,"cm"),
        legend.title = element_markdown(),
        legend.key.size = unit(1.5, "cm"))


#### CL Cren v temp Plot with axis break
ggplot(data = allData, aes(x = temp, y = cren_CL, fill = pH)) +
  geom_point( 
    pch = 21, size = 7, stroke = 0.2, color = 'black', alpha = 0.7, 
  ) +
  scale_y_break(c(0.04, 0.07), scales = 0.5)+
  scale_fill_gradient(low = "darkmagenta", high = "lavender") +
  season_facet +
  geom_smooth(method = 'lm', se=TRUE, size = 1, color = "black")+
  annotate("text", x = 50, y = 0.034, label = 'y = 0.00011x + 0.01', size = 5)+
  annotate("text", x = 30, y = .19, label = 'N = 41', size = 5)+
  annotate("text", x = 50, y = 0.037, label = 'p-value = 8.26E-1', size = 5)+
  annotate("text", x = 50, y = 0.04, label = expression('adj-R'^'2'*' = -0.024'), size = 5)+
  labs(
    x = 'Temperature (\u00B0C)',
    y = 'CL Rel Abundance Cren',
    title = "CL Cren v. Temp",
    fill = '**Spring pH**<br>(*measured at site*)'
  ) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))+
  theme(legend.position = c(.1,.6),
        legend.justification = 'left',
        legend.key.width = unit(1.5,"cm"),
        legend.title = element_markdown(),
        legend.key.size = unit(1.5, "cm"))


#### CL RI v pH Plot 
ggplot(data = allData, aes(x = pH, y = RI_CL_cren, fill = temp)) +
  geom_point(pch = 21, size = 7, stroke = 0.5, color = 'black', alpha = 0.7, 
  ) +
  scale_fill_gradientn(values=c(1, .5, 0), colours=c("red", "blue", "cyan"))+
  geom_smooth(method = 'lm', se=FALSE, size = 1, color = "black")+
  annotate("text", x = 3, y = 2.4, label = 'y = -0.26x + 4.5', size = 5)+
  annotate("text", x = 8.5, y = 4.4, label = 'N = 41', size = 5)+
  annotate("text", x = 3, y = 2.6, label = 'p-value = 1.65E-10', size = 5)+
  annotate("text", x = 3, y = 2.8, label = expression('adj-R'^'2'*' = 0.645'), size = 5)+
  season_facet +
  labs(
    x = 'pH',
    y = 'CL Ring Index with Cren',
    title = "CL Ring Index v. pH",
    fill = '**Spring Temperature**'
  ) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))+
  theme(plot.margin=unit(c(1,5,1,0.5),"cm")) +
  theme(legend.position = c(1.01,0.5),
        legend.justification = 'left',
        legend.key.width = unit(1.5,"cm"),
        legend.title = element_markdown(),
        legend.key.size = unit(1.5, "cm"))


#### CL RI v temp Plot 
ggplot(data = allData, aes(x = temp, y = RI_CL_cren, fill = pH)) +
  geom_point( 
    pch = 21, size = 7, stroke = 0.2, color = 'black', alpha = 0.7, 
  ) +
  scale_fill_gradient(low = "darkmagenta", high = "lavender") +
  season_facet +
  geom_smooth(method = 'lm', se=FALSE, size = 1, color = "black")+
  annotate("text", x = 55, y = 2.8, label = 'y = -0.03x + 5.3', size = 5)+
  annotate("text", x = 90, y = 4.5, label = 'N = 41', size = 5)+
  annotate("text", x = 55, y = 3, label = 'p-value = 4.27E-3', size = 5)+
  annotate("text", x = 55, y = 3.2, label = expression('adj-R'^'2'*' = 0.170'), size = 5)+
  labs(
    x = 'Temperature (\u00B0C)',
    y = 'CL RI with Cren',
    title = "CL RI v. Temp",
    fill = '**Spring pH**<br>(*measured at site*)'
  ) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))+
  theme(plot.margin=unit(c(1,5,1,0.5),"cm")) +
  theme(legend.position = c(1.01,0.5),
        legend.justification = 'left',
        legend.key.width = unit(1.5,"cm"),
        legend.title = element_markdown(),
        legend.key.size = unit(1.5, "cm"))




################################################################################
#CREN and RI V GEOCHEM Compilation Samples
################################################################################

#### CREN v pH Plot 
ggplot(data = allData, aes(x = pH, y = cren, fill = temp)) +
  geom_point(pch = 21, size = 7, stroke = 0.5, color = 'black', alpha = 0.7, 
  ) +
  scale_y_break(c(0.25, 0.25), scales = 0.5)+
  scale_fill_gradientn(values=c(1, .5, 0), colours=c("red", "blue", "cyan"))+
  geom_smooth(method = 'lm', se=FALSE, size = 1, color = "black")+
  annotate("text", x = 3, y = .06, label = 'y = 0.015x - 0.023', size = 4)+
  annotate("text", x = 1.5, y = 1.0, label = 'N = 299', size = 4)+
  annotate("text", x = 3, y = .08, label = 'p-value = 9.76E-4', size = 4)+
  annotate("text", x = 3, y = .10, label = expression('adj-R'^'2'*' = 0.033'), size = 4)+
  season_facet+
  labs(
    x = 'pH',
    y = 'Cren Relative Abundance',
    fill = '**Spring Temperature**<br>(*measured at site*)'
  ) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))+
  theme(plot.margin=unit(c(1,5,1,0.5),"cm"))+
  theme(legend.position = c(1.01,0.5),
        legend.justification = 'left',
        legend.key.width = unit(1.5,"cm"),
        legend.title = element_markdown(),
        legend.key.size = unit(1.5, "cm"))




#### Cren v temp Plot 
ggplot(data = allData, aes(x = temp, y = cren, fill = pH)) +
  geom_point( 
    pch = 21, size = 7, stroke = 0.2, color = 'black', alpha = 0.7, 
  ) +
  scale_y_break(c(0.25, 0.25), scales = 0.5)+
  scale_fill_gradient(low = "darkmagenta", high = "lavender") +
  season_facet +
  geom_smooth(method = 'lm', se=FALSE, size = 1, color = "black")+
  annotate("text", x = 25, y = .11, label = 'y = -0.0027x + 0.25', size = 4)+
  annotate("text", x = 20, y = 1.0, label = 'N = 299', size = 4)+
  annotate("text", x = 25, y = .13, label = 'p-value = 1.37E-6', size = 4)+
  annotate("text", x = 25, y = .15, label = expression('adj-R'^'2'*' = 0.073'), size = 4)+
  labs(
    x = 'Temperature (\u00B0C)',
    y = 'Cren Relative Abundance',
    #title = "CL RI v. Temp",
    fill = '**Spring pH**<br>(*measured at site*)'
  ) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))+
  theme(plot.margin=unit(c(1,5,1,0.5),"cm")) +
  theme(legend.position = c(1.01,0.5),
        legend.justification = 'left',
        legend.key.width = unit(1.5,"cm"),
        legend.title = element_markdown(),
        legend.key.size = unit(1.5, "cm"))




#### RI v pH Plot 
ggplot(data = allData, aes(x = pH, y = RI, fill = temp)) +
  geom_point(pch = 21, size = 7, stroke = 0.5, color = 'black', alpha = 0.7, 
  ) +
  scale_fill_gradientn(values=c(1, .5, 0), colours=c("red", "blue", "cyan"))+
  geom_smooth(method = 'lm', se=FALSE, size = 1, color = "black")+
  annotate("text", x = 3, y = .6, label = 'y = -0.18x + 3.9', size = 4)+
  annotate("text", x = 1.5, y = 5.0, label = 'N = 299', size = 4)+
  annotate("text", x = 3, y = .8, label = 'p-value = 9.65E-13', size = 4)+
  annotate("text", x = 3, y = 1, label = expression('adj-R'^'2'*' = 0.155'), size = 4)+
  season_facet +
  labs(
    x = 'pH',
    y = 'Ring Index',
    fill = '**Spring Temperature**<br>(*measured at site*)'
  ) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))+
  theme(plot.margin=unit(c(1,5,1,0.5),"cm"))+
  theme(legend.position = c(1.01,0.5),
        legend.justification = 'left',
        legend.key.width = unit(1.5,"cm"),
        legend.title = element_markdown(),
        legend.key.size = unit(1.5, "cm"))




####RI v temp Plot 
ggplot(data = allData, aes(x = temp, y = RI, fill = pH)) +
  geom_point( 
    pch = 21, size = 7, stroke = 0.2, color = 'black', alpha = 0.7, 
  ) +
  scale_fill_gradient(low = "darkmagenta", high = "lavender") +
  season_facet +
  geom_smooth(method = 'lm', se=FALSE, size = 1, color = "black")+
  annotate("text", x = 25, y = 1.1, label = 'y = 0.0017x + 2.6', size = 4)+
  annotate("text", x = 20, y = 5.0, label = 'N = 299', size = 4)+
  annotate("text", x = 25, y = 1.3, label = 'p-value = 6.23E-1', size = 4)+
  annotate("text", x = 25, y = 1.5, label = expression('adj-R'^'2'*' = -0.003'), size = 4)+
  labs(
    x = 'Temperature (\u00B0C)',
    y = 'Ring Index',
    fill = '**Spring pH**<br>(*measured at site*)'
  ) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
  theme(axis.title=element_text(size=20))+
  theme(plot.margin=unit(c(1,5,1,0.5),"cm")) +
  theme(legend.position = c(1.01,0.5),
        legend.justification = 'left',
        legend.key.width = unit(1.5,"cm"),
        legend.title = element_markdown(),
        legend.key.size = unit(1.5, "cm"))



################################################################################
#SPEARMAN'S RHO CORRELATIONS
################################################################################

#Input the variables of interest to get spearman's rho correlation results
#Examples are provided below

#Yellowstone Dataset
spear_ph_cren = cor.test(allData$pH, allData$cren_CL,  method = "spearman", exact = FALSE)
spear_ph_cren
#p-value = 4.455E-05

#Compilation Dataset
#Have to make sure allData = compilation dataset
spear_temp_cren = cor.test(allData$temp, allData$cren,  method = "spearman", exact = FALSE)
spear_ph_cren


#If you want to test only sites with measurable values of the parameter
#Create a new table that removed NA values - example below for DO

#Make data table with only non-NA Do values from Yellowstone dataset
doData = allData[!(is.na(do_mg))]
#Then run this table
spear_do_cren = cor.test(doData$do_mg, doData$cren_CL,  method = "spearman", exact = FALSE)
spear_do_cren


################################################################################
#SIMPLE LINEAR REGRESSIONS FOR CL 
################################################################################

#Yellowstone data example
slr_cren_temp = lm(cren_CL ~ temp, data = allData)
summary(slr_cren_temp)
#p-value = 0.8259
#R^2 = -0.02435

#Compilation data exmaple
slr_RI_pH = lm(RI ~ pH, data = allData)
summary(slr_RI_pH)


#Test between IPL and CL crenarhcaeol for sites with CL and IPL cren present for given subset
slr = lm(cren_IPL ~ cren_CL, data = allDataWithCren)
summary(slr)




################################################################################
#MULTIPLE LINEAR REGRESSIONS FOR CL 
################################################################################
#Yellowstone example
mult = lm(cren_CL ~ scale(pH) + scale(spc) + scale(orp) + scale(temp) + scale(s_ug), data = allData)
summary(mult)

#Compilation example
mult = lm(cren ~ scale(pH) + scale(temp), data = allData)
summary(mult)



#To run all possible multiple regression models
#Load this package
#install.packages("olsrr")
library("olsrr")

#Example for cren_CL
mult = lm(cren_CL ~ scale(pH) +  scale(temp) + scale(spc) + scale(orp) + scale(do_mg) + scale(fe) + scale(s_ug), data = allData)
allModels = ols_step_all_possible(mult)
#to see all models
allModels

#Fing which model has highest adjusted R^2 value
bestR2 = k[which.max(k$adjr),]
bestR2

#List top 10 explanatory models - cutoff found through trial and error
top10models = k[which(k$adjr >= 0.104),]
top10models



################################################################################
#BOX AND WHISKER PLOTS 
################################################################################
#Make sure have compialtion data loaded
allData = fread("allData.csv")

#Make new table to mess with
table = allData
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
binTable = mutate(binTable, binlast=bin[nrow,1])


#Make box and whisker plot in Temp bins
ggplot(binTable, aes(x=bin, y=cren, fill = bin) ) +
  geom_boxplot(alpha = 0.3) +
  scale_y_break(c(0.4, 0.35), scales = 0.2)+
  season_facet+
  annotate("text", x = 2, y = 0.8, label = 'N = 299')+
  ylab("Crenarchaeol Relative Abundance")+
  xlab("Temperature (\u00B0C)")+
  annotate("text",
           x = 1:length(table(binTable$bin)),
           y = aggregate(cren ~ bin, binTable, median)[ , 2],
           label = table(binTable$bin),
           col = "black",
           vjust = -1.3)


#Repeat process for pH bins
table = allData
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
ggplot(binTable, aes(x=bin, y=cren, fill = bin) ) +
  geom_boxplot(alpha = 0.3) +
  scale_y_break(c(0.4, 0.35), scales = 0.2)+
  season_facet+
  annotate("text", x = 2, y = .8, label = 'N = 299')+
  ylab("Crenarchaeol Relative Abundance")+
  xlab("pH")+
  annotate("text",
           x = 1:length(table(binTable$bin)),
           y = aggregate(cren ~ bin, binTable, median)[ , 2],
           label = table(binTable$bin),
           col = "black",
           vjust = -.3)





################################################################################
#Marginal distribution with ggplot2 and ggExtra
################################################################################
p = ggplot(allData, aes(x=pH, y=temp, color=cren, size=cren, fill = cren))+
  geom_point(aes(color = cren == 0, fill = cren ==0), pch = 21, stroke = 0.2, alpha = 0.7)+
  geom_point(aes(color = cren == 0, fill = cren ==0), pch = 21, stroke = 0.2, alpha = 0.7)+
  scale_color_manual(name = "cren", values = c('TRUE' = 'red','FALSE' = "cyan4"))+
  scale_fill_manual(name = "cren", values = c('TRUE' = 'red','FALSE' = "cyan4"))+
  season_facet+
  xlim(1,11)+
  ylim(10,100)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(
    x = 'pH',
    y = 'Temperature (\u00B0C)',
    size = 'Crenarchaeol Relative <br> Abundance'
  )+
  theme(legend.position = "left",
        legend.justification = 'left',
        legend.key.width = unit(1.0,"cm"),
        legend.title = element_markdown(),
        legend.key.size = unit(1.0, "cm"))

#Run again to show plot with added histograms
ggMarginal(p, type="histogram", size = 5, xparams = list(bins=18), yparams = list(bins = 16), groupFill = TRUE)







################################################################################
#LOESS Smoothing Curves
################################################################################

#95% confidence interval band by default 
#Make curve for cren v temp
lo = loess(cren ~ temp, data=allData, span=.9)

f = function(x){
  p = predict(lo, newdata = data.frame(temp=x))
  (p-40)^2
}

opt <- optimize(f, c(0,100))

#Plot LOESS curve 
ggplot(data = allData, aes(x = temp, y = cren)) +
  geom_point() +
  season_facet +
  scale_y_break(c(0.3, 0.3), scales = 0.5)+
  geom_smooth(method = "loess", size = 1.5, span = 0.9, se = TRUE) +
  geom_vline(xintercept = opt$minimum) + 
  ylim(0,1)+
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1.0, by = .10)) +
  labs(x="Temperature (\u00B0C)", y="Crenarchaeol Relative Abundance")+
  geom_hline(yintercept = .15)

#Find where ont he x-axis the LOESS curve is at a maximum
opt$minimum


#Repeat process for pH

########pH loess smooth 
lo = loess(cren ~ pH, data=allData, span=.9)

f = function(x){
  p = predict(lo, newdata = data.frame(pH=x))
  (p-7)^2
}

opt <- optimize(f, c(0,11))

ggplot(data = allData, aes(x = pH, y = cren)) +
  geom_point() +
  season_facet +
  scale_y_break(c(0.3, 0.3), scales = 0.5)+
  geom_smooth(method = "loess", size = 1.5, span = 0.9, se = TRUE) +
  geom_vline(xintercept = opt$minimum) + 
  scale_x_continuous(breaks = seq(0, 11, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1.0, by = .10)) +
  labs(x="pH", y="Crenarchaeol Relative Abundance")+
  geom_hline(yintercept = .118)

#Find where on the x-axis the LOESS curve is at a maximum
opt$minimum

