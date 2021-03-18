#Download librariers 
library(ggplot2)
library(dplyr)
library(viridis)
library(tidyverse)
library(ggpubr)
library(ggrepel)
library(ggsci)
library(ggsignif)
library(ggthemes)
#DISK DIFUSSION TESTS ANALYSIS 
# make a variable that contains all bacterial names and print it out. 
Titles <- c("Bacillus subtilis","Mycobacterium smegmatis","Proteus mirabilis")
#Order the titels 
Titles_test <- c(rep("Bacillus subtilis",3),rep("Mycobacterium smegmatis",3),rep("Proteus mirabilis",3))
#Treatment names 
Treatment_names <- c("Compound combination", "penicillin only", "Tebipenem only")
#Create variables that contain ZOIs measurements according to the bacteria and the treatment 
Bacillus_subtilis_Combination <- c(36.9, 34.44, 35.67)
Bacillus_subtilis_Tebipenem <-c(34.44,29.52,35.67)
Bacillus_subtilis_Penicilin <- c(0,0,0)
Mycobacterium_smegmatis_Combination <- c(39.36,36.9,44.28)
Mycobacterium_smegmatis_Tebipenem <- c(39.36,34.44,41.82)
Mycobacterium_smegmatis_Penicilin <- c(0,0,0)
Proteus_mirabilis_Combination <- c(30.75,31.98,33.21)
Proteus_mirabilis_Tebipenem <- c(33.21,31.98,34.44)
Proteus_mirabilis_Penicillin <- c(27.06,29.52,31.98)
#Create a data frame of pLates results(this will make a table of the results) and print it out
results <- data.frame(Bacteria=Titles_test,
                      Compound_combination= c(Bacillus_subtilis_Combination,
                                              Mycobacterium_smegmatis_Combination,
                                              Proteus_mirabilis_Combination),
                      Tebipenem_only= c(Bacillus_subtilis_Tebipenem,
                                        Mycobacterium_smegmatis_Tebipenem,
                                        Proteus_mirabilis_Tebipenem),
                      Penicillin_only= c(Bacillus_subtilis_Penicilin,
                                         Mycobacterium_smegmatis_Penicilin,
                                         Proteus_mirabilis_Penicillin))
print(results)
#make up the variable that constructs the boxplot for Compound combination effect on three bacteria
my_comparisons <- list(c("Bacillus subtilis","Mycobacterium smegmatis"),
                       c("Bacillus subtilis","Proteus mirabilis"),
                       c("Mycobacterium smegmatis","Proteus mirabilis"))
combin<-ggboxplot(results,x="Bacteria",y="Compound_combination",fill = "Bacteria")+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("c)Compound combination") +
  xlab("Bacteria")+
  ylab("ZOI in mm")+
  stat_boxplot(geom = "errorbar")+
  stat_compare_means(comparisons = my_comparisons,
                     label.y = c(47, 50, 53))+
  stat_compare_means(label.y = 54)
print(combin)
#Make up the boxplot for Tebipenem only on three bacteria
tebip <- ggboxplot(results,x="Bacteria",y="Tebipenem_only",fill = "Bacteria")+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("b)Tebipenem only") +
  xlab("Bacteria")+
  ylab("ZOI in mm")+
  stat_boxplot(geom = "errorbar")+
  stat_compare_means(comparisons = my_comparisons,
                     label.y = c(44, 46, 48))+
  stat_compare_means(label.y = 50)
print(tebip)
#Make up boxplot for Penicillin only on three bacteria
p<-results %>%
  ggplot(aes(x=Bacteria,y=Penicillin_only,fill=Bacteria))+
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("a) Penicilin only") +
  xlab("Bacteria")+
  ylab("ZOI in mm")+
  stat_boxplot(geom = "errorbar")+
  #Cannot compare pairwise because of the zero's
  stat_compare_means(method = "kruskal.test", label.y = 40)+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")
print(p)
#Compare drug effect on the Bacillus subtilis with a new dataframe + boxplot
my_comparisons_2 <- list(c("Compound combination","Tebipenem only"),
                         c("Compound combination","Penicillin only"),
                         c("Penicillin only","Tebipenem only"))
Bacillus_subtilis_data_frame <- data.frame(Treatments_names= c(rep("Compound combination",3),
                                                               rep("Tebipenem only",3),
                                                               rep("Penicillin only",3)),
                                           Treatment=c(Bacillus_subtilis_Combination,
                                                       Bacillus_subtilis_Tebipenem,
                                                       Bacillus_subtilis_Penicilin))
print(Bacillus_subtilis_data_frame)
#Making a boxplot
Bacillus_subtilis_boxplot <- 
  ggboxplot(Bacillus_subtilis_data_frame,x="Treatments_names",y="Treatment",
            fill = "Treatment")+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("c) Effect of drugs n Bacillus subtilis") +
  xlab("Antibiotic")+
  ylab("ZOI in mm")+
  stat_boxplot(geom = "errorbar")+
  stat_compare_means(comparisons = my_comparisons_2,
                     label.y = c(39, 41, 43))+
  stat_compare_means(label.y = 44)
print(Bacillus_subtilis_boxplot)
#Compare drugs effects on Mycobacterium smegmatis (do the same as the previous part)
Mycobacterium_smegmatis_data_frame <- data.frame(Treatments_names_2=
                                                   c(rep("Compound combination",3),
                                                     rep("Tebipenem only",3),
                                                     rep("Penicillin only",3)),
                                                 Treatment_2=c(Mycobacterium_smegmatis_Combination,
                                                               Mycobacterium_smegmatis_Tebipenem,
                                                               Mycobacterium_smegmatis_Penicilin))
print(Mycobacterium_smegmatis_data_frame)
Mycobacterium_smegmatis_boxplot <-
  ggboxplot(Mycobacterium_smegmatis_data_frame,x="Treatments_names_2",
            y="Treatment_2",fill="Treatment_2")+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("a) Effect of drugs on Mycobacterium smegmatis") +
  xlab("Antibiotic")+
  ylab("ZOI in mm")+
  stat_boxplot(geom = "errorbar")+
  stat_compare_means(comparisons = my_comparisons_2,
                     label.y = c(45, 47 , 50))+
  stat_compare_means(label.y = 50)
print(Mycobacterium_smegmatis_boxplot)
# Compare drugs effects on Proteus mirabilis (do the same as the previous part)
Proteus_mirabilis_dataframe <-data.frame(Treatments_names_3=
                                           c(rep("Compound combination",3),
                                             rep("Tebipenem only",3),
                                             rep("Penicillin only",3)),
                                         Treatment_3=c(Proteus_mirabilis_Combination,
                                                       Proteus_mirabilis_Tebipenem,
                                                       Proteus_mirabilis_Penicillin))
Proteus_mirabilis_boxplot <-
  ggboxplot(Proteus_mirabilis_dataframe,x="Treatments_names_3",
            y="Treatment_3",fill="Treatment_3")+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  )+
  ggtitle("b)Effect of drugs on Proteus mirabilis")+
  xlab("Antibiotic")+
  ylab("ZOI in mm")+
  stat_boxplot(geom = "errorbar")+
  stat_compare_means(comparisons = my_comparisons_2,
                     label.y = c(35,37,39))+
  stat_compare_means(label.y = 40)
print(Proteus_mirabilis_boxplot)

# CHECHERBOARD ASSAY TESTS ANALYSIS 
#BACILLUS SUBTILIS RESULTS AND PLOTS 
#Penicillin 100 ug/mL
Bacillus_Penicillin_100ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                             Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                             Optical_density_mean_values=c(((mean(0.053 , 0.052 , 0.05)-0.38)/(mean(0.053,0.054,0.054)-0.38))*100, 
                                                                           ((mean(0.054, 0.052, 0.052)-0.38)/(mean(0.053,0.054,0.054)-0.38))*100, 
                                                                           ((mean(0.054, 0.052, 0.052)-0.38)/(mean(0.053,0.054,0.054)-0.38))*100,
                                                                           ((mean(0.053,0.054,0.053)-0.38)/(mean(0.053,0.054,0.054)-0.38))*100,
                                                                           ((mean(0.054,0.044,0.053)-0.38)/(mean(0.053,0.054,0.054)-0.38))*100,
                                                                           ((mean(0.052,0.054,0.054)-0.38)/(mean(0.053,0.054,0.054)-0.38))*100,
                                                                           ((mean(0.054,0.054,0.053)-0.38)/(mean(0.053,0.054,0.054)-0.38))*100,
                                                                           ((mean(0.054,0.055,0.054)-0.38)/(mean(0.053,0.054,0.054)-0.38))*100,
                                                                           ((mean(0.038, 0.038,0.038)-mean(0.037,0.038,0.037))/(mean(0.406,0.401,0.392)-mean(0.037,0.038,0.037)))*100,
                                                                           ((mean(0.038,0.038,0.362)-mean(0.037,0.038,0.037))/(mean(0.406,0.401,0.392)-mean(0.037,0.038,0.037)))*100,
                                                                           ((mean(0.038,0.038,0.038)-mean(0.037,0.038,0.037))/(mean(0.406,0.401,0.392)-mean(0.037,0.038,0.037)))*100,
                                                                           ((mean(0.041,0.04,0.041)-mean(0.037,0.038,0.037))/(mean(0.406,0.401,0.392)-mean(0.037,0.038,0.037)))*100,
                                                                           ((mean(0.041,0.372,0.039)-mean(0.037,0.038,0.037))/(mean(0.406,0.401,0.392)-mean(0.037,0.038,0.037)))*100,
                                                                           ((mean(0.055,0.041,0.146)-mean(0.037,0.038,0.037))/(mean(0.406,0.401,0.392)-mean(0.037,0.038,0.037)))*100,
                                                                           ((mean(0.181,0.176,0.187)-mean(0.037,0.038,0.037))/(mean(0.406,0.401,0.392)-mean(0.037,0.038,0.037)))*100,
                                                                           ((mean(0.374,0.367,0.364)-mean(0.037,0.038,0.037))/(mean(0.406,0.401,0.392)-mean(0.037,0.038,0.037)))*100,
                                                                           ((mean(0.038,0.038,0.039)-mean(0.037,0.037,0.038))/(mean(0.388,0.432,0.44)-mean(0.037,0.037,0.038)))*100,
                                                                           ((mean(0.038,0.038,0.289)-mean(0.037,0.037,0.038))/(mean(0.388,0.432,0.44)-mean(0.037,0.037,0.038)))*100,
                                                                           ((mean(0.038,0.038,0.039)-mean(0.037,0.037,0.038))/(mean(0.388,0.432,0.44)-mean(0.037,0.037,0.038)))*100,
                                                                           ((mean(0.041,0.039,0.041)-mean(0.037,0.037,0.038))/(mean(0.388,0.432,0.44)-mean(0.037,0.037,0.038)))*100,
                                                                           ((mean(0.04,0.397,0.041)-mean(0.037,0.037,0.038))/(mean(0.388,0.432,0.44)-mean(0.037,0.037,0.038)))*100,
                                                                           ((mean(0.118,0.039,0.213)-mean(0.037,0.037,0.038))/(mean(0.388,0.432,0.44)-mean(0.037,0.037,0.038)))*100,
                                                                           ((mean(0.212,0.227,0.252)-mean(0.037,0.037,0.038))/(mean(0.388,0.432,0.44)-mean(0.037,0.037,0.038)))*100,
                                                                           ((mean(0.352,0.389,0.409)-mean(0.037,0.037,0.038))/(mean(0.388,0.432,0.44)-mean(0.037,0.037,0.038)))*100,
                                                                           ((mean(0.038,0.037,0.038)-0.036)/(mean(0.56,0.484,0.59)-0.036))*100,
                                                                           ((mean(0.038,0.038,0.518)-0.036)/(mean(0.56,0.484,0.59)-0.036))*100,
                                                                           ((mean(0.038,0.038,0.038)-0.036)/(mean(0.56,0.484,0.59)-0.036))*100,
                                                                           ((mean(0.04,0.04,0.038)-0.036)/(mean(0.56,0.484,0.59)-0.036))*100,
                                                                           ((mean(0.193,0.449,0.039)-0.036)/(mean(0.56,0.484,0.59)-0.036))*100,
                                                                           ((mean(0.4,0.122,0.568)-0.036)/(mean(0.56,0.484,0.59)-0.036))*100,
                                                                           ((mean(0.426,0.36,0.521)-0.036)/(mean(0.56,0.484,0.59)-0.036))*100,
                                                                           ((mean(0.478,0.444,0.57)-0.036)/(mean(0.56,0.484,0.59)-0.036))*100,
                                                                           ((mean(0.038,0.042,0.037)-0.036)/(mean(0.527,0.455,0.577)-0.036))*100,
                                                                           ((mean(0.038,0.037,0.479)-0.036)/(mean(0.527,0.455,0.577)-0.036))*100,
                                                                           ((mean(0.038,0.038,0.039)-0.036)/(mean(0.527,0.455,0.577)-0.036))*100,
                                                                           ((mean(0.039,0.039,0.06)-0.036)/(mean(0.527,0.455,0.577)-0.036))*100,
                                                                           ((mean(0.244,0.503,0.048)-0.036)/(mean(0.527,0.455,0.577)-0.036))*100,
                                                                           ((mean(0.473,0.205,0.575)-0.036)/(mean(0.527,0.455,0.577)-0.036))*100,
                                                                           ((mean(0.454,0.371,0.478)-0.036)/(mean(0.527,0.455,0.577)-0.036))*100,
                                                                           ((mean(0.517,0.452,0.566)-0.036)/(mean(0.527,0.455,0.577)-0.036))*100))
print(Bacillus_Penicillin_100ug_data)
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
bp1<- Bacillus_Penicillin_100ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 100 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Bacillus_Penicillin_100ug_data$Optical_density_mean_values-sd(Bacillus_Penicillin_100ug_data$Optical_density_mean_values), 
                ymax=Bacillus_Penicillin_100ug_data$Optical_density_mean_values+sd(Bacillus_Penicillin_100ug_data$Optical_density_mean_values),col="red")
print(bp1)
#Penicillin 50 ug/ml
Bacillus_subtilis_50ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                          Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                          Optical_density_mean_values=c(((mean(0.054,0.051,0.05)-0.036)/(mean(0.051,0.052,0.053)-0.036))*100,
                                                                        ((mean(0.053,0.078,0.052)-0.036)/(mean(0.051,0.052,0.053)-0.036))*100,
                                                                        ((mean(0.052,0.06,0.052)-0.036)/(mean(0.051,0.052,0.053)-0.036))*100,
                                                                        ((mean(0.052,0.056,0.053)-0.036)/(mean(0.051,0.052,0.053)-0.036))*100,
                                                                        ((mean(0.052,0.046,0.054)-0.036)/(mean(0.051,0.052,0.053)-0.036))*100,
                                                                        ((mean(0.052,0.056,0.054)-0.036)/(mean(0.051,0.052,0.053)-0.036))*100,
                                                                        ((mean(0.053,0.054,0.055)-0.036)/(mean(0.051,0.052,0.053)-0.036))*100,
                                                                        ((mean(0.053,0.054,0.054)-0.036)/(mean(0.051,0.052,0.053)-0.036))*100,
                                                                        ((mean(0.037,0.036,0.036)-mean(0.035,0.282,0.036))/(mean(0.416,0.428,0.418)-mean(0.035,0.282,0.036)))*100,
                                                                        ((mean(0.04,0.037,0.049)-mean(0.035,0.282,0.036))/(mean(0.416,0.428,0.418)-mean(0.035,0.282,0.036)))*100,
                                                                        ((mean(0.037,0.037,0.04)-mean(0.035,0.282,0.036))/(mean(0.416,0.428,0.418)-mean(0.035,0.282,0.036)))*100,
                                                                        ((mean(0.04,0.04,0.041)-mean(0.035,0.282,0.036))/(mean(0.416,0.428,0.418)-mean(0.035,0.282,0.036)))*100,
                                                                        ((mean(0.041,0.404,0.041)-mean(0.035,0.282,0.036))/(mean(0.416,0.428,0.418)-mean(0.035,0.282,0.036)))*100,
                                                                        ((mean(0.096,0.041,0.105)-mean(0.035,0.282,0.036))/(mean(0.416,0.428,0.418)-mean(0.035,0.282,0.036)))*100,
                                                                        ((mean(0.194,0.173,0.219)-mean(0.035,0.282,0.036))/(mean(0.416,0.428,0.418)-mean(0.035,0.282,0.036)))*100,
                                                                        ((mean(0.406,0.414,0.414)-mean(0.035,0.282,0.036))/(mean(0.416,0.428,0.418)-mean(0.035,0.282,0.036)))*100,
                                                                        ((mean(0.038,0.039,0.039)-mean(0.037,0.402,0.038))/(mean(0.462,0.485,0.457)-mean(0.037,0.402,0.038)))*100,
                                                                        ((mean(0.038,0.039,0.049)-mean(0.037,0.402,0.038))/(mean(0.462,0.485,0.457)-mean(0.037,0.402,0.038)))*100,
                                                                        ((mean(0.039,0.038,0.042)-mean(0.037,0.402,0.038))/(mean(0.462,0.485,0.457)-mean(0.037,0.402,0.038)))*100,
                                                                        ((mean(0.043,0.042,0.044)-mean(0.037,0.402,0.038))/(mean(0.462,0.485,0.457)-mean(0.037,0.402,0.038)))*100,
                                                                        ((mean(0.065,0.431,0.043)-mean(0.037,0.402,0.038))/(mean(0.462,0.485,0.457)-mean(0.037,0.402,0.038)))*100,
                                                                        ((mean(0.143,0.045,0.158)-mean(0.037,0.402,0.038))/(mean(0.462,0.485,0.457)-mean(0.037,0.402,0.038)))*100,
                                                                        ((mean(0.222,0.216,0.262)-mean(0.037,0.402,0.038))/(mean(0.462,0.485,0.457)-mean(0.037,0.402,0.038)))*100,
                                                                        ((mean(0.432,0.467,0.442)-mean(0.037,0.402,0.038))/(mean(0.462,0.485,0.457)-mean(0.037,0.402,0.038)))*100,
                                                                        ((mean(0.038,0.039,0.038)-mean(0.037,0.548,0.037))/(mean(0.655,0.558,0.655)-mean(0.037,0.548,0.037)))*100,
                                                                        ((mean(0.039,0.039,0.047)-mean(0.037,0.548,0.037))/(mean(0.655,0.558,0.655)-mean(0.037,0.548,0.037)))*100,
                                                                        ((mean(0.04,0.039,0.041)-mean(0.037,0.548,0.037))/(mean(0.655,0.558,0.655)-mean(0.037,0.548,0.037)))*100,
                                                                        ((mean(0.042,0.041,0.041)-mean(0.037,0.548,0.037))/(mean(0.655,0.558,0.655)-mean(0.037,0.548,0.037)))*100,
                                                                        ((mean(0.297,0.575,0.044)-mean(0.037,0.548,0.037))/(mean(0.655,0.558,0.655)-mean(0.037,0.548,0.037)))*100,
                                                                        ((mean(0.423,0.078,0.467)-mean(0.037,0.548,0.037))/(mean(0.655,0.558,0.655)-mean(0.037,0.548,0.037)))*100,
                                                                        ((mean(0.479,0.379,0.51)-mean(0.037,0.548,0.037))/(mean(0.655,0.558,0.655)-mean(0.037,0.548,0.037)))*100,
                                                                        ((mean(0.594,0.517,0.632)-mean(0.037,0.548,0.037))/(mean(0.655,0.558,0.655)-mean(0.037,0.548,0.037)))*100,
                                                                        ((mean(0.038,0.042,0.059)-mean(0.037,0.632,0.037))/(mean(0.647,0.596,0.701)-mean(0.037,0.632,0.037)))*100,
                                                                        ((mean(0.039,0.04,0.048)-mean(0.037,0.632,0.037))/(mean(0.647,0.596,0.701)-mean(0.037,0.632,0.037)))*100,
                                                                        ((mean(0.039,0.039,0.041)-mean(0.037,0.632,0.037))/(mean(0.647,0.596,0.701)-mean(0.037,0.632,0.037)))*100,
                                                                        ((mean(0.039,0.041,0.041)-mean(0.037,0.632,0.037))/(mean(0.647,0.596,0.701)-mean(0.037,0.632,0.037)))*100,
                                                                        ((mean(0.331,0.595,0.06)-mean(0.037,0.632,0.037))/(mean(0.647,0.596,0.701)-mean(0.037,0.632,0.037)))*100,
                                                                        ((mean(0.469,0.153,0.506)-mean(0.037,0.632,0.037))/(mean(0.647,0.596,0.701)-mean(0.037,0.632,0.037)))*100,
                                                                        ((mean(0.514,0.448,0.54)-mean(0.037,0.632,0.037))/(mean(0.647,0.596,0.701)-mean(0.037,0.632,0.037)))*100,
                                                                        ((mean(0.61,0.573,0.643)-mean(0.037,0.632,0.037))/(mean(0.647,0.596,0.701)-mean(0.037,0.632,0.037)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
bp2 <-Bacillus_subtilis_50ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 50 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Bacillus_subtilis_50ug_data$Optical_density_mean_values-sd(Bacillus_subtilis_50ug_data$Optical_density_mean_values), 
                ymax=Bacillus_subtilis_50ug_data$Optical_density_mean_values+sd(Bacillus_subtilis_50ug_data$Optical_density_mean_values), col="red")
print(bp2)
#Penicillin 25 ug/mL
Bacillus_subtilis_25ug_data <-data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                         Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                         Optical_density_mean_values =c(((mean(0.051,0.05,0.049)-0.035)/(mean(0.05,0.051,0.052)-0.035))*100,
                                                                        ((mean(0.059,0.05,0.052)-0.035)/(mean(0.05,0.051,0.052)-0.035))*100,
                                                                        ((mean(0.05,0.051,0.05)-0.035)/(mean(0.05,0.051,0.052)-0.035))*100,
                                                                        ((mean(0.051,0.053,0.053)-0.035)/(mean(0.05,0.051,0.052)-0.035))*100,
                                                                        ((mean(0.052,0.051,0.053)-0.035)/(mean(0.05,0.051,0.052)-0.035))*100,
                                                                        ((mean(0.051,0.053,0.051)-0.035)/(mean(0.05,0.051,0.052)-0.035))*100,
                                                                        ((mean(0.052,0.054,0.053)-0.035)/(mean(0.05,0.051,0.052)-0.035))*100,
                                                                        ((mean(0.052,0.053,0.054)-0.035)/(mean(0.05,0.051,0.052)-0.035))*100,
                                                                        ((mean(0.038,0.039,0.04)-mean(0.038,0.038,0.039))/(mean(0.396,0.414,0.404)-mean(0.038,0.038,0.039)))*100,
                                                                        ((mean(0.038,0.039,0.041)-mean(0.038,0.038,0.039))/(mean(0.396,0.414,0.404)-mean(0.038,0.038,0.039)))*100,
                                                                        ((mean(0.038,0.039,0.039)-mean(0.038,0.038,0.039))/(mean(0.396,0.414,0.404)-mean(0.038,0.038,0.039)))*100,
                                                                        ((mean(0.042,0.043,0.043)-mean(0.038,0.038,0.039))/(mean(0.396,0.414,0.404)-mean(0.038,0.038,0.039)))*100,
                                                                        ((mean(0.043,0.385,0.043)-mean(0.038,0.038,0.039))/(mean(0.396,0.414,0.404)-mean(0.038,0.038,0.039)))*100,
                                                                        ((mean(0.29,0.044,0.088)-mean(0.038,0.038,0.039))/(mean(0.396,0.414,0.404)-mean(0.038,0.038,0.039)))*100,
                                                                        ((mean(0.179,0.173,0.201)-mean(0.038,0.038,0.039))/(mean(0.396,0.414,0.404)-mean(0.038,0.038,0.039)))*100,
                                                                        ((mean(0.384,0.402,0.401)-mean(0.038,0.038,0.039))/(mean(0.396,0.414,0.404)-mean(0.038,0.038,0.039)))*100,
                                                                        ((mean(0.036,0.068,0.039)-mean(0.035,0.036,0.037))/(mean(0.418,0.429,0.415)-mean(0.035,0.036,0.037)))*100,
                                                                        ((mean(0.036,0.037,0.038)-mean(0.035,0.036,0.037))/(mean(0.418,0.429,0.415)-mean(0.035,0.036,0.037)))*100,
                                                                        ((mean(0.036,0.037,0.037)-mean(0.035,0.036,0.037))/(mean(0.418,0.429,0.415)-mean(0.035,0.036,0.037)))*100,
                                                                        ((mean(0.041,0.042,0.041)-mean(0.035,0.036,0.037))/(mean(0.418,0.429,0.415)-mean(0.035,0.036,0.037)))*100,
                                                                        ((mean(0.061,0.384,0.041)-mean(0.035,0.036,0.037))/(mean(0.418,0.429,0.415)-mean(0.035,0.036,0.037)))*100,
                                                                        ((mean(0.25,0.041,0.142)-mean(0.035,0.036,0.037))/(mean(0.418,0.429,0.415)-mean(0.035,0.036,0.037)))*100,
                                                                        ((mean(0.208,0.212,0.233)-mean(0.035,0.036,0.037))/(mean(0.418,0.429,0.415)-mean(0.035,0.036,0.037)))*100,
                                                                        ((mean(0.394,0.405,0.402)-mean(0.035,0.036,0.037))/(mean(0.418,0.429,0.415)-mean(0.035,0.036,0.037)))*100,
                                                                        ((mean(0.034,0.049,0.036)-mean(0.034,0.035,0.035))/(mean(0.61,0.489,0.61)-mean(0.034,0.035,0.035)))*100,
                                                                        ((mean(0.035,0.036,0.044)-mean(0.034,0.035,0.035))/(mean(0.61,0.489,0.61)-mean(0.034,0.035,0.035)))*100,
                                                                        ((mean(0.035,0.036,0.039)-mean(0.034,0.035,0.035))/(mean(0.61,0.489,0.61)-mean(0.034,0.035,0.035)))*100,
                                                                        ((mean(0.038,0.04,0.039)-mean(0.034,0.035,0.035))/(mean(0.61,0.489,0.61)-mean(0.034,0.035,0.035)))*100,
                                                                        ((mean(0.289,0.543,0.15)-mean(0.034,0.035,0.035))/(mean(0.61,0.489,0.61)-mean(0.034,0.035,0.035)))*100,
                                                                        ((mean(0.265,0.079,0.438)-mean(0.034,0.035,0.035))/(mean(0.61,0.489,0.61)-mean(0.034,0.035,0.035)))*100,
                                                                        ((mean(0.458,0.338,0.488)-mean(0.034,0.035,0.035))/(mean(0.61,0.489,0.61)-mean(0.034,0.035,0.035)))*100,
                                                                        ((mean(0.545,0.409,0.602)-mean(0.034,0.035,0.035))/(mean(0.61,0.489,0.61)-mean(0.034,0.035,0.035)))*100,
                                                                        ((mean(0.035,0.055,0.036)-mean(0.034,0.036,0.035))/(mean(0.599,0.542,0.667)-mean(0.034,0.036,0.035)))*100,
                                                                        ((mean(0.035,0.037,0.042)-mean(0.034,0.036,0.035))/(mean(0.599,0.542,0.667)-mean(0.034,0.036,0.035)))*100,
                                                                        ((mean(0.034,0.068,0.036)-mean(0.034,0.036,0.035))/(mean(0.599,0.542,0.667)-mean(0.034,0.036,0.035)))*100,
                                                                        ((mean(0.036,0.038,0.037)-mean(0.034,0.036,0.035))/(mean(0.599,0.542,0.667)-mean(0.034,0.036,0.035)))*100,
                                                                        ((mean(0.278,0.557,0.226)-mean(0.034,0.036,0.035))/(mean(0.599,0.542,0.667)-mean(0.034,0.036,0.035)))*100,
                                                                        ((mean(0.283,0.145,0.477)-mean(0.034,0.036,0.035))/(mean(0.599,0.542,0.667)-mean(0.034,0.036,0.035)))*100,
                                                                        ((mean(0.478,0.417,0.511)-mean(0.034,0.036,0.035))/(mean(0.599,0.542,0.667)-mean(0.034,0.036,0.035)))*100,
                                                                        ((mean(0.565,0.5,0.628)-mean(0.034,0.036,0.035))/(mean(0.599,0.542,0.667)-mean(0.034,0.036,0.035)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
bp3 <- Bacillus_subtilis_25ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 25 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Bacillus_subtilis_25ug_data$Optical_density_mean_values-sd(Bacillus_subtilis_25ug_data$Optical_density_mean_values),
                ymax=Bacillus_subtilis_25ug_data$Optical_density_mean_values+sd(Bacillus_subtilis_25ug_data$Optical_density_mean_values), col="red")
print(bp3)
#Penicillin 12.5 ug/mL
Bacillus_subtilis_12.5ug_data <-data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                           Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                           Optical_density_mean_values =c(((mean(0.058,0.057,0.054)-mean(0.038,0.039,0.038))/(mean(0.057,0.056,0.057)-mean(0.038,0.039,0.038)))*100,
                                                                          ((mean(0.059,0.054,0.057)-mean(0.038,0.039,0.038))/(mean(0.057,0.056,0.057)-mean(0.038,0.039,0.038)))*100,
                                                                          ((mean(0.057,0.059,0.055)-mean(0.038,0.039,0.038))/(mean(0.057,0.056,0.057)-mean(0.038,0.039,0.038)))*100,
                                                                          ((mean(0.057,0.058,0.058)-mean(0.038,0.039,0.038))/(mean(0.057,0.056,0.057)-mean(0.038,0.039,0.038)))*100,
                                                                          ((mean(0.057,0.057,0.058-mean(0.038,0.039,0.038))/(mean(0.057,0.056,0.057)-mean(0.038,0.039,0.038)))*100),
                                                                          ((mean(0.058,0.061,0.057)-mean(0.038,0.039,0.038))/(mean(0.057,0.056,0.057)-mean(0.038,0.039,0.038)))*100,
                                                                          ((mean(0.058,0.061,0.061)-mean(0.038,0.039,0.038))/(mean(0.057,0.056,0.057)-mean(0.038,0.039,0.038)))*100,
                                                                          ((mean(0.058,0.06,0.059)-mean(0.038,0.039,0.038))/(mean(0.057,0.056,0.057)-mean(0.038,0.039,0.038)))*100,
                                                                          ((mean(0.038,0.038,0.039)-mean(0.037,0.038,0.038))/(mean(0.409,0.422,0.416)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.038,0.039,0.039)-mean(0.037,0.038,0.038))/(mean(0.409,0.422,0.416)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.038,0.039,0.039)-mean(0.037,0.038,0.038))/(mean(0.409,0.422,0.416)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.042,0.043,0.043)-mean(0.037,0.038,0.038))/(mean(0.409,0.422,0.416)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.042,0.396,0.063)-mean(0.037,0.038,0.038))/(mean(0.409,0.422,0.416)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.251,0.044,0.122)-mean(0.037,0.038,0.038))/(mean(0.409,0.422,0.416)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.212,0.194,0.233)-mean(0.037,0.038,0.038))/(mean(0.409,0.422,0.416)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.409,0.41,0.403)-mean(0.037,0.038,0.038))/(mean(0.409,0.422,0.416)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.038,0.049,0.042)-mean(0.037,0.038,0.038))/(mean(0.424,0.42,0.409)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.038,0.04,0.041)-mean(0.037,0.038,0.038))/(mean(0.424,0.42,0.409)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.038,0.039,0.04)-mean(0.037,0.038,0.038))/(mean(0.424,0.42,0.409)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.044,0.044,0.044)-mean(0.037,0.038,0.038))/(mean(0.424,0.42,0.409)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.044,0.389,0.084)-mean(0.037,0.038,0.038))/(mean(0.424,0.42,0.409)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.277,0.043,0.175)-mean(0.037,0.038,0.038))/(mean(0.424,0.42,0.409)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.234,0.234,0.269)-mean(0.037,0.038,0.038))/(mean(0.424,0.42,0.409)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.391,0.395,0.396)-mean(0.037,0.038,0.038))/(mean(0.424,0.42,0.409)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.038,0.054,0.039)-mean(0.037,0.039,0.038))/(mean(0.574,0.503,0.604)-mean(0.037,0.039,0.038)))*100,
                                                                          ((mean(0.039,0.041,0.045)-mean(0.037,0.039,0.038))/(mean(0.574,0.503,0.604)-mean(0.037,0.039,0.038)))*100,
                                                                          ((mean(0.039,0.039,0.04)-mean(0.037,0.039,0.038))/(mean(0.574,0.503,0.604)-mean(0.037,0.039,0.038)))*100,
                                                                          ((mean(0.041,0.042,0.042)-mean(0.037,0.039,0.038))/(mean(0.574,0.503,0.604)-mean(0.037,0.039,0.038)))*100,
                                                                          ((mean(0.217,0.505,0.358)-mean(0.037,0.039,0.038))/(mean(0.574,0.503,0.604)-mean(0.037,0.039,0.038)))*100,
                                                                          ((mean(0.409,0.142,0.54)-mean(0.037,0.039,0.038))/(mean(0.574,0.503,0.604)-mean(0.037,0.039,0.038)))*100,
                                                                          ((mean(0.47,0.339,0.494)-mean(0.037,0.039,0.038))/(mean(0.574,0.503,0.604)-mean(0.037,0.039,0.038)))*100,
                                                                          ((mean(0.531,0.393,0.58)-mean(0.037,0.039,0.038))/(mean(0.574,0.503,0.604)-mean(0.037,0.039,0.038)))*100,
                                                                          ((mean(0.038,0.059,0.04)-mean(0.037,0.038,0.038))/(mean(0.585,0.531,0.654)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.039,0.04,0.041)-mean(0.037,0.038,0.038))/(mean(0.585,0.531,0.654)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.039,0.086,0.04)-mean(0.037,0.038,0.038))/(mean(0.585,0.531,0.654)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.039,0.04,0.041)-mean(0.037,0.038,0.038))/(mean(0.585,0.531,0.654)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.245,0.533,0.351)-mean(0.037,0.038,0.038))/(mean(0.585,0.531,0.654)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.398,0.241,0.532)-mean(0.037,0.038,0.038))/(mean(0.585,0.531,0.654)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.49,0.408,0.507)-mean(0.037,0.038,0.038))/(mean(0.585,0.531,0.654)-mean(0.037,0.038,0.038)))*100,
                                                                          ((mean(0.558,0.483,0.624-mean(0.037,0.038,0.038))/(mean(0.585,0.531,0.654)-mean(0.037,0.038,0.038)))*100)))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
bp4 <- Bacillus_subtilis_12.5ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point() +
  geom_area()+
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 12.5 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Bacillus_subtilis_12.5ug_data$Optical_density_mean_values-sd(Bacillus_subtilis_12.5ug_data$Optical_density_mean_values), 
                ymax=Bacillus_subtilis_12.5ug_data$Optical_density_mean_values+sd(Bacillus_subtilis_12.5ug_data$Optical_density_mean_values),col="red")
print(bp4)
#Penicillin 6.25 ug/mL
Bacillus_subtilis_6.25ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                            Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                            Optical_density_mean_values =c(((mean(0.055,0.055,0.052)-mean(0.036,0.036,0.039))/(mean(0.058,0.056,0.057)-mean(0.036,0.036,0.039)))*100,
                                                                           ((mean(0.057,0.053,0.054)-mean(0.036,0.036,0.039))/(mean(0.058,0.056,0.057)-mean(0.036,0.036,0.039)))*100,
                                                                           ((mean(0.057,0.056,0.054)-mean(0.036,0.036,0.039))/(mean(0.058,0.056,0.057)-mean(0.036,0.036,0.039)))*100,
                                                                           ((mean(0.057,0.061,0.056)-mean(0.036,0.036,0.039))/(mean(0.058,0.056,0.057)-mean(0.036,0.036,0.039)))*100,
                                                                           ((mean(0.055,0.054,0.056)-mean(0.036,0.036,0.039))/(mean(0.058,0.056,0.057)-mean(0.036,0.036,0.039)))*100,
                                                                           ((mean(0.055,0.055,0.054)-mean(0.036,0.036,0.039))/(mean(0.058,0.056,0.057)-mean(0.036,0.036,0.039)))*100,
                                                                           ((mean(0.055,0.058,0.057)-mean(0.036,0.036,0.039))/(mean(0.058,0.056,0.057)-mean(0.036,0.036,0.039)))*100,
                                                                           ((mean(0.059,0.055,0.057)-mean(0.036,0.036,0.039))/(mean(0.058,0.056,0.057)-mean(0.036,0.036,0.039)))*100,
                                                                           ((mean(0.036,0.036,0.038)-mean(0.035,0.036,0.035))/(mean(0.39,0.421,0.423)-mean(0.035,0.036,0.035)))*100,
                                                                           ((mean(0.036,0.037,0.037)-mean(0.035,0.036,0.035))/(mean(0.39,0.421,0.423)-mean(0.035,0.036,0.035)))*100,
                                                                           ((mean(0.036,0.037,0.037)-mean(0.035,0.036,0.035))/(mean(0.39,0.421,0.423)-mean(0.035,0.036,0.035)))*100,
                                                                           ((mean(0.04,0.04,0.041)-mean(0.035,0.036,0.035))/(mean(0.39,0.421,0.423)-mean(0.035,0.036,0.035)))*100,
                                                                           ((mean(0.04,0.403,0.042)-mean(0.035,0.036,0.035))/(mean(0.39,0.421,0.423)-mean(0.035,0.036,0.035)))*100,
                                                                           ((mean(0.107,0.042,0.111)-mean(0.035,0.036,0.035))/(mean(0.39,0.421,0.423)-mean(0.035,0.036,0.035)))*100,
                                                                           ((mean(0.188,0.183,0.233)-mean(0.035,0.036,0.035))/(mean(0.39,0.421,0.423)-mean(0.035,0.036,0.035)))*100,
                                                                           ((mean(0.393,0.411,0.408)-mean(0.035,0.036,0.035))/(mean(0.39,0.421,0.423)-mean(0.035,0.036,0.035)))*100,
                                                                           ((mean(0.036,0.037,0.039)-mean(0.035,0.039,0.036))/(mean(0.402,0.412,0.413)-mean(0.035,0.039,0.036)))*100,
                                                                           ((mean(0.036,0.037,0.037)-mean(0.035,0.039,0.036))/(mean(0.402,0.412,0.413)-mean(0.035,0.039,0.036)))*100,
                                                                           ((mean(0.036,0.037,0.036)-mean(0.035,0.039,0.036))/(mean(0.402,0.412,0.413)-mean(0.035,0.039,0.036)))*100,
                                                                           ((mean(0.041,0.041,0.041)-mean(0.035,0.039,0.036))/(mean(0.402,0.412,0.413)-mean(0.035,0.039,0.036)))*100,
                                                                           ((mean(0.041,0.391,0.067)-mean(0.035,0.039,0.036))/(mean(0.402,0.412,0.413)-mean(0.035,0.039,0.036)))*100,
                                                                           ((mean(0.175,0.041,0.174)-mean(0.035,0.039,0.036))/(mean(0.402,0.412,0.413)-mean(0.035,0.039,0.036)))*100,
                                                                           ((mean(0.239,0.224,0.271)-mean(0.035,0.039,0.036))/(mean(0.402,0.412,0.413)-mean(0.035,0.039,0.036)))*100,
                                                                           ((mean(0.388,0.394,0.392)-mean(0.035,0.039,0.036))/(mean(0.402,0.412,0.413)-mean(0.035,0.039,0.036)))*100,
                                                                           ((mean(0.036,0.097,0.041)-mean(0.035,0.037,0.036))/(mean(0.6,0.501,0.608)-mean(0.035,0.037,0.036)))*100,
                                                                           ((mean(0.036,0.037,0.044)-mean(0.035,0.037,0.036))/(mean(0.6,0.501,0.608)-mean(0.035,0.037,0.036)))*100,
                                                                           ((mean(0.037,0.038,0.037)-mean(0.035,0.037,0.036))/(mean(0.6,0.501,0.608)-mean(0.035,0.037,0.036)))*100,
                                                                           ((mean(0.038,0.212,0.04)-mean(0.035,0.037,0.036))/(mean(0.6,0.501,0.608)-mean(0.035,0.037,0.036)))*100,
                                                                           ((mean(0.214,0.516,0.358)-mean(0.035,0.037,0.036))/(mean(0.6,0.501,0.608)-mean(0.035,0.037,0.036)))*100,
                                                                           ((mean(0.495,0.14,0.488)-mean(0.035,0.037,0.036))/(mean(0.6,0.501,0.608)-mean(0.035,0.037,0.036)))*100,
                                                                           ((mean(0.458,0.33,0.5)-mean(0.035,0.037,0.036))/(mean(0.6,0.501,0.608)-mean(0.035,0.037,0.036)))*100,
                                                                           ((mean(0.539,0.374,0.579)-mean(0.035,0.037,0.036))/(mean(0.6,0.501,0.608)-mean(0.035,0.037,0.036)))*100,
                                                                           ((mean(0.036,0.104,0.04)-mean(0.039,0.037,0.036))/(mean(0.575,0.515,0.644)-mean(0.039,0.037,0.036)))*100,
                                                                           ((mean(0.037,0.038,0.038)-mean(0.039,0.037,0.036))/(mean(0.575,0.515,0.644)-mean(0.039,0.037,0.036)))*100,
                                                                           ((mean(0.036,0.038,0.037)-mean(0.039,0.037,0.036))/(mean(0.575,0.515,0.644)-mean(0.039,0.037,0.036)))*100,
                                                                           ((mean(0.037,0.216,0.038)-mean(0.039,0.037,0.036))/(mean(0.575,0.515,0.644)-mean(0.039,0.037,0.036)))*100,
                                                                           ((mean(0.251,0.517,0.374)-mean(0.039,0.037,0.036))/(mean(0.575,0.515,0.644)-mean(0.039,0.037,0.036)))*100,
                                                                           ((mean(0.536,0.257,0.504)-mean(0.039,0.037,0.036))/(mean(0.575,0.515,0.644)-mean(0.039,0.037,0.036)))*100,
                                                                           ((mean(0.492,0.392,0.51)-mean(0.039,0.037,0.036))/(mean(0.575,0.515,0.644)-mean(0.039,0.037,0.036)))*100,
                                                                           ((mean(0.539,0.464,0.62)-mean(0.039,0.037,0.036))/(mean(0.575,0.515,0.644)-mean(0.039,0.037,0.036)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
bp5<- Bacillus_subtilis_6.25ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 6.25 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Bacillus_subtilis_6.25ug_data$Optical_density_mean_values-sd(Bacillus_subtilis_6.25ug_data$Optical_density_mean_values),
                ymax=Bacillus_subtilis_6.25ug_data$Optical_density_mean_values+sd(Bacillus_subtilis_6.25ug_data$Optical_density_mean_values), col="red")
print(bp5)
#Penicillin 3.125 ug/mL
Bacillus_subtilis_3.125ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                             Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                             Optical_density_mean_values =c(((mean(0.053,0.054,0.053)-mean(0.037,0.037,0.037))/(mean(0.057,0.056,0.056)-mean(0.037,0.037,0.037)))*100,
                                                                            ((mean(0.054,0.054,0.053)-mean(0.037,0.037,0.037))/(mean(0.057,0.056,0.056)-mean(0.037,0.037,0.037)))*100,
                                                                            ((mean(0.054,0.055,0.053)-mean(0.037,0.037,0.037))/(mean(0.057,0.056,0.056)-mean(0.037,0.037,0.037)))*100,
                                                                            ((mean(0.055,0.056,0.054)-mean(0.037,0.037,0.037))/(mean(0.057,0.056,0.056)-mean(0.037,0.037,0.037)))*100,
                                                                            ((mean(0.052,0.055,0.055)-mean(0.037,0.037,0.037))/(mean(0.057,0.056,0.056)-mean(0.037,0.037,0.037)))*100,
                                                                            ((mean(0.054,0.055,0.054)-mean(0.037,0.037,0.037))/(mean(0.057,0.056,0.056)-mean(0.037,0.037,0.037)))*100,
                                                                            ((mean(0.053,0.057,0.056)-mean(0.037,0.037,0.037))/(mean(0.057,0.056,0.056)-mean(0.037,0.037,0.037)))*100,
                                                                            ((mean(0.057,0.055,0.055)-mean(0.037,0.037,0.037))/(mean(0.057,0.056,0.056)-mean(0.037,0.037,0.037)))*100,
                                                                            ((mean(0.037,0.037,0.041)-mean(0.036,0.308,0.303))/(mean(0.388,0.421,0.414)-mean(0.036,0.308,0.303)))*100,
                                                                            ((mean(0.037,0.038,0.04)-mean(0.036,0.308,0.303))/(mean(0.388,0.421,0.414)-mean(0.036,0.308,0.303)))*100,
                                                                            ((mean(0.038,0.038,0.04)-mean(0.036,0.308,0.303))/(mean(0.388,0.421,0.414)-mean(0.036,0.308,0.303)))*100,
                                                                            ((mean(0.042,0.041,0.043)-mean(0.036,0.308,0.303))/(mean(0.388,0.421,0.414)-mean(0.036,0.308,0.303)))*100,
                                                                            ((mean(0.041,0.399,0.046)-mean(0.036,0.308,0.303))/(mean(0.388,0.421,0.414)-mean(0.036,0.308,0.303)))*100,
                                                                            ((mean(0.123,0.042,0.135)-mean(0.036,0.308,0.303))/(mean(0.388,0.421,0.414)-mean(0.036,0.308,0.303)))*100,
                                                                            ((mean(0.191,0.209,0.229)-mean(0.036,0.308,0.303))/(mean(0.388,0.421,0.414)-mean(0.036,0.308,0.303)))*100,
                                                                            ((mean(0.392,0.43,0.41)-mean(0.036,0.308,0.303))/(mean(0.388,0.421,0.414)-mean(0.036,0.308,0.303)))*100,
                                                                            ((mean(0.037,0.038,0.041)-mean(0.036,0.383,0.384))/(mean(0.392,0.425,0.411)-mean(0.036,0.383,0.384)))*100,
                                                                            ((mean(0.038,0.038,0.04)-mean(0.036,0.383,0.384))/(mean(0.392,0.425,0.411)-mean(0.036,0.383,0.384)))*100,
                                                                            ((mean(0.039,0.038,0.039)-mean(0.036,0.383,0.384))/(mean(0.392,0.425,0.411)-mean(0.036,0.383,0.384)))*100,
                                                                            ((mean(0.042,0.042,0.043)-mean(0.036,0.383,0.384))/(mean(0.392,0.425,0.411)-mean(0.036,0.383,0.384)))*100,
                                                                            ((mean(0.041,0.385,0.077)-mean(0.036,0.383,0.384))/(mean(0.392,0.425,0.411)-mean(0.036,0.383,0.384)))*100,
                                                                            ((mean(0.169,0.042,0.193)-mean(0.036,0.383,0.384))/(mean(0.392,0.425,0.411)-mean(0.036,0.383,0.384)))*100,
                                                                            ((mean(0.224,0.247,0.264)-mean(0.036,0.383,0.384))/(mean(0.392,0.425,0.411)-mean(0.036,0.383,0.384)))*100,
                                                                            ((mean(0.379,0.416,0.399)-mean(0.036,0.383,0.384))/(mean(0.392,0.425,0.411)-mean(0.036,0.383,0.384)))*100,
                                                                            ((mean(0.037,0.037,0.043)-mean(0.036,0.421,0.63))/(mean(0.583,0.515,0.608)-mean(0.036,0.421,0.63)))*100,
                                                                            ((mean(0.037,0.039,0.045)-mean(0.036,0.421,0.63))/(mean(0.583,0.515,0.608)-mean(0.036,0.421,0.63)))*100,
                                                                            ((mean(0.038,0.038,0.039)-mean(0.036,0.421,0.63))/(mean(0.583,0.515,0.608)-mean(0.036,0.421,0.63)))*100,
                                                                            ((mean(0.038,0.041,0.04)-mean(0.036,0.421,0.63))/(mean(0.583,0.515,0.608)-mean(0.036,0.421,0.63)))*100,
                                                                            ((mean(0.223,0.498,0.291)-mean(0.036,0.421,0.63))/(mean(0.583,0.515,0.608)-mean(0.036,0.421,0.63)))*100,
                                                                            ((mean(0.47,0.109,0.54)-mean(0.036,0.421,0.63))/(mean(0.583,0.515,0.608)-mean(0.036,0.421,0.63)))*100,
                                                                            ((mean(0.456,0.335,0.497)-mean(0.036,0.421,0.63))/(mean(0.583,0.515,0.608)-mean(0.036,0.421,0.63)))*100,
                                                                            ((mean(0.545,0.436,0.585)-mean(0.036,0.421,0.63))/(mean(0.583,0.515,0.608)-mean(0.036,0.421,0.63)))*100,
                                                                            ((mean(0.037,0.038,0.04)-mean(0.036,0.534,0.546))/(mean(0.587,0.548,0.66)-mean(0.036,0.534,0.546)))*100,
                                                                            ((mean(0.037,0.038,0.039)-mean(0.036,0.534,0.546))/(mean(0.587,0.548,0.66)-mean(0.036,0.534,0.546)))*100,
                                                                            ((mean(0.037,0.075,0.038)-mean(0.036,0.534,0.546))/(mean(0.587,0.548,0.66)-mean(0.036,0.534,0.546)))*100,
                                                                            ((mean(0.037,0.039,0.039)-mean(0.036,0.534,0.546))/(mean(0.587,0.548,0.66)-mean(0.036,0.534,0.546)))*100,
                                                                            ((mean(0.239,0.513,0.275)-mean(0.036,0.534,0.546))/(mean(0.587,0.548,0.66)-mean(0.036,0.534,0.546)))*100,
                                                                            ((mean(0.482,0.234,0.527)-mean(0.036,0.534,0.546))/(mean(0.587,0.548,0.66)-mean(0.036,0.534,0.546)))*100,
                                                                            ((mean(0.489,0.403,0.535)-mean(0.036,0.534,0.546))/(mean(0.587,0.548,0.66)-mean(0.036,0.534,0.546)))*100,
                                                                            ((mean(0.557,0.507,0.673)-mean(0.036,0.534,0.546))/(mean(0.587,0.548,0.66)-mean(0.036,0.534,0.546)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
bp6 <- Bacillus_subtilis_3.125ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 3.125 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Bacillus_subtilis_3.125ug_data$Optical_density_mean_values-sd(Bacillus_subtilis_3.125ug_data$Optical_density_mean_values),
                ymax=Bacillus_subtilis_3.125ug_data$Optical_density_mean_values+sd(Bacillus_subtilis_3.125ug_data$Optical_density_mean_values), col="red")
print(bp6)
# Penicillin 1.5625
Bacillus_subtilis_1.5625ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                              Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                              Optical_density_mean_values =c(((mean(0.052,0.053,0.053)-0.036)/(0.054-0.036))*100,
                                                                             ((mean(0.053,0.053,0.052)-0.036)/(0.054-0.036))*100,
                                                                             ((mean(0.054,0.054,0.052)-0.036)/(0.054-0.036))*100,
                                                                             ((mean(0.052,0.054,0.054)-0.036)/(0.054-0.036))*100,
                                                                             ((mean(0.053,0.054,0.054)-0.036)/(0.054-0.036))*100,
                                                                             ((mean(0.053,0.053,0.054)-0.036)/(0.054-0.036))*100,
                                                                             ((mean(0.053,0.056,0.054)-0.036)/(0.054-0.036))*100,
                                                                             ((mean(0.054,0.054,0.054)-0.036)/(0.054-0.036))*100,
                                                                             ((mean(0.037,0.037,0.039)-mean(0.035,0.358,0.036))/(mean(0.386,0.411,0.412)-mean(0.035,0.358,0.036)))*100,
                                                                             ((mean(0.037,0.375,0.038)-mean(0.035,0.358,0.036))/(mean(0.386,0.411,0.412)-mean(0.035,0.358,0.036)))*100,
                                                                             ((mean(0.037,0.037,0.039)-mean(0.035,0.358,0.036))/(mean(0.386,0.411,0.412)-mean(0.035,0.358,0.036)))*100,
                                                                             ((mean(0.041,0.041,0.042)-mean(0.035,0.358,0.036))/(mean(0.386,0.411,0.412)-mean(0.035,0.358,0.036)))*100,
                                                                             ((mean(0.041,0.393,0.043)-mean(0.035,0.358,0.036))/(mean(0.386,0.411,0.412)-mean(0.035,0.358,0.036)))*100,
                                                                             ((mean(0.115,0.044,0.116)-mean(0.035,0.358,0.036))/(mean(0.386,0.411,0.412)-mean(0.035,0.358,0.036)))*100,
                                                                             ((mean(0.202,0.199,0.229)-mean(0.035,0.358,0.036))/(mean(0.386,0.411,0.412)-mean(0.035,0.358,0.036)))*100,
                                                                             ((mean(0.387,0.421,0.398)-mean(0.035,0.358,0.036))/(mean(0.386,0.411,0.412)-mean(0.035,0.358,0.036)))*100,
                                                                             ((mean(0.037,0.038,0.039)-mean(0.035,0.431,0.038))/(mean(0.392,0.441,0.403)-mean(0.035,0.431,0.038)))*100,
                                                                             ((mean(0.038,0.364,0.038)-mean(0.035,0.431,0.038))/(mean(0.392,0.441,0.403)-mean(0.035,0.431,0.038)))*100,
                                                                             ((mean(0.038,0.039,0.038)-mean(0.035,0.431,0.038))/(mean(0.392,0.441,0.403)-mean(0.035,0.431,0.038)))*100,
                                                                             ((mean(0.042,0.042,0.041)-mean(0.035,0.431,0.038))/(mean(0.392,0.441,0.403)-mean(0.035,0.431,0.038)))*100,
                                                                             ((mean(0.042,0.374,0.041)-mean(0.035,0.431,0.038))/(mean(0.392,0.441,0.403)-mean(0.035,0.431,0.038)))*100,
                                                                             ((mean(0.165,0.043,0.171)-mean(0.035,0.431,0.038))/(mean(0.392,0.441,0.403)-mean(0.035,0.431,0.038)))*100,
                                                                             ((mean(0.233,0.243,0.259)-mean(0.035,0.431,0.038))/(mean(0.392,0.441,0.403)-mean(0.035,0.431,0.038)))*100,
                                                                             ((mean(0.373,0.421,0.388)-mean(0.035,0.431,0.038))/(mean(0.392,0.441,0.403)-mean(0.035,0.431,0.038)))*100,
                                                                             ((mean(0.037,0.037,0.04)-mean(0.035,0.54,0.036))/(mean(0.586,0.522,0.609)-mean(0.035,0.54,0.036)))*100,
                                                                             ((mean(0.037,0.45,0.038)-mean(0.035,0.54,0.036))/(mean(0.586,0.522,0.609)-mean(0.035,0.54,0.036)))*100,
                                                                             ((mean(0.038,0.038,0.038)-mean(0.035,0.54,0.036))/(mean(0.586,0.522,0.609)-mean(0.035,0.54,0.036)))*100,
                                                                             ((mean(0.039,0.041,0.039)-mean(0.035,0.54,0.036))/(mean(0.586,0.522,0.609)-mean(0.035,0.54,0.036)))*100,
                                                                             ((mean(0.235,0.476,0.266)-mean(0.035,0.54,0.036))/(mean(0.586,0.522,0.609)-mean(0.035,0.54,0.036)))*100,
                                                                             ((mean(0.471,0.122,0.548)-mean(0.035,0.54,0.036))/(mean(0.586,0.522,0.609)-mean(0.035,0.54,0.036)))*100,
                                                                             ((mean(0.47,0.321,0.503)-mean(0.035,0.54,0.036))/(mean(0.586,0.522,0.609)-mean(0.035,0.54,0.036)))*100,
                                                                             ((mean(0.546,0.474,0.583)-mean(0.035,0.54,0.036))/(mean(0.586,0.522,0.609)-mean(0.035,0.54,0.036)))*100,
                                                                             ((mean(0.037,0.038,0.038)-mean(0.036,0.607,0.037))/(mean(0.578,0.555,0.578)-mean(0.036,0.607,0.037)))*100,
                                                                             ((mean(0.037,0.463,0.038)-mean(0.036,0.607,0.037))/(mean(0.578,0.555,0.578)-mean(0.036,0.607,0.037)))*100,
                                                                             ((mean(0.037,0.038,0.037)-mean(0.036,0.607,0.037))/(mean(0.578,0.555,0.578)-mean(0.036,0.607,0.037)))*100,
                                                                             ((mean(0.038,0.039,0.039)-mean(0.036,0.607,0.037))/(mean(0.578,0.555,0.578)-mean(0.036,0.607,0.037)))*100,
                                                                             ((mean(0.261,0.495,0.341)-mean(0.036,0.607,0.037))/(mean(0.578,0.555,0.578)-mean(0.036,0.607,0.037)))*100,
                                                                             ((mean(0.477,0.243,0.556)-mean(0.036,0.607,0.037))/(mean(0.578,0.555,0.578)-mean(0.036,0.607,0.037)))*100,
                                                                             ((mean(0.488,0.393,0.532)-mean(0.036,0.607,0.037))/(mean(0.578,0.555,0.578)-mean(0.036,0.607,0.037)))*100,
                                                                             ((mean(0.544,0.511,0.63)-mean(0.036,0.607,0.037))/(mean(0.578,0.555,0.578)-mean(0.036,0.607,0.037)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
bp7 <- Bacillus_subtilis_1.5625ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 1.5625 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Bacillus_subtilis_1.5625ug_data$Optical_density_mean_values-sd(Bacillus_subtilis_1.5625ug_data$Optical_density_mean_values),
                ymax=Bacillus_subtilis_1.5625ug_data$Optical_density_mean_values+sd(Bacillus_subtilis_1.5625ug_data$Optical_density_mean_values), col="red")
print(bp7)
#Penicillin 0 ug/mL
Bacillus_subtilis_0ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                         Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                         Optical_density_mean_values =c(((mean(0.059,0.054,0.054)-0.036)/(0.055-0.036))*100,
                                                                        ((mean(0.064,0.054,0.055)-0.036)/(0.055-0.036))*100,
                                                                        ((mean(0.054,0.054,0.053)-0.036)/(0.055-0.036))*100,
                                                                        ((mean(0.054,0.054,0.054)-0.036)/(0.055-0.036))*100,
                                                                        ((mean(0.053,0.055,0.055)-0.036)/(0.055-0.036))*100,
                                                                        ((mean(0.054,0.062,0.054)-0.036)/(0.055-0.036))*100,
                                                                        ((mean(0.054,0.054,0.054)-0.036)/(0.055-0.036))*100,
                                                                        ((mean(0.055,0.056,0.055)-0.036)/(0.055-0.036))*100,
                                                                        ((mean(0.042,0.038,0.038)-mean(0.036,0.036,0.037))/(mean(0.401,0.422,0.438)-mean(0.036,0.036,0.037)))*100,
                                                                        ((mean(0.048,0.038,0.039)-mean(0.036,0.036,0.037))/(mean(0.401,0.422,0.438)-mean(0.036,0.036,0.037)))*100,
                                                                        ((mean(0.039,0.038,0.039)-mean(0.036,0.036,0.037))/(mean(0.401,0.422,0.438)-mean(0.036,0.036,0.037)))*100,
                                                                        ((mean(0.043,0.041,0.045)-mean(0.036,0.036,0.037))/(mean(0.401,0.422,0.438)-mean(0.036,0.036,0.037)))*100,
                                                                        ((mean(0.043,0.406,0.044)-mean(0.036,0.036,0.037))/(mean(0.401,0.422,0.438)-mean(0.036,0.036,0.037)))*100,
                                                                        ((mean(0.128,0.041,0.127)-mean(0.036,0.036,0.037))/(mean(0.401,0.422,0.438)-mean(0.036,0.036,0.037)))*100,
                                                                        ((mean(0.21,0.184,0.229)-mean(0.036,0.036,0.037))/(mean(0.401,0.422,0.438)-mean(0.036,0.036,0.037)))*100,
                                                                        ((mean(0.401,0.418,0.42)-mean(0.036,0.036,0.037))/(mean(0.401,0.422,0.438)-mean(0.036,0.036,0.037)))*100,
                                                                        ((mean(0.041,0.038,0.038)-mean(0.036,0.037,0.037))/(mean(0.43,0.475,0.435)-mean(0.036,0.037,0.037)))*100,
                                                                        ((mean(0.044,0.039,0.039)-mean(0.036,0.037,0.037))/(mean(0.43,0.475,0.435)-mean(0.036,0.037,0.037)))*100,
                                                                        ((mean(0.039,0.039,0.039)-mean(0.036,0.037,0.037))/(mean(0.43,0.475,0.435)-mean(0.036,0.037,0.037)))*100,
                                                                        ((mean(0.041,0.042,0.042)-mean(0.036,0.037,0.037))/(mean(0.43,0.475,0.435)-mean(0.036,0.037,0.037)))*100,
                                                                        ((mean(0.042,0.413,0.042)-mean(0.036,0.037,0.037))/(mean(0.43,0.475,0.435)-mean(0.036,0.037,0.037)))*100,
                                                                        ((mean(0.16,0.044,0.189)-mean(0.036,0.037,0.037))/(mean(0.43,0.475,0.435)-mean(0.036,0.037,0.037)))*100,
                                                                        ((mean(0.226,0.233,0.266)-mean(0.036,0.037,0.037))/(mean(0.43,0.475,0.435)-mean(0.036,0.037,0.037)))*100,
                                                                        ((mean(0.407,0.455,0.414)-mean(0.036,0.037,0.037))/(mean(0.43,0.475,0.435)-mean(0.036,0.037,0.037)))*100,
                                                                        ((mean(0.04,0.038,0.038)-mean(0.035,0.037,0.036))/(mean(0.605,0.566,0.636)-mean(0.035,0.037,0.036)))*100,
                                                                        ((mean(0.043,0.038,0.04)-mean(0.035,0.037,0.036))/(mean(0.605,0.566,0.636)-mean(0.035,0.037,0.036)))*100,
                                                                        ((mean(0.038,0.039,0.038)-mean(0.035,0.037,0.036))/(mean(0.605,0.566,0.636)-mean(0.035,0.037,0.036)))*100,
                                                                        ((mean(0.04,0.042,0.04)-mean(0.035,0.037,0.036))/(mean(0.605,0.566,0.636)-mean(0.035,0.037,0.036)))*100,
                                                                        ((mean(0.186,0.52,0.277)-mean(0.035,0.037,0.036))/(mean(0.605,0.566,0.636)-mean(0.035,0.037,0.036)))*100,
                                                                        ((mean(0.529,0.093,0.567)-mean(0.035,0.037,0.036))/(mean(0.605,0.566,0.636)-mean(0.035,0.037,0.036)))*100,
                                                                        ((mean(0.476,0.373,0.534)-mean(0.035,0.037,0.036))/(mean(0.605,0.566,0.636)-mean(0.035,0.037,0.036)))*100,
                                                                        ((mean(0.582,0.531,0.606)-mean(0.035,0.037,0.036))/(mean(0.605,0.566,0.636)-mean(0.035,0.037,0.036)))*100,
                                                                        ((mean(0.04,0.038,0.073)-mean(0.036,0.038,0.036))/(mean(0.624,0.604,0.625)-mean(0.036,0.038,0.036)))*100,
                                                                        ((mean(0.042,0.038,0.066)-mean(0.036,0.038,0.036))/(mean(0.624,0.604,0.625)-mean(0.036,0.038,0.036)))*100,
                                                                        ((mean(0.038,0.039,0.092)-mean(0.036,0.038,0.036))/(mean(0.624,0.604,0.625)-mean(0.036,0.038,0.036)))*100,
                                                                        ((mean(0.039,0.04,0.041)-mean(0.036,0.038,0.036))/(mean(0.624,0.604,0.625)-mean(0.036,0.038,0.036)))*100,
                                                                        ((mean(0.244,0.551,0.27)-mean(0.036,0.038,0.036))/(mean(0.624,0.604,0.625)-mean(0.036,0.038,0.036)))*100,
                                                                        ((mean(0.479,0.199,0.562)-mean(0.036,0.038,0.036))/(mean(0.624,0.604,0.625)-mean(0.036,0.038,0.036)))*100,
                                                                        ((mean(0.511,0.432,0.592)-mean(0.036,0.038,0.036))/(mean(0.624,0.604,0.625)-mean(0.036,0.038,0.036)))*100,
                                                                        ((mean(0.598,0.568,0.704)-mean(0.036,0.038,0.036))/(mean(0.624,0.604,0.625)-mean(0.036,0.038,0.036)))*100))
print(Bacillus_subtilis_0ug_data)
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
bp8 <-Bacillus_subtilis_0ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 0 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Bacillus_subtilis_0ug_data$Optical_density_mean_values-sd(Bacillus_subtilis_0ug_data$Optical_density_mean_values),
                ymax=Bacillus_subtilis_0ug_data$Optical_density_mean_values+sd(Bacillus_subtilis_0ug_data$Optical_density_mean_values), col="red")
print(bp8)
#Align all graphs together 
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
figure <- 
  ggarrange(bp1,bp2,bp3,bp4, bp5, bp6, bp7, bp8,
                      labels=c("a","b","c","d","e","f","g","h"),
                      ncol=4,nrow=2,
                      common.legend = TRUE,legend = "bottom")
print(figure)
#Final Plot and Data frame that conrain only values that inhibited the growth of the bacteria 
Final_Bacillus_data <- data.frame(Time_f1=c("0","19","24","41","48"),
                                   Penicillin_concentrations= c(rep("PenV+Tebipenem (100 ug/mL+1.25 ug/mL",5),rep("PenV+Tebipenem (50 ug/mL+1.25 ug/mL)",5),
                                                                rep("PenV+Tebipenem (25 ug/mL+1.25 ug/mL)",5),rep("PenV+Tebipenem (12.5 ug/mL+1.25 ug/mL)",5),
                                                                rep("PenV+Tebipenem (6.25 ug/mL+1.25 ug/mL)",5),rep("PenV+Tebipenem (3.125 ug/mL+1.25 ug/mL)",5),
                                                                rep("PenV+Tebipenem (1.5625 ug/mL+1.25 ug/mL)",5),rep("PenV+Tebiepenem (0 ug/mL+1.25 ug/mL)",5)),
                                  Optical_density_values= c(((mean(0.053,0.054,0.053)-0.38)/(mean(0.053,0.054,0.054)-0.38))*100,
                                                            ((mean(0.041,0.04,0.041)-mean(0.037,0.038,0.037))/(mean(0.406,0.401,0.392)-mean(0.037,0.038,0.037)))*100,
                                                            ((mean(0.041,0.039,0.041)-mean(0.037,0.037,0.038))/(mean(0.388,0.432,0.44)-mean(0.037,0.037,0.038)))*100,
                                                            ((mean(0.04,0.04,0.038)-mean(0.036,0.036,0.036))/(mean(0.56,0.484,0.59)-mean(0.036,0.036,0.036)))*100,
                                                            ((mean(0.039,0.039,0.06)-0.036)/(mean(0.527,0.455,0.577)-0.036))*100,
                                                            ((mean(0.052,0.056,0.053)-mean(0.036,0.036,0.036))/(mean(0.051,0.052,0.053)-mean(0.036,0.036,0.036)))*100,
                                                            ((mean(0.04,0.04,0.041)-mean(0.035,0.282,0.036))/(mean(0.416,0.428,0.418)-mean(0.035,0.282,0.036)))*100,
                                                            ((mean(0.043,0.042,0.044)-mean(0.037,0.402,0.038))/(mean(0.462,0.485,0.457)-mean(0.037,0.402,0.038)))*100,
                                                            ((mean(0.042,0.041,0.041)-mean(0.037,0.548,0.037))/(mean(0.655,0.558,0.655)-mean(0.037,0.548,0.037)))*100,
                                                            ((mean(0.039,0.041,0.041)-mean(0.037,0.632,0.037))/(mean(0.647,0.596,0.701)-mean(0.037,0.632,0.037)))*100,
                                                            ((mean(0.051,0.053,0.053)-mean(0.035,0.035,0.035))/(mean(0.05,0.051,0.052)-mean(0.035,0.035,0.035)))*100,
                                                            ((mean(0.042,0.043,0.043)-mean(0.038,0.038,0.039))/(mean(0.396,0.414,0.404)-mean(0.038,0.038,0.039)))*100,
                                                            ((mean(0.041,0.042,0.041)-mean(0.035,0.036,0.037))/(mean(0.418,0.429,0.415)-mean(0.035,0.036,0.037)))*100,
                                                            ((mean(0.038,0.04,0.039)-mean(0.034,0.035,0.035))/(mean(0.61,0.489,0.61)-mean(0.034,0.035,0.035)))*100,
                                                            ((mean(0.036,0.038,0.037)-mean(0.034,0.036,0.035))/(mean(0.599,0.542,0.667)-mean(0.034,0.036,0.035)))*100,
                                                            ((mean(0.057,0.058,0.058)-mean(0.038,0.039,0.038))/(mean(0.057,0.056,0.057)-mean(0.038,0.039,0.038)))*100,
                                                            ((mean(0.042,0.043,0.043)-mean(0.037,0.038,0.038))/(mean(0.409,0.422,0.416)-mean(0.037,0.038,0.038)))*100,
                                                            ((mean(0.044,0.044,0.044)-mean(0.037,0.038,0.038))/(mean(0.424,0.42,0.409)-mean(0.037,0.038,0.038)))*100,
                                                            ((mean(0.041,0.042,0.042)-mean(0.037,0.039,0.038))/(mean(0.574,0.503,0.604)-mean(0.037,0.039,0.038)))*100,
                                                            ((mean(0.039,0,04,0.041)-mean(0.037,0.038,0.038))/(mean(0.585,0.531,0.654)-mean(0.037,0.038,0.038)))*100,
                                                            ((mean(0.057,0.061,0.056)-mean(0.036,0.036,0.039))/(mean(0.058,0.056,0.057)-mean(0.036,0.036,0.039)))*100,
                                                            ((mean(0.04,0.04,0.041)-mean(0.035,0.036,0.035))/(mean(0.39,0.421,0.423)-mean(0.035,0.036,0.035)))*100,
                                                            ((mean(0.041,0.041,0.041)-mean(0.035,0.039,0.036))/(mean(0.402,0.412,0.413)-mean(0.035,0.039,0.036)))*100,
                                                            ((mean(0.038,0.212,0.04)-mean(0.035,0.037,0.036))/(mean(0.6,0.501,0.608)-mean(0.035,0.037,0.036)))*100,
                                                            ((mean(0.037,0.216,0.038)-mean(0.039,0.037,0.036))/(mean(0.575,0.515,0.644)-mean(0.039,0.037,0.036)))*100,
                                                            ((mean(0.055,0.056,0.054)-mean(0.037,0.037,0.037))/(mean(0.057,0.056,0.056)-mean(0.037,0.037,0.037)))*100,
                                                            ((mean(0.042,0.041,0.043)-mean(0.036,0.308,0.303))/(mean(0.388,0.421,0.414)-mean(0.036,0.308,0.303)))*100,
                                                            ((mean(0.042,0.042,0.043)-mean(0.036,0.383,0.384))/(mean(0.392,0.425,0.411)-mean(0.036,0.383,0.384)))*100,
                                                            ((mean(0.038,0.041,0.04)-mean(0.036,0.421,0.63))/(mean(0.583,0.515,0.608)-mean(0.036,0.421,0.63)))*100,
                                                            ((mean(0.037,0.039,0.039)-mean(0.036,0.534,0.546))/(mean(0.587,0.548,0.66)-mean(0.036,0.534,0.546)))*100,
                                                            ((mean(0.052,0.054,0.054)-mean(0.036,0.036,0.036))/(mean(0.054,0.054,0.054)-mean(0.036,0.036,0.036)))*100,
                                                            ((mean(0.041,0.041,0.042)-mean(0.035,0.358,0.036))/(mean(0.386,0.411,0.412)-mean(0.035,0.358,0.036)))*100,
                                                            ((mean(0.042,0.042,0.041)-mean(0.035,0.431,0.038))/(mean(0.392,0.441,0.403)-mean(0.035,0.431,0.038)))*100,
                                                            ((mean(0.039,0.041,0.039)-mean(0.035,0.54,0.036))/(mean(0.586,0.522,0.609)-mean(0.035,0.54,0.036)))*100,
                                                            ((mean(0.038,0.039,0.039)-mean(0.036,0.607,0.037))/(mean(0.578,0.555,0.578)-mean(0.036,0.607,0.037)))*100,
                                                            ((mean(0.054,0.054,0.054)-0.036)/(0.055-0.036))*100,
                                                            ((mean(0.043,0.041,0.045)-mean(0.036,0.036,0.037))/(mean(0.401,0.422,0.438)-mean(0.036,0.036,0.037)))*100,
                                                            ((mean(0.041,0.042,0.042)-mean(0.036,0.037,0.037))/(mean(0.43,0.475,0.435)-mean(0.036,0.037,0.037)))*100,
                                                            ((mean(0.04,0.042,0.04)-mean(0.035,0.037,0.036))/(mean(0.605,0.566,0.636)-mean(0.035,0.037,0.036)))*100,
                                                            ((mean(0.039,0.04,0.041)-mean(0.036,0.038,0.036))/(mean(0.624,0.604,0.625)-mean(0.036,0.038,0.036)))*100))
print(Final_Bacillus_data)
#Line graph 
Final_Bac <- Final_Bacillus_data %>%
  ggplot(aes(x=Time_f1, y=Optical_density_values, group=Penicillin_concentrations, shape=Penicillin_concentrations))+
  geom_line()+
  geom_point()+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  stat_compare_means(method = "kruskal.test", label.y =120)+
  stat_compare_means(label = "p.signif")+
  geom_errorbar(ymin=Final_Bacillus_data$Optical_density_values-sd(Final_Bacillus_data$Optical_density_values),
                ymax=Final_Bacillus_data$Optical_density_values+sd(Final_Bacillus_data$Optical_density_values),
                width=.2)+
  geom_hline(yintercept = 0, linetype="dashed", col="grey")
print(Final_Bac)
#PROTEUS MIRABILIS RESULTS AND PLOTS
#Penicillin 100 ug/mL
Proteus_mirabilis_100ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                           Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                           Optical_density_mean_values =c(((mean(0.04, 0.06, 0.806)-0.038)/(mean(0.064,0.063,0.062)-0.038))*100,
                                                                          ((mean(0.06,0.061, 0.061)-0.038)/(mean(0.064,0.063,0.062)-0.038))*100,
                                                                          ((mean(0.06,0.061,0.061)-0.038)/(mean(0.064,0.063,0.062)-0.038))*100,
                                                                          ((mean(0.062,0.061,0.061)-0.038)/(mean(0.064,0.063,0.062)-0.038))*100,
                                                                          ((mean(0.062,0.061,0.066)-0.038)/(mean(0.064,0.063,0.062)-0.038))*100,
                                                                          ((mean(0.062,0.061,0.064)-0.038)/(mean(0.064,0.063,0.062)-0.038))*100,
                                                                          ((mean(0.062,0.061,0.061)-0.038)/(mean(0.064,0.063,0.062)-0.038))*100,
                                                                          ((mean(0.063,0.062,0.062)-0.038)/(mean(0.064,0.063,0.062)-0.038))*100,
                                                                          ((mean(0.04,0.049,0.051)-mean(0.038,0.037,0.037))/(mean(0.321,0.32,0.324)-mean(0.038,0.037,0.037)))*100,
                                                                          ((mean(0.05,0.06,0.054)-mean(0.038,0.037,0.037))/(mean(0.321,0.32,0.324)-mean(0.038,0.037,0.037)))*100,
                                                                          ((mean(0.089,0.1,0.093)-mean(0.038,0.037,0.037))/(mean(0.321,0.32,0.324)-mean(0.038,0.037,0.037)))*100,
                                                                          ((mean(0.122,0.127,0.121)-mean(0.038,0.037,0.037))/(mean(0.321,0.32,0.324)-mean(0.038,0.037,0.037)))*100,
                                                                          ((mean(0.137,0.152,0.141)-mean(0.038,0.037,0.037))/(mean(0.321,0.32,0.324)-mean(0.038,0.037,0.037)))*100,
                                                                          ((mean(0.144,0.159,0.146)-mean(0.038,0.037,0.037))/(mean(0.321,0.32,0.324)-mean(0.038,0.037,0.037)))*100,
                                                                          ((mean(0.145,0.161,0.14)-mean(0.038,0.037,0.037))/(mean(0.321,0.32,0.324)-mean(0.038,0.037,0.037)))*100,
                                                                          ((mean(0.313,0.319,0.321)-mean(0.038,0.037,0.037))/(mean(0.321,0.32,0.324)-mean(0.038,0.037,0.037)))*100,
                                                                          ((mean(0.04,0.048,0.043)-mean(0.038,0.037,0.038))/(mean(0.349,0.335,0.334)-mean(0.038,0.037,0.038)))*100,
                                                                          ((mean(0.051,0.052,0.051)-mean(0.038,0.037,0.038))/(mean(0.349,0.335,0.334)-mean(0.038,0.037,0.038)))*100,
                                                                          ((mean(0.097,0.1,0.098)-mean(0.038,0.037,0.038))/(mean(0.349,0.335,0.334)-mean(0.038,0.037,0.038)))*100,
                                                                          ((mean(0.123,0.125,0.12)-mean(0.038,0.037,0.038))/(mean(0.349,0.335,0.334)-mean(0.038,0.037,0.038)))*100,
                                                                          ((mean(0.143,0.149,0.139)-mean(0.038,0.037,0.038))/(mean(0.349,0.335,0.334)-mean(0.038,0.037,0.038)))*100,
                                                                          ((mean(0.149,0.159,0.149)-mean(0.038,0.037,0.038))/(mean(0.349,0.335,0.334)-mean(0.038,0.037,0.038)))*100,
                                                                          ((mean(0.154,0.167,0.15)-mean(0.038,0.037,0.038))/(mean(0.349,0.335,0.334)-mean(0.038,0.037,0.038)))*100,
                                                                          ((mean(0.346,0.344,0.337)-mean(0.038,0.037,0.038))/(mean(0.349,0.335,0.334)-mean(0.038,0.037,0.038)))*100,
                                                                          ((mean(0.041,0.049,0.039)-mean(0.038,0.036,0.037))/(mean(0.477,0.471,0.411)-mean(0.038,0.036,0.037)))*100,
                                                                          ((mean(0.053,0.053,0.051)-mean(0.038,0.036,0.037))/(mean(0.477,0.471,0.411)-mean(0.038,0.036,0.037)))*100,
                                                                          ((mean(0.074,0.081,0.081)-mean(0.038,0.036,0.037))/(mean(0.477,0.471,0.411)-mean(0.038,0.036,0.037)))*100,
                                                                          ((mean(0.11,0.114,0.11)-mean(0.038,0.036,0.037))/(mean(0.477,0.471,0.411)-mean(0.038,0.036,0.037)))*100,
                                                                          ((mean(0.133,0.152,0.14)-mean(0.038,0.036,0.037))/(mean(0.477,0.471,0.411)-mean(0.038,0.036,0.037)))*100,
                                                                          ((mean(0.155,0.182,0.157)-mean(0.038,0.036,0.037))/(mean(0.477,0.471,0.411)-mean(0.038,0.036,0.037)))*100,
                                                                          ((mean(0.167,0.201,0.165)-mean(0.038,0.036,0.037))/(mean(0.477,0.471,0.411)-mean(0.038,0.036,0.037)))*100,
                                                                          ((mean(0.49,0.494,0.416)-mean(0.038,0.036,0.037))/(mean(0.477,0.471,0.411)-mean(0.038,0.036,0.037)))*100,
                                                                          ((mean(0.041,0.048,0.036)-mean(0.038,0.036,0.036))/(mean(0.494,0.454,0.501)-mean(0.038,0.036,0.036)))*100,
                                                                          ((mean(0.051,0.051,0.052)-mean(0.038,0.036,0.036))/(mean(0.494,0.454,0.501)-mean(0.038,0.036,0.036)))*100,
                                                                          ((mean(0.068,0.075,0.085)-mean(0.038,0.036,0.036))/(mean(0.494,0.454,0.501)-mean(0.038,0.036,0.036)))*100,
                                                                          ((mean(0.105,0.109,0.111)-mean(0.038,0.036,0.036))/(mean(0.494,0.454,0.501)-mean(0.038,0.036,0.036)))*100,
                                                                          ((mean(0.131,0.144,0.141)-mean(0.038,0.036,0.036))/(mean(0.494,0.454,0.501)-mean(0.038,0.036,0.036)))*100,
                                                                          ((mean(0.154,0.176,0.161)-mean(0.038,0.036,0.036))/(mean(0.494,0.454,0.501)-mean(0.038,0.036,0.036)))*100,
                                                                          ((mean(0.174,0.206,0.186)-mean(0.038,0.036,0.036))/(mean(0.494,0.454,0.501)-mean(0.038,0.036,0.036)))*100,
                                                                          ((mean(0.513,0.501,0.49)-mean(0.038,0.036,0.036))/(mean(0.494,0.454,0.501)-mean(0.038,0.036,0.036)))*100))
print(Proteus_mirabilis_100ug_data)
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Pp1 <- Proteus_mirabilis_100ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area()+
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 100 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Proteus_mirabilis_100ug_data$Optical_density_mean_values-sd(Proteus_mirabilis_100ug_data$Optical_density_mean_values),
                ymax=Proteus_mirabilis_100ug_data$Optical_density_mean_values+sd(Proteus_mirabilis_100ug_data$Optical_density_mean_values), col="red")
print(Pp1)
#Penicillin 50 ug/mL
Proteus_mirabilis_50ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                          Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                          Optical_density_mean_values =c(((mean(0.06,0.06,0.04)-mean(0.035,0.036,0.037))/(mean(0.062,0.063,0.061)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.06,0.06,0.06)-mean(0.035,0.036,0.037))/(mean(0.062,0.063,0.061)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.06,0.061,0.06)-mean(0.035,0.036,0.037))/(mean(0.062,0.063,0.061)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.06,0.06,0.061)-mean(0.035,0.036,0.037))/(mean(0.062,0.063,0.061)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.06,0.06,0.06)-mean(0.035,0.036,0.037))/(mean(0.062,0.063,0.061)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.06,0.06,0.061)-mean(0.035,0.036,0.037))/(mean(0.062,0.063,0.061)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.061,0.06,0.059)-mean(0.035,0.036,0.037))/(mean(0.062,0.063,0.061)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.061,0.06,0.06)-mean(0.035,0.036,0.037))/(mean(0.062,0.063,0.061)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.046,0.048,0.194)-mean(0.035,0.036,0.037))/(mean(0.335,0.338,0.325)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.063,0.097,0.069)-mean(0.035,0.036,0.037))/(mean(0.335,0.338,0.325)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.103,0.108,0.106)-mean(0.035,0.036,0.037))/(mean(0.335,0.338,0.325)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.127,0.129,0.124)-mean(0.035,0.036,0.037))/(mean(0.335,0.338,0.325)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.139,0.143,0.137)-mean(0.035,0.036,0.037))/(mean(0.335,0.338,0.325)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.14,0.15,0.138)-mean(0.035,0.036,0.037))/(mean(0.335,0.338,0.325)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.143,0.154,0.143)-mean(0.035,0.036,0.037))/(mean(0.335,0.338,0.325)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.326,0.341,0.327)-mean(0.035,0.036,0.037))/(mean(0.335,0.338,0.325)-mean(0.035,0.036,0.037)))*100,
                                                                         ((mean(0.049,0.051,0.239)-mean(0.037,0.038,0.038))/(mean(0.361,0.339,0.336)-mean(0.037,0.038,0.038)))*100,
                                                                         ((mean(0.057,0.1,0.067)-mean(0.037,0.038,0.038))/(mean(0.361,0.339,0.336)-mean(0.037,0.038,0.038)))*100,
                                                                         ((mean(0.105,0.11,0.11)-mean(0.037,0.038,0.038))/(mean(0.361,0.339,0.336)-mean(0.037,0.038,0.038)))*100,
                                                                         ((mean(0.128,0.129,0.127)-mean(0.037,0.038,0.038))/(mean(0.361,0.339,0.336)-mean(0.037,0.038,0.038)))*100,
                                                                         ((mean(0.142,0.145,0.144)-mean(0.037,0.038,0.038))/(mean(0.361,0.339,0.336)-mean(0.037,0.038,0.038)))*100,
                                                                         ((mean(0.148,0.156,0.147)-mean(0.037,0.038,0.038))/(mean(0.361,0.339,0.336)-mean(0.037,0.038,0.038)))*100,
                                                                         ((mean(0.155,0.166,0.154)-mean(0.037,0.038,0.038))/(mean(0.361,0.339,0.336)-mean(0.037,0.038,0.038)))*100,
                                                                         ((mean(0.352,0.346,0.337)-mean(0.037,0.038,0.038))/(mean(0.361,0.339,0.336)-mean(0.037,0.038,0.038)))*100,
                                                                         ((mean(0.05,0.052,0.239)-0.038)/(mean(0.505,0.514,0.419)-0.038))*100,
                                                                         ((mean(0.054,0.082,0.056)-0.038)/(mean(0.505,0.514,0.419)-0.038))*100,
                                                                         ((mean(0.097,0.104,0.104)-0.038)/(mean(0.505,0.514,0.419)-0.038))*100,
                                                                         ((mean(0.122,0.125,0.12)-0.038)/(mean(0.505,0.514,0.419)-0.038))*100,
                                                                         ((mean(0.138,0.149,0.145)-0.038)/(mean(0.505,0.514,0.419)-0.038))*100,
                                                                         ((mean(0.156,0.176,0.159)-0.038)/(mean(0.505,0.514,0.419)-0.038))*100,
                                                                         ((mean(0.165,0.19,0.162)-0.038)/(mean(0.505,0.514,0.419)-0.038))*100,
                                                                         ((mean(0.471,0.504,0.413)-0.038)/(mean(0.505,0.514,0.419)-0.038))*100,
                                                                         ((mean(0.05,0.051,0.168)-0.038)/(mean(0.521,0.515,0.491)-0.038))*100,
                                                                         ((mean(0.056,0.083,0.061)-0.038)/(mean(0.521,0.515,0.491)-0.038))*100,
                                                                         ((mean(0.099,0.107,0.109)-0.038)/(mean(0.521,0.515,0.491)-0.038))*100,
                                                                         ((mean(0.119,0.121,0.12)-0.038)/(mean(0.521,0.515,0.491)-0.038))*100,
                                                                         ((mean(0.138,0.148,0.147)-0.038)/(mean(0.521,0.515,0.491)-0.038))*100,
                                                                         ((mean(0.158,0.177,0.162)-0.038)/(mean(0.521,0.515,0.491)-0.038))*100,
                                                                         ((mean(0.171,0.2,0.171)-0.038)/(mean(0.521,0.515,0.491)-0.038))*100,
                                                                         ((mean(0.511,0.525,0.474)-0.038)/(mean(0.521,0.515,0.491)-0.038))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Pp2 <- Proteus_mirabilis_50ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 50 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Proteus_mirabilis_50ug_data$Optical_density_mean_values-sd(Proteus_mirabilis_50ug_data$Optical_density_mean_values),
                ymax=Proteus_mirabilis_50ug_data$Optical_density_mean_values+sd(Proteus_mirabilis_50ug_data$Optical_density_mean_values), col="red")
print(Pp2)
#Penicillin 25 ug/mL
Proteus_mirabilis_25ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                          Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                          Optical_density_mean_values =c(((mean(0.058,0.057,0.056)-0.035)/(mean(0.061,0.061,0.066)-0.035))*100,
                                                                         ((mean(0.06,0.057,0.06)-0.035)/(mean(0.061,0.061,0.066)-0.035))*100,
                                                                         ((mean(0.058,0.056,0.058)-0.035)/(mean(0.061,0.061,0.066)-0.035))*100,
                                                                         ((mean(0.058,0.057,0.059)-0.035)/(mean(0.061,0.061,0.066)-0.035))*100,
                                                                         ((mean(0.058,0.058,0.058)-0.035)/(mean(0.061,0.061,0.066)-0.035))*100,
                                                                         ((mean(0.059,0.062,0.057)-0.035)/(mean(0.061,0.061,0.066)-0.035))*100,
                                                                         ((mean(0.059,0.057,0.057)-0.035)/(mean(0.061,0.061,0.066)-0.035))*100,
                                                                         ((mean(0.059,0.058,0.057)-0.035)/(mean(0.061,0.061,0.066)-0.035))*100,
                                                                         ((mean(0.05,0.05,0.05)-mean(0.315,0.038,0.318))/(mean(0.335,0.333,0.368)-mean(0.315,0.038,0.318)))*100,
                                                                         ((mean(0.095,0.09,0.094)-mean(0.315,0.038,0.318))/(mean(0.335,0.333,0.368)-mean(0.315,0.038,0.318)))*100,
                                                                         ((mean(0.108,0.106,0.109)-mean(0.315,0.038,0.318))/(mean(0.335,0.333,0.368)-mean(0.315,0.038,0.318)))*100,
                                                                         ((mean(0.124,0.127,0.12)-mean(0.315,0.038,0.318))/(mean(0.335,0.333,0.368)-mean(0.315,0.038,0.318)))*100,
                                                                         ((mean(0.132,0.137,0.133)-mean(0.315,0.038,0.318))/(mean(0.335,0.333,0.368)-mean(0.315,0.038,0.318)))*100,
                                                                         ((mean(0.136,0.144,0.132)-mean(0.315,0.038,0.318))/(mean(0.335,0.333,0.368)-mean(0.315,0.038,0.318)))*100,
                                                                         ((mean(0.135,0.144,0.136)-mean(0.315,0.038,0.318))/(mean(0.335,0.333,0.368)-mean(0.315,0.038,0.318)))*100,
                                                                         ((mean(0.311,0.333,0.32)-mean(0.315,0.038,0.318))/(mean(0.335,0.333,0.368)-mean(0.315,0.038,0.318)))*100,
                                                                         ((mean(0.048,0.048,0.046)-mean(0.343,0.036,0.351))/(mean(0.35,0.337,0.37)-mean(0.343,0.036,0.351)))*100,
                                                                         ((mean(0.096,0.089,0.098)-mean(0.343,0.036,0.351))/(mean(0.35,0.337,0.37)-mean(0.343,0.036,0.351)))*100,
                                                                         ((mean(0.106,0.103,0.109)-mean(0.343,0.036,0.351))/(mean(0.35,0.337,0.37)-mean(0.343,0.036,0.351)))*100,
                                                                         ((mean(0.121,0.124,0.122)-mean(0.343,0.036,0.351))/(mean(0.35,0.337,0.37)-mean(0.343,0.036,0.351)))*100,
                                                                         ((mean(0.133,0.136,0.136)-mean(0.343,0.036,0.351))/(mean(0.35,0.337,0.37)-mean(0.343,0.036,0.351)))*100,
                                                                         ((mean(0.14,0.146,0.135)-mean(0.343,0.036,0.351))/(mean(0.35,0.337,0.37)-mean(0.343,0.036,0.351)))*100,
                                                                         ((mean(0.143,0.152,0.144)-mean(0.343,0.036,0.351))/(mean(0.35,0.337,0.37)-mean(0.343,0.036,0.351)))*100,
                                                                         ((mean(0.333,0.335,0.324)-mean(0.343,0.036,0.351))/(mean(0.35,0.337,0.37)-mean(0.343,0.036,0.351)))*100,
                                                                         ((mean(0.047,0.048,0.046)-mean(0.46,0.035,0.456))/(mean(0.482,0.504,0.443)-mean(0.46,0.035,0.456)))*100,
                                                                         ((mean(0.08,0.055,0.088)-mean(0.46,0.035,0.456))/(mean(0.482,0.504,0.443)-mean(0.46,0.035,0.456)))*100,
                                                                         ((mean(0.103,0.102,0.104)-mean(0.46,0.035,0.456))/(mean(0.482,0.504,0.443)-mean(0.46,0.035,0.456)))*100,
                                                                         ((mean(0.119,0.12,0.117)-mean(0.46,0.035,0.456))/(mean(0.482,0.504,0.443)-mean(0.46,0.035,0.456)))*100,
                                                                         ((mean(0.129,0.137,0.135)-mean(0.46,0.035,0.456))/(mean(0.482,0.504,0.443)-mean(0.46,0.035,0.456)))*100,
                                                                         ((mean(0.147,0.162,0.147)-mean(0.46,0.035,0.456))/(mean(0.482,0.504,0.443)-mean(0.46,0.035,0.456)))*100,
                                                                         ((mean(0.153,0.175,0.152)-mean(0.46,0.035,0.456))/(mean(0.482,0.504,0.443)-mean(0.46,0.035,0.456)))*100,
                                                                         ((mean(0.447,0.489,0.399)-mean(0.46,0.035,0.456))/(mean(0.482,0.504,0.443)-mean(0.46,0.035,0.456)))*100,
                                                                         ((mean(0.048,0.048,0.046)-mean(0.488,0.035,0.506))/(mean(0.495,0.5,0.474)-mean(0.488,0.035,0.506)))*100,
                                                                         ((mean(0.081,0.057,0.09)-mean(0.488,0.035,0.506))/(mean(0.495,0.5,0.474)-mean(0.488,0.035,0.506)))*100,
                                                                         ((mean(0.104,0.104,0.108)-mean(0.488,0.035,0.506))/(mean(0.495,0.5,0.474)-mean(0.488,0.035,0.506)))*100,
                                                                         ((mean(0.117,0.117,0.117)-mean(0.488,0.035,0.506))/(mean(0.495,0.5,0.474)-mean(0.488,0.035,0.506)))*100,
                                                                         ((mean(0.13,0.137,0.137)-mean(0.488,0.035,0.506))/(mean(0.495,0.5,0.474)-mean(0.488,0.035,0.506)))*100,
                                                                         ((mean(0.149,0.163,0.149)-mean(0.488,0.035,0.506))/(mean(0.495,0.5,0.474)-mean(0.488,0.035,0.506)))*100,
                                                                         ((mean(0.157,0.179,0.157)-mean(0.488,0.035,0.506))/(mean(0.495,0.5,0.474)-mean(0.488,0.035,0.506)))*100,
                                                                         ((mean(0.482,0.495,0.45)-mean(0.488,0.035,0.506))/(mean(0.495,0.5,0.474)-mean(0.488,0.035,0.506)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Pp3 <- Proteus_mirabilis_25ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 25 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Proteus_mirabilis_25ug_data$Optical_density_mean_values-sd(Proteus_mirabilis_25ug_data$Optical_density_mean_values),
                ymax=Proteus_mirabilis_25ug_data$Optical_density_mean_values+sd(Proteus_mirabilis_25ug_data$Optical_density_mean_values), col="red")
print(Pp3)
#Penicillin 12.5 ug/mL
Proteus_mirabilis_12.5ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                          Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                          Optical_density_mean_values =c(((mean(0.063,0.065,0.062)-0.038)/(mean(0.065,0.066,0.063)-0.038))*100,
                                                                         ((mean(0.065,0.065,0.064)-0.038)/(mean(0.065,0.066,0.063)-0.038))*100,
                                                                         ((mean(0.063,0.064,0.064)-0.038)/(mean(0.065,0.066,0.063)-0.038))*100,
                                                                         ((mean(0.064,0.063,0.064)-0.038)/(mean(0.065,0.066,0.063)-0.038))*100,
                                                                         ((mean(0.063,0.064,0.063)-0.038)/(mean(0.065,0.066,0.063)-0.038))*100,
                                                                         ((0.064-0.038)/(mean(0.065,0.066,0.063)-0.038))*100,
                                                                         ((mean(0.065,0.064,0.063)-0.038)/(mean(0.065,0.066,0.063)-0.038))*100,
                                                                         ((mean(0.064,0.063,0.063)-0.038)/(mean(0.065,0.066,0.063)-0.038))*100,
                                                                         ((mean(0.052,0.053,0.053)-mean(0.334,0.038,0.321))/(mean(0.353,0.358,0.339)-mean(0.334,0.038,0.321)))*100,
                                                                         ((mean(0.1,0.104,0.103)-mean(0.334,0.038,0.321))/(mean(0.353,0.358,0.339)-mean(0.334,0.038,0.321)))*100,
                                                                         ((mean(0.121,0.117,0.114)-mean(0.334,0.038,0.321))/(mean(0.353,0.358,0.339)-mean(0.334,0.038,0.321)))*100,
                                                                         ((mean(0.135,0.137,0.131)-mean(0.334,0.038,0.321))/(mean(0.353,0.358,0.339)-mean(0.334,0.038,0.321)))*100,
                                                                         ((mean(0.142,0.149,0.143)-mean(0.334,0.038,0.321))/(mean(0.353,0.358,0.339)-mean(0.334,0.038,0.321)))*100,
                                                                         ((mean(0.148,0.153,0.148)-mean(0.334,0.038,0.321))/(mean(0.353,0.358,0.339)-mean(0.334,0.038,0.321)))*100,
                                                                         ((mean(0.15,0.157,0.15)-mean(0.334,0.038,0.321))/(mean(0.353,0.358,0.339)-mean(0.334,0.038,0.321)))*100,
                                                                         ((mean(0.333,0.35,0.331)-mean(0.334,0.038,0.321))/(mean(0.353,0.358,0.339)-mean(0.334,0.038,0.321)))*100,
                                                                         ((mean(0.052,0.053,0.051)-mean(0.355,0.038,0.372))/(mean(0.373,0.353,0.344)-mean(0.355,0.038,0.372)))*100,
                                                                         ((mean(0.103,0.105,0.107)-mean(0.355,0.038,0.372))/(mean(0.373,0.353,0.344)-mean(0.355,0.038,0.372)))*100,
                                                                         ((mean(0.12,0.117,0.117)-mean(0.355,0.038,0.372))/(mean(0.373,0.353,0.344)-mean(0.355,0.038,0.372)))*100,
                                                                         ((mean(0.133,0.135,0.134)-mean(0.355,0.038,0.372))/(mean(0.373,0.353,0.344)-mean(0.355,0.038,0.372)))*100,
                                                                         ((mean(0.143,0.148,0.147)-mean(0.355,0.038,0.372))/(mean(0.373,0.353,0.344)-mean(0.355,0.038,0.372)))*100,
                                                                         ((mean(0.149,0.156,0.151)-mean(0.355,0.038,0.372))/(mean(0.373,0.353,0.344)-mean(0.355,0.038,0.372)))*100,
                                                                         ((mean(0.156,0.167,0.158)-mean(0.355,0.038,0.372))/(mean(0.373,0.353,0.344)-mean(0.355,0.038,0.372)))*100,
                                                                         ((mean(0.352,0.351,0.334)-mean(0.355,0.038,0.372))/(mean(0.373,0.353,0.344)-mean(0.355,0.038,0.372)))*100,
                                                                         ((mean(0.053,0.054,0.053)-mean(0.462,0.038,0.473))/(mean(0.507,0.508,0.422)-mean(0.462,0.038,0.473)))*100,
                                                                         ((mean(0.094,0.091,0.104)-mean(0.462,0.038,0.473))/(mean(0.507,0.508,0.422)-mean(0.462,0.038,0.473)))*100,
                                                                         ((mean(0.119,0.115,0.111)-mean(0.462,0.038,0.473))/(mean(0.507,0.508,0.422)-mean(0.462,0.038,0.473)))*100,
                                                                         ((mean(0.133,0.132,0.129)-mean(0.462,0.038,0.473))/(mean(0.507,0.508,0.422)-mean(0.462,0.038,0.473)))*100,
                                                                         ((mean(0.14,0.148,0.149)-mean(0.462,0.038,0.473))/(mean(0.507,0.508,0.422)-mean(0.462,0.038,0.473)))*100,
                                                                         ((mean(0.156,0.169,0.16)-mean(0.462,0.038,0.473))/(mean(0.507,0.508,0.422)-mean(0.462,0.038,0.473)))*100,
                                                                         ((mean(0.168,0.185,0.166)-mean(0.462,0.038,0.473))/(mean(0.507,0.508,0.422)-mean(0.462,0.038,0.473)))*100,
                                                                         ((mean(0.444,0.492,0.405)-mean(0.462,0.038,0.473))/(mean(0.507,0.508,0.422)-mean(0.462,0.038,0.473)))*100,
                                                                         ((mean(0.054,0.054,0.055)-mean(0.49,0.038,0.506))/(mean(0.52,0.504,0.478)-mean(0.49,0.038,0.506)))*100,
                                                                         ((mean(0.095,0.093,0.103)-mean(0.49,0.038,0.506))/(mean(0.52,0.504,0.478)-mean(0.49,0.038,0.506)))*100,
                                                                         ((mean(0.119,0.117,0.115)-mean(0.49,0.038,0.506))/(mean(0.52,0.504,0.478)-mean(0.49,0.038,0.506)))*100,
                                                                         ((mean(0.131,0.13,0.128)-mean(0.49,0.038,0.506))/(mean(0.52,0.504,0.478)-mean(0.49,0.038,0.506)))*100,
                                                                         ((mean(0.141,0.149,0.15)-mean(0.49,0.038,0.506))/(mean(0.52,0.504,0.478)-mean(0.49,0.038,0.506)))*100,
                                                                         ((mean(0.159,0.171,0.162)-mean(0.49,0.038,0.506))/(mean(0.52,0.504,0.478)-mean(0.49,0.038,0.506)))*100,
                                                                         ((mean(0.171,0.187,0.171)-mean(0.49,0.038,0.506))/(mean(0.52,0.504,0.478)-mean(0.49,0.038,0.506)))*100,
                                                                         ((mean(0.477,0.501,0.447)-mean(0.49,0.038,0.506))/(mean(0.52,0.504,0.478)-mean(0.49,0.038,0.506)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Pp4 <- Proteus_mirabilis_12.5ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 12.5 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Proteus_mirabilis_12.5ug_data $Optical_density_mean_values-sd(Proteus_mirabilis_12.5ug_data $Optical_density_mean_values),
                ymax=Proteus_mirabilis_12.5ug_data $Optical_density_mean_values+sd(Proteus_mirabilis_12.5ug_data $Optical_density_mean_values), col="red")
print(Pp4)
#Penicillin 6.25 ug/mL
Proteus_mirabilis_6.25ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                            Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                            Optical_density_mean_values =c(((mean(0.062,0.063,0.061)-0.036)/(mean(0.064,0.065,0.063)-0.036))*100,
                                                                           ((mean(0.062,0.065,0.062)-0.036)/(mean(0.064,0.065,0.063)-0.036))*100,
                                                                           ((mean(0.061,0.062,0.062)-0.036)/(mean(0.064,0.065,0.063)-0.036))*100,
                                                                           ((mean(0.062,0.063,0.063)-0.036)/(mean(0.064,0.065,0.063)-0.036))*100,
                                                                           ((mean(0.063,0.062,0.063)-0.036)/(mean(0.064,0.065,0.063)-0.036))*100,
                                                                           ((mean(0.061,0.062,0.062)-0.036)/(mean(0.064,0.065,0.063)-0.036))*100,
                                                                           ((mean(0.062,0.062,0.061)-0.036)/(mean(0.064,0.065,0.063)-0.036))*100,
                                                                           ((mean(0.062,0.062,0.063)-0.036)/(mean(0.064,0.065,0.063)-0.036))*100,
                                                                           ((mean(0.057,0.057,0.05)-mean(0.035,0.035,0.338))/(mean(0.361,0.368,0.352)-mean(0.035,0.035,0.338)))*100,
                                                                           ((mean(0.105,0.106,0.107)-mean(0.035,0.035,0.338))/(mean(0.361,0.368,0.352)-mean(0.035,0.035,0.338)))*100,
                                                                           ((mean(0.122,0.121,0.117)-mean(0.035,0.035,0.338))/(mean(0.361,0.368,0.352)-mean(0.035,0.035,0.338)))*100,
                                                                           ((mean(0.133,0.143,0.135)-mean(0.035,0.035,0.338))/(mean(0.361,0.368,0.352)-mean(0.035,0.035,0.338)))*100,
                                                                           ((mean(0.149,0.155,0.149)-mean(0.035,0.035,0.338))/(mean(0.361,0.368,0.352)-mean(0.035,0.035,0.338)))*100,
                                                                           ((mean(0.151,0.156,0.153)-mean(0.035,0.035,0.338))/(mean(0.361,0.368,0.352)-mean(0.035,0.035,0.338)))*100,
                                                                           ((mean(0.152,0.165,0.157)-mean(0.035,0.035,0.338))/(mean(0.361,0.368,0.352)-mean(0.035,0.035,0.338)))*100,
                                                                           ((mean(0.339,0.351,0.349)-mean(0.035,0.035,0.338))/(mean(0.361,0.368,0.352)-mean(0.035,0.035,0.338)))*100,
                                                                           ((mean(0.05,0.051,0.05)-mean(0.035,0.037,0.366))/(mean(0.378,0.365,0.352)-mean(0.035,0.037,0.366)))*100,
                                                                           ((mean(0.108,0.106,0.109)-mean(0.035,0.037,0.366))/(mean(0.378,0.365,0.352)-mean(0.035,0.037,0.366)))*100,
                                                                           ((mean(0.121,0.122,0.119)-mean(0.035,0.037,0.366))/(mean(0.378,0.365,0.352)-mean(0.035,0.037,0.366)))*100,
                                                                           ((mean(0.132,0.141,0.138)-mean(0.035,0.037,0.366))/(mean(0.378,0.365,0.352)-mean(0.035,0.037,0.366)))*100,
                                                                           ((mean(0.148,0.154,0.153)-mean(0.035,0.037,0.366))/(mean(0.378,0.365,0.352)-mean(0.035,0.037,0.366)))*100,
                                                                           ((mean(0.154,0.16,0.156)-mean(0.035,0.037,0.366))/(mean(0.378,0.365,0.352)-mean(0.035,0.037,0.366)))*100,
                                                                           ((mean(0.158,0.173,0.164)-mean(0.035,0.037,0.366))/(mean(0.378,0.365,0.352)-mean(0.035,0.037,0.366)))*100,
                                                                           ((mean(0.357,0.353,0.342)-mean(0.035,0.037,0.366))/(mean(0.378,0.365,0.352)-mean(0.035,0.037,0.366)))*100,
                                                                           ((mean(0.051,0.052,0.052)-mean(0.036,0.037,0.469))/(mean(0.511,0.503,0.425)-mean(0.036,0.037,0.469)))*100,
                                                                           ((mean(0.102,0.097,0.111)-mean(0.036,0.037,0.469))/(mean(0.511,0.503,0.425)-mean(0.036,0.037,0.469)))*100,
                                                                           ((mean(0.12,0.121,0.115)-mean(0.036,0.037,0.469))/(mean(0.511,0.503,0.425)-mean(0.036,0.037,0.469)))*100,
                                                                           ((mean(0.132,0.135,0.131)-mean(0.036,0.037,0.469))/(mean(0.511,0.503,0.425)-mean(0.036,0.037,0.469)))*100,
                                                                           ((mean(0.142,0.151,0.152)-mean(0.036,0.037,0.469))/(mean(0.511,0.503,0.425)-mean(0.036,0.037,0.469)))*100,
                                                                           ((mean(0.162,0.172,0.164)-mean(0.036,0.037,0.469))/(mean(0.511,0.503,0.425)-mean(0.036,0.037,0.469)))*100,
                                                                           ((mean(0.166,0.189,0.169)-mean(0.036,0.037,0.469))/(mean(0.511,0.503,0.425)-mean(0.036,0.037,0.469)))*100,
                                                                           ((mean(0.451,0.475,0.407)-mean(0.036,0.037,0.469))/(mean(0.511,0.503,0.425)-mean(0.036,0.037,0.469)))*100,
                                                                           ((mean(0.051,0.052,0.055)-mean(0.036,0.037,0.49))/(mean(0.523,0.494,0.479)-mean(0.036,0.037,0.49)))*100,
                                                                           ((mean(0.105,0.101,0.113)-mean(0.036,0.037,0.49))/(mean(0.523,0.494,0.479)-mean(0.036,0.037,0.49)))*100,
                                                                           ((mean(0.124,0.125,0.121)-mean(0.036,0.037,0.49))/(mean(0.523,0.494,0.479)-mean(0.036,0.037,0.49)))*100,
                                                                           ((mean(0.133,0.135,0.134)-mean(0.036,0.037,0.49))/(mean(0.523,0.494,0.479)-mean(0.036,0.037,0.49)))*100,
                                                                           ((mean(0.147,0.154,0.157)-mean(0.036,0.037,0.49))/(mean(0.523,0.494,0.479)-mean(0.036,0.037,0.49)))*100,
                                                                           ((mean(0.168,0.178,0.171)-mean(0.036,0.037,0.49))/(mean(0.523,0.494,0.479)-mean(0.036,0.037,0.49)))*100,
                                                                           ((mean(0.173,0.194,0.177)-mean(0.036,0.037,0.49))/(mean(0.523,0.494,0.479)-mean(0.036,0.037,0.49)))*100,
                                                                           ((mean(0.479,0.493,0.453)-mean(0.036,0.037,0.49))/(mean(0.523,0.494,0.479)-mean(0.036,0.037,0.49)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Pp5 <- Proteus_mirabilis_6.25ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 6.25 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Proteus_mirabilis_6.25ug_data$Optical_density_mean_values-sd(Proteus_mirabilis_6.25ug_data$Optical_density_mean_values),
                ymax=Proteus_mirabilis_6.25ug_data$Optical_density_mean_values+sd(Proteus_mirabilis_6.25ug_data$Optical_density_mean_values), col="red")
print(Pp5)
#Penicillin 3.125 ug/mL
Proteus_mirabilis_3.125ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                            Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                            Optical_density_mean_values =c(((mean(0.061,0.063,0.06)-mean(0.037,0.036,0.037))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.037)))*100,
                                                                           ((mean(0.062,0.062,0.062)-mean(0.037,0.036,0.037))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.037)))*100,
                                                                           ((mean(0.062,0.061,0.062)-mean(0.037,0.036,0.037))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.037)))*100,
                                                                           ((mean(0.061,0.062,0.062)-mean(0.037,0.036,0.037))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.037)))*100,
                                                                           ((mean(0.061,0.062,0.063)-mean(0.037,0.036,0.037))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.037)))*100,
                                                                           ((mean(0.061,0.061,0.063)-mean(0.037,0.036,0.037))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.037)))*100,
                                                                           ((mean(0.062,0.061,0.061)-mean(0.037,0.036,0.037))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.037)))*100,
                                                                           ((mean(0.062,0.06,0.063)-mean(0.037,0.036,0.037))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.037)))*100,
                                                                           ((mean(0.057,0.054,0.052)-mean(0.318,0.038,0.036))/(mean(0.352,0.368,0.355)-mean(0.318,0.038,0.036)))*100,
                                                                           ((mean(0.104,0.106,0.105)-mean(0.318,0.038,0.036))/(mean(0.352,0.368,0.355)-mean(0.318,0.038,0.036)))*100,
                                                                           ((mean(0.115,0.118,0.114)-mean(0.318,0.038,0.036))/(mean(0.352,0.368,0.355)-mean(0.318,0.038,0.036)))*100,
                                                                           ((mean(0.131,0.137,0.131)-mean(0.318,0.038,0.036))/(mean(0.352,0.368,0.355)-mean(0.318,0.038,0.036)))*100,
                                                                           ((mean(0.141,0.151,0.141)-mean(0.318,0.038,0.036))/(mean(0.352,0.368,0.355)-mean(0.318,0.038,0.036)))*100,
                                                                           ((mean(0.147,0.151,0.143)-mean(0.318,0.038,0.036))/(mean(0.352,0.368,0.355)-mean(0.318,0.038,0.036)))*100,
                                                                           ((mean(0.151,0.159,0.151)-mean(0.318,0.038,0.036))/(mean(0.352,0.368,0.355)-mean(0.318,0.038,0.036)))*100,
                                                                           ((mean(0.34,0.352,0.339)-mean(0.318,0.038,0.036))/(mean(0.352,0.368,0.355)-mean(0.318,0.038,0.036)))*100,
                                                                           ((mean(0.05,0.052,0.052)-mean(0.346,0.036,0.036))/(mean(0.369,0.36,0.348)-mean(0.346,0.036,0.036)))*100,
                                                                           ((mean(0.107,0.107,0.108)-mean(0.346,0.036,0.036))/(mean(0.369,0.36,0.348)-mean(0.346,0.036,0.036)))*100,
                                                                           ((mean(0.115,0.12,0.118)-mean(0.346,0.036,0.036))/(mean(0.369,0.36,0.348)-mean(0.346,0.036,0.036)))*100,
                                                                           ((mean(0.131,0.136,0.134)-mean(0.346,0.036,0.036))/(mean(0.369,0.36,0.348)-mean(0.346,0.036,0.036)))*100,
                                                                           ((mean(0.142,0.151,0.146)-mean(0.346,0.036,0.036))/(mean(0.369,0.36,0.348)-mean(0.346,0.036,0.036)))*100,
                                                                           ((mean(0.151,0.158,0.15)-mean(0.346,0.036,0.036))/(mean(0.369,0.36,0.348)-mean(0.346,0.036,0.036)))*100,
                                                                           ((mean(0.157,0.169,0.159)-mean(0.346,0.036,0.036))/(mean(0.369,0.36,0.348)-mean(0.346,0.036,0.036)))*100,
                                                                           ((mean(0.354,0.347,0.332)-mean(0.346,0.036,0.036))/(mean(0.369,0.36,0.348)-mean(0.346,0.036,0.036)))*100,
                                                                           ((mean(0.051,0.053,0.052)-mean(0.46,0.038,0.037))/(mean(0.498,0.501,0.42)-mean(0.46,0.038,0.037)))*100,
                                                                           ((mean(0.101,0.099,0.111)-mean(0.46,0.038,0.037))/(mean(0.498,0.501,0.42)-mean(0.46,0.038,0.037)))*100,
                                                                           ((mean(0.117,0.122,0.116)-mean(0.46,0.038,0.037))/(mean(0.498,0.501,0.42)-mean(0.46,0.038,0.037)))*100,
                                                                           ((mean(0.135,0.135,0.131)-mean(0.46,0.038,0.037))/(mean(0.498,0.501,0.42)-mean(0.46,0.038,0.037)))*100,
                                                                           ((mean(0.141,0.153,0.149)-mean(0.46,0.038,0.037))/(mean(0.498,0.501,0.42)-mean(0.46,0.038,0.037)))*100,
                                                                           ((mean(0.161,0.171,0.162)-mean(0.46,0.038,0.037))/(mean(0.498,0.501,0.42)-mean(0.46,0.038,0.037)))*100,
                                                                           ((mean(0.168,0.185,0.168)-mean(0.46,0.038,0.037))/(mean(0.498,0.501,0.42)-mean(0.46,0.038,0.037)))*100,
                                                                           ((mean(0.437,0.475,0.402)-mean(0.46,0.038,0.037))/(mean(0.498,0.501,0.42)-mean(0.46,0.038,0.037)))*100,
                                                                           ((mean(0.052,0.053,0.053)-mean(0.478,0.037,0.036))/(mean(0.513,0.504,0.474)-mean(0.478,0.037,0.036)))*100,
                                                                           ((mean(0.104,0.103,0.112)-mean(0.478,0.037,0.036))/(mean(0.513,0.504,0.474)-mean(0.478,0.037,0.036)))*100,
                                                                           ((mean(0.118,0.122,0.12)-mean(0.478,0.037,0.036))/(mean(0.513,0.504,0.474)-mean(0.478,0.037,0.036)))*100,
                                                                           ((mean(0.132,0.132,0.13)-mean(0.478,0.037,0.036))/(mean(0.513,0.504,0.474)-mean(0.478,0.037,0.036)))*100,
                                                                           ((mean(0.142,0.154,0.151)-mean(0.478,0.037,0.036))/(mean(0.513,0.504,0.474)-mean(0.478,0.037,0.036)))*100,
                                                                           ((mean(0.163,0.176,0.164)-mean(0.478,0.037,0.036))/(mean(0.513,0.504,0.474)-mean(0.478,0.037,0.036)))*100,
                                                                           ((mean(0.172,0.185,0.171)-mean(0.478,0.037,0.036))/(mean(0.513,0.504,0.474)-mean(0.478,0.037,0.036)))*100,
                                                                           ((mean(0.462,0.482,0.437)-mean(0.478,0.037,0.036))/(mean(0.513,0.504,0.474)-mean(0.478,0.037,0.036)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Pp6 <- Proteus_mirabilis_3.125ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 3.125 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Proteus_mirabilis_3.125ug_data$Optical_density_mean_values-sd(Proteus_mirabilis_3.125ug_data$Optical_density_mean_values),
                ymax=Proteus_mirabilis_3.125ug_data$Optical_density_mean_values+sd(Proteus_mirabilis_3.125ug_data$Optical_density_mean_values), col="red")
print(Pp6)
#Penicillin 1.5625 ug/mL
Proteus_mirabilis_1.5625ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                             Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                             Optical_density_mean_values =c(((mean(0.06,0.063,0.059)-0.036)/(mean(0.062,0.061,0.061)-0.036))*100,
                                                                            ((mean(0.059,0.062,0.06)-0.036)/(mean(0.062,0.061,0.061)-0.036))*100,
                                                                            ((mean(0.06,0.06,0.06)-0.036)/(mean(0.062,0.061,0.061)-0.036))*100,
                                                                            ((mean(0.06,0.06,0.061)-0.036)/(mean(0.062,0.061,0.061)-0.036))*100,
                                                                            ((mean(0.06,0.06,0.061)-0.036)/(mean(0.062,0.061,0.061)-0.036))*100,
                                                                            ((mean(0.06,0.06,0.061)-0.036)/(mean(0.062,0.061,0.061)-0.036))*100,
                                                                            ((mean(0.06,0.06,0.06)-0.036)/(mean(0.062,0.061,0.061)-0.036))*100,
                                                                            ((mean(0.061,0.06,0.061)-0.036)/(mean(0.062,0.061,0.061)-0.036))*100,
                                                                            ((mean(0.051,0.054,0.051)-0.036)/(mean(0.35,0.362,0.349)-0.036))*100,
                                                                            ((mean(0.1,0.106,0.102)-0.036)/(mean(0.35,0.362,0.349)-0.036))*100,
                                                                            ((mean(0.111,0.115,0.112)-0.036)/(mean(0.35,0.362,0.349)-0.036))*100,
                                                                            ((mean(0.127,0.133,0.127)-0.036)/(mean(0.35,0.362,0.349)-0.036))*100,
                                                                            ((mean(0.137,0.139,0.345)-0.036)/(mean(0.35,0.362,0.349)-0.036))*100,
                                                                            ((mean(0.144,0.152,0.138)-0.036)/(mean(0.35,0.362,0.349)-0.036))*100,
                                                                            ((mean(0.15,0.157,0.142)-0.036)/(mean(0.35,0.362,0.349)-0.036))*100,
                                                                            ((mean(0.343,0.349,0.341)-0.036)/(mean(0.35,0.362,0.349)-0.036))*100,
                                                                            ((mean(0.049,0.051,0.053)-mean(0.037,0.036,0.037))/(mean(0.366,0.361,0.345)-mean(0.037,0.036,0.037)))*100,
                                                                            ((mean(0.104,0.108,0.112)-mean(0.037,0.036,0.037))/(mean(0.366,0.361,0.345)-mean(0.037,0.036,0.037)))*100,
                                                                            ((mean(0.112,0.117,0.116)-mean(0.037,0.036,0.037))/(mean(0.366,0.361,0.345)-mean(0.037,0.036,0.037)))*100,
                                                                            ((mean(0.127,0.133,0.13)-mean(0.037,0.036,0.037))/(mean(0.366,0.361,0.345)-mean(0.037,0.036,0.037)))*100,
                                                                            ((mean(0.137,0.14,0.337)-mean(0.037,0.036,0.037))/(mean(0.366,0.361,0.345)-mean(0.037,0.036,0.037)))*100,
                                                                            ((mean(0.149,0.158,0.147)-mean(0.037,0.036,0.037))/(mean(0.366,0.361,0.345)-mean(0.037,0.036,0.037)))*100,
                                                                            ((mean(0.157,0.167,0.154)-mean(0.037,0.036,0.037))/(mean(0.366,0.361,0.345)-mean(0.037,0.036,0.037)))*100,
                                                                            ((mean(0.359,0.347,0.335)-mean(0.037,0.036,0.037))/(mean(0.366,0.361,0.345)-mean(0.037,0.036,0.037)))*100,
                                                                            ((mean(0.052,0.053,0.053)-mean(0.037,0.037,0.036))/(mean(0.5,0.521,0.42)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.097,0.102,0.111)-mean(0.037,0.037,0.036))/(mean(0.5,0.521,0.42)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.116,0.119,0.116)-mean(0.037,0.037,0.036))/(mean(0.5,0.521,0.42)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.13,0.132,0.129-mean(0.037,0.037,0.036))/(mean(0.5,0.521,0.42)-mean(0.037,0.037,0.036)))*100),
                                                                            ((mean(0.139,0.138,0.406)-mean(0.037,0.037,0.036))/(mean(0.5,0.521,0.42)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.16,0.173,0.158)-mean(0.037,0.037,0.036))/(mean(0.5,0.521,0.42)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.168,0.183,0.163)-mean(0.037,0.037,0.036))/(mean(0.5,0.521,0.42)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.467,0.502,0.408)-mean(0.037,0.037,0.036))/(mean(0.5,0.521,0.42)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.052,0.052,0.052)-mean(0.037,0.037,0.036))/(mean(0.51,0.503,0.469)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.1,0.105,0.109)-mean(0.037,0.037,0.036))/(mean(0.51,0.503,0.469)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.119,0.123,0.119)-mean(0.037,0.037,0.036))/(mean(0.51,0.503,0.469)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.129,0.13,0.13)-mean(0.037,0.037,0.036))/(mean(0.51,0.503,0.469)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.139,0.136,0.427)-mean(0.037,0.037,0.036))/(mean(0.51,0.503,0.469)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.161,0.174,0.16)-mean(0.037,0.037,0.036))/(mean(0.51,0.503,0.469)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.172,0.187,0.166)-mean(0.037,0.037,0.036))/(mean(0.51,0.503,0.469)-mean(0.037,0.037,0.036)))*100,
                                                                            ((mean(0.482,0.478,0.44)-mean(0.037,0.037,0.036))/(mean(0.51,0.503,0.469)-mean(0.037,0.037,0.036)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Pp7 <- Proteus_mirabilis_1.5625ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 1.5625 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Proteus_mirabilis_1.5625ug_data$Optical_density_mean_values-sd(Proteus_mirabilis_1.5625ug_data$Optical_density_mean_values),
                ymax=Proteus_mirabilis_1.5625ug_data$Optical_density_mean_values+sd(Proteus_mirabilis_1.5625ug_data$Optical_density_mean_values), col="red")
print(Pp7)
#Penicillin 0 ug/mL
Proteus_mirabilis_0ug_data <- data.frame(Time= c(rep("0",8),rep("19",8),rep("24",8),rep("41",8),rep("48",8)),
                                              Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                              Optical_density_mean_values =c(((mean(0.062,0.066,0.062)-mean(0.037,0.036,0.036))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.036)))*100,
                                                                             ((mean(0.062,0.064,0.062)-mean(0.037,0.036,0.036))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.036)))*100,
                                                                             ((mean(0.062,0.063,0.063)-mean(0.037,0.036,0.036))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.036)))*100,
                                                                             ((mean(0.062,0.062,0.063)-mean(0.037,0.036,0.036))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.036)))*100,
                                                                             ((mean(0.061,0.061,0.063)-mean(0.037,0.036,0.036))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.036)))*100,
                                                                             ((mean(0.062,0.063,0.063)-mean(0.037,0.036,0.036))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.036)))*100,
                                                                             ((mean(0.062,0.061,0.062)-mean(0.037,0.036,0.036))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.036)))*100,
                                                                             ((mean(0.062,0.061,0.063)-mean(0.037,0.036,0.036))/(mean(0.063,0.064,0.063)-mean(0.037,0.036,0.036)))*100,
                                                                             ((mean(0.055,0.055,0.051)-mean(0.317,0.036,0.036))/(mean(0.378,0.386,0.366)-mean(0.317,0.036,0.036)))*100,
                                                                             ((mean(0.107,0.108,0.104)-mean(0.317,0.036,0.036))/(mean(0.378,0.386,0.366)-mean(0.317,0.036,0.036)))*100,
                                                                             ((mean(0.12,0.12,0.116)-mean(0.317,0.036,0.036))/(mean(0.378,0.386,0.366)-mean(0.317,0.036,0.036)))*100,
                                                                             ((mean(0.139,0.139,0.137)-mean(0.317,0.036,0.036))/(mean(0.378,0.386,0.366)-mean(0.317,0.036,0.036)))*100,
                                                                             ((mean(0.142,0.157,0.375)-mean(0.317,0.036,0.036))/(mean(0.378,0.386,0.366)-mean(0.317,0.036,0.036)))*100,
                                                                             ((mean(0.164,0.168,0.162)-mean(0.317,0.036,0.036))/(mean(0.378,0.386,0.366)-mean(0.317,0.036,0.036)))*100,
                                                                             ((mean(0.176,0.183,0.165)-mean(0.317,0.036,0.036))/(mean(0.378,0.386,0.366)-mean(0.317,0.036,0.036)))*100,
                                                                             ((mean(0.377,0.38,0.375)-mean(0.317,0.036,0.036))/(mean(0.378,0.386,0.366)-mean(0.317,0.036,0.036)))*100,
                                                                             ((mean(0.051,0.054,0.053)-mean(0.359,0.037,0.038))/(mean(0.385,0.383,0.36)-mean(0.359,0.037,0.038)))*100,
                                                                             ((mean(0.113,0.109,0.11)-mean(0.359,0.037,0.038))/(mean(0.385,0.383,0.36)-mean(0.359,0.037,0.038)))*100,
                                                                             ((mean(0.122,0.122,0.121)-mean(0.359,0.037,0.038))/(mean(0.385,0.383,0.36)-mean(0.359,0.037,0.038)))*100,
                                                                             ((mean(0.137,0.139,0.14)-mean(0.359,0.037,0.038))/(mean(0.385,0.383,0.36)-mean(0.359,0.037,0.038)))*100,
                                                                             ((mean(0.145,0.16,0.357)-mean(0.359,0.037,0.038))/(mean(0.385,0.383,0.36)-mean(0.359,0.037,0.038)))*100,
                                                                             ((mean(0.17,0.176,0.169)-mean(0.359,0.037,0.038))/(mean(0.385,0.383,0.36)-mean(0.359,0.037,0.038)))*100,
                                                                             ((mean(0.186,0.196,0.174)-mean(0.359,0.037,0.038))/(mean(0.385,0.383,0.36)-mean(0.359,0.037,0.038)))*100,
                                                                             ((mean(0.382,0.376,0.359)-mean(0.359,0.037,0.038))/(mean(0.385,0.383,0.36)-mean(0.359,0.037,0.038)))*100,
                                                                             ((mean(0.052,0.054,0.053)-mean(0.528,0.038,0.038))/(mean(0.521,0.578,0.422)-mean(0.528,0.038,0.038)))*100,
                                                                             ((mean(0.099,0.1,0.112)-mean(0.528,0.038,0.038))/(mean(0.521,0.578,0.422)-mean(0.528,0.038,0.038)))*100,
                                                                             ((mean(0.121,0.121,0.12)-mean(0.528,0.038,0.038))/(mean(0.521,0.578,0.422)-mean(0.528,0.038,0.038)))*100,
                                                                             ((mean(0.134,0.134,0.137)-mean(0.528,0.038,0.038))/(mean(0.521,0.578,0.422)-mean(0.528,0.038,0.038)))*100,
                                                                             ((mean(0.147,0.164,0.422)-mean(0.528,0.038,0.038))/(mean(0.521,0.578,0.422)-mean(0.528,0.038,0.038)))*100,
                                                                             ((mean(0.183,0.192,0.181)-mean(0.528,0.038,0.038))/(mean(0.521,0.578,0.422)-mean(0.528,0.038,0.038)))*100,
                                                                             ((mean(0.194,0.215,0.188)-mean(0.528,0.038,0.038))/(mean(0.521,0.578,0.422)-mean(0.528,0.038,0.038)))*100,
                                                                             ((mean(0.509,0.562,0.426)-mean(0.528,0.038,0.038))/(mean(0.521,0.578,0.422)-mean(0.528,0.038,0.038)))*100,
                                                                             ((mean(0.052,0.053,0.052)-mean(0.529,0.037,0.038))/(mean(0.523,0.523,0.485)-mean(0.529,0.037,0.038)))*100,
                                                                             ((mean(0.101,0.104,0.113)-mean(0.529,0.037,0.038))/(mean(0.523,0.523,0.485)-mean(0.529,0.037,0.038)))*100,
                                                                             ((mean(0.123,0.126,0.124)-mean(0.529,0.037,0.038))/(mean(0.523,0.523,0.485)-mean(0.529,0.037,0.038)))*100,
                                                                             ((mean(0.132,0.137,0.137)-mean(0.529,0.037,0.038))/(mean(0.523,0.523,0.485)-mean(0.529,0.037,0.038)))*100,
                                                                             ((mean(0.149,0.168,0.445)-mean(0.529,0.037,0.038))/(mean(0.523,0.523,0.485)-mean(0.529,0.037,0.038)))*100,
                                                                             ((mean(0.181,0.194,0.184)-mean(0.529,0.037,0.038))/(mean(0.523,0.523,0.485)-mean(0.529,0.037,0.038)))*100,
                                                                             ((mean(0.199,0.23,0.199)-mean(0.529,0.037,0.038))/(mean(0.523,0.523,0.485)-mean(0.529,0.037,0.038)))*100,
                                                                             ((mean(0.525,0.519,0.471)-mean(0.529,0.037,0.038))/(mean(0.523,0.523,0.485)-mean(0.529,0.037,0.038)))*100))

#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Pp8 <- Proteus_mirabilis_0ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 0 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Proteus_mirabilis_0ug_data$Optical_density_mean_values-sd(Proteus_mirabilis_0ug_data$Optical_density_mean_values),
                ymax=Proteus_mirabilis_0ug_data$Optical_density_mean_values+sd(Proteus_mirabilis_0ug_data$Optical_density_mean_values), col="red")
print(Pp8)
#Align all graphs together 
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
figure_2 <- ggarrange(Pp1,Pp2,Pp3,Pp4, Pp5, Pp6, Pp7, Pp8,
                    labels=c("a","b","c","d","e","f","g","h"),
                    ncol=4,nrow=2,
                    common.legend = TRUE,legend = "bottom")
print(figure_2)
#Compare concentrations that inhibt the bacterial growth 
Final_Proteus_data <- data.frame(Time_f2 =c("0","19","24","41","48"),
                                 Concentrations= c(rep("PenV+Tebipenem (100 ug/mL+10 ug/mL)",5),rep("PenV+Tebipenem (100 ug/mL+5 ug/mL)",5),
                                                   rep("PenV+Tebipenem (50 ug/mL+10 ug/mL)",5),rep("PenV+Tebipenem (50 ug/mL+5 ug/mL)",5),
                                                   rep("PenV+Tebipenem (6.25 ug/mL+10 ug/mL)",5),rep("PenV+Tebipenem (1.5625 ug/mL+10 ug/mL)",5)),
                                 Optical_density=c(((mean(0.04,0.06,0.806)-0.038)/(mean(0.064,0.063,0.062)-0.038))*100,
                                                   ((mean(0.04,0.049,0.051)-mean(0.038,0.037,0.037))/(mean(0.321,0.32,0.324)-mean(0.038,0.037,0.037)))*100,
                                                   ((mean(0.04,0.048,0.043)-mean(0.038,0.037,0.038))/(mean(0.349,0.335,0.334)-mean(0.038,0.037,0.038)))*100,
                                                   ((mean(0.041,0.049,0.039)-mean(0.038,0.036,0.037))/(mean(0.477,0.471,0.411)-mean(0.038,0.036,0.037)))*100,
                                                   ((mean(0.041,0.048,0.036)-mean(0.038,0.036,0.036))/(mean(0.494,0.454,0.501)-mean(0.038,0.036,0.036)))*100,
                                                   ((mean(0.06,0.061,0.061)-0.038)/(mean(0.064,0.063,0.062)-0.038))*100,
                                                   ((mean(0.05,0.06,0.054)-mean(0.038,0.037,0.037))/(mean(0.321,0.32,0.324)-mean(0.038,0.037,0.037)))*100,
                                                   ((mean(0.051,0.052,0.051)-mean(0.038,0.037,0.038))/(mean(0.349,0.335,0.334)-mean(0.038,0.037,0.038)))*100,
                                                   ((mean(0.053,0.053,0.051)-mean(0.038,0.036,0.037))/(mean(0.477,0.471,0.411)-mean(0.038,0.036,0.037)))*100,
                                                   ((mean(0.051,0.051,0.052)-mean(0.038,0.036,0.036))/(mean(0.494,0.454,0.501)-mean(0.038,0.036,0.036)))*100,
                                                   ((mean(0.06,0.06,0.04)-mean(0.035,0.036,0.037))/(mean(0.062,0.063,0.061)-mean(0.035,0.036,0.037)))*100,
                                                   ((mean(0.046,0.048,0.194)-mean(0.035,0.036,0.037))/(mean(0.335,0.338,0.325,)-mean(0.035,0.036,0.037)))*100,
                                                   ((mean(0.049,0.051,0.239)-mean(0.037,0.038,0.038))/(mean(0.361,0.339,0.336)-mean(0.037,0.038,0.038)))*100,
                                                   ((mean(0.05,0.052,0.239)-0.038)/(mean(0.505,0.514,0.419)-0.038))*100,
                                                   ((mean(0.05,0.051,0.168)-0.038)/(mean(0.521,0.515,0.491)-0.038))*100,
                                                   ((mean(0.06,0.06,0.06)-mean(0.035,0.036,0.037))/(mean(0.062,0.063,0.061)-mean(0.035,0.036,0.037)))*100,
                                                   ((mean(0.063,0.097,0.069)-mean(0.035,0.036,0.037))/(mean(0.335,0.338,0.325)-mean(0.035,0.036,0.037)))*100,
                                                   ((mean(0.057,0.1,0.067)-mean(0.037,0.038,0.038))/(mean(0.361,0.339,0.336)-mean(0.037,0.038,0.038)))*100,
                                                   ((mean(0.054,0.082,0.056)-0.038)/(mean(0.505,0.514,0.419)-0.038))*100,
                                                   ((mean(0.056,0.083,0.061)-0.038)/(mean(0.521,0.515,0.491)-0.038))*100,
                                                   ((mean(0.062,0.063,0.061)-0.036)/(mean(0.064,0.065,0.063)-0.036))*100,
                                                   ((mean(0.057,0.057,0.05)-mean(0.035,0.035,0.338))/(mean(0.361,0.368,0.352)-mean(0.035,0.035,0.338)))*100,
                                                   ((mean(0.05,0.051,0.05)-mean(0.035,0.037,0.366))/(mean(0.378,0.365,0.352)-mean(0.035,0.037,0.366)))*100,
                                                   ((mean(0.051,0.052,0.052)-mean(0.036,0.037,0.469))/(mean(0.511,0.503,0.425)-mean(0.036,0.037,0.469)))*100,
                                                   ((mean(0.051,0.052,0.055)-mean(0.036,0.037,0.49))/(mean(0.523,0.494,0.479)-mean(0.036,0.037,0.49)))*100,
                                                   ((mean(0.06,0.063,0.059)-0.036)/(mean(0.062,0.061,0.061)-0.036))*100,
                                                   ((mean(0.051,0.054,0.051)-0.036)/(mean(0.35,0.362,0.349)-0.036))*100,
                                                   ((mean(0.049,0.051,0.053)-mean(0.037,0.036,0.037))/(mean(0.366,0.361,0.345)-mean(0.037,0.036,0.037)))*100,
                                                   ((mean(0.052,0.053,0.053)-mean(0.037,0.037,0.036))/(mean(0.5,0.521,0.42)-mean(0.037,0.037,0.036)))*100,
                                                   ((0.052- mean(0.037,0.037,0.036))/(mean(0.51,0.503,0.469)-mean(0.037,0.037,0.036)))*100))
print(Final_Proteus_data)
#Make up a line graph 
Final_Prot <- Final_Proteus_data %>%
  ggplot(aes(x=Time_f2, y=Optical_density,group=Concentrations, shape=Concentrations))+
  geom_line()+
  geom_point()+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Final_Proteus_data$Optical_density-sd(Final_Proteus_data$Optical_density),
                ymax=Final_Proteus_data$Optical_density+sd(Final_Proteus_data$Optical_density),
                width=.2)+
  stat_compare_means(method = "kruskal.test", label.y =127)+
  stat_compare_means(label = "p.signif", label.y = c(100,100,100,100,100), label.x = 10)+
  geom_hline(yintercept = 0, linetype="dashed", col="grey")
print(Final_Prot)
#Mycobacterium smegmatis RESULTS AND PLOTS
#Penicillin 100 ug/ml 
Mycobacterium_smegmatis_100ug_data <- data.frame(Time= c(rep("0",8),rep("24",8),rep("48",8),rep("72",8),rep("90",8)),
                                           Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                           Optical_density_mean_values =c(((mean(0.047,0.053,0.049)-mean(0.036,0.036,0.037))/(mean(0.05,0.051,0.053)-mean(0.036,0.036,0.037)))*100,
                                                                          ((mean(0.047,0.051,0.047)-mean(0.036,0.036,0.037))/(mean(0.05,0.051,0.053)-mean(0.036,0.036,0.037)))*100,
                                                                          ((mean(0.047,0.046,0.046)-mean(0.036,0.036,0.037))/(mean(0.05,0.051,0.053)-mean(0.036,0.036,0.037)))*100,
                                                                          ((mean(0.047,0.047,0.047)-mean(0.036,0.036,0.037))/(mean(0.05,0.051,0.053)-mean(0.036,0.036,0.037)))*100,
                                                                          ((mean(0.048,0.045,0.045)-mean(0.036,0.036,0.037))/(mean(0.05,0.051,0.053)-mean(0.036,0.036,0.037)))*100,
                                                                          ((mean(0.046,0.045,0.045)-mean(0.036,0.036,0.037))/(mean(0.05,0.051,0.053)-mean(0.036,0.036,0.037)))*100,
                                                                          ((mean(0.046,0.044,0.046)-mean(0.036,0.036,0.037))/(mean(0.05,0.051,0.053)-mean(0.036,0.036,0.037)))*100,
                                                                          ((mean(0.046,0.044,0.046)-mean(0.036,0.036,0.037))/(mean(0.05,0.051,0.053)-mean(0.036,0.036,0.037)))*100,
                                                                          ((mean(0.057,0.064,0.047)-mean(0.08,0.065,0.04))/(mean(0.135,0.135,0.162)-mean(0.08,0.065,0.04)))*100,
                                                                          ((mean(0.067,0.076,0.101)-mean(0.08,0.065,0.04))/(mean(0.135,0.135,0.162)-mean(0.08,0.065,0.04)))*100,
                                                                          ((mean(0.078,0.075,0.126)-mean(0.08,0.065,0.04))/(mean(0.135,0.135,0.162)-mean(0.08,0.065,0.04)))*100,
                                                                          ((mean(0.086,0.114,0.125)-mean(0.08,0.065,0.04))/(mean(0.135,0.135,0.162)-mean(0.08,0.065,0.04)))*100,
                                                                          ((mean(0.118,0.103,0.146)-mean(0.08,0.065,0.04))/(mean(0.135,0.135,0.162)-mean(0.08,0.065,0.04)))*100,
                                                                          ((mean(0.123,0.144,0.138)-mean(0.08,0.065,0.04))/(mean(0.135,0.135,0.162)-mean(0.08,0.065,0.04)))*100,
                                                                          ((mean(0.109,0.106,0.152)-mean(0.08,0.065,0.04))/(mean(0.135,0.135,0.162)-mean(0.08,0.065,0.04)))*100,
                                                                          ((mean(0.135,0.12,0.163)-mean(0.08,0.065,0.04))/(mean(0.135,0.135,0.162)-mean(0.08,0.065,0.04)))*100,
                                                                          ((mean(0.049,0.044,0.08)-mean(0.052,0.047,0.038))/(mean(0.237,0.25,0.27)-mean(0.052,0.047,0.038)))*100,
                                                                          ((mean(0.1,0.065,0.119)-mean(0.052,0.047,0.038))/(mean(0.237,0.25,0.27)-mean(0.052,0.047,0.038)))*100,
                                                                          ((mean(0.093,0.068,0.084)-mean(0.052,0.047,0.038))/(mean(0.237,0.25,0.27)-mean(0.052,0.047,0.038)))*100,
                                                                          ((mean(0.105,0.226,0.182)-mean(0.052,0.047,0.038))/(mean(0.237,0.25,0.27)-mean(0.052,0.047,0.038)))*100,
                                                                          ((mean(0.224,0.138,0.183)-mean(0.052,0.047,0.038))/(mean(0.237,0.25,0.27)-mean(0.052,0.047,0.038)))*100,
                                                                          ((mean(0.217,0.263,0.163)-mean(0.052,0.047,0.038))/(mean(0.237,0.25,0.27)-mean(0.052,0.047,0.038)))*100,
                                                                          ((mean(0.214,0.174,0.237)-mean(0.052,0.047,0.038))/(mean(0.237,0.25,0.27)-mean(0.052,0.047,0.038)))*100,
                                                                          ((mean(0.226,0.201,0.25)-mean(0.052,0.047,0.038))/(mean(0.237,0.25,0.27)-mean(0.052,0.047,0.038)))*100,
                                                                          ((mean(0.049,0.058,0.043)-mean(0.047,0.049,0.041))/(mean(0.363,0.42,0.358)-mean(0.047,0.049,0.041)))*100,
                                                                          ((mean(0.048,0.044,0.128)-mean(0.047,0.049,0.041))/(mean(0.363,0.42,0.358)-mean(0.047,0.049,0.041)))*100,
                                                                          ((mean(0.136,0.066,0.048)-mean(0.047,0.049,0.041))/(mean(0.363,0.42,0.358)-mean(0.047,0.049,0.041)))*100,
                                                                          ((mean(0.126,0.32,0.153)-mean(0.047,0.049,0.041))/(mean(0.363,0.42,0.358)-mean(0.047,0.049,0.041)))*100,
                                                                          ((mean(0.252,0.187,0.302)-mean(0.047,0.049,0.041))/(mean(0.363,0.42,0.358)-mean(0.047,0.049,0.041)))*100,
                                                                          ((mean(0.249,0.368,0.209)-mean(0.047,0.049,0.041))/(mean(0.363,0.42,0.358)-mean(0.047,0.049,0.041)))*100,
                                                                          ((mean(0.341,0.292,0.315)-mean(0.047,0.049,0.041))/(mean(0.363,0.42,0.358)-mean(0.047,0.049,0.041)))*100,
                                                                          ((mean(0.293,0.326,0.34)-mean(0.047,0.049,0.041))/(mean(0.363,0.42,0.358)-mean(0.047,0.049,0.041)))*100,
                                                                          ((mean(0.044,0.045,0.041)-mean(0.119,0.097,0.048))/(mean(0.444,0.576,0.536)-mean(0.119,0.097,0.048)))*100,
                                                                          ((mean(0.044,0.046,0.14)-mean(0.119,0.097,0.048))/(mean(0.444,0.576,0.536)-mean(0.119,0.097,0.048)))*100,
                                                                          ((mean(0.052,0.088,0.083)-mean(0.119,0.097,0.048))/(mean(0.444,0.576,0.536)-mean(0.119,0.097,0.048)))*100,
                                                                          ((mean(0.115,0.587,0.137)-mean(0.119,0.097,0.048))/(mean(0.444,0.576,0.536)-mean(0.119,0.097,0.048)))*100,
                                                                          ((mean(0.276,0.245,0.373)-mean(0.119,0.097,0.048))/(mean(0.444,0.576,0.536)-mean(0.119,0.097,0.048)))*100,
                                                                          ((mean(0.289,0.44,0.281)-mean(0.119,0.097,0.048))/(mean(0.444,0.576,0.536)-mean(0.119,0.097,0.048)))*100,
                                                                          ((mean(0.638,0.462,0.637)-mean(0.119,0.097,0.048))/(mean(0.444,0.576,0.536)-mean(0.119,0.097,0.048)))*100,
                                                                          ((mean(0.388,0.418,0.498)-mean(0.119,0.097,0.048))/(mean(0.444,0.576,0.536)-mean(0.119,0.097,0.048)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Mp1 <- Mycobacterium_smegmatis_100ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 100 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Mycobacterium_smegmatis_100ug_data $Optical_density_mean_values-sd(Mycobacterium_smegmatis_100ug_data$Optical_density_mean_values),
                ymax=Mycobacterium_smegmatis_100ug_data $Optical_density_mean_values+sd(Mycobacterium_smegmatis_100ug_data$Optical_density_mean_values), col="red")
print(Mp1)
#Penicillin 50 ug/mL 
Mycobacterium_smegmatis_50ug_data <- data.frame(Time= c(rep("0",8),rep("24",8),rep("48",8),rep("72",8),rep("90",8)),
                                                 Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                                 Optical_density_mean_values =c(((mean(0.049,0.05,0.049)-mean(0.036,0.035,0.035))/(mean(0.049,0.05,0.047)-mean(0.036,0.035,0.035)))*100,
                                                                                ((mean(0.048,0.048,0.064)-mean(0.036,0.035,0.035))/(mean(0.049,0.05,0.047)-mean(0.036,0.035,0.035)))*100,
                                                                                ((mean(0.048,0.047,0.051)-mean(0.036,0.035,0.035))/(mean(0.049,0.05,0.047)-mean(0.036,0.035,0.035)))*100,
                                                                                ((mean(0.048,0.047,0.047)-mean(0.036,0.035,0.035))/(mean(0.049,0.05,0.047)-mean(0.036,0.035,0.035)))*100,
                                                                                ((mean(0.048,0.046,0.047)-mean(0.036,0.035,0.035))/(mean(0.049,0.05,0.047)-mean(0.036,0.035,0.035)))*100,
                                                                                ((mean(0.048,0.046,0.047)-mean(0.036,0.035,0.035))/(mean(0.049,0.05,0.047)-mean(0.036,0.035,0.035)))*100,
                                                                                ((mean(0.048,0.047,0.047)-mean(0.036,0.035,0.035))/(mean(0.049,0.05,0.047)-mean(0.036,0.035,0.035)))*100,
                                                                                ((mean(0.047,0.047,0.047)-mean(0.036,0.035,0.035))/(mean(0.049,0.05,0.047)-mean(0.036,0.035,0.035)))*100,
                                                                                ((mean(0.067,0.074,0.056)-mean(0.09,0.075,0.041))/(mean(0.141,0.143,0.156)-mean(0.09,0.075,0.041)))*100,
                                                                                ((mean(0.082,0.082,0.059)-mean(0.09,0.075,0.041))/(mean(0.141,0.143,0.156)-mean(0.09,0.075,0.041)))*100,
                                                                                ((mean(0.086,0.085,0.108)-mean(0.09,0.075,0.041))/(mean(0.141,0.143,0.156)-mean(0.09,0.075,0.041)))*100,
                                                                                ((mean(0.092,0.101,0.146)-mean(0.09,0.075,0.041))/(mean(0.141,0.143,0.156)-mean(0.09,0.075,0.041)))*100,
                                                                                ((mean(0.114,0.106,0.153)-mean(0.09,0.075,0.041))/(mean(0.141,0.143,0.156)-mean(0.09,0.075,0.041)))*100,
                                                                                ((mean(0.125,0.106,0.153)-mean(0.09,0.075,0.041))/(mean(0.141,0.143,0.156)-mean(0.09,0.075,0.041)))*100,
                                                                                ((mean(0.134,0.114,0.162)-mean(0.09,0.075,0.041))/(mean(0.141,0.143,0.156)-mean(0.09,0.075,0.041)))*100,
                                                                                ((mean(0.129,0.142,0.175)-mean(0.09,0.075,0.041))/(mean(0.141,0.143,0.156)-mean(0.09,0.075,0.041)))*100,
                                                                                ((mean(0.053,0.049,0.072)-mean(0.049,0.046,0.038))/(mean(0.246,0.235,0.246)-mean(0.049,0.046,0.038)))*100,
                                                                                ((mean(0.1,0.063,0.088)-mean(0.049,0.046,0.038))/(mean(0.246,0.235,0.246)-mean(0.049,0.046,0.038)))*100,
                                                                                ((mean(0.085,0.09,0.093)-mean(0.049,0.046,0.038))/(mean(0.246,0.235,0.246)-mean(0.049,0.046,0.038)))*100,
                                                                                ((mean(0.121,0.151,0.175)-mean(0.049,0.046,0.038))/(mean(0.246,0.235,0.246)-mean(0.049,0.046,0.038)))*100,
                                                                                ((mean(0.157,0.159,0.201)-mean(0.049,0.046,0.038))/(mean(0.246,0.235,0.246)-mean(0.049,0.046,0.038)))*100,
                                                                                ((mean(0.196,0.169,0.164)-mean(0.049,0.046,0.038))/(mean(0.246,0.235,0.246)-mean(0.049,0.046,0.038)))*100,
                                                                                ((mean(0.246,0.201,0.219)-mean(0.049,0.046,0.038))/(mean(0.246,0.235,0.246)-mean(0.049,0.046,0.038)))*100,
                                                                                ((mean(0.22,0.253,0.273)-mean(0.049,0.046,0.038))/(mean(0.246,0.235,0.246)-mean(0.049,0.046,0.038)))*100,
                                                                                ((mean(0.057,0.051,0.053)-mean(0.049,0.05,0.04))/(mean(0.377,0.364,0.494)-mean(0.049,0.05,0.04)))*100,
                                                                                ((mean(0.065,0.048,0.046)-mean(0.049,0.05,0.04))/(mean(0.377,0.364,0.494)-mean(0.049,0.05,0.04)))*100,
                                                                                ((mean(0.178,0.074,0.054)-mean(0.049,0.05,0.04))/(mean(0.377,0.364,0.494)-mean(0.049,0.05,0.04)))*100,
                                                                                ((mean(0.129,0.217,0.193)-mean(0.049,0.05,0.04))/(mean(0.377,0.364,0.494)-mean(0.049,0.05,0.04)))*100,
                                                                                ((mean(0.22,0.209,0.278)-mean(0.049,0.05,0.04))/(mean(0.377,0.364,0.494)-mean(0.049,0.05,0.04)))*100,
                                                                                ((mean(0.247,0.291,0.229)-mean(0.049,0.05,0.04))/(mean(0.377,0.364,0.494)-mean(0.049,0.05,0.04)))*100,
                                                                                ((mean(0.333,0.299,0.303)-mean(0.049,0.05,0.04))/(mean(0.377,0.364,0.494)-mean(0.049,0.05,0.04)))*100,
                                                                                ((mean(0.294,0.354,0.323)-mean(0.049,0.05,0.04))/(mean(0.377,0.364,0.494)-mean(0.049,0.05,0.04)))*100,
                                                                                ((mean(0.062,0.061,0.046)-mean(0.135,0.118,0.057))/(mean(0.42,0.432,0.609)-mean(0.135,0.118,0.057)))*100,
                                                                                ((mean(0.05,0.066,0.094)-mean(0.135,0.118,0.057))/(mean(0.42,0.432,0.609)-mean(0.135,0.118,0.057)))*100,
                                                                                ((mean(0.043,0.043,0.071)-mean(0.135,0.118,0.057))/(mean(0.42,0.432,0.609)-mean(0.135,0.118,0.057)))*100,
                                                                                ((mean(0.117,0.207,0.22)-mean(0.135,0.118,0.057))/(mean(0.42,0.432,0.609)-mean(0.135,0.118,0.057)))*100,
                                                                                ((mean(0.311,0.272,0.245)-mean(0.135,0.118,0.057))/(mean(0.42,0.432,0.609)-mean(0.135,0.118,0.057)))*100,
                                                                                ((mean(0.497,0.427,0.301)-mean(0.135,0.118,0.057))/(mean(0.42,0.432,0.609)-mean(0.135,0.118,0.057)))*100,
                                                                                ((mean(0.7,0.498,0.581)-mean(0.135,0.118,0.057))/(mean(0.42,0.432,0.609)-mean(0.135,0.118,0.057)))*100,
                                                                                ((mean(0.506,0.521,0.543)-mean(0.135,0.118,0.057))/(mean(0.42,0.432,0.609)-mean(0.135,0.118,0.057)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Mp2 <- Mycobacterium_smegmatis_50ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 50 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Mycobacterium_smegmatis_50ug_data$Optical_density_mean_values-sd(Mycobacterium_smegmatis_50ug_data$Optical_density_mean_values),
                ymax=Mycobacterium_smegmatis_50ug_data$Optical_density_mean_values+sd(Mycobacterium_smegmatis_50ug_data$Optical_density_mean_values), col="red")
print(Mp2)
#Penicillin 25 ug/mL 
Mycobacterium_smegmatis_25ug_data <- data.frame(Time= c(rep("0",8),rep("24",8),rep("48",8),rep("72",8),rep("90",8)),
                                                Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                                Optical_density_mean_values =c(((mean(0.044,0.044,0.044)-mean(0.034,0.034,0.034))/(mean(0.046,0.047,0.044)-mean(0.034,0.034,0.034)))*100,
                                                                               ((mean(0.044,0.046,0.045)-mean(0.034,0.034,0.034))/(mean(0.046,0.047,0.044)-mean(0.034,0.034,0.034)))*100,
                                                                               ((mean(0.044,0.044,0.043)-mean(0.034,0.034,0.034))/(mean(0.046,0.047,0.044)-mean(0.034,0.034,0.034)))*100,
                                                                               ((mean(0.048,0.042,0.043)-mean(0.034,0.034,0.034))/(mean(0.046,0.047,0.044)-mean(0.034,0.034,0.034)))*100,
                                                                               ((mean(0.044,0.043,0.044)-mean(0.034,0.034,0.034))/(mean(0.046,0.047,0.044)-mean(0.034,0.034,0.034)))*100,
                                                                               ((mean(0.044,0.043,0.042)-mean(0.034,0.034,0.034))/(mean(0.046,0.047,0.044)-mean(0.034,0.034,0.034)))*100,
                                                                               ((mean(0.045,0.043,0.044)-mean(0.034,0.034,0.034))/(mean(0.046,0.047,0.044)-mean(0.034,0.034,0.034)))*100,
                                                                               ((mean(0.045,0.044,0.043)-mean(0.034,0.034,0.034))/(mean(0.046,0.047,0.044)-mean(0.034,0.034,0.034)))*100,
                                                                               ((mean(0.071,0.068,0.048)-mean(0.086,0.072,0.045))/(mean(0.136,0.14,0.156)-mean(0.086,0.072,0.045)))*100,
                                                                               ((mean(0.079,0.085,0.062)-mean(0.086,0.072,0.045))/(mean(0.136,0.14,0.156)-mean(0.086,0.072,0.045)))*100,
                                                                               ((mean(0.076,0.079,0.104)-mean(0.086,0.072,0.045))/(mean(0.136,0.14,0.156)-mean(0.086,0.072,0.045)))*100,
                                                                               ((mean(0.084,0.089,0.111)-mean(0.086,0.072,0.045))/(mean(0.136,0.14,0.156)-mean(0.086,0.072,0.045)))*100,
                                                                               ((mean(0.107,0.101,0.144)-mean(0.086,0.072,0.045))/(mean(0.136,0.14,0.156)-mean(0.086,0.072,0.045)))*100,
                                                                               ((mean(0.11,0.102,0.133)-mean(0.086,0.072,0.045))/(mean(0.136,0.14,0.156)-mean(0.086,0.072,0.045)))*100,
                                                                               ((mean(0.112,0.105,0.172)-mean(0.086,0.072,0.045))/(mean(0.136,0.14,0.156)-mean(0.086,0.072,0.045)))*100,
                                                                               ((mean(0.127,0.127,0.163)-mean(0.086,0.072,0.045))/(mean(0.136,0.14,0.156)-mean(0.086,0.072,0.045)))*100,
                                                                               ((mean(0.043,0.042,0.075)-mean(0.056,0.05,0.037))/(mean(0.254,0.229,0.227)-mean(0.056,0.05,0.037)))*100,
                                                                               ((mean(0.086,0.053,0.106)-mean(0.056,0.05,0.037))/(mean(0.254,0.229,0.227)-mean(0.056,0.05,0.037)))*100,
                                                                               ((mean(0.101,0.091,0.106)-mean(0.056,0.05,0.037))/(mean(0.254,0.229,0.227)-mean(0.056,0.05,0.037)))*100,
                                                                               ((mean(0.103,0.147,0.127)-mean(0.056,0.05,0.037))/(mean(0.254,0.229,0.227)-mean(0.056,0.05,0.037)))*100,
                                                                               ((mean(0.127,0.144,0.235)-mean(0.056,0.05,0.037))/(mean(0.254,0.229,0.227)-mean(0.056,0.05,0.037)))*100,
                                                                               ((mean(0.225,0.187,0.144)-mean(0.056,0.05,0.037))/(mean(0.254,0.229,0.227)-mean(0.056,0.05,0.037)))*100,
                                                                               ((mean(0.236,0.181,0.209)-mean(0.056,0.05,0.037))/(mean(0.254,0.229,0.227)-mean(0.056,0.05,0.037)))*100,
                                                                               ((mean(0.213,0.228,0.241)-mean(0.056,0.05,0.037))/(mean(0.254,0.229,0.227)-mean(0.056,0.05,0.037)))*100,
                                                                               ((mean(0.051,0.052,0.049)-mean(0.055,0.054,0.042))/(mean(0.313,0.338,0.352)-mean(0.055,0.054,0.042)))*100,
                                                                               ((mean(0.053,0.04,0.044)-mean(0.055,0.054,0.042))/(mean(0.313,0.338,0.352)-mean(0.055,0.054,0.042)))*100,
                                                                               ((mean(0.164,0.041,0.054)-mean(0.055,0.054,0.042))/(mean(0.313,0.338,0.352)-mean(0.055,0.054,0.042)))*100,
                                                                               ((mean(0.112,0.251,0.14)-mean(0.055,0.054,0.042))/(mean(0.313,0.338,0.352)-mean(0.055,0.054,0.042)))*100,
                                                                               ((mean(0.239,0.187,0.311)-mean(0.055,0.054,0.042))/(mean(0.313,0.338,0.352)-mean(0.055,0.054,0.042)))*100,
                                                                               ((mean(0.264,0.284,0.188)-mean(0.055,0.054,0.042))/(mean(0.313,0.338,0.352)-mean(0.055,0.054,0.042)))*100,
                                                                               ((mean(0.271,0.297,0.256)-mean(0.055,0.054,0.042))/(mean(0.313,0.338,0.352)-mean(0.055,0.054,0.042)))*100,
                                                                               ((mean(0.299,0.302,0.302)-mean(0.055,0.054,0.042))/(mean(0.313,0.338,0.352)-mean(0.055,0.054,0.042)))*100,
                                                                               ((mean(0.041,0.046,0.05)-mean(0.121,0.105,0.058))/(mean(0.445,0.424,0.581)-mean(0.121,0.105,0.058)))*100,
                                                                               ((mean(0.041,0.079,0.059)-mean(0.121,0.105,0.058))/(mean(0.445,0.424,0.581)-mean(0.121,0.105,0.058)))*100,
                                                                               ((mean(0.04,0.04,0.057)-mean(0.121,0.105,0.058))/(mean(0.445,0.424,0.581)-mean(0.121,0.105,0.058)))*100,
                                                                               ((mean(0.147,0.243,0.165)-mean(0.121,0.105,0.058))/(mean(0.445,0.424,0.581)-mean(0.121,0.105,0.058)))*100,
                                                                               ((mean(0.289,0.294,0.677)-mean(0.121,0.105,0.058))/(mean(0.445,0.424,0.581)-mean(0.121,0.105,0.058)))*100,
                                                                               ((mean(0.506,0.455,0.358)-mean(0.121,0.105,0.058))/(mean(0.445,0.424,0.581)-mean(0.121,0.105,0.058)))*100,
                                                                               ((mean(0.71,0.486,0.542)-mean(0.121,0.105,0.058))/(mean(0.445,0.424,0.581)-mean(0.121,0.105,0.058)))*100,
                                                                               ((mean(0.471,0.468,0.603)-mean(0.121,0.105,0.058))/(mean(0.445,0.424,0.581)-mean(0.121,0.105,0.058)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Mp3 <- Mycobacterium_smegmatis_25ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 25 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Mycobacterium_smegmatis_25ug_data$Optical_density_mean_values-sd(Mycobacterium_smegmatis_25ug_data$Optical_density_mean_values),
                ymax=Mycobacterium_smegmatis_25ug_data$Optical_density_mean_values+sd(Mycobacterium_smegmatis_25ug_data$Optical_density_mean_values), col="red")
print(Mp3)
#Penicillin 12.5 ug/mL 
Mycobacterium_smegmatis_12.5ug_data <- data.frame(Time= c(rep("0",8),rep("24",8),rep("48",8),rep("72",8),rep("90",8)),
                                                Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                                Optical_density_mean_values =c(((mean(0.049,0.05,0.047)-mean(0.036,0.036,0.036))/(mean(0.049,0.049,0.048)-mean(0.036,0.036,0.036)))*100,
                                                                               ((mean(0.048,0.048,0.048)-mean(0.036,0.036,0.036))/(mean(0.049,0.049,0.048)-mean(0.036,0.036,0.036)))*100,
                                                                               ((mean(0.048,0.047,0.048)-mean(0.036,0.036,0.036))/(mean(0.049,0.049,0.048)-mean(0.036,0.036,0.036)))*100,
                                                                               ((mean(0.05,0.046,0.049)-mean(0.036,0.036,0.036))/(mean(0.049,0.049,0.048)-mean(0.036,0.036,0.036)))*100,
                                                                               ((mean(0.048,0.046,0.048)-mean(0.036,0.036,0.036))/(mean(0.049,0.049,0.048)-mean(0.036,0.036,0.036)))*100,
                                                                               ((mean(0.048,0.048,0.047)-mean(0.036,0.036,0.036))/(mean(0.049,0.049,0.048)-mean(0.036,0.036,0.036)))*100,
                                                                               ((mean(0.048,0.046,0.047)-mean(0.036,0.036,0.036))/(mean(0.049,0.049,0.048)-mean(0.036,0.036,0.036)))*100,
                                                                               ((mean(0.048,0.047,0.047)-mean(0.036,0.036,0.036))/(mean(0.049,0.049,0.048)-mean(0.036,0.036,0.036)))*100,
                                                                               ((mean(0.071,0.072,0.053)-mean(0.088,0.081,0.045))/(mean(0.141,0.141,0.188)-mean(0.088,0.081,0.045)))*100,
                                                                               ((mean(0.085,0.081,0.063)-mean(0.088,0.081,0.045))/(mean(0.141,0.141,0.188)-mean(0.088,0.081,0.045)))*100,
                                                                               ((mean(0.083,0.08,0.122)-mean(0.088,0.081,0.045))/(mean(0.141,0.141,0.188)-mean(0.088,0.081,0.045)))*100,
                                                                               ((mean(0.093,0.088,0.125)-mean(0.088,0.081,0.045))/(mean(0.141,0.141,0.188)-mean(0.088,0.081,0.045)))*100,
                                                                               ((mean(0.112,0.105,0.153)-mean(0.088,0.081,0.045))/(mean(0.141,0.141,0.188)-mean(0.088,0.081,0.045)))*100,
                                                                               ((mean(0.119,0.114,0.135)-mean(0.088,0.081,0.045))/(mean(0.141,0.141,0.188)-mean(0.088,0.081,0.045)))*100,
                                                                               ((mean(0.125,0.119,0.16)-mean(0.088,0.081,0.045))/(mean(0.141,0.141,0.188)-mean(0.088,0.081,0.045)))*100,
                                                                               ((mean(0.133,0.129,0.165)-mean(0.088,0.081,0.045))/(mean(0.141,0.141,0.188)-mean(0.088,0.081,0.045)))*100,
                                                                               ((mean(0.076,0.055,0.082)-mean(0.054,0.063,0.038))/(mean(0.228,0.228,0.27)-mean(0.054,0.063,0.038)))*100,
                                                                               ((mean(0.083,0.056,0.119)-mean(0.054,0.063,0.038))/(mean(0.228,0.228,0.27)-mean(0.054,0.063,0.038)))*100,
                                                                               ((mean(0.108,0.107,0.119)-mean(0.054,0.063,0.038))/(mean(0.228,0.228,0.27)-mean(0.054,0.063,0.038)))*100,
                                                                               ((mean(0.108,0.13,0.147)-mean(0.054,0.063,0.038))/(mean(0.228,0.228,0.27)-mean(0.054,0.063,0.038)))*100,
                                                                               ((mean(0.171,0.178,0.148)-mean(0.054,0.063,0.038))/(mean(0.228,0.228,0.27)-mean(0.054,0.063,0.038)))*100,
                                                                               ((mean(0.198,0.221,0.157)-mean(0.054,0.063,0.038))/(mean(0.228,0.228,0.27)-mean(0.054,0.063,0.038)))*100,
                                                                               ((mean(0.22,0.198,0.213)-mean(0.054,0.063,0.038))/(mean(0.228,0.228,0.27)-mean(0.054,0.063,0.038)))*100,
                                                                               ((mean(0.226,0.231,0.247)-mean(0.054,0.063,0.038))/(mean(0.228,0.228,0.27)-mean(0.054,0.063,0.038)))*100,
                                                                               ((mean(0.083,0.05,0.048)-mean(0.053,0.063,0.04))/(mean(0.332,0.311,0.375)-mean(0.053,0.063,0.04)))*100,
                                                                               ((mean(0.049,0.044,0.052)-mean(0.053,0.063,0.04))/(mean(0.332,0.311,0.375)-mean(0.053,0.063,0.04)))*100,
                                                                               ((mean(0.147,0.048,0.065)-mean(0.053,0.063,0.04))/(mean(0.332,0.311,0.375)-mean(0.053,0.063,0.04)))*100,
                                                                               ((mean(0.114,0.172,0.147)-mean(0.053,0.063,0.04))/(mean(0.332,0.311,0.375)-mean(0.053,0.063,0.04)))*100,
                                                                               ((mean(0.247,0.293,0.258)-mean(0.053,0.063,0.04))/(mean(0.332,0.311,0.375)-mean(0.053,0.063,0.04)))*100,
                                                                               ((mean(0.238,0.34,0.222)-mean(0.053,0.063,0.04))/(mean(0.332,0.311,0.375)-mean(0.053,0.063,0.04)))*100,
                                                                               ((mean(0.264,0.316,0.266)-mean(0.053,0.063,0.04))/(mean(0.332,0.311,0.375)-mean(0.053,0.063,0.04)))*100,
                                                                               ((mean(0.303,0.3,0.397)-mean(0.053,0.063,0.04))/(mean(0.332,0.311,0.375)-mean(0.053,0.063,0.04)))*100,
                                                                               ((mean(0.046,0.061,0.048)-mean(0.126,0.117,0.057))/(mean(0.448,0.376,0.466)-mean(0.126,0.117,0.057)))*100,
                                                                               ((mean(0.045,0.05,0.057)-mean(0.126,0.117,0.057))/(mean(0.448,0.376,0.466)-mean(0.126,0.117,0.057)))*100,
                                                                               ((mean(0.049,0.08,0.048)-mean(0.126,0.117,0.057))/(mean(0.448,0.376,0.466)-mean(0.126,0.117,0.057)))*100,
                                                                               ((mean(0.202,0.19,0.155)-mean(0.126,0.117,0.057))/(mean(0.448,0.376,0.466)-mean(0.126,0.117,0.057)))*100,
                                                                               ((mean(0.354,0.304,0.433)-mean(0.126,0.117,0.057))/(mean(0.448,0.376,0.466)-mean(0.126,0.117,0.057)))*100,
                                                                               ((mean(0.587,0.504,0.291)-mean(0.126,0.117,0.057))/(mean(0.448,0.376,0.466)-mean(0.126,0.117,0.057)))*100,
                                                                               ((mean(0.802,0.45,0.489)-mean(0.126,0.117,0.057))/(mean(0.448,0.376,0.466)-mean(0.126,0.117,0.057)))*100,
                                                                               ((mean(0.381,0.484,0.508)-mean(0.126,0.117,0.057))/(mean(0.448,0.376,0.466)-mean(0.126,0.117,0.057)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Mp4 <- Mycobacterium_smegmatis_12.5ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 12.5 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Mycobacterium_smegmatis_12.5ug_data$Optical_density_mean_values-sd(Mycobacterium_smegmatis_12.5ug_data$Optical_density_mean_values),
                ymax=Mycobacterium_smegmatis_12.5ug_data$Optical_density_mean_values+sd(Mycobacterium_smegmatis_12.5ug_data$Optical_density_mean_values), col="red")
print(Mp4)
#Penicillin 6.25 ug/mL 
Mycobacterium_smegmatis_6.25ug_data <- data.frame(Time= c(rep("0",8),rep("24",8),rep("48",8),rep("72",8),rep("90",8)),
                                                  Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                                  Optical_density_mean_values =c(((mean(0.049,0.048,0.047)-mean(0.035,0.035,0.035))/(mean(0.047,0.048,0.047)-mean(0.035,0.035,0.035)))*100,
                                                                                 ((mean(0.046,0.047,0.046)-mean(0.035,0.035,0.035))/(mean(0.047,0.048,0.047)-mean(0.035,0.035,0.035)))*100,
                                                                                 ((mean(0.047,0.045,0.045)-mean(0.035,0.035,0.035))/(mean(0.047,0.048,0.047)-mean(0.035,0.035,0.035)))*100,
                                                                                 ((mean(0.046,0.046,0.045)-mean(0.035,0.035,0.035))/(mean(0.047,0.048,0.047)-mean(0.035,0.035,0.035)))*100,
                                                                                 ((mean(0.047,0.046,0.045)-mean(0.035,0.035,0.035))/(mean(0.047,0.048,0.047)-mean(0.035,0.035,0.035)))*100,
                                                                                 ((mean(0.047,0.045,0.045)-mean(0.035,0.035,0.035))/(mean(0.047,0.048,0.047)-mean(0.035,0.035,0.035)))*100,
                                                                                 ((mean(0.047,0.045,0.046)-mean(0.035,0.035,0.035))/(mean(0.047,0.048,0.047)-mean(0.035,0.035,0.035)))*100,
                                                                                 ((mean(0.047,0.046,0.048)-mean(0.035,0.035,0.035))/(mean(0.047,0.048,0.047)-mean(0.035,0.035,0.035)))*100,
                                                                                 ((mean(0.073,0.071,0.046)-mean(0.093,0.078,0.047))/(mean(0.149,0.15,0.181)-mean(0.093,0.078,0.047)))*100,
                                                                                 ((mean(0.083,0.078,0.067)-mean(0.093,0.078,0.047))/(mean(0.149,0.15,0.181)-mean(0.093,0.078,0.047)))*100,
                                                                                 ((mean(0.081,0.081,0.114)-mean(0.093,0.078,0.047))/(mean(0.149,0.15,0.181)-mean(0.093,0.078,0.047)))*100,
                                                                                 ((mean(0.088,0.091,0.138)-mean(0.093,0.078,0.047))/(mean(0.149,0.15,0.181)-mean(0.093,0.078,0.047)))*100,
                                                                                 ((mean(0.111,0.114,0.149)-mean(0.093,0.078,0.047))/(mean(0.149,0.15,0.181)-mean(0.093,0.078,0.047)))*100,
                                                                                 ((mean(0.116,0.118,0.138)-mean(0.093,0.078,0.047))/(mean(0.149,0.15,0.181)-mean(0.093,0.078,0.047)))*100,
                                                                                 ((mean(0.121,0.121,0.147)-mean(0.093,0.078,0.047))/(mean(0.149,0.15,0.181)-mean(0.093,0.078,0.047)))*100,
                                                                                 ((mean(0.137,0.143,0.204)-mean(0.093,0.078,0.047))/(mean(0.149,0.15,0.181)-mean(0.093,0.078,0.047)))*100,
                                                                                 ((mean(0.05,0.048,0.077)-mean(0.061,0.055,0.036))/(mean(0.268,0.25,0.254)-mean(0.061,0.055,0.036)))*100,
                                                                                 ((mean(0.084,0.085,0.1)-mean(0.061,0.055,0.036))/(mean(0.268,0.25,0.254)-mean(0.061,0.055,0.036)))*100,
                                                                                 ((mean(0.108,0.104,0.143)-mean(0.061,0.055,0.036))/(mean(0.268,0.25,0.254)-mean(0.061,0.055,0.036)))*100,
                                                                                 ((mean(0.108,0.134,0.153)-mean(0.061,0.055,0.036))/(mean(0.268,0.25,0.254)-mean(0.061,0.055,0.036)))*100,
                                                                                 ((mean(0.136,0.179,0.16)-mean(0.061,0.055,0.036))/(mean(0.268,0.25,0.254)-mean(0.061,0.055,0.036)))*100,
                                                                                 ((mean(0.224,0.199,0.149)-mean(0.061,0.055,0.036))/(mean(0.268,0.25,0.254)-mean(0.061,0.055,0.036)))*100,
                                                                                 ((mean(0.216,0.221,0.201)-mean(0.061,0.055,0.036))/(mean(0.268,0.25,0.254)-mean(0.061,0.055,0.036)))*100,
                                                                                 ((mean(0.237,0.256,0.275)-mean(0.061,0.055,0.036))/(mean(0.268,0.25,0.254)-mean(0.061,0.055,0.036)))*100,
                                                                                 ((mean(0.051,0.047,0.05)-mean(0.058,0.057,0.04))/(mean(0.362,0.338,0.315)-mean(0.058,0.057,0.04)))*100,
                                                                                 ((mean(0.057,0.039,0.046)-mean(0.058,0.057,0.04))/(mean(0.362,0.338,0.315)-mean(0.058,0.057,0.04)))*100,
                                                                                 ((mean(0.113,0.051,0.085)-mean(0.058,0.057,0.04))/(mean(0.362,0.338,0.315)-mean(0.058,0.057,0.04)))*100,
                                                                                 ((mean(0.119,0.215,0.144)-mean(0.058,0.057,0.04))/(mean(0.362,0.338,0.315)-mean(0.058,0.057,0.04)))*100,
                                                                                 ((mean(0.241,0.279,0.294)-mean(0.058,0.057,0.04))/(mean(0.362,0.338,0.315)-mean(0.058,0.057,0.04)))*100,
                                                                                 ((mean(0.255,0.306,0.259)-mean(0.058,0.057,0.04))/(mean(0.362,0.338,0.315)-mean(0.058,0.057,0.04)))*100,
                                                                                 ((mean(0.334,0.355,0.275)-mean(0.058,0.057,0.04))/(mean(0.362,0.338,0.315)-mean(0.058,0.057,0.04)))*100,
                                                                                 ((mean(0.341,0.304,0.342)-mean(0.058,0.057,0.04))/(mean(0.362,0.338,0.315)-mean(0.058,0.057,0.04)))*100,
                                                                                 ((mean(0.045,0.044,0.054)-mean(0.13,0.116,0.061))/(mean(0.46,0.583,0.559)-mean(0.13,0.116,0.061)))*100,
                                                                                 ((mean(0.043,0.044,0.05)-mean(0.13,0.116,0.061))/(mean(0.46,0.583,0.559)-mean(0.13,0.116,0.061)))*100,
                                                                                 ((mean(0.06,0.132,0.044)-mean(0.13,0.116,0.061))/(mean(0.46,0.583,0.559)-mean(0.13,0.116,0.061)))*100,
                                                                                 ((mean(0.225,0.135,0.198)-mean(0.13,0.116,0.061))/(mean(0.46,0.583,0.559)-mean(0.13,0.116,0.061)))*100,
                                                                                 ((mean(0.414,0.364,0.278)-mean(0.13,0.116,0.061))/(mean(0.46,0.583,0.559)-mean(0.13,0.116,0.061)))*100,
                                                                                 ((mean(0.485,0.542,0.309)-mean(0.13,0.116,0.061))/(mean(0.46,0.583,0.559)-mean(0.13,0.116,0.061)))*100,
                                                                                 ((mean(0.539,0.499,0.383)-mean(0.13,0.116,0.061))/(mean(0.46,0.583,0.559)-mean(0.13,0.116,0.061)))*100,
                                                                                 ((mean(0.385,0.458,0.497)-mean(0.13,0.116,0.061))/(mean(0.46,0.583,0.559)-mean(0.13,0.116,0.061)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Mp5 <- Mycobacterium_smegmatis_6.25ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 6.25 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Mycobacterium_smegmatis_6.25ug_data$Optical_density_mean_values-sd(Mycobacterium_smegmatis_6.25ug_data$Optical_density_mean_values),
                ymax=Mycobacterium_smegmatis_6.25ug_data$Optical_density_mean_values+sd(Mycobacterium_smegmatis_6.25ug_data$Optical_density_mean_values), col="red")
print(Mp5)
#Penicillin 3.125 ug/mL 
Mycobacterium_smegmatis_3.125ug_data <- data.frame(Time= c(rep("0",8),rep("24",8),rep("48",8),rep("72",8),rep("90",8)),
                                                  Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                                  Optical_density_mean_values =c(((mean(0.047,0.049,0.047)-mean(0.036,0.035,0.035))/(mean(0.047,0.052,0.046)-mean(0.036,0.035,0.035)))*100,
                                                                                 ((mean(0.049,0.046,0.046)-mean(0.036,0.035,0.035))/(mean(0.047,0.052,0.046)-mean(0.036,0.035,0.035)))*100,
                                                                                 ((mean(0.047,0.045,0.046)-mean(0.036,0.035,0.035))/(mean(0.047,0.052,0.046)-mean(0.036,0.035,0.035)))*100,
                                                                                 ((mean(0.046,0.045,0.046)-mean(0.036,0.035,0.035))/(mean(0.047,0.052,0.046)-mean(0.036,0.035,0.035)))*100,
                                                                                 ((mean(0.047,0.047,0.045)-mean(0.036,0.035,0.035))/(mean(0.047,0.052,0.046)-mean(0.036,0.035,0.035)))*100,
                                                                                 ((mean(0.047,0.046,0.045)-mean(0.036,0.035,0.035))/(mean(0.047,0.052,0.046)-mean(0.036,0.035,0.035)))*100,
                                                                                 ((mean(0.046,0.045,0.045)-mean(0.036,0.035,0.035))/(mean(0.047,0.052,0.046)-mean(0.036,0.035,0.035)))*100,
                                                                                 ((mean(0.047,0.046,0.046)-mean(0.036,0.035,0.035))/(mean(0.047,0.052,0.046)-mean(0.036,0.035,0.035)))*100,
                                                                                 ((mean(0.078,0.088,0.045)-mean(0.097,0.08,0.046))/(mean(0.153,0.171,0.189)-mean(0.097,0.08,0.046)))*100,
                                                                                 ((mean(0.089,0.111,0.06)-mean(0.097,0.08,0.046))/(mean(0.153,0.171,0.189)-mean(0.097,0.08,0.046)))*100,
                                                                                 ((mean(0.087,0.085,0.099)-mean(0.097,0.08,0.046))/(mean(0.153,0.171,0.189)-mean(0.097,0.08,0.046)))*100,
                                                                                 ((mean(0.094,0.094,0.129)-mean(0.097,0.08,0.046))/(mean(0.153,0.171,0.189)-mean(0.097,0.08,0.046)))*100,
                                                                                 ((mean(0.116,0.108,0.156)-mean(0.097,0.08,0.046))/(mean(0.153,0.171,0.189)-mean(0.097,0.08,0.046)))*100,
                                                                                 ((mean(0.123,0.116,0.152)-mean(0.097,0.08,0.046))/(mean(0.153,0.171,0.189)-mean(0.097,0.08,0.046)))*100,
                                                                                 ((mean(0.131,0.12,0.167)-mean(0.097,0.08,0.046))/(mean(0.153,0.171,0.189)-mean(0.097,0.08,0.046)))*100,
                                                                                 ((mean(0.149,0.153,0.187)-mean(0.097,0.08,0.046))/(mean(0.153,0.171,0.189)-mean(0.097,0.08,0.046)))*100,
                                                                                 ((mean(0.046,0.05,0.054)-mean(0.057,0.05,0.039))/(mean(0.28,0.269,0.268)-mean(0.057,0.05,0.039)))*100,
                                                                                 ((mean(0.077,0.052,0.123)-mean(0.057,0.05,0.039))/(mean(0.28,0.269,0.268)-mean(0.057,0.05,0.039)))*100,
                                                                                 ((mean(0.141,0.077,0.145)-mean(0.057,0.05,0.039))/(mean(0.28,0.269,0.268)-mean(0.057,0.05,0.039)))*100,
                                                                                 ((mean(0.114,0.172,0.142)-mean(0.057,0.05,0.039))/(mean(0.28,0.269,0.268)-mean(0.057,0.05,0.039)))*100,
                                                                                 ((mean(0.161,0.186,0.17)-mean(0.057,0.05,0.039))/(mean(0.28,0.269,0.268)-mean(0.057,0.05,0.039)))*100,
                                                                                 ((mean(0.235,0.203,0.168)-mean(0.057,0.05,0.039))/(mean(0.28,0.269,0.268)-mean(0.057,0.05,0.039)))*100,
                                                                                 ((mean(0.214,0.205,0.224)-mean(0.057,0.05,0.039))/(mean(0.28,0.269,0.268)-mean(0.057,0.05,0.039)))*100,
                                                                                 ((mean(0.237,0.266,0.276)-mean(0.057,0.05,0.039))/(mean(0.28,0.269,0.268)-mean(0.057,0.05,0.039)))*100,
                                                                                 ((mean(0.046,0.047,0.047)-mean(0.052,0.048,0.039))/(mean(0.341,0.348,0.436)-mean(0.052,0.048,0.039)))*100,
                                                                                 ((mean(0.046,0.042,0.057)-mean(0.052,0.048,0.039))/(mean(0.341,0.348,0.436)-mean(0.052,0.048,0.039)))*100,
                                                                                 ((mean(0.071,0.041,0.058)-mean(0.052,0.048,0.039))/(mean(0.341,0.348,0.436)-mean(0.052,0.048,0.039)))*100,
                                                                                 ((mean(0.126,0.198,0.149)-mean(0.052,0.048,0.039))/(mean(0.341,0.348,0.436)-mean(0.052,0.048,0.039)))*100,
                                                                                 ((mean(0.237,0.252,0.27)-mean(0.052,0.048,0.039))/(mean(0.341,0.348,0.436)-mean(0.052,0.048,0.039)))*100,
                                                                                 ((mean(0.253,0.369,0.216)-mean(0.052,0.048,0.039))/(mean(0.341,0.348,0.436)-mean(0.052,0.048,0.039)))*100,
                                                                                 ((mean(0.282,0.278,0.293)-mean(0.052,0.048,0.039))/(mean(0.341,0.348,0.436)-mean(0.052,0.048,0.039)))*100,
                                                                                 ((mean(0.346,0.322,0.316)-mean(0.052,0.048,0.039))/(mean(0.341,0.348,0.436)-mean(0.052,0.048,0.039)))*100,
                                                                                 ((mean(0.045,0.044,0.05)-mean(0.137,0.118,0.061))/(mean(0.432,0.54,0.618)-mean(0.137,0.118,0.061)))*100,
                                                                                 ((mean(0.042,0.044,0.044)-mean(0.137,0.118,0.061))/(mean(0.432,0.54,0.618)-mean(0.137,0.118,0.061)))*100,
                                                                                 ((mean(0.068,0.05,0.043)-mean(0.137,0.118,0.061))/(mean(0.432,0.54,0.618)-mean(0.137,0.118,0.061)))*100,
                                                                                 ((mean(0.173,0.186,0.225)-mean(0.137,0.118,0.061))/(mean(0.432,0.54,0.618)-mean(0.137,0.118,0.061)))*100,
                                                                                 ((mean(0.347,0.316,0.338)-mean(0.137,0.118,0.061))/(mean(0.432,0.54,0.618)-mean(0.137,0.118,0.061)))*100,
                                                                                 ((mean(0.448,0.411,0.326)-mean(0.137,0.118,0.061))/(mean(0.432,0.54,0.618)-mean(0.137,0.118,0.061)))*100,
                                                                                 ((mean(0.465,0.319,0.348)-mean(0.137,0.118,0.061))/(mean(0.432,0.54,0.618)-mean(0.137,0.118,0.061)))*100,
                                                                                 ((mean(0.522,0.592,0.428)-mean(0.137,0.118,0.061))/(mean(0.432,0.54,0.618)-mean(0.137,0.118,0.061)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Mp6 <- Mycobacterium_smegmatis_3.125ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 3.125 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Mycobacterium_smegmatis_3.125ug_data$Optical_density_mean_values-sd(Mycobacterium_smegmatis_3.125ug_data$Optical_density_mean_values),
                ymax=Mycobacterium_smegmatis_3.125ug_data$Optical_density_mean_values+sd(Mycobacterium_smegmatis_3.125ug_data$Optical_density_mean_values), col="red")
print(Mp6)
#Penicillin 1.5625 ug/mL 
Mycobacterium_smegmatis_1.5625ug_data <- data.frame(Time= c(rep("0",8),rep("24",8),rep("48",8),rep("72",8),rep("90",8)),
                                                   Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                                   Optical_density_mean_values =c(((mean(0.047,0.049,0.046)-mean(0.034,0.034,0.039))/(mean(0.047,0.052,0.046)-mean(0.034,0.034,0.039)))*100,
                                                                                  ((mean(0.047,0.047,0.046)-mean(0.034,0.034,0.039))/(mean(0.047,0.052,0.046)-mean(0.034,0.034,0.039)))*100,
                                                                                  ((mean(0.047,0.047,0.046)-mean(0.034,0.034,0.039))/(mean(0.047,0.052,0.046)-mean(0.034,0.034,0.039)))*100,
                                                                                  ((mean(0.048,0.045,0.047)-mean(0.034,0.034,0.039))/(mean(0.047,0.052,0.046)-mean(0.034,0.034,0.039)))*100,
                                                                                  ((mean(0.047,0.047,0.047)-mean(0.034,0.034,0.039))/(mean(0.047,0.052,0.046)-mean(0.034,0.034,0.039)))*100,
                                                                                  ((mean(0.046,0.045,0.045)-mean(0.034,0.034,0.039))/(mean(0.047,0.052,0.046)-mean(0.034,0.034,0.039)))*100,
                                                                                  ((mean(0.047,0.045,0.046)-mean(0.034,0.034,0.039))/(mean(0.047,0.052,0.046)-mean(0.034,0.034,0.039)))*100,
                                                                                  ((mean(0.046,0.045,0.046)-mean(0.034,0.034,0.039))/(mean(0.047,0.052,0.046)-mean(0.034,0.034,0.039)))*100,
                                                                                  ((mean(0.068,0.07,0.056)-mean(0.073,0.061,0.04))/(mean(0.137,0.181,0.182)-mean(0.073,0.061,0.04)))*100,
                                                                                  ((mean(0.094,0.084,0.052)-mean(0.073,0.061,0.04))/(mean(0.137,0.181,0.182)-mean(0.073,0.061,0.04)))*100,
                                                                                  ((mean(0.081,0.081,0.126)-mean(0.073,0.061,0.04))/(mean(0.137,0.181,0.182)-mean(0.073,0.061,0.04)))*100,
                                                                                  ((mean(0.113,0.094,0.119)-mean(0.073,0.061,0.04))/(mean(0.137,0.181,0.182)-mean(0.073,0.061,0.04)))*100,
                                                                                  ((mean(0.111,0.111,0.156)-mean(0.073,0.061,0.04))/(mean(0.137,0.181,0.182)-mean(0.073,0.061,0.04)))*100,
                                                                                  ((mean(0.11,0.139,0.145)-mean(0.073,0.061,0.04))/(mean(0.137,0.181,0.182)-mean(0.073,0.061,0.04)))*100,
                                                                                  ((mean(0.121,0.12,0.165)-mean(0.073,0.061,0.04))/(mean(0.137,0.181,0.182)-mean(0.073,0.061,0.04)))*100,
                                                                                  ((mean(0.134,0.153,0.181)-mean(0.073,0.061,0.04))/(mean(0.137,0.181,0.182)-mean(0.073,0.061,0.04)))*100,
                                                                                  ((mean(0.053,0.048,0.059)-mean(0.046,0.043,0.037))/(mean(0.233,0.282,0.261)-mean(0.046,0.043,0.037)))*100,
                                                                                  ((mean(0.072,0.05,0.101)-mean(0.046,0.043,0.037))/(mean(0.233,0.282,0.261)-mean(0.046,0.043,0.037)))*100,
                                                                                  ((mean(0.131,0.105,0.143)-mean(0.046,0.043,0.037))/(mean(0.233,0.282,0.261)-mean(0.046,0.043,0.037)))*100,
                                                                                  ((mean(0.14,0.165,0.128)-mean(0.046,0.043,0.037))/(mean(0.233,0.282,0.261)-mean(0.046,0.043,0.037)))*100,
                                                                                  ((mean(0.14,0.201,0.177)-mean(0.046,0.043,0.037))/(mean(0.233,0.282,0.261)-mean(0.046,0.043,0.037)))*100,
                                                                                  ((mean(0.179,0.216,0.161)-mean(0.046,0.043,0.037))/(mean(0.233,0.282,0.261)-mean(0.046,0.043,0.037)))*100,
                                                                                  ((mean(0.222,0.222,0.224)-mean(0.046,0.043,0.037))/(mean(0.233,0.282,0.261)-mean(0.046,0.043,0.037)))*100,
                                                                                  ((mean(0.228,0.258,0.286)-mean(0.046,0.043,0.037))/(mean(0.233,0.282,0.261)-mean(0.046,0.043,0.037)))*100,
                                                                                  ((mean(0.047,0.049,0.048)-mean(0.041,0.04,0.037))/(mean(0.338,0.307,0.386)-mean(0.041,0.04,0.037)))*100,
                                                                                  ((mean(0.044,0.042,0.043)-mean(0.041,0.04,0.037))/(mean(0.338,0.307,0.386)-mean(0.041,0.04,0.037)))*100,
                                                                                  ((mean(0.118,0.044,0.048)-mean(0.041,0.04,0.037))/(mean(0.338,0.307,0.386)-mean(0.041,0.04,0.037)))*100,
                                                                                  ((mean(0.164,0.209,0.121)-mean(0.041,0.04,0.037))/(mean(0.338,0.307,0.386)-mean(0.041,0.04,0.037)))*100,
                                                                                  ((mean(0.256,0.271,0.383)-mean(0.041,0.04,0.037))/(mean(0.338,0.307,0.386)-mean(0.041,0.04,0.037)))*100,
                                                                                  ((mean(0.352,0.296,0.247)-mean(0.041,0.04,0.037))/(mean(0.338,0.307,0.386)-mean(0.041,0.04,0.037)))*100,
                                                                                  ((mean(0.388,0.281,0.28)-mean(0.041,0.04,0.037))/(mean(0.338,0.307,0.386)-mean(0.041,0.04,0.037)))*100,
                                                                                  ((mean(0.284,0.284,0.331)-mean(0.041,0.04,0.037))/(mean(0.338,0.307,0.386)-mean(0.041,0.04,0.037)))*100,
                                                                                  ((mean(0.05,0.045,0.045)-mean(0.098,0.084,0.047))/(mean(0.428,0.38,0.71)-mean(0.098,0.084,0.047)))*100,
                                                                                  ((mean(0.044,0.043,0.044)-mean(0.098,0.084,0.047))/(mean(0.428,0.38,0.71)-mean(0.098,0.084,0.047)))*100,
                                                                                  ((mean(0.069,0.042,0.066)-mean(0.098,0.084,0.047))/(mean(0.428,0.38,0.71)-mean(0.098,0.084,0.047)))*100,
                                                                                  ((mean(0.215,0.219,0.262)-mean(0.098,0.084,0.047))/(mean(0.428,0.38,0.71)-mean(0.098,0.084,0.047)))*100,
                                                                                  ((mean(0.305,0.322,0.537)-mean(0.098,0.084,0.047))/(mean(0.428,0.38,0.71)-mean(0.098,0.084,0.047)))*100,
                                                                                  ((mean(0.424,0.465,0.299)-mean(0.098,0.084,0.047))/(mean(0.428,0.38,0.71)-mean(0.098,0.084,0.047)))*100,
                                                                                  ((mean(0.521,0.322,0.4)-mean(0.098,0.084,0.047))/(mean(0.428,0.38,0.71)-mean(0.098,0.084,0.047)))*100,
                                                                                  ((mean(0.442,0.349,0.383)-mean(0.098,0.084,0.047))/(mean(0.428,0.38,0.71)-mean(0.098,0.084,0.047)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Mp7 <- Mycobacterium_smegmatis_1.5625ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 1.5625 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Mycobacterium_smegmatis_1.5625ug_data$Optical_density_mean_values-sd(Mycobacterium_smegmatis_1.5625ug_data$Optical_density_mean_values),
                ymax=Mycobacterium_smegmatis_1.5625ug_data$Optical_density_mean_values+sd(Mycobacterium_smegmatis_1.5625ug_data$Optical_density_mean_values), col="red")
print(Mp7)
#Penicillin 0 ug/mL 
Mycobacterium_smegmatis_0ug_data <- data.frame(Time= c(rep("0",8),rep("24",8),rep("48",8),rep("72",8),rep("90",8)),
                                                    Tebipenem_concentrations=c(10.0,5.0,2.5,1.25,0.625,0.3125,0.15625,0),
                                                    Optical_density_mean_values =c(((mean(0.05,0.052,0.051)-mean(0.035,0.035,0.034))/(mean(0.049,0.052,0.049)-mean(0.035,0.035,0.034)))*100,
                                                                                   ((mean(0.049,0.051,0.049)-mean(0.035,0.035,0.034))/(mean(0.049,0.052,0.049)-mean(0.035,0.035,0.034)))*100,
                                                                                   ((mean(0.049,0.049,0.049)-mean(0.035,0.035,0.034))/(mean(0.049,0.052,0.049)-mean(0.035,0.035,0.034)))*100,
                                                                                   ((mean(0.048,0.048,0.049)-mean(0.035,0.035,0.034))/(mean(0.049,0.052,0.049)-mean(0.035,0.035,0.034)))*100,
                                                                                   ((mean(0.048,0.047,0.048)-mean(0.035,0.035,0.034))/(mean(0.049,0.052,0.049)-mean(0.035,0.035,0.034)))*100,
                                                                                   ((mean(0.049,0.048,0.048)-mean(0.035,0.035,0.034))/(mean(0.049,0.052,0.049)-mean(0.035,0.035,0.034)))*100,
                                                                                   ((mean(0.048,0.046,0.047)-mean(0.035,0.035,0.034))/(mean(0.049,0.052,0.049)-mean(0.035,0.035,0.034)))*100,
                                                                                   ((mean(0.048,0.048,0.048)-mean(0.035,0.035,0.034))/(mean(0.049,0.052,0.049)-mean(0.035,0.035,0.034)))*100,
                                                                                   ((mean(0.07,0.084,0.047)-mean(0.064,0.053,0.038))/(mean(0.136,0.17,0.214)-mean(0.064,0.053,0.038)))*100,
                                                                                   ((mean(0.108,0.098,0.047)-mean(0.064,0.053,0.038))/(mean(0.136,0.17,0.214)-mean(0.064,0.053,0.038)))*100,
                                                                                   ((mean(0.094,0.093,0.091)-mean(0.064,0.053,0.038))/(mean(0.136,0.17,0.214)-mean(0.064,0.053,0.038)))*100,
                                                                                   ((mean(0.108,0.109,0.136)-mean(0.064,0.053,0.038))/(mean(0.136,0.17,0.214)-mean(0.064,0.053,0.038)))*100,
                                                                                   ((mean(0.125,0.118,0.198)-mean(0.064,0.053,0.038))/(mean(0.136,0.17,0.214)-mean(0.064,0.053,0.038)))*100,
                                                                                   ((mean(0.121,0.144,0.16)-mean(0.064,0.053,0.038))/(mean(0.136,0.17,0.214)-mean(0.064,0.053,0.038)))*100,
                                                                                   ((mean(0.12,0.127,0.175)-mean(0.064,0.053,0.038))/(mean(0.136,0.17,0.214)-mean(0.064,0.053,0.038)))*100,
                                                                                   ((mean(0.129,0.152,0.209)-mean(0.064,0.053,0.038))/(mean(0.136,0.17,0.214)-mean(0.064,0.053,0.038)))*100,
                                                                                   ((mean(0.051,0.052,0.07)-mean(0.041,0.039,0.037))/(mean(0.249,0.29,0.281)-mean(0.041,0.039,0.037)))*100,
                                                                                   ((mean(0.093,0.046,0.051)-mean(0.041,0.039,0.037))/(mean(0.249,0.29,0.281)-mean(0.041,0.039,0.037)))*100,
                                                                                   ((mean(0.125,0.064,0.152)-mean(0.041,0.039,0.037))/(mean(0.249,0.29,0.281)-mean(0.041,0.039,0.037)))*100,
                                                                                   ((mean(0.142,0.178,0.155)-mean(0.041,0.039,0.037))/(mean(0.249,0.29,0.281)-mean(0.041,0.039,0.037)))*100,
                                                                                   ((mean(0.161,0.179,0.279)-mean(0.041,0.039,0.037))/(mean(0.249,0.29,0.281)-mean(0.041,0.039,0.037)))*100,
                                                                                   ((mean(0.259,0.244,0.181)-mean(0.041,0.039,0.037))/(mean(0.249,0.29,0.281)-mean(0.041,0.039,0.037)))*100,
                                                                                   ((mean(0.267,0.229,0.249)-mean(0.041,0.039,0.037))/(mean(0.249,0.29,0.281)-mean(0.041,0.039,0.037)))*100,
                                                                                   ((mean(0.255,0.277,0.286)-mean(0.041,0.039,0.037))/(mean(0.249,0.29,0.281)-mean(0.041,0.039,0.037)))*100,
                                                                                   ((mean(0.053,0.048,0.045)-mean(0.038,0.038,0.037))/(mean(0.44,0.37,0.338)-mean(0.038,0.038,0.037)))*100,
                                                                                   ((mean(0.044,0.065,0.05)-mean(0.038,0.038,0.037))/(mean(0.44,0.37,0.338)-mean(0.038,0.038,0.037)))*100,
                                                                                   ((mean(0.079,0.042,0.084)-mean(0.038,0.038,0.037))/(mean(0.44,0.37,0.338)-mean(0.038,0.038,0.037)))*100,
                                                                                   ((mean(0.234,0.062,0.134)-mean(0.038,0.038,0.037))/(mean(0.44,0.37,0.338)-mean(0.038,0.038,0.037)))*100,
                                                                                   ((mean(0.251,0.287,0.362)-mean(0.038,0.038,0.037))/(mean(0.44,0.37,0.338)-mean(0.038,0.038,0.037)))*100,
                                                                                   ((mean(0.239,0.391,0.33)-mean(0.038,0.038,0.037))/(mean(0.44,0.37,0.338)-mean(0.038,0.038,0.037)))*100,
                                                                                   ((mean(0.372,0.238,0.303)-mean(0.038,0.038,0.037))/(mean(0.44,0.37,0.338)-mean(0.038,0.038,0.037)))*100,
                                                                                   ((mean(0.371,0.315,0.392)-mean(0.038,0.038,0.037))/(mean(0.44,0.37,0.338)-mean(0.038,0.038,0.037)))*100,
                                                                                   ((mean(0.048,0.049,0.052)-mean(0.09,0.08,0.044))/(mean(0.527,0.577,0.646)-mean(0.09,0.08,0.044)))*100,
                                                                                   ((mean(0.044,0.047,0.046)-mean(0.09,0.08,0.044))/(mean(0.527,0.577,0.646)-mean(0.09,0.08,0.044)))*100,
                                                                                   ((mean(0.044,0.042,0.07)-mean(0.09,0.08,0.044))/(mean(0.527,0.577,0.646)-mean(0.09,0.08,0.044)))*100,
                                                                                   ((mean(0.19,0.183,0.296)-mean(0.09,0.08,0.044))/(mean(0.527,0.577,0.646)-mean(0.09,0.08,0.044)))*100,
                                                                                   ((mean(0.297,0.297,0.466)-mean(0.09,0.08,0.044))/(mean(0.527,0.577,0.646)-mean(0.09,0.08,0.044)))*100,
                                                                                   ((mean(0.458,0.447,0.395)-mean(0.09,0.08,0.044))/(mean(0.527,0.577,0.646)-mean(0.09,0.08,0.044)))*100,
                                                                                   ((mean(0.535,0.454,0.482)-mean(0.09,0.08,0.044))/(mean(0.527,0.577,0.646)-mean(0.09,0.08,0.044)))*100,
                                                                                   ((mean(0.428,0.521,0.461)-mean(0.09,0.08,0.044))/(mean(0.527,0.577,0.646)-mean(0.09,0.08,0.044)))*100))
#Make a line graphs for changing in OD with time according to Tebipenem concentrations
Mp8 <- Mycobacterium_smegmatis_0ug_data %>%
  ggplot(aes(x=Time,y=Optical_density_mean_values,group=Tebipenem_concentrations, fill=Tebipenem_concentrations))+
  geom_line()+
  geom_point()+
  geom_area() +
  facet_wrap(~Tebipenem_concentrations)+
  ggtitle("Penicillin 0 ug/mL")+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Mycobacterium_smegmatis_0ug_data$Optical_density_mean_values-sd(Mycobacterium_smegmatis_0ug_data$Optical_density_mean_values),
                ymax=Mycobacterium_smegmatis_0ug_data$Optical_density_mean_values+sd(Mycobacterium_smegmatis_0ug_data$Optical_density_mean_values), col="red")
print(Mp8)
#Align all graphs together 
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
figure_3 <- ggarrange(Mp1,Mp2,Mp3,Mp4, Mp5, Mp6, Mp7, Mp8,
                      labels=c("a","b","c","d","e","f","g","h"),
                      ncol=4,nrow=2,
                      common.legend = TRUE,legend = "bottom")
print(figure_3)
#Compare concentrations that inhibt the bacterial growth 
Final_Myco_data <- data.frame(Time_f3 =c("0","24","48","72","96"),
                                 Concentrations= c(rep("PenicillinV 100 ug/mL + Tebip 1.25 ug/mL",5),
                                                   rep("PenicillinV 50 ug/mL + Tebip 1.25 ug/mL",5),
                                                   rep("PenicillinV 1.5625 ug/mL + Tebip 2.5 ug/mL",5),
                                                   rep("PeenicillinV 0 ug/mL + Tebip 2.5 ug/mL",5)),
                                 Optical_density_value=c(((mean(0.047,0.047,0.047)-mean(0.036,0.036,0.037))/(mean(0.05,0.051,0.053)-mean(0.036,0.036,0.037)))*100,
                                                         ((mean(0.086,0.114,0.125)-mean(0.08,0.065,0.04))/(mean(0.135,0.135,0.162)-mean(0.08,0.065,0.04)))*100,
                                                         ((mean(0.105,0.226,0.182)-mean(0.052,0.047,0.038))/(mean(0.237,0.25,0.27)-mean(0.052,0.047,0.038)))*100,
                                                         ((mean(0.126,0.32,0.153)-mean(0.047,0.049,0.041))/(mean(0.363,0.42,0.358)-mean(0.047,0.049,0.041)))*100,
                                                         ((mean(0.115,0.587,0.137)-mean(0.119,0.097,0.048))/(mean(0.444,0.576,0.536)-mean(0.119,0.097,0.048)))*100,# end of 100+1.25
                                                         ((mean(0.048,0.047,0.047)-mean(0.036,0.035,0.035))/(mean(0.049,0.05,0.047)-mean(0.036,0.035,0.035)))*100,
                                                         ((mean(0.092,0.101,0.146)-mean(0.09,0.075,0.041))/(mean(0.141,0.143,0.156)-mean(0.09,0.075,0.041)))*100,
                                                         ((mean(0.121,0.151,0.175)-mean(0.049,0.046,0.038))/(mean(0.246,0.235,0.246)-mean(0.049,0.046,0.038)))*100,
                                                         ((mean(0.129,0.217,0.193)-mean(0.049,0.05,0.04))/(mean(0.377,0.364,0.494)-mean(0.049,0.05,0.04)))*100,
                                                         ((mean(0.117,0.207,0.22)-mean(0.135,0.118,0.057))/(mean(0.42,0.432,0.609)-mean(0.135,0.118,0.057)))*100, #end of 50+1.25
                                                         ((mean(0.047,0.047,0.046)-mean(0.034,0.034,0.039))/(mean(0.047,0.052,0.046)-mean(0.034,0.034,0.039)))*100,
                                                         ((mean(0.081,0.081,0.126)-mean(0.073,0.061,0.04))/(mean(0.137,0.181,0.182)-mean(0.073,0.061,0.04)))*100,
                                                         ((mean(0.131,0.105,0.143)-mean(0.046,0.043,0.037))/(mean(0.233,0.282,0.261)-mean(0.046,0.043,0.037)))*100,
                                                         ((mean(0.118,0.044,0.048)-mean(0.041,0.04,0.037))/(mean(0.338,0.307,0.386)-mean(0.041,0.04,0.037)))*100,
                                                         ((mean(0.069,0.042,0.066)-mean(0.098,0.084,0.047))/(mean(0.428,0.38,0.71)-mean(0.098,0.084,0.047)))*100, #end of 1.5625+2.5
                                                         ((mean(0.049,0.049,0.049)-mean(0.035,0.035,0.034))/(mean(0.049,0.052,0.049)-mean(0.035,0.035,0.034)))*100,
                                                         ((mean(0.094,0.093,0.091)-mean(0.064,0.053,0.038))/(mean(0.136,0.17,0.214)-mean(0.064,0.053,0.038)))*100,
                                                         ((mean(0.125,0.064,0.152)-mean(0.041,0.039,0.037))/(mean(0.249,0.29,0.281)-mean(0.041,0.039,0.037)))*100,
                                                         ((mean(0.079,0.042,0.084)-mean(0.038,0.038,0.037))/(mean(0.44,0.37,0.338)-mean(0.038,0.038,0.037)))*100,
                                                         ((mean(0.044,0.042,0.07)-mean(0.09,0.08,0.044))/(mean(0.527,0.577,0.646)-mean(0.09,0.08,0.044)))*100)) #end of 0+2.5 
#Make up a line graph 
Final_Myco<- Final_Myco_data %>%
  ggplot(aes(x=Time_f3, y=Optical_density_value,group=Concentrations, shape=Concentrations))+
  geom_line()+
  geom_point(size=2)+
  xlab("Time (h)")+
  ylab("OD 570 nm (%)")+
  geom_errorbar(ymin=Final_Myco_data$Optical_density_value-sd(Final_Myco_data$Optical_density_value),
                ymax=Final_Myco_data$Optical_density_value+sd(Final_Myco_data$Optical_density_value),
                width=.2)+
  stat_compare_means(method = "kruskal.test", label.y =140)+
  stat_compare_means(label = "p.signif", label.y = c(110,100,90,70,50))+
  geom_hline(yintercept = 0, linetype="dashed", col="grey")
print(Final_Myco)
