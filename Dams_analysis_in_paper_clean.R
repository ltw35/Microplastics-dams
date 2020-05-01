#### Info ----
# Script for creating figures & analysis used in:
# "The effect of dams on river transport of microplastic pollution"
# Code by Lisa Watkins
# Cleaned for posting 4/30/2020
# To run, ensure Rproj file is located in same directory as this code and listed .csv

#### Load libraries ----
library(ggplot2) #for plotting
library(dplyr) #for cleaning dataframes
library(tidyr) #for pivot_longer()

#### Load data ----  ### FOR THE FUTURE: create one raw data file and produce these subframes from it in the code
raw = read.csv("Dams_Microplastics_full.csv")

#### Create separate dataframes for easier analysis ----
dataset = raw %>% 
  mutate(nTotal = Fiber+ Film+ Fragment+ Foam+ Bead) %>% 
  mutate(Concentration = nTotal/Weight) %>% 
  select(DamID, Location, Sample, Concentration)

typeset = raw %>% 
  rename(Fiber = nFiber, Film = nFilm, Fragment = nFragment,Foam=nFoam, Bead = nBead) %>% 
  mutate(sample_total_count = Fiber + Film + Fragment + Foam + Bead) %>% 
  pivot_longer(cols = c(Fiber,Film,Fragment,Foam,Bead),names_to = "Type", values_to = "Count") %>% 
  mutate(Concentration = Count/Weight) %>%
  mutate(Percent = Count/sample_total_count) %>% 
  select(-sample_total_count, -Count, -Weight)

avgs = raw %>% 
  mutate(nTotal = nFiber+ nFilm+ nFragment+ nFoam+ nBead) %>% 
  mutate(Concentration_wblank = nTotal/Weight-3.33) %>% 
  select(DamID, Location, Sample, Concentration_wblank) %>% 
  mutate(Stream = if_else(DamID == "FR"|DamID =="BL","Fall Creek", "Six Mile"))
sed_avgs = avgs %>% 
  filter(Sample == "Sediment") %>% 
  group_by(Stream, Location) %>% 
  summarize(conc_wblank = mean(Concentration_wblank), stdev =sd(Concentration_wblank)) %>% 
  mutate(Sample = "Sediment")
wat_avgs = avgs %>% 
  filter(Sample == "Water") %>% 
  group_by(Stream, Location) %>% 
  summarize(conc_wblank = mean(Concentration_wblank), stdev =sd(Concentration_wblank)) %>% 
  mutate(Sample = "Water")  

#### Reformat dataframes ----
### Reformat dataframe of of concentrations by dam ("dataset") for proper analysis

# Step 1: Add in surface area measurements (done using google earth)
dataset$Surfacearea_km2 = c(0.0085,0.0085,0.0085,0.069,0.069,0.069,0.028,0.028,0.028,0.18,0.18,0.18,0.065,0.065,0.065,0.007,0.007,0.007,0.0085,0.0085,0.0085,0.069,0.069,0.069,0.028,0.028,0.028,0.18,0.18,0.18,0.065,0.065,0.065,0.007,0.007,0.007) #from National Hydrography dataset + GIS

#Step 2: Remove number of particles found in lab blanks (a conservative approach)
dataset$conc_wblank = dataset$Concentration - 3.33 # blank concentration for standard 2L or 2kg samples = 6.66/2L or kg = 3.33

#Step 3: Factor variables into levels
dataset$DamID <- factor(dataset$DamID, levels = c("FR","BL","SD","3D","2D","1D"))
dataset$Location <- factor(dataset$Location, levels = c("Upstream","Reservoir","Downstream"))
dataset$Sample <- factor(dataset$Sample, levels = c("Water","Sediment"))

#Step 4: proportion of total plastics each location at a dam contains (this is terrible form, but was all I could do back then)
dataset$proprank = c(0.314049587,0.272727273,0.41322314,0.504587156,0.211009174,0.28440367,0.4,0.325,0.275,0.565217391,0.195652174,0.239130435,0.425925926,0.185185185,0.388888889,0.306451613,0.370967742,0.322580645,0.308108108,0.518918919,0.172972973,0.245283019,0.518867925,0.235849057,0.121621622,0.581081081,0.297297297,0.3,0.46,0.24,0.25,0.5,0.25,0.516666667,0.233333333,0.25)

#Step 5: Subset the dataset into two new dataframes
dataWater <- droplevels(dataset[grep("Water",dataset$Sample),]) #for water samples only
dataSed <- droplevels(dataset[grep("Sediment",dataset$Sample),]) # for sediment samples only

#Step 6: Adding new column 
#   To identify which river the dam is on (either Fall Creek "FC" or Six Mile Creek "SM")
dataWater$river = c('FC','FC','FC','FC','FC','FC','SM','SM','SM','SM','SM','SM','SM','SM','SM','SM','SM','SM')
dataSed$river = c('FC','FC','FC','FC','FC','FC','SM','SM','SM','SM','SM','SM','SM','SM','SM','SM','SM','SM')
dataWater$RiverName = c('Fall Creek','Fall Creek','Fall Creek','Fall Creek','Fall Creek','Fall Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek')
dataSed$RiverName = c('Fall Creek','Fall Creek','Fall Creek','Fall Creek','Fall Creek','Fall Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek')
#   To give each dam a unique number-name
dataWater$DamNum = c('6','6','6','5','5','5','4','4','4','3','3','3','2','2','2','1','1','1')
dataSed$DamNum = c('6','6','6','5','5','5','4','4','4','3','3','3','2','2','2','1','1','1')
  #   factor datanum so it shows up in the right order on graphs
    dataSed$DamNum = factor(dataSed$DamNum, levels=c('6','5','4','3','2','1'))
    dataWater$DamNum = factor(dataWater$DamNum, levels=c('6','5','4','3','2','1'))
#   Add rank relative to up-res-within
dataWater$rank = c(2,3,1,1,3,2,1,2,3,1,3,2,1,3,2,3,1,2)
dataSed$rank = c(2,1,3,2,1,3,3,1,2,3,1,2,2,1,3,1,3,2)
#   Add distance upstream from mouth (in km, done using google earth)
dataWater$distup = c(5.46,5.33,5.25,3.34,2.84,2.51,9.82,7.61,7.46,7,6.29,6.13,5.04,4.65,4.41,3.32,3.19,3.05)
dataSed$distup = c(5.46,5.33,5.25,3.34,2.84,2.51,9.82,7.61,7.46,7,6.29,6.13,5.04,4.65,4.41,3.32,3.19,3.05)

### Reformat dataframe of particle types by dam ("typeset") for proper analysis
#Step 1: Factor rank/levels into columns that require ordered valuesPlastic categories for each sample
typeset$Location <- factor(typeset$Location, levels = c("Upstream","Reservoir","Downstream"))
typeset$Sample <- factor(typeset$Sample, levels = c("Water","Sediment"))
typeset$Type <- factor(typeset$Type, levels = c("Fiber","Film","Fragment","Foam","Bead"))

#Step 2: Pull out each category of particle
typeFiber <- droplevels(typeset[grep("Fiber",typeset$Type),])
typeFilm <- droplevels(typeset[grep("Film",typeset$Type),])
typeFrag <- droplevels(typeset[grep("Fragment",typeset$Type),])
typeFoam <- droplevels(typeset[grep("Foam",typeset$Type),])
typeBead <- droplevels(typeset[grep("Bead",typeset$Type),])
typeAvg <- aggregate(typeset[,c('Concentration','Percent')], list(Sample=typeset$Sample,Type=typeset$Type), mean)

#Step 3: Add column to denote river name for each dam
typeFiber$River <- c('Fall Creek','Fall Creek','Fall Creek','Fall Creek','Fall Creek','Fall Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek','Six Mile Creek')

#Step 4: subset samples by location (upstream-reservoir-downstream)
up_sed = subset(dataSed,dataSed$Location == "Upstream")
res_sed = subset(dataSed,dataSed$Location =="Reservoir")
down_sed = subset(dataSed,dataSed$Location =="Downstream")
up_wat = subset(dataWater,dataWater$Location == "Upstream")
res_wat = subset(dataWater,dataWater$Location =="Reservoir")
down_wat = subset(dataWater,dataWater$Location =="Downstream")

#Step 5: To more easily plot an "other" category of plastics, create and factor a new column
typeset$Category = typeset$Type
typeset$Category=as.character(typeset$Category)
typeset$Category[typeset$Type == "Film"]="Other" ###THIS ISNT WORKING
typeset$Category[typeset$Type == "Foam"]="Other"
typeset$Category[typeset$Type == "Bead"]="Other"
typeset$Category <- factor(typeset$Category, levels = c("Other","Fragment","Fiber"))

### Reformat dataframe of averages ("dataSed_avgs" and "dataWater_avgs") for proper analysis
#Step 1: Make sure upstream-reservoir-downstream are ranked/ordered
sed_avgs$Location <- factor(sed_avgs$Location, levels = c("Upstream","Reservoir","Downstream"))
wat_avgs$Location = factor(wat_avgs$Location, levels = c("Upstream","Reservoir","Downstream"))


####ANALYSIS----
### Linear model to test relationship between concentration & sample location
  #A reminder: these are run on conc_wblank, which have contamination measurements subtracted out
#Linear model, Sediment
mod = lm(conc_wblank~Location+river, data = dataSed) #used this one for submission 1
summary(mod)
anova(mod)
#check sediment residuals
plot(predict(mod), residuals(mod)) # looks pretty random to me?
hist(residuals(mod))
qqnorm(residuals(mod))
qqline(residuals(mod))

#Linear model, Water
modlm_wat_river = lm(conc_wblank~Location + river, data = dataWater) #used this one for submission 1
summary(modlm_wat_river)
anova(modlm_wat_river)
#check water residuals
plot(predict(modlm_wat_river), residuals(modlm_wat_river)) # super random
hist(residuals(modlm_wat_river))
qqnorm(residuals(modlm_wat_river))
qqline(residuals(modlm_wat_river))

### t-test to see whether fiber contents & fragment contents are different between water and sediment samples
# Used 95% confidence to define "significant" for this project
t.test(Percent~Sample, data=typeFiber)
t.test(Percent~Sample, data=typeFrag)

#### Plots ----
dodge = position_dodge(width = 0.9)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colorblind-friendly palette
mytheme = theme(axis.line=element_line(color="black"), 
                legend.position="bottom", 
                text = element_text(),
                axis.title = element_text(face = "bold",size = rel(1)),
                axis.title.y = element_text(angle=90,vjust =2),
                axis.title.x = element_text(vjust = -0.2),
                axis.text = element_text(), 
                axis.ticks = element_line(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background=element_blank(),
                #plot.background = element_rect(colour = NA),
                legend.key = element_rect(colour = NA),
                legend.direction = "horizontal",
                legend.key.size= unit(0.2, "cm"),
                #legend.margin = unit(0, "cm"),
                legend.title = element_text(face="italic"),
                plot.margin=unit(c(10,5,5,5),"mm"),
                strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                strip.text = element_text(face="bold"))

##Figure 2. Sediment microplastic concentrations measured at three sampling locations for each dam. 
#           Colors are dam-specific, and dams are grouped by river.
ggplot(data = dataSed, aes(x = Location, y = conc_wblank, fill = DamNum, shape=Location, colour=DamNum))+ 
  guides(fill = "none",colour = "legend")+
  scale_shape_manual(values = c(24,21,25))+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  geom_point(stat = "identity", size = 2)+
  geom_line(aes(group = dataSed$DamNum))+
  facet_grid(. ~ RiverName, scales = "free_x", space = "free")+
  labs(x = "Location Sampled",y="Concentration (particles / kg)",color = "Dam")+
  mytheme
#ggsave("Dam_sed_conc_dots.png", dpi=600, height=4, width=5, units="in")

#Fig. 3. Surface water microplastic concentrations measured at three sampling locations for each dam. 
#        Colors are dam-specific, and dams are grouped by river.
ggplot(data = dataWater, aes(x = Location, y = conc_wblank, fill = DamNum, shape = Location, colour=DamNum))+
  guides(fill = "none",colour = "legend")+
  scale_shape_manual(values = c(24,21,25))+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  geom_point(stat = "identity", size = 2)+
  geom_line(aes(group = dataSed$DamNum))+
  facet_grid(. ~ RiverName, scales = "free_x", space = "free")+
  labs(x = "Location Sampled",y="Concentration (particles / L)",color = "Dam")+
  mytheme
#ggsave("Dam_wat_conc_dots.png", dpi=600, height=4, width=5, units="in")

#Fig. 4. Categories of plastics found, 
#        plotted as proportions of the total plastics counted in water and sediment samples collected at upstream, reservoir and downstream sampling locations. 
#        Colors represent categories found: fiber (black), fragment (dark grey), and all other particles, which includes films, foams, and beads (light grey).
ggplot(typeset, aes(x=Location, y=Concentration, fill = Category)) + 
  geom_bar(stat="identity",position = "fill") +
  xlab("Location") +
  ylab("Proportion of total plastics counted")+ 
  facet_grid(~ Sample)+
  theme_classic()+
  theme(axis.title=element_text(size=14),axis.text.x=element_text(size=12,angle=30,hjust=1),axis.text.y=element_text(size=12),strip.text.x = element_text(size = 13))+
  scale_fill_manual(values = c("#CCCCCC","#777888","#333333"),labels = c("Other","Fragment","Fiber"))
#ggsave("Proport_categories_dam_bw.tiff", height=6, width=8, units='in', dpi=700) 

#Figure S2. Microplastic concentration in (a) sediments and (b) surface water 
#           plotted from farthest upstream to closest to river mouth, left to right, 
#           for both of the studied streams. Shapes correlate to sampling location, 
#           as designated on Figures 1-3. Dams are designated by a single color. 
#           Lines are to help visualize samples collected around a given dam and 
#           do not imply known values between sampled locations.
##Water
ggplot(data = dataWater, aes(x = distup, y = Concentration, color = DamNum,fill = DamNum, shape = Location))+
  geom_point(size = 2)+
  geom_line(aes(group = DamNum))+
  scale_shape_manual(values = c(24,21,25))+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  facet_grid(. ~ RiverName, scales = "free_x", space = "free")+
  labs(x = "Distance upstream from river mouth (km)",y="Concentration (particles / L)",fill = "")+
  mytheme+
  guides(size = "none",color = "legend",shape = "legend",fill = "none")+
  labs(shape="Location", color = "Dam")
#ggsave("Distup_water.png", dpi=600, height=4, width=5, units="in")
##Sediment
ggplot(data = dataSed, aes(x = distup, y = Concentration, color = DamNum,fill = DamNum, shape = Location))+
  geom_point(size = 2)+
  geom_line(aes(group = DamNum))+
  scale_shape_manual(values = c(24,21,25))+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  facet_grid(. ~ RiverName, scales = "free_x", space = "free")+
  labs(x = "Distance upstream from river mouth (km)",y="Concentration (particles / L)",fill = "")+
  mytheme+
  guides(size = "none",color = "legend",shape = "legend",fill = "none")+
  labs(shape="Location", color = "Dam")
#ggsave("Distup_sed_linedownstream.png", dpi=600, height=4, width=5, units="in")