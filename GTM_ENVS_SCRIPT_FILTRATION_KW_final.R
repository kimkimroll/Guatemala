#SCRIPT FOR PPLB/PMDDL-ENVS########################

###################################################
#HOW TO USE
#STEP1: Make sure you have two packages installed: xlsx, ggplot2
#COPY and PASTE code below, and RUN in R:

#install.packages("xlsx")
#install.packages("ggplot2")


###################################################
#HOW TO USE
#STEP2: Outputs a PNG image file of Guatemala current results
#COPY and PASTE code below, and RUN in R:


#read in data from Environmental Surveillance spreadsheet
setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Environmental/R_analysis")

library(xlsx)
eolddata <- read.xlsx("allsamples_envsdata.xlsx", sheetName = "envsdata_2016-2019", header=TRUE)
enewdata <- read.xlsx("allsamples_envsdata.xlsx", sheetName = "2020_envs data", header=TRUE)

#filter only guatemala samples
names(eolddata)[names(eolddata) == "pH"] <- "collection_pH"
gtmold<-(eolddata[eolddata$country == "GTM",])
gtmnew<-(enewdata[enewdata$country == "GTM",])

#combine old (2019 or past) with new (2020) data
allg <- merge(gtmold, gtmnew, 
              by.x = c("country", "city", "site", "site_name", "lat", "long", "sample_id", "dash", "collection_date", "collection_time", "collection_month", "method", "processor1", "processor2", "collection_temp", "collection_pH", "volume_filtered", "conc_factor", "processing_date", "report_date", "result1", "result2", "result3"),
              by.y = c("country", "city", "site", "site_name", "lat", "long", "sample_id", "dash", "collection_date", "collection_time", "collection_month", "method", "processor1", "processor2", "collection_temp", "collection_pH", "volume_filtered", "conc_factor", "processing_date", "report_date", "result1", "result2", "result3"),
              all.x=TRUE, all.y=TRUE)

#make r_result column and remove rows with no results
allg$r_result <-paste(allg$result1, allg$result2, allg$result3, sep="_")
allg$r_result<-as.character(allg$r_result)
allg$r_result[allg$r_result == "SL1_SL3_NPEV"] <- 'SL1 & SL3'
allg$r_result[allg$r_result == "SL1_SL3_NA"] <- 'SL1 & SL3'
allg$r_result[allg$r_result == "SL1_NPEV_NA"] <- 'SL1'
allg$r_result[allg$r_result == "SL1_NPEV_NEV"] <- 'SL1'
allg$r_result[allg$r_result == "SL1_NA_NA"] <- 'SL1'
allg$r_result[allg$r_result == "SL3_NPEV_NA"] <- 'SL3'
allg$r_result[allg$r_result == "SL3_NPEV_NEV"] <- 'SL3'
allg$r_result[allg$r_result == "SL3_NA_NA"] <- 'SL3'
allg$r_result[allg$r_result == "NA_NA_NA"] <- 'NA'
allg$r_result[allg$r_result == "NPEV_NA_NA"] <- 'NPEV'
allg$r_result[allg$r_result == "NPEV_NEV_NA"] <- 'NPEV'
allg$r_result[allg$r_result == "NEG_NA_NA"] <- 'NEG'
allg$r_result[grep("SL3 Discordant", allg$r_result)]<- 'VDPV3'
allg$r_result[grep("SL1 Discordant", allg$r_result)]<- 'VDPV1'
allg$r_result<-as.character(allg$r_result)  
allg <- allg[allg$r_result != "NA", ]
allg$r_result<-as.factor(allg$r_result)  

#make collection month and year columns
allg[, "year"] <- format(allg[,"collection_date"], "%Y")
allg[, "month"] <- format(allg[,"collection_date"], "%m")

#filter only Filtration 
allg<-(allg[allg$method == "Filtration",])

#filter filtration only dates
allg <- allg[allg$collection_date >= as.Date("2019-10-01"),]

#set dates to plot for each month
allg$collection_final <- format(as.Date(paste0(allg$collection_month, allg$year, "01"), 
                                        format="%b%Y%d"), "%m-%d-%Y")
allg$collection_date<-as.Date(allg$collection_final, "%m-%d-%Y")

#filter "SL" and "VDPV"
sl1 <- (allg[allg$r_result == "SL1" | allg$r_result == "VDPV1",])
sl1$r_result<-as.character(sl1$r_result) 
sl1$r_result[sl1$r_result == "SL1"] <- 'SL'
sl1$r_result[sl1$r_result == "VDPV1"] <- 'VDPV'
sl3 <- (allg[allg$r_result == "SL3" | allg$r_result == "VDPV3",])
sl3$r_result<-as.character(sl3$r_result) 
sl3$r_result[sl3$r_result == "SL3"] <- 'SL'
sl3$r_result[sl3$r_result == "VDPV3"] <- 'VDPV'

#combine SL1 and SL3 to same axis line
allg$r_result<-as.character(allg$r_result)
allg$r_result[allg$r_result == "SL1"] <- 'SL'
allg$r_result[allg$r_result == "SL3"] <- 'SL'
allg$r_result[allg$r_result == "VDPV1"] <- 'VDPV'
allg$r_result[allg$r_result == "VDPV3"] <- 'VDPV'

#graph
library(ggplot2)

resultcolors<-c(" " = "grey",
                "NEG" = "grey20",
                "NA" = "grey",
                "NPEV" = "#0065D1", 
                "SL" = "#84cff4", 
                "SL1 & SL3" = "#ffc215",
                "SL2" = "#FF2E2E",
                "VDPV" = "#FF2E2E")

methodcolors<-c("Filtration" = "grey0",
                "two_phase" = "lightgrey") 

#methodsize<-as.numeric(methodsize)  

methodlabels<-c("Filtration" = "Filtration",
                "two_phase" = "Two-Phase")  

result_order <- c('NEG', 'NPEV', 'SL', 'SL1 & SL3', 'VDPV')

kgtml<-ggplot(allg, aes(x=collection_date, y=factor(r_result, level=result_order), 
                        group=method, 
                        fill=r_result), 
              na.rm=TRUE) +
  geom_line(aes(x=collection_date, 
                y=factor(r_result, level=result_order), colour=method), 
            size = 1, na.rm=TRUE) +
  geom_point(shape = 21, 
             size=6, 
             color="white", 
             alpha = 1, show.legend=FALSE)+
  geom_point(data = sl1, aes(x = collection_date), 
             shape = 49, 
             size = 3.5, show.legend=FALSE)+
  geom_point(data = sl3, aes(x = collection_date), 
             shape = 51, 
             size = 3.5, show.legend=FALSE)+
  geom_hline(aes(yintercept="NPEV"), 
             linetype="solid", 
             color = "#0065D1", 
             alpha=0.25, 
             size=2)+
  scale_x_date(date_labels = "%b", 
               date_breaks = "1 months", 
               name='Collection Date', 
               #limits = c(as.Date("2019-01-01"), as.Date("2021-12-31"))
               #limits = c(as.Date("2019-10-01"), NA)
               #expand = (0,0)
               expand = expansion(add = 25)
               )+
  scale_y_discrete(name = "Result", 
                   limits = c('NEG', 'NPEV', 'SL', 'SL1 & SL3', 'VDPV'))+
  scale_colour_manual(name='Method', 
                      values = methodcolors, 
                      labels = methodlabels) +
  scale_fill_manual(values = resultcolors) +
  theme_classic()+
  theme( 
    axis.text = element_text( size = 20 ),
    axis.text.x = element_text(size = 14, angle = 0),
    axis.text.y = element_text(size = 12, angle = 0),
    axis.title = element_text( size = 16),
    axis.title.x = element_text(vjust=-2),
    strip.text.x = element_text( size = 16),
    strip.text.y = element_text( size = 20, angle =0),
    #strip.background.x = element_rect(size=.2),
    legend.text = element_text( size = 10, angle =0),
    legend.title = element_text( size = 12, angle =0),
    legend.position="bottom",
    panel.border = element_blank(),
    panel.spacing.x = unit(0, "mm"),
    panel.spacing.y = unit(3, "mm"),
    panel.grid.minor.x = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill="white", colour="white", size=.5))  +
  guides(color=guide_legend(ncol=1))+
  #facet_grid(city+site~., space="free", scales="free_y") 
  facet_grid(city+site~year, 
             space="free", 
             scales="free", switch = "x"
             ) +
  coord_cartesian(clip = "off") +
  #xlim("2019", "2020", "2021")+
  labs(caption = paste(Sys.time()))

#save PNG graph to Environmental Surveillance folder
setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Environmental/R_analysis")

ggsave("k_script_envs_filtration_GTM.png", dpi = 500, height = 18, width = 28, units = "cm")



#END SCRIPT##########################################################################
