#SCRIPT FOR PPLB/PMDDL-ENVS########################

###################################################
#HOW TO USE
#STEP1: Make sure you have two packages installed: xlsx, ggplot2
#COPY and PASTE code below, and RUN in R:

install.packages("xlsx")
install.packages("ggplot2")


###################################################
#HOW TO USE
#STEP2: Outputs a PNG image file of Guatemala current results
#COPY and PASTE code below, and RUN in R:


setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Environmental")

library(xlsx)
eolddata <- read.xlsx("allsamples_envsdata.xlsx", sheetName = "envsdata_2016-2019", header=TRUE)
enewdata <- read.xlsx("allsamples_envsdata.xlsx", sheetName = "2020_envs data", header=TRUE)

#filter only guatemala samples
names(eolddata)[names(eolddata) == "pH"] <- "collection_pH"
gtmold<-(eolddata[eolddata$country == "GTM",])
gtmnew<-(enewdata[enewdata$country == "GTM",])

#combine old (2019 or past) with new (2020) data
allg <- merge(gtmold, gtmnew, by.x = c("country", "city", "site", "site_name", "lat", "long", "sample_id", "dash", "collection_date", "collection_time", "method", "processor1", "processor2", "collection_temp", "collection_pH", "volume_filtered", "conc_factor", "processing_date", "report_date", "result1", "result2", "result3"),
              by.y = c("country", "city", "site", "site_name", "lat", "long", "sample_id", "dash", "collection_date", "collection_time", "method", "processor1", "processor2", "collection_temp", "collection_pH", "volume_filtered", "conc_factor", "processing_date", "report_date", "result1", "result2", "result3"),
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
allg$r_result[grep("SL3 Discordant", allg$r_result)]<- 'SL3 Discordant'
allg$r_result[grep("SL1 Discordant", allg$r_result)]<- 'SL1 Discordant'
allg$r_result<-as.character(allg$r_result)  
allg <- allg[allg$r_result != "NA", ]
allg$r_result<-as.factor(allg$r_result)  

#make collection month and year columns
allg[, "year"] <- format(allg[,"collection_date"], "%%Y")
allg[, "month"] <- format(allg[,"collection_date"], "%m")

#filter only Filtration results
allg<-(allg[allg$method == "Filtration",])

#graph
library(ggplot2)

resultcolors<-c(" " = "grey",
                "NEG" = "lightpink",
                "NA" = "grey",
                "NPEV" = "blue", 
                "SL1" = "yellow", 
                "SL3" = "yellow", 
                "SL1 & SL3" = "orange",
                "SL2" = "red",
                "SL1 Discordant" = "red", 
                "SL3 Discordant" = "red")

result_order <- c('NEG', 'NPEV', 'SL3', 'SL1', 'SL1 & SL3', 'SL3 Discordant', 'SL1 Discordant')

kgtml<-ggplot(allg, aes(x=collection_date, y=factor(r_result, level=result_order), group=site), na.rm=TRUE) +
  geom_line(size=2, colour="grey", na.rm=TRUE) +
  geom_point(aes(colour=r_result), size=4.5)+
  geom_hline(aes(yintercept="NPEV"), linetype="solid", 
             color = "blue", alpha=0.25, size=0.75)+
  scale_x_date(date_labels = "%b %Y", breaks="1 month", name='Collection Date')+
  scale_y_discrete(name = "Result")+
  scale_colour_manual(name='Result', values = resultcolors) +
  theme_bw()+
  theme( 
    axis.text = element_text( size = 20 ),
    axis.text.x = element_text(size = 14, angle = 90),
    axis.text.y = element_text(size = 12, angle = 0),
    axis.title = element_text( size = 14),
    strip.text.x = element_text( size = 12),
    strip.text.y = element_text( size = 12, angle =0),
    strip.background.x = element_rect(size=.2),
    legend.text = element_text( size = 14, angle =0),
    legend.title = element_text( size = 14, angle =0),
    legend.position="none",
    panel.border = element_rect(colour="black"),
    panel.spacing = unit(0, "mm"),
    strip.background = element_rect(fill="white", colour="black"))  +
  facet_grid(city+site~., space="free", scales="free_y") +labs(caption = paste(Sys.time()))


setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Environmental/R_analysis")
ggsave("k_script_envs_gtm_line.png", dpi = 500, height = 12, width = 20 , units = "in")


#END SCRIPT###########################################################
