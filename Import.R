library(tidyverse)
library(dataRetrieval)
library(viridis)

#Finding Sediment Locations 
pCode <- c("80154")
nwis_site <- readNWISdata(stateCd="MD", parameterCd = pCode,  service = "site", seriesCatalogOutput=TRUE)

ssc_site <- nwis_site %>% filter (parm_cd %in% pCode) %>%
  filter(data_type_cd == "dv")# %>%
 # filter (count_nu > 100) 


ssc_list <- ssc_site$site_no
#data<-renameNWISColumns(data)


parameterCd <- c('00060',"63680")
startDate <- ""
endDate <- ""
siteNumber <- "01649190"

###Paint Branch - 01649190
## Northeast Anacostia - 01649500 
##Rock Creek- 01648010
## Hicky 01651770 - look at instant
## Watts 01651800 - look at instant

data <- readNWISdv(siteNumber, parameterCd, startDate, endDate)


Site_ssc<-data %>%
  renameNWISColumns()%>%
  mutate("Date1"=Date) %>%
  separate("Date1",c("Year","Month","Day"), sep = "-") %>%
  filter(Year>2000) %>%
  filter(Turb>-1)

ggplot(Site_ssc, aes(x=log(Flow), y=log(Turb), group=as.numeric(Year), color=as.numeric(Year))) +
  geom_point(alpha=0.1)+
  #facet_wrap(~Month)+
  geom_smooth(method=lm)+
  scale_color_viridis()

library(broom)

lm_stat<- Site_ssc %>%
  group_by(site_no, Year) %>%
  do(tidy(lm((Turb)~(Flow),data=.)))

lm_stat %>%
  select(site_no, Year,term,estimate) %>%
  spread(term,estimate) %>%
  ggplot(aes(x=`Flow`,y=`(Intercept)`,color=as.numeric(Year),label=Year))+
  geom_label()+
  geom_point()+
  geom_path()+
  facet_wrap(~site_no)+
  scale_color_viridis()

  fit <- lm(data=subset(Site_ssc, Year=="2011"&site_no=="01589000"),X_80155~Flow)
  
#Look at Diel fluctuations in sediment - continuous data
  
  parameterCd <- c('00060',"63680")
  startDate <- ""
  endDate <- ""
  siteNumber <- "01649190"

  data <- readNWISuv(siteNumber, parameterCd, startDate, endDate)
  
  Site_uv_ssc<-data %>%
    renameNWISColumns()%>%
    mutate("dateTime1"=dateTime) %>%
    separate("dateTime1",c("Date","Time"), sep = " ") %>%
    separate("Date",c("Year","Month","Day"), sep = "-") %>%
    filter(Year>2000) %>%
    filter(Turb_Inst>-1) %>%
    na.omit()
  
  