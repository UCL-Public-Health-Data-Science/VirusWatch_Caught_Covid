#Title: Reported Location of SARS-CoV-2 Acquisition

#Packages
library(data.table)
library(dplyr)
library(tidyverse)
library(gtsummary)
library(RColorBrewer)

#Load data
data1 <- fread("data access requests can be made to the Virus Watch chief investigators - AH  or RWA - at viruswatch@ucl.ac.uk")#contact status and perceived location of infection
data2 <- fread("data access requests can be made to the Virus Watch chief investigators - AH  or RWA - at viruswatch@ucl.ac.uk")#participant demographics
pcr1 <- fread("data access requests can be made to the Virus Watch chief investigators - AH  or RWA - at viruswatch@ucl.ac.uk") %>%
  filter(result==1)#PCR positive participants with dates during study follow-up
pcr2 <- fread("data access requests can be made to the Virus Watch chief investigators - AH  or RWA - at viruswatch@ucl.ac.uk") %>%
  filter(result==1)#PCR positive participants with month reported at baseline

#Merge and clean contact status/reported location of infection with participant characteristics
merge <- merge(data2,data1, by="individual_id", all.x=TRUE) %>%
  #filter only people who reported having Covid-19
  filter(covcontact___0=="0")%>%
  #filter only people who filled in the survey (currently they may be coded as 0)
  filter(covcontact___1=="1"|covcontact___2=="1"|covcontact___3=="1")

#Characterise demographic variables
merge$ethnicity <- as.factor(merge$hh_ethnic_group)
#break up  into categories - we made it 70+ because small number of 80+ (17)
merge <- merge %>%
  mutate(agecat=cut(hh_age_on_entry, breaks=c(0,16,35,50,65,Inf), labels=c("0 to 17","18 to 34",
                                                                           "35 to 49","50 to 64","65+"))) %>%
  
  mutate(gender=ifelse(hh_birth_gender==1,"Male",ifelse(hh_birth_gender==2, "Female", ""))) 

#Label perceived location of infection and contact status variables
merge <- merge %>%
  dplyr::rename(confirmed_contact=covcontact___1,
                suspected_contact=covcontact___2,
                unknown_contact=covcontact___3,
                home=covloc___1,
                someone_else_home=covloc___2,
                public_transport=covloc___3,
                work_colleague=covloc___4,
                work_client_customer=covloc___5,
                education=covloc___6,
                essential_shop=covloc___7,
                nonessential_shop=covloc___8,
                hairdresser_salon=covloc___9,
                healthcare_setting=covloc___10,
                restaurant_cafe_canteen=covloc___11,
                bar_pub_club=covloc___12,
                gym_indoor_sport=covloc___13,
                other=covloc___14)

merge <- merge %>%
  mutate(contact=case_when(unknown_contact=="1" ~ "No Confirmed or Suspected Contact",
                           suspected_contact=="1" ~ "Suspected Contact",
                           confirmed_contact=="1" ~ "Confirmed Contact"))
merge <- merge %>%
  mutate(contact2=case_when(unknown_contact=="1" ~ "No Confirmed or Suspected Contact",
                            suspected_contact=="1" ~ "Confirmed or Suspected Contact",
                            confirmed_contact=="1" ~ "Confirmed or Suspected Contact"))

#Create collapsed locations
merge <- merge %>%
  mutate(work=case_when(work_colleague=="1"|work_client_customer=="1"~1,
                        work_colleague!="1"|work_client_customer!="1"~0))%>%
  mutate(leisure=case_when(restaurant_cafe_canteen=="1"|bar_pub_club=="1"|gym_indoor_sport=="1"|nonessential_shop=="1"|hairdresser_salon~1,
                           restaurant_cafe_canteen!="1"|bar_pub_club!="1"|gym_indoor_sport!="1"|nonessential_shop!="1"|hairdresser_salon!="1"~0))

#Join all PCR data and main data to identify pandemic wave when infected
pcrmerge <- merge(pcr2,pcr1, by="individual_id", all.x=TRUE)
pcrmerge <- merge(pcrmerge, merge, by="individual_id", all.x=TRUE)%>%
  filter(!is.na(result.x)|!is.na(result.y))

#Categorise pandemic wave based on available data (weekly for PCR during follow-up, monthly for baseline)
pcrmerge<-pcrmerge %>%
  mutate(period=case_when(week_number<7 | month_id<6 ~ "Jan-May 2020",
                          week_number %in% c(7:19) | month_id %in% c(6:8) ~ "Jun-Aug 2020", 
                          week_number%in% c(20:37) | month_id %in% c(9:12) ~ "Sep 2020-Dec 2020",
                          week_number>=38 | month_id>=13 ~ "Jan 2021-current"))

#Frequency Tables

#Overall frequencies for contact status
contact<-merge%>%
  select(confirmed_contact:unknown_contact)%>%
  tbl_summary(missing="no",percent="column")

#Overall frequencies for perceived setting of SARS-CoV-2 acquisition
loa1 <- merge %>%
  select(home:public_transport,education,
         essential_shop,healthcare_setting,work,leisure
         ,other)%>%
  tbl_summary(missing="no",percent="column")

#Perceived setting of acquisition by age
locations <- merge %>%
  select(agecat, home:public_transport,education,
         essential_shop,healthcare_setting,work,leisure,
         other)%>%
  tbl_summary(by=agecat,missing="no",percent="column")

#Perceived setting of acquisition by contact status
locations_contact <- merge %>%
  select(contact, home:public_transport,education,
         essential_shop,healthcare_setting,work,leisure,
         other)%>%
  tbl_summary(by=contact,missing="no",percent="column") 

#Perceived setting of acquisition reported by age for confirmed/suspected cases only
locations_age_contact <- merge %>%
  filter(contact2=="Confirmed or Suspected Contact")%>%
  select(agecat, home:public_transport,education,
         essential_shop,healthcare_setting,work,leisure,
         other)%>%
  tbl_summary(by=agecat,missing="no",percent="column") 

#Perceived setting of acquisition by pandemic wave
locations_wave <- pcrmerge %>%
  select(period, home:public_transport,education,
         essential_shop,healthcare_setting,work,leisure,
         other)%>%
  tbl_summary(by=period,missing="no",percent="column")

#Plots

#Frequency Plot by Pandemic Wave

#Data in required format
wave1 <- with(pcrmerge, table(home,period)) 
wave2 <- with(pcrmerge, table(someone_else_home, period))
wave3 <- with(pcrmerge, table(work, period))
wave4 <- with(pcrmerge, table(education, period))
wave5 <- with(pcrmerge, table(public_transport, period))
wave6 <- with(pcrmerge, table(essential_shop, period))
wave7 <- with(pcrmerge, table(healthcare_setting, period))
wave8 <- with(pcrmerge, table(leisure, period))
wave9 <- with(pcrmerge, table(other, period))

#convert to data frames
wave1 <- as.data.frame.matrix(wave1)
new_colnames1 <- c("Mar-May 2020", "Jan-Mar 2021", "Jun-Aug 2020", "Sep-Dec 2020")
colnames(wave1) <- new_colnames1
wave1<- wave1[-c(1),] #keep frequencies for 'yes'
rownames(wave1)=c("Home") #label location

wave2 <- as.data.frame.matrix(wave2)
colnames(wave2) <- new_colnames1
wave2<- wave2[-c(1),] #keep frequencies for 'yes'
rownames(wave2)=c("Someone else's home") #label location

wave3 <- as.data.frame.matrix(wave3)
colnames(wave3) <- new_colnames1
wave3<- wave3[-c(1),] #keep frequencies for 'yes'
rownames(wave3)=c("Work") #label location

wave4 <- as.data.frame.matrix(wave4)
colnames(wave4) <- new_colnames1
wave4<- wave4[-c(1),] #keep frequencies for 'yes'
rownames(wave4)=c("Place of education") #label location

wave5 <- as.data.frame.matrix(wave5)
colnames(wave5) <- new_colnames1
wave5<- wave5[-c(1),] #keep frequencies for 'yes'
rownames(wave5)=c("Public transport") #label location

wave6 <- as.data.frame.matrix(wave6)
colnames(wave6) <- new_colnames1
wave6<- wave6[-c(1),] #keep frequencies for 'yes'
rownames(wave6)=c("Essential shop") #label location

wave7 <- as.data.frame.matrix(wave7)
colnames(wave7) <- new_colnames1
wave7<- wave7[-c(1),] #keep frequencies for 'yes'
rownames(wave7)=c("Healthcare setting") #label location

wave8 <- as.data.frame.matrix(wave8)
colnames(wave8) <- new_colnames1
wave8<- wave8[-c(1),] #keep frequencies for 'yes'
rownames(wave8)=c("Leisure") #label location

wave9 <- as.data.frame.matrix(wave9)
colnames(wave8) <- new_colnames1
wave9<- wave9[-c(1),] #keep frequencies for 'yes'
rownames(wave9)=c("Other") #label location

#Combine into a larger table and reformat for plot
wave_plot <- rbind(wave1, wave2, wave3, wave4, wave5, wave6, wave7, wave8, wave9)
wave_plot$Location <- rownames(wave_plot)
wave_plot <- wave_plot[,c(5,1:4)]

wave_plot<-wave_plot %>% as_tibble()%>%
  pivot_longer(!Location, 1:4, names_to="Wave", values_to="Count") 

waveplot <- wave_plot %>% 
  ggplot(aes(x=Wave,y=Count, fill=Location)) +
  geom_bar(position = "fill", stat="identity", color="black") + 
  scale_y_continuous(labels= scales::percent_format()) +
  scale_fill_brewer(palette="Paired")+
  xlab("Time Period") +
  ylab("Percentage") +
  guides(fill=guide_legend(reverse=TRUE)) #Create plot

#Frequency Plot by Contact Status

#Data in required format
c1 <- with(merge, table(home,contact)) 
c2 <- with(merge, table(someone_else_home, contact))
c3 <- with(merge, table(work, contact))
c4 <- with(merge, table(education, contact))
c5 <- with(merge, table(public_transport, contact))
c6 <- with(merge, table(essential_shop, contact))
c7 <- with(merge, table(healthcare_setting, contact))
c8 <- with(merge, table(leisure, contact))
c9 <- with(merge, table(other, contact))

#convert to data frames
c1 <- as.data.frame.matrix(c1)
new_colnames2 <- c("Confirmed Contact", "No Known Contact", "Suspected Contact")
colnames(c1) <- new_colnames2
c1<- c1[-c(1),] #keep frequencies for 'yes'
rownames(c1)=c("Home") #label location

c2 <- as.data.frame.matrix(c2)
colnames(c2) <- new_colnames2
c2<- c2[-c(1),] #keep frequencies for 'yes'
rownames(c2)=c("Someone else's home") #label location

c3 <- as.data.frame.matrix(c3)
colnames(c4) <- new_colnames2
c3<- c3[-c(1),] #keep frequencies for 'yes'
rownames(c3)=c("Work") #label location

c4 <- as.data.frame.matrix(c4)
colnames(c5) <- new_colnames2
c4<- c4[-c(1),] #keep frequencies for 'yes'
rownames(c4)=c("Place of education") #label location

c5 <- as.data.frame.matrix(c5)
colnames(c5) <- new_colnames2
c5<- c5[-c(1),] #keep frequencies for 'yes'
rownames(c5)=c("Public transport") #label location

c6 <- as.data.frame.matrix(c6)
colnames(c6) <- new_colnames2
c6<- c6[-c(1),] #keep frequencies for 'yes'
rownames(c6)=c("Essential shop") #label location

c7 <- as.data.frame.matrix(c7)
colnames(c7) <- new_colnames2
c7<- c7[-c(1),] #keep frequencies for 'yes'
rownames(c7)=c("Healthcare setting") #label location

c8 <- as.data.frame.matrix(c8)
colnames(c8) <- new_colnames2
c8<- c8[-c(1),] #keep frequencies for 'yes'
rownames(c8)=c("Leisure") #label location

c9 <- as.data.frame.matrix(c9)
colnames(c9) <- new_colnames2
c9<- c9[-c(1),] #keep frequencies for 'yes'
rownames(c9)=c("Other") #label location

#Combine into a larger table and reformat for plot
conplot <- rbind(c1, c2, c3, c4, c5, c6, c7, c8, c9)
conplot$Location <- rownames(conplot)
conplot <- conplot[,c(4,1,3,2)]
conplot$Overall <- rowSums(conplot[2:4])

conplot<-conplot %>% as_tibble()%>%
  pivot_longer(!Location, 1:5, names_to="Contact Status", values_to="Count") 

con_plot <- conplot %>% 
  ggplot(aes(x=`Contact Status`,y=Count, fill=Location)) +
  geom_bar(position = "fill", stat="identity", color="black") + 
  scale_y_continuous(labels= scales::percent_format()) +
  scale_fill_brewer(palette="Paired")+
  xlab("Contact Status") +
  ylab("Percentage") +
  guides(fill=guide_legend(reverse=TRUE)) #Create plot

#Frequency Plot by Age Group

#Data in required format
a1 <- with(merge, table(home,agecat)) 
a2 <- with(merge, table(someone_else_home, agecat))
a3 <- with(merge, table(work, agecat))
a4 <- with(merge, table(education, agecat))
a5 <- with(merge, table(public_transport, agecat))
a6 <- with(merge, table(essential_shop, agecat))
a7 <- with(merge, table(healthcare_setting, agecat))
a8 <- with(merge, table(leisure, agecat))
a9 <- with(merge, table(other, agecat))

#convert to data frames
a1 <- as.data.frame.matrix(a1)
new_colnames3 <- c("0-17", "18-34", "35-49", "50-64", "65+")
colnames(a1) <- new_colnames3
a1<- a1[-c(1),] #keep frequencies for 'yes'
rownames(a1)=c("Home") #label location

a2 <- as.data.frame.matrix(a2)
colnames(a2) <- new_colnames3
a2<- a2[-c(1),] #keep frequencies for 'yes'
rownames(a2)=c("Someone else's home") #label location

a3 <- as.data.frame.matrix(a3)
colnames(a3) <- new_colnames3
a3<- a3[-c(1),] #keep frequencies for 'yes'
rownames(a3)=c("Work") #label location

a4 <- as.data.frame.matrix(a4)
colnames(a4) <- new_colnames3
a4<- a4[-c(1),] #keep frequencies for 'yes'
rownames(a4)=c("Place of education") #label location

a5 <- as.data.frame.matrix(a5)
colnames(a5) <- new_colnames3
a5<- a5[-c(1),] #keep frequencies for 'yes'
rownames(a5)=c("Public transport") #label location

a6 <- as.data.frame.matrix(a6)
colnames(a6) <- new_colnames3
a6<- a6[-c(1),] #keep frequencies for 'yes'
rownames(a6)=c("Essential shop") #label location

a7 <- as.data.frame.matrix(a7)
colnames(a7) <- new_colnames3
a7<- a7[-c(1),] #keep frequencies for 'yes'
rownames(a7)=c("Healthcare setting") #label location

a8 <- as.data.frame.matrix(a8)
colnames(a8) <- new_colnames3
a8<- a8[-c(1),] #keep frequencies for 'yes'
rownames(a8)=c("Leisure") #label location

a9 <- as.data.frame.matrix(a9)
colnames(a9) <- new_colnames3
a9<- a9[-c(1),] #keep frequencies for 'yes'
rownames(a9)=c("Other") #label location

#Combine into a larger table and reformat for plot
ageplot <- rbind(a1, a2, a3, a4, a5, a6, a7, a8, a9)
ageplot$Location <- rownames(ageplot)
ageplot <- ageplot[,c(6,1,3,2,4,5)]

ageplot<-ageplot %>% as_tibble()%>%
  pivot_longer(!Location, 1:5, names_to="Age Group", values_to="Count") 

age_plot <- ageplot %>% 
  ggplot(aes(x=`Age Group`,y=Count, fill=Location)) +
  geom_bar(position = "fill", stat="identity", color="black") + 
  scale_y_continuous(labels= scales::percent_format()) +
  scale_fill_brewer(palette="Paired")+
  xlab("Age Group") +
  ylab("Percentage") +
  guides(fill=guide_legend(reverse=TRUE)) #Create plot

#Frequency Plot by Age Group for Contacts of Confirmed or Suspected Cases
age_contact_data <- merge %>%
  filter(contact2=="Confirmed or Suspected Contact")

#Data in required format
ac1 <- with(age_contact_data, table(home,agecat)) 
ac2 <- with(age_contact_data, table(someone_else_home, agecat))
ac3 <- with(age_contact_data, table(work, agecat))
ac4 <- with(age_contact_data, table(education, agecat))
ac5 <- with(age_contact_data, table(public_transport, agecat))
ac6 <- with(age_contact_data, table(essential_shop, agecat))
ac7 <- with(age_contact_data, table(healthcare_setting, agecat))
ac8 <- with(age_contact_data, table(leisure, agecat))
ac9 <- with(age_contact_data, table(other, agecat))

#convert to data frames
ac1 <- as.data.frame.matrix(ac1)
colnames(ac1) <- new_colnames3
ac1<- ac1[-c(1),] #keep frequencies for 'yes'
rownames(ac1)=c("Home") #label location

ac2 <- as.data.frame.matrix(ac2)
colnames(ac2) <- new_colnames3
ac2<- ac2[-c(1),] #keep frequencies for 'yes'
rownames(ac2)=c("Someone else's home") #label location

ac3 <- as.data.frame.matrix(ac3)
colnames(ac3) <- new_colnames3
ac3<- ac3[-c(1),] #keep frequencies for 'yes'
rownames(ac3)=c("Work") #label location

ac4 <- as.data.frame.matrix(ac4)
colnames(ac4) <- new_colnames3
ac4<- ac4[-c(1),] #keep frequencies for 'yes'
rownames(ac4)=c("Place of education") #label location

ac5 <- as.data.frame.matrix(ac5)
colnames(ac5) <- new_colnames3
ac5<- ac5[-c(1),] #keep frequencies for 'yes'
rownames(ac5)=c("Public transport") #label location

ac6 <- as.data.frame.matrix(ac6)
colnames(ac6) <- new_colnames3
ac6<- ac6[-c(1),] #keep frequencies for 'yes'
rownames(ac6)=c("Essential shop") #label location

ac7 <- as.data.frame.matrix(ac7)
colnames(ac7) <- new_colnames3
ac7<- ac7[-c(1),] #keep frequencies for 'yes'
rownames(ac7)=c("Healthcare setting") #label location

ac8 <- as.data.frame.matrix(ac8)
colnames(ac8) <- new_colnames3
ac8<- ac8[-c(1),] #keep frequencies for 'yes'
rownames(ac8)=c("Leisure") #label location

ac9 <- as.data.frame.matrix(ac9)
colnames(ac9) <- new_colnames3
ac9<- ac9[-c(1),] #keep frequencies for 'yes'
rownames(ac9)=c("Other") #label location

#Combine into a larger table and reformat for plot
ageconfplot <- rbind(ac1, ac2, ac3, ac4, ac5, ac6, ac7, ac8, ac9)
ageconfplot$Location <- rownames(ageconfplot)
ageconfplot <- ageconfplot[,c(6,1,3,2,4,5)]

ageconfplot<-ageconfplot %>% as_tibble()%>%
  pivot_longer(!Location, 1:5, names_to="Age Group", values_to="Count") 

age_conf_plot <- ageconfplot %>% 
  ggplot(aes(x=`Age Group`,y=Count, fill=Location)) +
  geom_bar(position = "fill", stat="identity", color="black") + 
  scale_y_continuous(labels= scales::percent_format()) +
  scale_fill_brewer(palette="Paired")+
  xlab("Age Group - Contacts of Confirmed or Suspected Cases") +
  ylab("Percentage") +
  guides(fill=guide_legend(reverse=TRUE)) #Create plot

#Combine plots into single figure
a <- age_plot+ylab("")
a2 <- age_conf_plot+ylab("")
w <- waveplot+ylab("")
c<- con_plot+ylab("")
figure_1 <- ggarrange(w,c,a,a2,
                      labels=c("a", "b", "c", "d"),
                      ncol=2, nrow=2, 
                      common.legend=TRUE,
                      legend="right"
)