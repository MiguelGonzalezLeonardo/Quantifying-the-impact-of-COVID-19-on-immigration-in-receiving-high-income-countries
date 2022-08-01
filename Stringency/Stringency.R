
setwd("D:\\IIASA\\QUANTMIG\\Immigration_COVID\\Repository\\Stringency")


#Libraries
library(reshape)
library(stringr)
library(dplyr)
library(ggcorrplot)



######################################
########### Data and means ###########
######################################



#### Data stringency

data <- read.csv(file = 'Stringency_index.csv', sep = ',')

data <- melt(data, id=c("country_code", "country_name"))

#Substract and select year
data$variable2 <- str_sub(data$variable ,-2,-1)
data <- subset(data, variable2 == "20")

#Select countries
data <- subset(data, country_name == "Australia" | country_name == "Spain" | country_name == "Sweden" | country_name == "United States"
                 | country_name == "France" | country_name == "Germany" | country_name == "Italy" | country_name == "Canada"
                 | country_name == "Netherlands" | country_name == "Austria" | country_name == "Ireland" | country_name == "Norway"
                 | country_name == "Denmark" | country_name == "Switzerland" | country_name == "Finland")

#Yearly means
means <- aggregate(data$value, list(data$country_name), FUN=mean)
colnames(means) <- c("Country","Value")
means$Country[means$Country=="United States"]<-"USA"

#Data forecasting
data_Dif <- read.csv(file = 'Forecasting.csv', sep = ',')
data_Dif  <- data_Dif [, c('Country', 'Dif')]

#Join stringency index with forecasting data
means <- left_join(means, data_Dif, by ="Country", "")
means$Dif <- means$Dif * -1


### Data travel restrictions

data_T <- read.csv(file = 'Travel.csv', sep = ',')

data_T <- melt(data_T, id=c("country_code", "country_name"))

#Substract and select year
data_T$variable2 <- str_sub(data_T$variable ,-2,-1)
data_T <- subset(data_T, variable2 == "20")

#Select countries
data_T <- subset(data_T, country_name == "Australia" | country_name == "Spain" | country_name == "Sweden" | country_name == "United States"
               | country_name == "France" | country_name == "Germany" | country_name == "Italy" | country_name == "Canada"
               | country_name == "Netherlands" | country_name == "Austria" | country_name == "Ireland" | country_name == "Norway"
               | country_name == "Denmark" | country_name == "Switzerland" | country_name == "Finland")

#Yearly means
means_T <- aggregate(data_T$value, list(data_T$country_name), FUN=mean)
colnames(means_T) <- c("Country","Value_T")
means_T$Country[means_T$Country=="United States"]<-"USA"

#Join data
means <- left_join(means, means_T, by ="Country", "")


#### Data movements restrictions

data_Mov <- read.csv(file = 'Movements_rest.csv', sep = ',')

data_Mov <- melt(data_Mov, id=c("country_code", "country_name"))

#Substract and select year
data_Mov$variable2 <- str_sub(data_Mov$variable ,-2,-1)
data_Mov <- subset(data_Mov, variable2 == "20")

#Select countries
data_Mov <- subset(data_Mov, country_name == "Australia" | country_name == "Spain" | country_name == "Sweden" | country_name == "United States"
                 | country_name == "France" | country_name == "Germany" | country_name == "Italy" | country_name == "Canada"
                 | country_name == "Netherlands" | country_name == "Austria" | country_name == "Ireland" | country_name == "Norway"
                 | country_name == "Denmark" | country_name == "Switzerland" | country_name == "Finland")

#Yearly means
means_Mov <- aggregate(data_Mov$value, list(data_Mov$country_name), FUN=mean)
colnames(means_Mov) <- c("Country","Value_Mov")
means_Mov$Country[means_Mov$Country=="United States"]<-"USA"

#Join data
means <- left_join(means, means_Mov, by ="Country", "")


### Stay at home

data_home <- read.csv(file = 'Stay_home.csv', sep = ',')

data_home <- melt(data_home, id=c("country_code", "country_name"))

#Substract and select year
data_home$variable2 <- str_sub(data_home$variable ,-2,-1)
data_home <- subset(data_home, variable2 == "20")

#Select countries
data_home <- subset(data_home, country_name == "Australia" | country_name == "Spain" | country_name == "Sweden" | country_name == "United States"
                   | country_name == "France" | country_name == "Germany" | country_name == "Italy" | country_name == "Canada"
                   | country_name == "Netherlands" | country_name == "Austria" | country_name == "Ireland" | country_name == "Norway"
                   | country_name == "Denmark" | country_name == "Switzerland" | country_name == "Finland")

#Yearly means
means_home <- aggregate(data_home$value, list(data_home$country_name), FUN=mean)
colnames(means_home) <- c("Country","Value_home")
means_home$Country[means_home$Country=="United States"]<-"USA"

#Join data
means <- left_join(means, means_home, by ="Country", "")


### Work closing

data_W <- read.csv(file = 'Work_closure.csv', sep = ',')

data_W <- melt(data_W, id=c("country_code", "country_name"))

#Substract and select year
data_W$variable2 <- str_sub(data_W$variable ,-2,-1)
data_W <- subset(data_W, variable2 == "20")

#Select countries
data_W <- subset(data_W, country_name == "Australia" | country_name == "Spain" | country_name == "Sweden" | country_name == "United States"
                    | country_name == "France" | country_name == "Germany" | country_name == "Italy" | country_name == "Canada"
                    | country_name == "Netherlands" | country_name == "Austria" | country_name == "Ireland" | country_name == "Norway"
                    | country_name == "Denmark" | country_name == "Switzerland" | country_name == "Finland")

#Yearly means
means_W <- aggregate(data_W$value, list(data_W$country_name), FUN=mean)
colnames(means_W) <- c("Country","Value_W")
means_W$Country[means_W$Country=="United States"]<-"USA"

#Join data
means <- left_join(means, means_W, by ="Country", "")


### School closing

data_Sh <- read.csv(file = 'School_closing.csv', sep = ',')

data_Sh <- melt(data_Sh, id=c("country_code", "country_name"))

#Substract and select year
data_Sh$variable2 <- str_sub(data_Sh$variable ,-2,-1)
data_Sh <- subset(data_Sh, variable2 == "20")

#Select countries
data_Sh <- subset(data_Sh, country_name == "Australia" | country_name == "Spain" | country_name == "Sweden" | country_name == "United States"
                 | country_name == "France" | country_name == "Germany" | country_name == "Italy" | country_name == "Canada"
                 | country_name == "Netherlands" | country_name == "Austria" | country_name == "Ireland" | country_name == "Norway"
                 | country_name == "Denmark" | country_name == "Switzerland" | country_name == "Finland")

#Yearly means
means_Sh <- aggregate(data_Sh$value, list(data_Sh$country_name), FUN=mean)
colnames(means_Sh) <- c("Country","Value_Sh")
means_Sh$Country[means_Sh$Country=="United States"]<-"USA"

#Join data
means <- left_join(means, means_Sh, by ="Country", "")


#### Unemployment

data_U <- read.csv(file = 'Unemployment.csv', sep = ',')

data_U <- data_U[, c('Country', 'U_change')]

#Join data
means <- left_join(means, data_U, by ="Country", "")



###############################################
################# Heat map ####################
###############################################

#Country + immigration change
means$Dif <- round(-means$Dif, 1)
means$Country <- paste(means$Country, " (", means$Dif, "%)", sep='')
means <- means[order(means$Dif),]
means$Dif <- NULL
means$Value_I <- NULL

#Get maximun values
means %>% summarise_if(is.numeric, max)

#Calculate scales over 1
means2 <- means
means2$Value <- round(means$Value / 64.66803, 1)
means2$Value_T <- round(means$Value_T / 3.530055, 1)
means2$Value_Mov <- round(means$Value_Mov / 1.587432, 1)
means2$Value_home <- round(means$Value_home / 1.710383, 1)
means2$Value_W <- round(means$Value_W / 1.967213, 1)
means2$Value_Sh <- round(means$Value_Sh / 2.47541, 1)
means2$U_change <- round(means$U_change / 120.2, 1)
means2$U_change <- ifelse(means2$U_change>=0,means2$U_change,0)

colnames(means2) <- c("Country","Stringency index", "Travel restrictions", "Mobility restrictions",
                      "Stay-at-home", "Work closing", "School closing","Increase of unemployment")


#Add starts for statistically significant countries
means2$Country <- recode(means2$Country, "Australia (-59.9%)"="Australia (-59.9%)***",
                         "Austria (-11.1%)"="Austria (-11.1%)",
                         "Canada (-20.2%)"="Canada (-20.2%)***",
                         "Denmark (-13.7%)"="Denmark (-13.7%)",
                         "Finland (3.6%)"="Finland (3.6%)",        
                         "France (-26.5%)"="France (-26.5%)***",
                         "Germany (-21.9%)"="Germany (-21.9%)",
                         "Ireland (-13.3%)"="Ireland (-13.3%)",
                         "Italy (-21.6%)"="Italy (-21.6%)***",
                         "Netherlands (-15.5%)"="Netherlands (-15.5%)***",
                         "Norway (-25.5%)"="Norway (-25.5%)***",
                         "Spain (-45.4%)"="Spain (-45.4%)***",
                         "Sweden (-36.3%)"="Sweden (-36.3%)***",
                         "Switzerland (-4.4%)"="Switzerland (-4.4%)",
                         "USA (-27.2%)"="USA (-27.2%)***")

means2$Country <- as.factor(means2$Country)
levels(means2$Country)


### Plot

means3 <- as.matrix(means2 [,-1])
rownames(means3) <- means2$Country


bmp(file="Stringency.bmp", width = 6, height = 4, units = 'in', res = 300)

ggcorrplot(means3,
           outline.col = "gray30",
           lab = TRUE, lab_size = 1.8,digits = 1,
           method = "circle")+
  
  #coord_flip()+
  
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size =7.5, angle = 90, hjust =1, vjust = 0.5),
        axis.text.y = element_text(size = 7.5),
        
        legend.position = "none")

dev.off()

