
setwd("C:/Users/A/Documents/FS23_ML1/Project Data/Git_repository/ML01")
eqdata <- read.csv("significant-earthquake-database.tsv", header = TRUE, sep = "\t")

library("dplyr")

eqdata <- eqdata %>% filter(Year >= 1900)
eqdata <- eqdata %>% filter(!is.na(Mag))
eqdata <- eqdata %>% filter(!is.na(Deaths))
eqdata$Mag.full <- floor(eqdata$Mag)
sum(is.na(eqdata$Deaths))


eqdata.no.na.total.death <- eqdata %>%
  filter(!is.NA(eqdata$Location.Name))%>%
  str_split(eqdata$Location.Name,OR(":"," "))

# Piping total death to remove NAs from columns total death, magnitude,intensity and focal depth

eqdata.no.na.total.death <- eqdata %>%
  select(Year, Total.Deaths,Total.Death.Description,Mag, MMI.Int, Focal.Depth..km., Vol, Tsu, Location.Name, Longitude, Latitude)%>%
  filter(!is.na(Total.Deaths))%>%
  filter(!is.na(MMI.Int))%>%
  filter(!is.na(Focal.Depth..km.))

head(eqdata.no.na.total.death)
names(eqdata)


plot(Mag ~ factor(Year), data = eqdata)
plot(Mag ~ Latitude, data = eqdata)
plot(Mag ~ Longitude, data = eqdata)
plot(Mag ~ Focal.Depth..km., data = eqdata)
plot(Mag ~ factor(MMI.Int), data = eqdata)
plot(Mag ~ Total.Deaths, data = eqdata)

hist_mag <- ggplot(data = eqdata,
                   aes(x=0)) + 
  geom_histogram()



hist_int <- ggplot(data = eqdata,
                   aes(x=MMI.Int)) + 
  geom_histogram(aes(y = ..density..), color="black", fill="lightblue")+
  geom_density(lwd = 1.2,
               linetype = 1,
               colour = 2)

hist_int



######################################################################
#New Try Linear Regression:

 #Outcome continuous variable -> magnitude

# Magnitude and intensity: 
plot(Mag ~ factor(MMI.Int),data = eqdata)

plot(log(Total.Deaths)~ Mag,data = eqdata)
plot(log(Total.Deaths)~ factor(MMI.Int),data = eqdata)
plot(log(Total.Deaths)~ Focal.Depth..km.,data = eqdata)
plot(log(Total.Deaths)~ Longitude,data = eqdata)

lm.death <- lm(log(Total.Deaths) ~ Mag,data = eqdata)
summary(lm.death)
exp(1.01668)
exp(-3.72601)

  





















######################################################################

# There are no na values in Magnitude as we have filtered out those already.

library("dplyr")
library("tidyr")
library("stringr")

eqdata.no.na.total.death <- eqdata %>%
  filter(!is.NA(eqdata$Location.Name))%>%
  str_split(eqdata$Location.Name,OR(":"," "))

# Piping total death to remove NAs from columns total death, magnitude,intensity and focal depth

eqdata.no.na.total.death <- eqdata %>%
  select(Year, Total.Deaths,Total.Death.Description,Mag, MMI.Int, Focal.Depth..km., Vol, Tsu, Location.Name, Longitude, Latitude)%>%
  filter(!is.na(Total.Deaths))%>%
  filter(!is.na(MMI.Int))%>%
  filter(!is.na(Focal.Depth..km.))%>%
  mutate_at(c("Vol", "Tsu"), ~replace_na(., 0))
  

#Our filtered data set contains so many rows:
linear.count.rows.death <- sum(!is.na(eqdata.no.na.total.death$Total.Deaths))

str(eqdata.no.na.total.death)


lm.all <- lm(Total.Deaths ~ Mag + MMI.Int + Focal.Depth..km. + Year + Vol + Tsu + Latitude + Longitude, data = eqdata.no.na.total.death)
summary(lm.all)
coef(lm.all)


# Plot Magnitude vs. Total Death
qplot(y = Total.Deaths, x = Mag,
      data = eqdata.no.na.total.death)

lm.mag <- lm(Total.Deaths ~ Mag, data = eqdata.no.na.total.death)
summary(lm.mag)
coef(lm.mag)

ggplot(eqdata.no.na.total.death,aes(Mag,Total.Deaths)) +
  geom_point() +
  geom_smooth(method='lm',se=FALSE)

ggplot(eqdata.no.na.total.death,aes(Mag, Total.Deaths))+ 
  geom_boxplot()+
  aes(group = Mag)+
  geom_point(aes(colour = factor(Mag)), size = 4)

mean.Mag <- mean(eqdata.no.na.total.death$Mag)
mean.Mag
eqdata.no.na.total.death$Mag.centered <- eqdata.no.na.total.death$Mag - mean.Mag

lm.mag.centered <- lm(Total.Deaths ~ Mag.centered, data = eqdata.no.na.total.death)
summary(lm.mag.centered)
coef(lm.mag.centered)


ggplot(eqdata.no.na.total.death,aes(Mag.centered,Total.Deaths)) +
  geom_point() +
  geom_smooth(method='lm',se=FALSE)


# Plot Intensity vs. Total Death
qplot(y = Total.Deaths, x = MMI.Int,
      data = eqdata.no.na.total.death)

lm.MMI.Int <- lm(Total.Deaths ~ MMI.Int, data = eqdata.no.na.total.death)
summary(lm.MMI.Int)
coef(lm.MMI.Int)

ggplot(eqdata.no.na.total.death,aes(MMI.Int, Total.Deaths))+ 
  geom_boxplot()+
  aes(group = MMI.Int)+
  geom_point(aes(colour = factor(MMI.Int)), size = 4)


ggplot(eqdata.no.na.total.death,aes(MMI.Int,Total.Deaths)) +
  geom_point() +
  geom_smooth(method='lm',se=FALSE)

ggplot(nlme::eqdata.no.na.total.death, aes(MMI.Int, Total.Deaths)) + geom_boxplot()


eqdata.no.na.total.death

# Plot Focal Dept vs. Total Death

ggplot(eqdata.no.na.total.death,aes(Focal.Depth..km.,Total.Deaths)) +
  geom_point() +
  geom_smooth(method='lm',se=FALSE)

lm.Focal.Depth <- lm(Total.Deaths ~ Focal.Depth..km., data = eqdata.no.na.total.death)
summary(lm.Focal.Depth)
coef(lm.Focal.Depth)

# Plot Focal Dept vs. Total Death

lm.all <- lm(Total.Deaths ~ Mag + MMI.Int + Focal.Depth..km. + Year + Vol + Tsu, data = eqdata.no.na.total.death)
summary(lm.all)
coef(lm.all)






#############################3DAMAGE#################################33


# Piping total damage to remove NAs from columns total damage, magnitude,intensity and focal depth

eqdata.no.na.total.damage <- eqdata %>%
  select(Year, Total.Damage...Mil.,Total.Damage.Description,Mag, MMI.Int, Focal.Depth..km., Vol, Tsu, Location.Name, Longitude, Latitude)%>%
  filter(!is.na(Total.Damage...Mil.))%>%
  filter(!is.na(MMI.Int))%>%
  filter(!is.na(Focal.Depth..km.))%>%
  mutate_at(c("Vol", "Tsu"), ~replace_na(., 0))

#Our filtered data set contains so many rows:
linear.count.rows.damage <- sum(!is.na(eqdata.no.na.total.damage$Total.Damage...Mil.))

str(linear.count.rows.damage)



```{r linear regression fit model death6, message = FALSE, error = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, include = TRUE}

coef(lm.all.death6)

```

<br>
  
  ```{r linear regression fit model damage1, message = FALSE, error = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, include = TRUE}

lm.all.damage <- lm(Total.Damage...Mil. ~ Mag + MMI.Int + Focal.Depth..km. + Year + Vol + Tsu + Latitude + Longitude, data = eqdata.no.na.total.damage)
summary(lm.all.damage)

```

<br>
  
  ```{r linear regression fit model damage2, message = FALSE, error = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, include = TRUE}

coef(lm.all.damage)

```

```{r linear regression 3, message = FALSE, error = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, include = TRUE}


# Plot Magnitude vs. Total Damage

ggplot(eqdata.no.na.total.damage,aes(Mag,Total.Damage...Mil.)) +
  geom_point() +
  geom_smooth(method='lm',se=FALSE)

```

<br>
  
  ```{r linear regression 4, message = FALSE, error = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, include = TRUE}


# Plot Intensity vs. Total Damage

ggplot(eqdata.no.na.total.damage,aes(MMI.Int,Total.Damage...Mil.)) +
  geom_point() +
  geom_smooth(method='lm',se=FALSE)

```




names(eqdata)

#Checking NA in total death & total damage
sum(is.na(eqdata$Total.Deaths))
sum(is.na(eqdata$Total.Death.Description))
sum(is.na(eqdata$Total.Damage...Mil.))
sum(is.na(eqdata$Total.Damage.Description))
sum(is.na(eqdata$MMI.Int))
sum(is.na(eqdata$Focal.Depth..km))

#Checking 0 in total death and total damage
sum(eqdata$Total.Deaths == 0, na.rm = TRUE)
sum(eqdata$Total.Death.Description == 0, na.rm = TRUE)
sum(eqdata$Total.Damage...Mil == 0, na.rm = TRUE)
sum(eqdata$Total.Total.Damage.Description == 0, na.rm = TRUE)
sum(eqdata$Total.MMI.Int == 0, na.rm = TRUE)
sum(eqdata$Total.Focal.Depth == 0, na.rm = TRUE)

# Cleaning data: Total Death: Conclusion - many NAs but no zero values.
sum(is.na(eqdata$Total.Deaths))
sum(eqdata$Total.Deaths == 0, na.rm = TRUE)

# As we do not know the Total Death numbers where NAs are filled in, we filter them out.
eqdata.no.na.total.death <- eqdata %>% filter(!is.na(Total.Deaths))
sum(is.na(eqdata.no.na.total.death$Total.Deaths))

# NAs bei magnitude we have filtered out already
sum(is.na(eqdata.no.na.total.death$Mag))

# NAs at intensity:
sum(is.na(eqdata.no.na.total.death$MMI.Int))
# NAs at focal depth:
sum(is.na(eqdata.no.na.total.death$Focal.Depth..km.))

# NAs at tsunami -> shall be converted in 0 and 1
sum(is.na(eqdata.no.na.total.death$Tsu))
eqdata.no.na.total.death$Tsu

# NAs at vulcano -> shall be converted in 0 and 1
sum(is.na(eqdata.no.na.total.death$Vol))
eqdata.no.na.total.death$Vol


# Piping Total death - without NAs at magnitude,intensity and focal depth
################################################################3

eqdata.no.na.total.death <- eqdata %>% 
  filter(!is.na(Total.Deaths))%>%
  filter(!is.na(MMI.Int))%>%
  filter(!is.na(Focal.Depth..km.))

sum(is.na(eqdata.no.na.total.death$Total.Deaths))
sum(is.na(eqdata.no.na.total.death$Mag))
sum(is.na(eqdata.no.na.total.death$MMI.Int))
sum(is.na(eqdata.no.na.total.death$Focal.Depth..km.))

#Our filtered dataset contains so many rows:
sum(!is.na(eqdata.no.na.total.death$Total.Deaths))

# Plot Magnitude vs. Total Death
qplot(y = Total.Deaths, x = Mag,
      data = eqdata.no.na.total.death)

lm.mag <- lm(Total.Deaths ~ Mag, data = eqdata.no.na.total.death)
summary(lm.mag)
coef(lm.mag)

# Plot Intensity vs. Total Death
#################################################

# We filter out all observations, where na is in total death, intensity

# If we 
sum(!is.na(eqdata.no.na.total.death$MMI.Int))
sum(is.na(eqdata.no.na.total.death$MMI.Int))






eqdata.replace.na.death.damage <- eqdata %>% 
  mutate_at(c("Total.Death.Description","Total.Damage.Description"), ~replace_na(.,0))

sum(is.na(eqdata.replace.na.death.damage$Total.Death.Description))
sum(eqdata.replace.na.death.damage$Total.Death.Description == 0, na.rm = TRUE)
sum(is.na(eqdata.replace.na.death.damage$Total.Damage.Description))
sum(eqdata.replace.na.death.damage$Total.Damage.Description == 0, na.rm = TRUE)

qplot(y = Total.Deaths, x = Mag,
      data = eqdata.replace.na.death.damage)

qplot(y = Total.Deaths, x = Mag,
      data = eqdata)

lm.mag <- lm(Total.Deaths ~ Mag, data = eqdata.replace.na.death.damage)
summary(lm.mag)
coef(lm.mag)

mean.Mag <- mean(eqdata.replace.na.death.damage$Mag)
mean.Mag
eqdata.replace.na.death.damage$Mag.centered <- eqdata.replace.na.death.damage$Mag - mean.Mag

qplot(y = Total.Deaths, x = Mag.centered,
      data = eqdata.replace.na.death.damage)

lm.mag.centered <- lm(Total.Deaths ~ Mag.centered, data = eqdata.replace.na.death.damage)
summary(lm.mag.centered)
coef(lm.mag.centered)

#par(mfrow=c(1,1))
#plot(Total.Deaths ~ Mag, data = eqdata,
#     main = "Total Deaths against Magnitude",
#     ylab = "Total deaths")

# Magnitude vs. Total Death
# There are no na values in Magnitude as we have filtered out those already.

qplot(y = Total.Deaths, x = Mag,
      data = eqdata)

sum(is.na(eqdata$MMI.Int))
eqdata_int <- eqdata %>% filter(!is.na(MMI.Int))
sum(is.na(eqdata_int$MMI.Int))

qplot(y = Total.Deaths, x = Mag,
      data = eqdata)


sum(is.na(eqdata$MMI.Int))
eqdata_int <- eqdata %>% filter(!is.na(MMI.Int))
sum(is.na(eqdata_int$MMI.Int))
eqdata_int <- eqdata_int%>% filter(Focal.Depth..km. < 250)

par(mfrow=c(1,1))
plot(Total.Deaths ~ MMI.Int, data = eqdata,
     main = "Total Deaths against Intensity",
     ylab = "Total deaths")

boxplot(Total.Deaths ~ MMI.Int, data = eqdata,
        main = "Total Deaths against Intensity",
        ylab = "Total Deaths Decsription")

sum(is.na(eqdata$Focal.Depth..km.))
eqdata_focal.depth <- eqdata %>% filter(!is.na(Focal.Depth..km.))
sum(is.na(eqdata_focal.depth$Focal.Depth..km.))
eqdata_focal.depth <- eqdata_focal.depth %>% filter(Focal.Depth..km. < 250)

plot(Total.Deaths ~ Focal.Depth..km., data = eqdata_focal.depth,
     main = "Total Deaths against Focal.Depth",
     ylab = "Total deaths")

boxplot(Total.Deaths ~ Focal.Depth..km., data = eqdata,
        main = "Total Deaths against Focal.Depth",
        ylab = "Total Deaths Decsription")
```


```{r linear regression 1, message = FALSE, error = FALSE, warning = FALSE, eval = FALSE, echo = TRUE, include = TRUE}

names(eqdata)

par(mfrow=c(2,1))
plot(Deaths ~ Mag, data = eqdata,
     main = "Number of deaths during time - Primary effects")
plot(Total.Deaths ~ Mag, data = eqdata,
     main = "Number of deaths during time - Incl. Secondary effects")

par(mfrow=c(2,1))
plot(Deaths ~ Year, data = eqdata,
     col = Mag[c(c(0:2.99),c(3:5.99),c(6:8.99), c(9:12))],
     main = "Number of deaths during time - Primary effects")


plot(Total.Deaths ~ Year, data = eqdata,
     col = Mag
     main = "Number of deaths during time - Incl. Secondary effects")
