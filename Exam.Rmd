---
title: "Predictive Modeling Exercise"
author: "Yuyan Shi"
date: "8/17/2020"
output:
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE}
library(mosaic)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(viridis)
library(plotly)
library(rworldmap)
library(maps)
library(dplyr)
library(ggmap)
library(quantmod)
library(dbplyr)
library(foreach)
library(tm) 
library(magrittr)
library(slam)
library(proxy)
library(caret)
library(plyr)
library(e1071)
library(arules) 
library(arulesViz)
library(randomForest)
library(class)
set.seed(65)
```

# Visual Story Telling Part 1: Green Buildings

```{r warning=FALSE, include=FALSE}
greenbuildings <- read.csv("~/Desktop/Exam/greenbuildings.csv")
df_green = greenbuildings[c(3,5,6,7,8,10,11,14,15,16)]
df_green['class'] = 'C'
df_green$class[df_green$class_a == 1] <- 'A'
df_green$class[df_green$class_b == 1] <- 'B'


df_green['green_rating'] = as.factor(df_green$green_rating)
df_green['class_a'] = as.factor(df_green$class_a)
df_green['class_b'] = as.factor(df_green$class_b)
df_green['net'] = as.factor(df_green$net)
df_green['amenities'] = as.factor(df_green$amenities)


df_green = df_green[!(df_green$leasing_rate < 10),]
```

Described stats guru would be perfect if the green or not is the only attribute to impact the rent. Based on the question, there are actually confounders for the relationship between rent and green status ---- such as, **size, leasing_rate, stories, age, etc.**. They share the same property , which is that they are different from buildings in the same cluster. On the contrary, variables such as employment growth rate and costs are the same for both green_building and non_green building in the same cluster. Thus, our goal is to predict 

Let's do some quick plots!

We can see the difference is not obvious in class a and b. In class c, the rent of green-building is more than that of non-green building.
```{r echo=FALSE, warning=FALSE}
ggplot(data = df_green) + 
  geom_boxplot(aes(x = class, y = Rent, fill = green_rating)) 
```

For the net contract, the diffence between two groups is subtle. But the rent is higher in green-building group without net contract.
```{r echo=FALSE, warning=FALSE}
ggplot(data = df_green) + 
  geom_boxplot(aes(x = net, y = Rent, fill = green_rating))
```

In both, the rent on green-building is higher.
```{r echo=FALSE, warning=FALSE}
ggplot(data = df_green) + 
  geom_boxplot(aes(x = amenities, y = Rent, fill = green_rating)) 
```

Not a lot of useful information is gained from the scatter plot.
```{r echo=FALSE, warning=FALSE}
ggplot(data = df_green) + 
  geom_point(mapping = aes(x = size, y = Rent, color = green_rating))
```
```{r echo=FALSE, warning=FALSE}
ggplot(data = df_green) + 
  geom_point(mapping = aes(x = leasing_rate, y = Rent, color = green_rating))
```
```{r echo=FALSE, warning=FALSE}
ggplot(data = df_green) + 
  geom_point(mapping = aes(x = stories, y = Rent, color = green_rating))
```
```{r echo=FALSE, warning=FALSE}
ggplot(data = df_green) + 
  geom_point(mapping = aes(x = age, y = Rent, color = green_rating))
```

Only predicting log(rent) on green_rating, we can see the rent higher for green building.
```{r echo=FALSE, warning=FALSE}
model = lm(Rent ~ green_rating, data = df_green)
summary(model)
```

```{r echo=FALSE, warning=FALSE}
model = lm(log(Rent)~ . - class - size + log(size), data = df_green)
summary(model)
```

```{r echo=FALSE, warning=FALSE}
summary(df_green)
```

Based on stats guru, the size is 250000, greater than the mean, so the extra revenue should be higher. The location is pretty good (just across from downtown), assuming class a, higher rent. So I think there should be more than $2.60 per square foot per year. It is definitely promising investment.

# Visual Story Telling Part 2: Flights at ABIA

Notes: I tried to put multiple plots in one window ut it fails. I think it's becasue the bubble maps are interactive.
```{r}
ABIA <- read.csv("~/Desktop/Exam/ABIA.csv")
airport <- read.csv("~/Desktop/Exam/airport.csv")
ABIA = ABIA[c(4,15:18)]
airport = airport[-1]
```

```{r}
## Monday Arrival in Austin

x = ABIA[ABIA$DayOfWeek==1,]
ABIA_dep_Mon = x[x$Dest=='AUS',]
ABIA_dep_Mon = ABIA_dep_Mon[,c(1,2,4)]
ABIA_dep_Mon = aggregate(ArrDelay ~ Origin, ABIA_dep_Mon, mean)
ABIA_dep_Mon <- merge(ABIA_dep_Mon, airport,by.x = "Origin", by.y = "iata_code")
ABIA_dep_Mon$q <- with(ABIA_dep_Mon, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_dep_Mon$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_dep_Mon$q <- as.ordered(ABIA_dep_Mon$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_dep_Mon <- plot_geo(ABIA_dep_Mon, locationmode = 'USA-states', sizes = c(1, 250))
fig_dep_Mon <- fig_dep_Mon %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_dep_Mon$Origin, "<br />", ABIA_dep_Mon$ArrDelay, " minutes")
)
fig_dep_Mon <- fig_dep_Mon %>% layout(title = 'Averaged Delayed Arrival Time for Flight Arrived at Austin on Monday by Airports', geo = g)
```

```{r}
## Tuesday Arrival in Austin

x = ABIA[ABIA$DayOfWeek==2,]
ABIA_dep_Tu = x[x$Dest=='AUS',]
ABIA_dep_Tu = ABIA_dep_Tu[,c(1,2,4)]
ABIA_dep_Tu = aggregate(ArrDelay ~ Origin, ABIA_dep_Tu, mean)
ABIA_dep_Tu <- merge(ABIA_dep_Tu, airport,by.x = "Origin", by.y = "iata_code")
ABIA_dep_Tu$q <- with(ABIA_dep_Tu, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_dep_Tu$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_dep_Tu$q <- as.ordered(ABIA_dep_Tu$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_dep_Tu <- plot_geo(ABIA_dep_Tu, locationmode = 'USA-states', sizes = c(1, 250))
fig_dep_Tu <- fig_dep_Tu %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_dep_Tu$Origin, "<br />", ABIA_dep_Tu$ArrDelay, " minutes")
)
fig_dep_Tu <- fig_dep_Tu %>% layout(title = 'Average Delayed Arrival Time for Flight Arrived at Austin on Tuesday by Airports', geo = g)
```

```{r}
## Wednsday Arrival in Austin

x = ABIA[ABIA$DayOfWeek==3,]
ABIA_dep_W = x[x$Dest=='AUS',]
ABIA_dep_W = ABIA_dep_W[,c(1,2,4)]
ABIA_dep_W = aggregate(ArrDelay ~ Origin, ABIA_dep_W, mean)
ABIA_dep_W <- merge(ABIA_dep_W, airport,by.x = "Origin", by.y = "iata_code")
ABIA_dep_W$q <- with(ABIA_dep_W, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_dep_W$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_dep_W$q <- as.ordered(ABIA_dep_W$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_dep_W <- plot_geo(ABIA_dep_W, locationmode = 'USA-states', sizes = c(1, 250))
fig_dep_W <- fig_dep_W %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_dep_W$Origin, "<br />", ABIA_dep_W$ArrDelay, " minutes")
)
fig_dep_W <- fig_dep_W %>% layout(title = 'Averaged Delayed Arrival Time for Flight Arrived at Austin on Wednsday by Airports', geo = g)
```

```{r}
## Thursday Arrival in Austin

x = ABIA[ABIA$DayOfWeek==4,]
ABIA_dep_Th = x[x$Dest=='AUS',]
ABIA_dep_Th = ABIA_dep_Th[,c(1,2,4)]
ABIA_dep_Th = aggregate(ArrDelay ~ Origin, ABIA_dep_Th, mean)
ABIA_dep_Th <- merge(ABIA_dep_Th, airport,by.x = "Origin", by.y = "iata_code")
ABIA_dep_Th$q <- with(ABIA_dep_Th, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_dep_Th$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_dep_Th$q <- as.ordered(ABIA_dep_Th$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_dep_Th <- plot_geo(ABIA_dep_Th, locationmode = 'USA-states', sizes = c(1, 250))
fig_dep_Th <- fig_dep_Th %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_dep_Th$Origin, "<br />", ABIA_dep_Th$ArrDelay, " minutes")
)
fig_dep_Th <- fig_dep_Th %>% layout(title = 'Averaged Delayed Arrival Time for Flight Arrived at Austin on Thursday by Airports', geo = g)
```

```{r}
## Friday Arrival in Austin

x = ABIA[ABIA$DayOfWeek==5,]
ABIA_dep_F = x[x$Dest=='AUS',]
ABIA_dep_F = ABIA_dep_F[,c(1,2,4)]
ABIA_dep_F = aggregate(ArrDelay ~ Origin, ABIA_dep_F, mean)
ABIA_dep_F <- merge(ABIA_dep_F, airport,by.x = "Origin", by.y = "iata_code")
ABIA_dep_F$q <- with(ABIA_dep_F, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_dep_F$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_dep_F$q <- as.ordered(ABIA_dep_F$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_dep_F <- plot_geo(ABIA_dep_F, locationmode = 'USA-states', sizes = c(1, 250))
fig_dep_F <- fig_dep_F %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_dep_F$Origin, "<br />", ABIA_dep_F$ArrDelay, " minutes")
)
fig_dep_F <- fig_dep_F %>% layout(title = 'Averaged Delayed Arrival Time for Flight Arrived at Austin on Friday by Airports', geo = g)
```

```{r}
## Saturday Arrival in Austin
x = ABIA[ABIA$DayOfWeek==6,]
ABIA_dep_Sat = x[x$Dest=='AUS',]
ABIA_dep_Sat = ABIA_dep_Sat[,c(1,2,4)]
ABIA_dep_Sat = aggregate(ArrDelay ~ Origin, ABIA_dep_Sat, mean)
ABIA_dep_Sat <- merge(ABIA_dep_Sat, airport,by.x = "Origin", by.y = "iata_code")
ABIA_dep_Sat$q <- with(ABIA_dep_Sat, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_dep_Sat$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_dep_Sat$q <- as.ordered(ABIA_dep_Sat$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_dep_Sat <- plot_geo(ABIA_dep_Sat, locationmode = 'USA-states', sizes = c(1, 250))
fig_dep_Sat <- fig_dep_Sat %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_dep_Sat$Origin, "<br />", ABIA_dep_Sat$ArrDelay, " minutes")
)
fig_dep_Sat <- fig_dep_Sat %>% layout(title = 'Averaged Delayed Arrival Time for Flight Arrived at Austin on Saturday by Airports', geo = g)
```

```{r}
## Saturday Arrival in Austin
x = ABIA[ABIA$DayOfWeek==7,]
ABIA_dep_Sun = x[x$Dest=='AUS',]
ABIA_dep_Sun = ABIA_dep_Sun[,c(1,2,4)]
ABIA_dep_Sun = aggregate(ArrDelay ~ Origin, ABIA_dep_Sun, mean)
ABIA_dep_Sun <- merge(ABIA_dep_Sun, airport,by.x = "Origin", by.y = "iata_code")
ABIA_dep_Sun$q <- with(ABIA_dep_Sun, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_dep_Sun$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_dep_Sun$q <- as.ordered(ABIA_dep_Sun$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_dep_Sun <- plot_geo(ABIA_dep_Sun, locationmode = 'USA-states', sizes = c(1, 250))
fig_dep_Sun <- fig_dep_Sun %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_dep_Sun$Origin, "<br />", ABIA_dep_Sun$ArrDelay, " minutes")
)
fig_dep_Sun <- fig_dep_Sun %>% layout(title = 'Averaged Delayed Arrival Time for Flight Arrived at Austin on Sunday by Airports', geo = g)
```

```{r echo=FALSE, warning=FALSE}
fig_dep_Mon
fig_dep_Tu
fig_dep_W
fig_dep_Th
fig_dep_F
fig_dep_Sat
fig_dep_Sun
```

```{r}
## Monday Departure from Austin

x = ABIA[ABIA$DayOfWeek==1,]
ABIA_arr_Mon = x[x$Origin=='AUS',]
ABIA_arr_Mon = ABIA_arr_Mon[,c(1,2,5)]
ABIA_arr_Mon = aggregate(ArrDelay ~ Dest, ABIA_arr_Mon, mean)
ABIA_arr_Mon <- merge(ABIA_arr_Mon, airport,by.x = "Dest", by.y = "iata_code")
ABIA_arr_Mon$q <- with(ABIA_arr_Mon, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_arr_Mon$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_arr_Mon$q <- as.ordered(ABIA_arr_Mon$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_arr_Mon <- plot_geo(ABIA_arr_Mon, locationmode = 'USA-states', sizes = c(1, 250))
fig_arr_Mon <- fig_arr_Mon %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_arr_Mon$Dest, "<br />", ABIA_arr_Mon$ArrDelay, " minutes")
)
fig_arr_Mon <- fig_arr_Mon %>% layout(title = 'Averaged Delayed Depature Time for Flight Arrived at Austin on Monday by Airports', geo = g)
```

```{r}
## Tuesday Departure from Austin

x = ABIA[ABIA$DayOfWeek==2,]
ABIA_arr_Tu = x[x$Origin=='AUS',]
ABIA_arr_Tu = ABIA_arr_Tu[,c(1,2,5)]
ABIA_arr_Tu = aggregate(ArrDelay ~ Dest, ABIA_arr_Tu, mean)
ABIA_arr_Tu <- merge(ABIA_arr_Tu, airport,by.x = "Dest", by.y = "iata_code")
ABIA_arr_Tu$q <- with(ABIA_arr_Tu, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_arr_Tu$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_arr_Tu$q <- as.ordered(ABIA_arr_Tu$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_arr_Tu <- plot_geo(ABIA_arr_Tu, locationmode = 'USA-states', sizes = c(1, 250))
fig_arr_Tu <- fig_arr_Tu %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_arr_Tu$Dest, "<br />", ABIA_arr_Tu$ArrDelay, " minutes")
)
fig_arr_Tu <- fig_arr_Tu %>% layout(title = 'Averaged Delayed Depature Time for Flight Arrived at Austin on Tuesday by Airports', geo = g)
```

```{r}
## Wednesday Departure from Austin

x = ABIA[ABIA$DayOfWeek==3,]
ABIA_arr_W = x[x$Origin=='AUS',]
ABIA_arr_W = ABIA_arr_W[,c(1,2,5)]
ABIA_arr_W = aggregate(ArrDelay ~ Dest, ABIA_arr_W, mean)
ABIA_arr_W <- merge(ABIA_arr_W, airport,by.x = "Dest", by.y = "iata_code")
ABIA_arr_W$q <- with(ABIA_arr_W, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_arr_W$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_arr_W$q <- as.ordered(ABIA_arr_W$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_arr_W <- plot_geo(ABIA_arr_W, locationmode = 'USA-states', sizes = c(1, 250))
fig_arr_W <- fig_arr_W %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_arr_W$Dest, "<br />", ABIA_arr_W$ArrDelay, " minutes")
)
fig_arr_W <- fig_arr_W %>% layout(title = 'Averaged Delayed Depature Time for Flight Arrived at Austin on Wednesday by Airports', geo = g)
```

```{r}
## Thursday Departure from Austin

x = ABIA[ABIA$DayOfWeek==4,]
ABIA_arr_Th = x[x$Origin=='AUS',]
ABIA_arr_Th = ABIA_arr_Th[,c(1,2,5)]
ABIA_arr_Th = aggregate(ArrDelay ~ Dest, ABIA_arr_Th, mean)
ABIA_arr_Th <- merge(ABIA_arr_Th, airport,by.x = "Dest", by.y = "iata_code")
ABIA_arr_Th$q <- with(ABIA_arr_Th, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_arr_Th$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_arr_Th$q <- as.ordered(ABIA_arr_Th$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_arr_Th <- plot_geo(ABIA_arr_Th, locationmode = 'USA-states', sizes = c(1, 250))
fig_arr_Th <- fig_arr_Th %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_arr_Th$Dest, "<br />", ABIA_arr_Th$ArrDelay, " minutes")
)
fig_arr_Th <- fig_arr_Th %>% layout(title = 'Averaged Delayed Depature Time for Flight Arrived at Austin on Thursday by Airports', geo = g)
```

```{r}
## Friday Departure from Austin

x = ABIA[ABIA$DayOfWeek==5,]
ABIA_arr_F = x[x$Origin=='AUS',]
ABIA_arr_F = ABIA_arr_F[,c(1,2,5)]
ABIA_arr_F = aggregate(ArrDelay ~ Dest, ABIA_arr_F, mean)
ABIA_arr_F <- merge(ABIA_arr_F, airport,by.x = "Dest", by.y = "iata_code")
ABIA_arr_F$q <- with(ABIA_arr_F, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_arr_F$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_arr_F$q <- as.ordered(ABIA_arr_F$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_arr_F <- plot_geo(ABIA_arr_F, locationmode = 'USA-states', sizes = c(1, 250))
fig_arr_F <- fig_arr_F %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_arr_F$Dest, "<br />", ABIA_arr_F$ArrDelay, " minutes")
)
fig_arr_F <- fig_arr_F %>% layout(title = 'Averaged Delayed Depature Time for Flight Arrived at Austin on Friday by Airports', geo = g)
```

```{r}
## Saturday Departure from Austin

x = ABIA[ABIA$DayOfWeek==6,]
ABIA_arr_Sat = x[x$Origin=='AUS',]
ABIA_arr_Sat = ABIA_arr_Sat[,c(1,2,5)]
ABIA_arr_Sat = aggregate(ArrDelay ~ Dest, ABIA_arr_Sat, mean)
ABIA_arr_Sat <- merge(ABIA_arr_Sat, airport,by.x = "Dest", by.y = "iata_code")
ABIA_arr_Sat$q <- with(ABIA_arr_Sat, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_arr_Sat$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_arr_Sat$q <- as.ordered(ABIA_arr_Sat$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_arr_Sat <- plot_geo(ABIA_arr_Sat, locationmode = 'USA-states', sizes = c(1, 250))
fig_arr_Sat <- fig_arr_Sat %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_arr_Sat$Dest, "<br />", ABIA_arr_Sat$ArrDelay, " minutes")
)
fig_arr_Sat <- fig_arr_Sat %>% layout(title = 'Averaged Delayed Depature Time for Flight Arrived at Austin on Saturday by Airports', geo = g)
```

```{r}
## Sunday Departure from Austin

x = ABIA[ABIA$DayOfWeek==7,]
ABIA_arr_Sun = x[x$Origin=='AUS',]
ABIA_arr_Sun = ABIA_arr_Sun[,c(1,2,5)]
ABIA_arr_Sun = aggregate(ArrDelay ~ Dest, ABIA_arr_Sun, mean)
ABIA_arr_Sun <- merge(ABIA_arr_Sun, airport,by.x = "Dest", by.y = "iata_code")
ABIA_arr_Sun$q <- with(ABIA_arr_Sun, cut(ArrDelay, quantile(ArrDelay)))
levels(ABIA_arr_Sun$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
ABIA_arr_Sun$q <- as.ordered(ABIA_arr_Sun$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig_arr_Sun <- plot_geo(ABIA_arr_Sun, locationmode = 'USA-states', sizes = c(1, 250))
fig_arr_Sun <- fig_arr_Sun %>% add_markers(
  x = ~long, y = ~lat, size = ~ArrDelay, color = ~q, hoverinfo = "text",
  text = ~paste(ABIA_arr_Sun$Dest, "<br />", ABIA_arr_Sun$ArrDelay, " minutes")
)
fig_arr_Sun <- fig_arr_Sun %>% layout(title = 'Averaged Delayed Depature Time for Flight Arrived at Austin on Sunday by Airports', geo = g)
```

```{r echo=FALSE, warning=FALSE}
fig_arr_Mon
fig_arr_Tu
fig_arr_W
fig_arr_Th
fig_arr_F
fig_arr_Sat
fig_arr_Sun
```

# Portfolio Modeling
I set a seed for this problem since I don't want the outcome would be different everytime.
Portfolio 1 includes three ETFs from oil & gas sectors.
```{r warning=FALSE}
mystocks = c("UNG", "DBO", "DBE")
myprices = getSymbols(mystocks, from = "2015-08-10")

for(ticker in mystocks) {
	expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
	eval(parse(text=expr))
}

all_returns = cbind(ClCl(UNGa),
								ClCl(DBOa),
								ClCl(DBEa))
all_returns = as.matrix(na.omit(all_returns))
```

```{r}
total_wealth = 100000
weights = c(0.3,0.3,0.4)
holdings = weights * total_wealth
n_days = 20
wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
for(today in 1:n_days) {
	return.today = resample(all_returns, 1, orig.ids=FALSE)
	holdings = holdings + holdings*return.today
	total_wealth = sum(holdings)
	wealthtracker[today] = total_wealth
}
total_wealth
plot(wealthtracker, type='l')
```

```{r}
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
	total_wealth = initial_wealth
	weights = c(0.3,0.3,0.4)
	holdings = weights * total_wealth
	n_days = 20
	wealthtracker = rep(0, n_days)
	for(today in 1:n_days) {
		return.today = resample(all_returns, 1, orig.ids=FALSE)
		holdings = holdings + holdings*return.today
		total_wealth = sum(holdings)
		wealthtracker[today] = total_wealth
	}
	wealthtracker
}
head(sim1)
hist(sim1[,n_days], 25)

# Profit/loss
mean(sim1[,n_days])
mean(sim1[,n_days] - initial_wealth)
hist(sim1[,n_days]- initial_wealth, breaks=30)

# 5% value at risk:
quantile(sim1[,n_days]- initial_wealth, prob=0.05)
```
Portfolio 2
```{r warning=FALSE}
mystocks = c("BOND","VNQ","IYR","MNA","JJN","VCSH","JPST","BND")
myprices = getSymbols(mystocks, from = "2015-08-10")

for(ticker in mystocks) {
	expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
	eval(parse(text=expr))
}

all_returns = cbind(ClCl(BONDa),
								ClCl(VNQa),
								ClCl(IYRa),
								ClCl(MNAa),
								ClCl(JJNa),
								ClCl(VCSHa),
								ClCl(JPSTa),
								ClCl(BNDa))
all_returns = as.matrix(na.omit(all_returns))
```

```{r echo=FALSE, warning=FALSE}
total_wealth = 100000
weights = c(0.1,0.2,0.1,0.1,0.1,0.1,0.1,0.2)
holdings = weights * total_wealth
n_days = 20
wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
for(today in 1:n_days) {
	return.today = resample(all_returns, 1, orig.ids=FALSE)
	holdings = holdings + holdings*return.today
	total_wealth = sum(holdings)
	wealthtracker[today] = total_wealth
}
total_wealth
plot(wealthtracker, type='l')
```

```{r echo=FALSE, warning=FALSE}
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
	total_wealth = initial_wealth
	weights = c(0.1,0.2,0.1,0.1,0.1,0.1,0.1,0.2)
	holdings = weights * total_wealth
	n_days = 20
	wealthtracker = rep(0, n_days)
	for(today in 1:n_days) {
		return.today = resample(all_returns, 1, orig.ids=FALSE)
		holdings = holdings + holdings*return.today
		total_wealth = sum(holdings)
		wealthtracker[today] = total_wealth
	}
	wealthtracker
}
head(sim1)
hist(sim1[,n_days], 25)

# Profit/loss
mean(sim1[,n_days])
mean(sim1[,n_days] - initial_wealth)
hist(sim1[,n_days]- initial_wealth, breaks=30)

# 5% value at risk:
quantile(sim1[,n_days]- initial_wealth, prob=0.05)
```
Portfolio 3
```{r warning=FALSE}
mystocks = c("AGG","ADME","SHY","SHV","UUP","FXE","USO","DBB")
myprices = getSymbols(mystocks, from = "2015-08-10")

for(ticker in mystocks) {
	expr = paste0(ticker, "a = adjustOHLC(", ticker, ")")
	eval(parse(text=expr))
}

all_returns = cbind(ClCl(AGGa),
								ClCl(ADMEa),
								ClCl(SHYa),
								ClCl(SHVa),
								ClCl(UUPa),
								ClCl(FXEa),
								ClCl(USOa),
								ClCl(DBBa))
all_returns = as.matrix(na.omit(all_returns))
```

```{r echo=FALSE, warning=FALSE}
total_wealth = 100000
weights = c(0.1,0.1,0.2,0.2,0.1,0.1,0.1,0.1)
holdings = weights * total_wealth
n_days = 20
wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
for(today in 1:n_days) {
	return.today = resample(all_returns, 1, orig.ids=FALSE)
	holdings = holdings + holdings*return.today
	total_wealth = sum(holdings)
	wealthtracker[today] = total_wealth
}
total_wealth
plot(wealthtracker, type='l')
```

```{r echo=FALSE, warning=FALSE}
initial_wealth = 100000
sim1 = foreach(i=1:5000, .combine='rbind') %do% {
	total_wealth = initial_wealth
	weights = c(0.1,0.2,0.1,0.1,0.1,0.1,0.1,0.2)
	holdings = weights * total_wealth
	n_days = 20
	wealthtracker = rep(0, n_days)
	for(today in 1:n_days) {
		return.today = resample(all_returns, 1, orig.ids=FALSE)
		holdings = holdings + holdings*return.today
		total_wealth = sum(holdings)
		wealthtracker[today] = total_wealth
	}
	wealthtracker
}
head(sim1)
hist(sim1[,n_days], 25)

# Profit/loss
mean(sim1[,n_days])
mean(sim1[,n_days] - initial_wealth)
hist(sim1[,n_days]- initial_wealth, breaks=30)

# 5% value at risk:
quantile(sim1[,n_days]- initial_wealth, prob=0.05)
```

The first portforlio is all about the gas and oil ETFs. I would think it would have the least profit because it is not diversiied. Surprisingly, it has the highest VaR (13381) if we see it as positive number. I guess it might be because the oil and gas ETFs perform better these years. 
The second portfolio is consisted of ETFs of real estate, bond, and corporate bond. This portfolio would make about 5123 profit. I would say 5% in 20 days is still pretty good. 
The third portfolio includes two government bond ETFs, two currency ETFs, and each one from bond market, hedge fund, metal and oil&gas. I think this protfolio is the most diversified but it performs the worst out of the three. The VaR is 4034.

# Market Segmentation
I dropped columns __photo_sharing__ and __religion__ because I think they ust be overlapped with other categories. Before doing some clustering code, I regroup the 36 groups by similarity. Thus, there are only 12 groups now.      
* __Chatty__ group is the sum of __chatter__ and __uncategorized__.       
* __Up_to_date__ group is the sum of __current_events__, __politics__, and __news__.         
* Healthy&Outgoing group is the sum of __travel__, __healthy_nutrition__, __outdoors__, __automotive__, and __personal_fitness__.         
* __Homie__ group is the sum of __tv_film__, __music__, __online_gaming__, and __computers__.            
* __Sports__ group is the sum of __sports_fandom__ and __sports_playing__.        
* __Foodie__ group is the sum of __food__ and __cooking__.            
* __Businessman__ group is the sum of __business__ and __small_business__.            
* __Familish__ group is the sum of __family__, __home and garden__, and __parenting__.      
* __Shop__ group is the sum of __shopping__, __beauty__, and __fashion__.       
* __Young__ group is the sum of __college_uni__, __school__, and __dating__.        
* __Spam_user__ group is the sum of __spam__ and __adult__.            
* __Creative__ group is the sum of __crafts__ and __art__.
```{r}
social_marketing <- read.csv("~/Desktop/Exam/social_marketing.csv", row.names=1)
social_marketing = social_marketing[,-c(4,27)]
social_marketing["chatty"] = social_marketing['chatter'] + social_marketing['uncategorized']
social_marketing["up_to_date"] = social_marketing['current_events'] + social_marketing['politics'] + social_marketing['news']
social_marketing["healthy_outgoing"] = social_marketing["travel"] + social_marketing["health_nutrition"] + social_marketing["outdoors"] + social_marketing["automotive"] + social_marketing["personal_fitness"]
social_marketing["homie"] = social_marketing["tv_film"] + social_marketing["music"] + social_marketing["online_gaming"] + social_marketing["computers"]
social_marketing["sports"] = social_marketing["sports_fandom"] + social_marketing["sports_playing"]
social_marketing["foodie"] = social_marketing["food"] + social_marketing["cooking"]
social_marketing["businessman"] = social_marketing["business"] + social_marketing["small_business"]
social_marketing["familish"] = social_marketing["family"] + social_marketing["home_and_garden"] + social_marketing["parenting"]
social_marketing["shop"] = social_marketing["shopping"] + social_marketing["beauty"] + social_marketing["fashion"]
social_marketing["young"] = social_marketing["college_uni"] + social_marketing["school"] + social_marketing["dating"]
social_marketing["spam_user"] = social_marketing["spam"] + social_marketing["adult"]
social_marketing["creative"] = social_marketing["crafts"] + social_marketing["art"]
social_marketing = social_marketing[,-c(2:35)]
```

```{r echo=FALSE, warning=FALSE}
X = scale(social_marketing, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

# Run k-means with 6 clusters and 25 starts
clust1 = kmeans(X, 6, nstart=25)

social_marketing['Cluster'] = clust1$cluster
cluster1 = social_marketing[social_marketing$Cluster==1,]
cluster2 = social_marketing[social_marketing$Cluster==2,]
cluster3 = social_marketing[social_marketing$Cluster==3,]
cluster4 = social_marketing[social_marketing$Cluster==4,]
cluster5 = social_marketing[social_marketing$Cluster==5,]
cluster6 = social_marketing[social_marketing$Cluster==6,]
print("cluster1's column means")
colMeans(cluster1)
print("cluster2's column means")
colMeans(cluster2)
print("cluster3's column means")
colMeans(cluster3)
print("cluster4's column means")
colMeans(cluster4)
print("cluster5's column means")
colMeans(cluster5)
print("cluster6's column means")
colMeans(cluster6)
```
After running the KMeans clustering model and looking at the statistics of each cluster, there are 6 clusters I got.      
* Cluster 1 is the group of followers who like posting tweets about sports and family.      
* Cluster 2 is the group of people who are concerned about current news, politics, and peosonal health and also like outdoors activity.        
* Cluster 3 is the group of people who like shopping and food and tweeting some random things.        
* Cluster 4 doesn't have some obvious patterns. I assume this is group of people who don'e reveal strong interests in some area.          
* Cluster 5 is the group of followers who are young, businessman, homebody, or  creative.           
* Cluster 6 is the spam group.

# Author Attribution
```{r}
train=Sys.glob('~/Desktop/Exam/ReutersC50/C50train/*')
test=Sys.glob('~/Desktop/Exam/ReutersC50/C50test/*')
```

```{r}
comb_art=NULL
labels=NULL

for (name in train)
{ 
  author=substring(name,first=50)
  article=Sys.glob(paste0(name,'/*.txt'))
  comb_art=append(comb_art,article)
  labels=append(labels,rep(author,length(article)))
}
```

```{r}
readerPlain <- function(fname)
  {
				readPlain(elem=list(content=readLines(fname)), 
							id=fname, language='en') 
  }

comb = lapply(comb_art, readerPlain) 
names(comb) = comb_art
names(comb) = sub('.txt', '', names(comb))
``` 

```{r}
corp_tr=Corpus(VectorSource(comb))
```

```{r, warning=FALSE}
corp_tr_cp=corp_tr
corp_tr_cp = tm_map(corp_tr_cp, content_transformer(tolower))
corp_tr_cp = tm_map(corp_tr_cp, content_transformer(removeNumbers))
corp_tr_cp = tm_map(corp_tr_cp, content_transformer(removePunctuation))
corp_tr_cp = tm_map(corp_tr_cp, content_transformer(stripWhitespace))
corp_tr_cp = tm_map(corp_tr_cp, content_transformer(removeWords),stopwords("en"))
DTM_train = DocumentTermMatrix(corp_tr_cp)
DTM_tr=removeSparseTerms(DTM_train,.99)
tf_idf_mat = weightTfIdf(DTM_tr)
DTM_trr<-as.matrix(tf_idf_mat)
```

```{r}
comb_art1=NULL
labels1=NULL
for (name in test)
{ 
  author1=substring(name,first=49)
  article1=Sys.glob(paste0(name,'/*.txt'))
  comb_art1=append(comb_art1,article1)
  labels1=append(labels1,rep(author1,length(article1)))
}
``` 

```{r}
comb1 = lapply(comb_art1, readerPlain) 
names(comb1) = comb_art1
names(comb1) = sub('.txt', '', names(comb1))
```

```{r}
corp_ts=Corpus(VectorSource(comb1))
```

```{r, warning=FALSE}
corp_ts_cp=corp_ts
corp_ts_cp = tm_map(corp_ts_cp, content_transformer(tolower))
corp_ts_cp = tm_map(corp_ts_cp, content_transformer(removeNumbers))
corp_ts_cp = tm_map(corp_ts_cp, content_transformer(removePunctuation))
corp_ts_cp = tm_map(corp_ts_cp, content_transformer(stripWhitespace))
corp_ts_cp = tm_map(corp_ts_cp, content_transformer(removeWords),stopwords("en"))
```

```{r,warning=FALSE}
DTM_ts=DocumentTermMatrix(corp_ts_cp,list(dictionary=colnames(DTM_tr)))
tf_idf_mat_ts = weightTfIdf(DTM_ts)
DTM_tss<-as.matrix(tf_idf_mat_ts)
```

```{r}
DTM_trr_1<-DTM_trr[,which(colSums(DTM_trr) != 0)] 
DTM_tss_1<-DTM_tss[,which(colSums(DTM_tss) != 0)]
```

```{r}
DTM_tss_1 = DTM_tss_1[,intersect(colnames(DTM_tss_1),colnames(DTM_trr_1))]
DTM_trr_1 = DTM_trr_1[,intersect(colnames(DTM_tss_1),colnames(DTM_trr_1))]
```

```{r}
mod_pca = prcomp(DTM_trr_1,scale=TRUE)
pred_pca=predict(mod_pca,newdata = DTM_tss_1)
```

```{r}
tr_class = data.frame(mod_pca$x[,1:724])
tr_class['author']=labels
tr_load = mod_pca$rotation[,1:724]

ts_class_pre <- scale(DTM_tss_1) %*% tr_load
ts_class <- as.data.frame(ts_class_pre)
ts_class['author']=labels1
```

Random Forest   

```{r}
mod_rand<-randomForest(as.factor(author)~.,data=tr_class, mtry=7,importance=TRUE)
```

```{r echo=FALSE, warning=FALSE}
pre_rand<-predict(mod_rand,data=ts_class)

tab_rand<-as.data.frame(table(pre_rand,as.factor(ts_class$author)))
predicted<-pre_rand
actual<-as.factor(ts_class$author)
temp<-as.data.frame(cbind(actual,predicted))
temp$flag<-ifelse(temp$actual==temp$predicted,1,0)
accuracy_rf = sum(temp$flag)*100/nrow(temp)
accuracy_rf
```

Naive Baye's  

```{r}
mod_naive=naiveBayes(as.factor(author)~.,data=tr_class)
pred_naive=predict(mod_naive,ts_class)
``` 

```{r echo=FALSE, warning=FALSE}
predicted_nb=pred_naive
actual_nb=as.factor(ts_class$author)

temp_nb<-as.data.frame(cbind(actual_nb,predicted_nb))
temp_nb$flag<-ifelse(temp_nb$actual_nb==temp_nb$predicted_nb,1,0)
accuracy_nb = sum(temp_nb$flag)*100/nrow(temp_nb)
accuracy_nb
```

K-Nearest Neighbors  

```{r}
train.X = subset(tr_class, select = -c(author))
test.X = subset(ts_class,select=-c(author))
train.author=as.factor(tr_class$author)
test.author=as.factor(ts_class$author)
```

```{r echo=FALSE, warning=FALSE}
knn_pred=knn(train.X,test.X,train.author,k=1)
temp_knn=as.data.frame(cbind(knn_pred,test.author))
temp_knn_flag<-ifelse(as.integer(knn_pred)==as.integer(test.author),1,0)
accuracy_knn = sum(temp_knn_flag)*100/nrow(temp_knn)
accuracy_knn
```

### **Conclusion**
```{r echo=FALSE, warning=FALSE}
library(ggplot2)
comp<-data.frame("Model"=c("Random Forest","Naive Baye's","KNN"), "Test.accuracy"=c(accuracy_rf, accuracy_nb, accuracy_knn))
comp
ggplot(comp,aes(x=Model,y=Test.accuracy))+geom_col()
```
I used three methods. It turned out that the random forest model performs way better than the other two. 


# Association Rule Mining

```{r echo=FALSE, warning=FALSE}
groceries_raw = scan("~/Desktop/Exam/groceries.txt", what = "", sep = "\n")
groceries = strsplit(groceries_raw, ",")
transaction = as(groceries, "transactions")
cat("There are", nrow(groctrans), "rows in the transaction tables.")
summary(transaction)
itemFrequencyPlot(groctrans, topN = 20)
```

```{r echo=FALSE, warning=FALSE}
rules_1 = apriori(transaction, 
                  parameter=list(support=0.05, confidence=.1, minlen=2))
arules::inspect(crules_1)
plot(crules_1, method='graph')
```
There are only 6 rules due to high support value(0.05) and low confidence value(0.1). We will fix that in the next part. We can notice that the most involved items are whole milk and rolls/buns, which are consistent with what we go from the frequency plot.

```{r echo=FALSE, warning=FALSE}
rules_2 = apriori(transaction, 
                  parameter=list(support=0.03, confidence=.3, minlen=2))
arules::inspect(rules_2)
plot(head(rules_2,15,by='lift'), method='graph')
```
Now there are 14 rules. We will fix that in the next part. However, whole milk still happens a lot of times.

```{r echo=FALSE, warning=FALSE}
rules_3 = apriori(transaction, 
                  parameter=list(support=0.001, confidence=.8, minlen=2))
arules::inspect(rules_3)
plot(head(rules_3, 5, by='lift'), method='graph')
```
This time we get 100 rules. We found several rules:      
* People are likely to buy bottled beer if they buy reb/blush wine or liquor.     
* People are lkely ot buy tropical fruit if they buy vegetable juice, citrus fruit, or grape.
