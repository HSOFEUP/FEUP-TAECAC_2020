
  
  ## Load Libraries

library(arules)
library(arulesViz)
library(data.table)
library(FSelector)
library(ggplot2)
library(plyr)
library(xlsx)
library(dplyr)
library(proj4)
library(ggmap)




#### 1.1 - Read file

WorkingDir <- "Data Source"
df <- fread( paste( WorkingDir, "Accidents_2015.csv", sep= "/" ))





#### 1.2 - Quick analysis on data

(colnames(df))
(str(df))
(ncol(df))
(nrow(df))
(head(df))


## Change messy column names
setnames( df  , "2nd_Road_Class"             , "Second_Road_Class"        )
setnames( df  , "2nd_Road_Number"            , "Second_Road_Number"       )
setnames( df  , "1st_Road_Class"             , "First_Road_Class"         )
setnames( df  , "1st_Road_Number"            , "First_Road_Number"        )
setnames( df  , "Local_Authority_(District)" , "Local_Authority_District" )
setnames( df  , "Local_Authority_(Highway)"  , "Local_Authority_Highway"  )




#### 1.3 - Check number of NAs

(na_count <- sapply( df, function(x) sum( is.na(x) )))
## Alternatively
#(apply(is.na(df), 2, sum))




#### 1.4 - Get the number of unique levels for each column in order analise those with many levels. (Those with many levels can be discarded later or summarized)

(apply( df, 2, function(x) length( unique(x) )))



#### 1.5 - Lets transform some columns code numbers to names using the lookup table.

df_copy <- df 

map <- data.table( number_sheet  = 3:21,
                   name_variable  = c( 'Police_Force',
                                       'Accident_Severity', 
                                       'Day_of_Week',
                                       'Local_Authority_District', 
                                       'Local_Authority_Highway',
                                       'First_Road_Class', 
                                       'Road_Type',
                                       'Junction_Detail',
                                       'Junction_Control',
                                       'Second_Road_Class', 
                                       'Pedestrian_Crossing-Human_Control',
                                       'Pedestrian_Crossing-Physical_Facilities',
                                       'Light_Conditions',
                                       'Weather_Conditions', 
                                       'Road_Surface_Conditions',
                                       'Special_Conditions_at_Site',
                                       'Carriageway_Hazards',
                                       'Urban_or_Rural_Area', 
                                       'Did_Police_Officer_Attend_Scene_of_Accident')
)

## Iterate

for( Index in map$number_sheet ){
  
  look<- data.table(read.xlsx(file = paste( WorkingDir,
                                            "Road-Accident-Safety-Data-Guide.xls" , sep= "/" ),
                              sheetIndex = Index, header = TRUE)
  )
  names(look)[1]            <- tolower( names(look)[1])
  name_variable             <- map[ number_sheet == Index , name_variable ]
  df_copy [[name_variable]] <- look[ match( df[[name_variable]], look[['code']]) ]$label
}


## No longuer need df and look, lets remove from memory to save space
rm(df)
rm(look)



#### 1.6 - Some Features have many factors, other dont contain any relevant info.
##### Indentify those were none/unknow/unclassified, etc are more frequent and take some action if needed.

for ( Index in map$number_sheet ){
  name_variable   <- map[ number_sheet == Index , name_variable ]
  cat ( paste0( "\n\n",name_variable, ":" ))
  print ( table( df_copy[[name_variable]] ))
}





#### 1.7 - Quick location analisys were the number of involved vehicles is large

## Subset to number of vehicles larger that threshold
data_large <- df_copy[Number_of_Vehicles >9]

## Do an scatter plot of the regions
Longitude  <- data_large$Longitude
Latitude   <- data_large$Latitude
## Plot
plot( Longitude, Latitude, main= "Location Scatter plot" )



##### Analysis: Aparently the exist two regions in close proximity that may suggest an Auto-way. Lets dig into.

#### 1.7.1 - Quick barplot to see the frequency

# Show an quick barplot
barplot(prop.table(table(data_large$Road_Type)), las=2, cex.names=.5)
## Adding Spped_limit 
(data_large %>% group_by(Road_Type, Speed_limit) %>% 
    tally() %>% 
    arrange(desc(n)))
## Adding weather conditions
(data_large %>% group_by(Road_Type, Speed_limit, Weather_Conditions) %>%
    tally() %>%
    arrange(desc(n)))


##### Analysis: Considering the close proximity, the Road Type (Dual Carriage) and the Speed limit(70), we may find some areas were major crash in series occurs. Perhaps some more attention is needed to those spots by local Authorities.



#### 1.7.2 - Lets bring the area density to an more detailed analysis

## get the Uk map
UK <- get_map(location = 'England', zoom=8)
map <- ggmap(UK)
## Convert our coordinated to dataframe
coords <- data.frame(lon = data_large$Longitude, lat = data_large$Latitude)

## Build data
d2 <- coords
d2$Road_Type <- data_large$Road_Type
d2 <- na.omit(d2)

## Superimpose plot
map +
  stat_density2d(
    aes(x =lon, y= lat, fill= ..level.., alpha =..level..),
    size=1, bins=10, data=d2,
    gem="polygon")

## No longuer need data_large
rm(data_large)





#### 1.8 - Assume some relevant features
##### Select some relevant columns to start analysis.
##### Also reduce the granilarity of those columns we find relevant.

df_copy[, Accident_Index := NULL]


## Convert factor date to date 

df_copy[, Date        := as.Date( as.character( Date ), format = "%d/%m/%Y" ) ]

df_copy[, Day_of_year := as.Date( format( Date, "%m-%d" ), format = "%m-%d" )]

## Now as factors (data as Winter, Spring, Summer, Fall) (To be precise in the days)
df_copy[, Season      := ifelse( Day_of_year >= as.Date( "03-20", format = "%m-%d" ) & Day_of_year < as.Date( "06-21", format = "%m-%d" ) ,"Spring",
                                 ifelse( Day_of_year >= as.Date( "06-21", format = "%m-%d" ) & Day_of_year < as.Date( "09-22", format = "%m-%d" ) ,"Summer",
                                         ifelse( Day_of_year >= as.Date( "09-22", format = "%m-%d" ) & Day_of_year <= as.Date( "12-21", format = "%m-%d" ) ,"Fall", "Winter")))]

## Get the month of the year
df_copy[, c( "Month", "Day_of_year" ) := list( factor( format( Date, "%b" )),NULL ) ]

## Conver hour:minutes only to hours
df_copy[, Hour := as.numeric( substr( Time,1,2 )) ]

## Convert hour:minutes only to hours
df_copy[, Hour := as.numeric( substr( Time,1,2 )) ]



## Convert time to Morning, Afternoon, Night and Evening (Reduce Granularity)
breaks <- c( 0, 6, 12, 18, 23 )
labels <- c( "Night", "Morning", "Afternoon", "Evening" )
df_copy[, Day_Period := cut( as.numeric( Hour ), breaks = breaks,
                             labels = labels        , include.lowest=TRUE )]

## Check conversion CHECK
(apply(is.na(df_copy), 2, sum)) # Time 18 not being right converted


## Factorize number casualties ("Small", "Medium", "Large", "Overweelming")
breaks <- c( "0", "2", "4", "12", "inf" )
labels <- c( "Small", "Medium", "Large", "Overweelming" )

df_copy[, Casualties_Class := cut( Number_of_Casualties , breaks = breaks    ,
                                   labels = labels      , include.lowest=TRUE )]


## Summarize number of vehicles to (small, medium, high)
breaks <- c( "0", "2", "8",  "inf" )
labels <- c( "Small", "Medium", "Large" )
df_copy [, Number_Vehicles_Class := cut( Number_of_Vehicles, breaks = breaks,
                                  labels=labels     , include.lowest=TRUE )]

df_copy$Speed_limit  <- as.factor( df_copy$Speed_limit )


## Local copy to maintain the analisys further in the same domain.
df_subset <- df_copy

## Remove aux used vars
rm(df_copy)




#### 1.9 - Make some analysis in histograms.

## Show the histogram for accidents severety
barplot(prop.table(table(df_subset$Accident_Severity)),las=2, cex.names=.5)
(table(df_subset$Accident_Severity))

## Show the most commnon number of casualties
barplot(prop.table(table(df_subset$Number_of_Casualties)), cex.names=.5)

## Show coorrelation between Accident_severety and Number of casualties
ggplot(df_subset, aes(Casualties_Class,fill =Accident_Severity)) + 
  geom_bar()+ 
  theme_bw() +
  geom_bar( position = "fill")+
  theme(axis.text.x=element_text(angle=90, hjust=1))



## Show the histogram of the acccidents by day of week
barplot(prop.table(table(df_subset$Day_of_Week)), las=2, cex.names=.5)

## Correlated Accident severety with day of the week
ggplot( df_subset, aes( Day_of_Week, fill = Accident_Severity ))+ 
  geom_bar()+ 
  theme_bw()+
  geom_bar( position = "fill")+
  theme(axis.text.x=element_text(angle=90, hjust=1))




## Show the most commnon speed limit at accident site
barplot(prop.table(table(df_subset$Speed_limit)), las=2, cex.names=.5)

## Correlated Accident severety with speed limit
ggplot(df_subset, aes(Speed_limit,fill =Accident_Severity))+
  geom_bar()+
  theme_bw()+
  geom_bar( position = "fill")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))

######### HERE

## Show the road specs were occur more accidents
barplot(prop.table(table(df_subset$Road_Type)), las=2, cex.names=.5)

## Correlated Accident severety with Road Type
ggplot(df_subset, aes(Road_Type,fill =Accident_Severity)) + 
  geom_bar()+ 
  theme_bw()+
  geom_bar( position = "fill")+ 
  theme_bw()+ 
  theme(axis.text.x=element_text(angle=90, hjust=1))



## Show what are the more common road surface conditions
barplot(prop.table(table(df_subset$Road_Surface_Conditions)), las=2, cex.names=.5)

## Correlated Accident severety with Road Surface Conditions
ggplot(df_subset, aes(Road_Surface_Conditions,fill =Accident_Severity))+
  geom_bar()+
  theme_bw()+
  geom_bar( position = "fill")+ 
  theme_bw()+ 
  theme(axis.text.x=element_text(angle=90, hjust=1))



## Show the seasons accident distributions
barplot(prop.table(table(df_subset$Season)), las=2, cex.names=.5)

## Correlated Accident severety with Season
ggplot(df_subset, aes(Season,fill =Accident_Severity))+
  geom_bar()+
  theme_bw()+
  geom_bar( position = "fill")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))



## More detailed by month
barplot(prop.table(table(df_subset$Month)), las=2, cex.names=.5)

## Correlated Accident severety with Month
ggplot(df_subset, aes(Month,fill =Accident_Severity))+
  geom_bar()+
  theme_bw()+
  geom_bar( position = "fill")+ 
  theme_bw()+ 
  theme(axis.text.x=element_text(angle=90, hjust=1))



## By hour
barplot(prop.table(table(df_subset$Hour)), las=2, cex.names=.5)

#### Correlated Accident severety with Hour 
ggplot(df_subset, aes(Hour,fill =Accident_Severity))+
  geom_bar()+
  theme_bw()+
  geom_bar( position = "fill")+ 
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))



## Resumed, by day period
barplot(prop.table(table(df_subset$Day_Period)), las=2, cex.names=.5) ## ok obvious, last info tells 17h

#### Correlated Accident severety with Day period
ggplot(df_subset, aes(Day_Period,fill =Accident_Severity))+
  geom_bar()+
  theme_bw()+
  geom_bar( position = "fill")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))



## Show were occur the majority of the accidents (Urban, Rural)
barplot(prop.table(table(df_subset$Urban_or_Rural_Area)), las=2, cex.names=.5)

#### Correlated Accident severety with Urban or Rural
ggplot(df_subset, aes(Urban_or_Rural_Area,fill =Accident_Severity))+
  geom_bar()+
  theme_bw()+
  geom_bar( position = "fill")+
  theme_bw()+ 
  theme(axis.text.x=element_text(angle=90, hjust=1))



## Most of the accident occurs daylight, 17H, lets corroborate.
barplot(prop.table(table(df_subset$Light_Conditions)), las=2, cex.names=.5)

#### Correlated Accident severety with Urban or Rural
ggplot(df_subset, aes(Light_Conditions,fill =Accident_Severity))+
  geom_bar()+
  theme_bw()+
  geom_bar( position = "fill")+ 
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))



## Show the fisrt road class
barplot(prop.table(table(df_subset$First_Road_Class)), las=2, cex.names=.5)

#### Correlated Accident severety with First road class
ggplot(df_subset, aes(First_Road_Class,fill =Accident_Severity))+
  geom_bar()+
  theme_bw()+
  geom_bar( position = "fill")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))



## Most of accidents occur in uncontrolled juntions
barplot(prop.table(table(df_subset$Junction_Control)), las=2, cex.names=.5)

#### Correlated Accident severety with Junction_Control
ggplot(df_subset, aes(Junction_Control,fill =Accident_Severity))+
  geom_bar()+
  theme_bw()+
  geom_bar( position = "fill")+ 
  theme_bw()+ 
  theme(axis.text.x=element_text(angle=90, hjust=1))



## Show the pedestrial facilities were occur more accidents
barplot(prop.table(table(df_subset$`Pedestrian_Crossing-Physical_Facilities`)), las=2, cex.names=.5)

#### Correlated Accident severety with Pedestrian_Crossing-Physical_Facilities
ggplot(df_subset, aes(`Pedestrian_Crossing-Physical_Facilities`,fill =Accident_Severity))+
  geom_bar()+
  theme_bw()+
  geom_bar( position = "fill")+ 
  theme_bw()+ 
  theme(axis.text.x=element_text(angle=90, hjust=1))



## Show the histogram in Police Force areas
barplot(prop.table(table(df_subset$Police_Force)), las=2, cex.names=.5)

#### Correlated Accident severety with Police_Force
ggplot(df_subset, aes(Police_Force, fill = Accident_Severity))+
  geom_bar()+
  theme_bw()+
  geom_bar( position = "fill")+ 
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))



### Show correlations with the remainder columns
## Weather_Conditions
ggplot(df_subset, aes(Weather_Conditions, fill = Accident_Severity))+
  geom_bar()+
  theme_bw()+
  geom_bar( position = "fill")+  
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))




##### Analysis: We see a increase of accidnet in the fall, perhaps suggest lack of adaptation to the changing of weather conditions.

##### Analysis: Peak our 17H and 8H, people going to work and going to home.

##### Analisys: We see a increase of accidents in the fall, perhaps suggest lack of adaptation to the changing of weather conditions.

##### Analisys: Sunday, less accidents, people more relaxed or at home.

##### Analisys: We may also find that higher speeds lead to increase of fata accidents.

##### Analisys: We also find that the police force metropolitan police force concentrates the majority of the accidents. That suggest a very dense road area with alot of cross intersections. Lets corraborate that.





#### 1.9.1 - Corraborating the metropolitan area .

## Subset, just another way to do it
data <- filter(df_subset, Police_Force == "Metropolitan Police")
## Show some relevent levels factors ordered descent
data %>% group_by(Road_Type, Speed_limit, Weather_Conditions, Urban_or_Rural_Area ) %>% tally() %>% arrange(desc(n))



##### Analysis: Ok, we can easily identify that is an urban area, corrabores by speed limit and urban.


  
  #### 2 - Mining Stage
  ##### Some quick theory to be used as reminder
  ##### Support: Importance of a set (Number Transactions containing the set S)
  
  ##### Confidence: Measures strong of the rule (Percentage of Transactions that having an set of items also have other item) conf(A->B) = sup(A U B)/sup(A)
  
  ##### Improvement: Minimal difference between confidence rule and immediate simplications
  
  ##### Lift: Measures the ration between confidence and support of the itemset appearing on the consequent
  
##### lift(A -> B) = sup(A U B)/sup(A)*sup(B) (< 1- Negative Correlated, =1 Independent, >1 Positive Correlated) NOTE: Only measure co-ocorrence (not implication!!)

##### Conviction: Sensitive to rule direction.
##### Convi (A ->B) = 1-sup(A)/1-conf(A->B) (=1 Independent, >>1 Consequnt depend on the antecedent strongly)

#### 2.1 - Convert into transactions (all must be factors!)
#### Select Some relevant columns tha may be associated with accident values

## Check the variable relation in terms of the numeric values
df_subset_Ent            <- df_subset[, setdiff( names( df_subset ),c( "Longitude"                 ,"Latitude"              , "First_Road_Number"     ,
                                                               "Second_Road_Number"        , "Number_of_Casualties" , "Time"                  ,
                                                               "Date"                      , "Number_of_Vehicles"   , "Hour"                  ,
                                                               "LSOA_of_Accident_Location" , "Location_Easting_OSGR", "Location_Northing_OSGR" )), with=F]

## Check the entropy gains regarding the Accident Severity
entropy.gains                  <- information.gain ( Accident_Severity~. , df_subset_Ent )

## Compute the corollation
correlation.filter.selection   <- cfs              ( Accident_Severity~. , df_subset_Ent )

## Select the variables to use
variable.use <- unique( c( cutoff.k( entropy.gains, 11 ), correlation.filter.selection ))

## Create the subset
subset_entropy <- df_subset[, c( "Accident_Severity", variable.use ), with=F ]



## Convert to factor the rest of then
df_subset$Season <- as.factor(df_subset$Season)
df_subset$Hour <- as.factor(df_subset$Hour)

## First only choose a subset of relevant variables to analysis.
subset <- subset(df_subset, select =c("Accident_Severity",
                                      "Speed_limit",
                                      "Light_Conditions",
                                      "Urban_or_Rural_Area",
                                      "Month",
                                      "Pedestrian_Crossing-Physical_Facilities",
                                      "Day_Period",
                                      "Road_Type",
                                      "Road_Surface_Conditions",
                                      "Junction_Detail",
                                      "Junction_Control",
                                      "Casualties_Class",
                                      "Weather_Conditions",
                                      "Season"))

## Convert then to transacions
trans <- as(subset, "transactions")
summary(trans)

## Check some items labels
head(itemLabels(trans))

## Check the item frequency.
itemFrequencyPlot(trans, topN=50,  cex.names=.5)

## No longuer need  subset, lets remove from memory to save space.
rm(subset)





#### 2.2 - Look at similarity between items

#d <- dissimilarity(sample(trans, 500), method = "phi", which= "items")
#d[is.na(d)] <- 1 # Removing missing values
#plot(hclust(d), cex=.5)




#### 2.3 - Lest look at some subsets of transactions containing the itemset of interest
##### 2.3.1 - Focus in the Accident_Severity=Fatal

trans_sub <- subset(trans, items %in% "Accident_Severity=Fatal")
itemFrequencyPlot(trans_sub, topN = 25, population = trans, cex.names=.5)

## Order By lift
itemFrequencyPlot(trans_sub, topN = 25, population = trans, lift=TRUE, cex.names=.5)


##### Analysis:  The graph manifest that the Accident_Severity=Fatal appear frequently as the no phisycal crossing facilities within 50 meters also dry conditions and rural areas.

#### Focus in the Light_Conditions=Darkness - lights lit

trans_sub <- subset(trans, items %in% "Light_Conditions=Darkness - lights lit")
itemFrequencyPlot(trans_sub, topN = 25, population = trans, cex.names=.5)

## Order By lift
itemFrequencyPlot(trans_sub, topN = 25, population = trans, lift=TRUE, cex.names=.5)


##### Analysis: The graph manifest that the light conditions - Darkness lights lit Urbarn_or_Rural_Area=Urban and Speed_limit=30 appear frequently, what makes sense to a urban city area.

##### 2.3.2 - Focus in the Road_Surface_Conditions=Wet or damp

trans_sub <- subset(trans, items %in% "Road_Surface_Conditions=Wet or damp")
itemFrequencyPlot(trans_sub, topN = 25, population = trans, cex.names=.5)

## Order By lift
itemFrequencyPlot(trans_sub, topN = 25, population = trans, lift=TRUE, cex.names=.5)


##### Analysis: The graph manifest that the the Road_Surface_Conditions=Wet_or_damp apper frequently with Road_Type=Single Carriageway.
##### Also ordering by lift we may find a strong co-occurence of Surface_Conditions=Wet or damp with the winter months (Nov, Dec, Jan) what is obvious.

##### 2.3.3 - Focus in the Day_Period=Night

trans_sub <- subset(trans, items %in% "Day_Period=Night")
itemFrequencyPlot(trans_sub, topN = 25, population = trans, cex.names=.5)

## Order By lift
itemFrequencyPlot(trans_sub, topN = 25, population = trans, lift=TRUE, cex.names=.5)

## Drop trans_sub so save memory
rm(trans_sub)




##### Analysis: Analysing the lift graph, we find that the Day_period=Night co-occcur toguether with Light_Conditions=Darkneess - lights unlit and Darkness -no Lighting and we also may find a small co-occurence of the fatal accidents severety. The lift value is not very high.



#### 2.3 - Mining frequent items

## Find an interesting support (have at least 500 transactions)
(500/nrow(trans))

## User parameters
min_supp <- 0.0036
min_conf <- 0.5
min_lift <- 0.6
min_len <- 2
max_len <- 4

## Generate frequent itemsets
itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=min_supp, minlen = min_len, maxlen =max_len))

## Inspect the 10 most frequent item
inspect(head(sort(itemsets), n=10))


##### Analysis:  Here we can see that the majority of the accidents severety are Slight and the number of casualties small.
##### Analysis: Another info is that the small casuality occur mostly without existence of any pedestrian physiscal facilities.
##### Analisys: Another info is that the majority of the small accidents occur in the Single carriage road type.


##### 2.3.1 - Adding a extra measure of quality

quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = trans)

## Lets analyse
inspect(head(sort(itemsets, by = "lift"), n=10))

## Plot itemsets as a graph. Different subgroups with items that are related to each other can be identified.)
plot(head(sort(itemsets, by = "lift"), n=10), method = "graph", control=list(cex=.8))

## Plotting Iteractively
#plot(head(sort(itemsets, by = "lift"), n=10), method = "graph", interactive = TRUE)


##### Analisys: A first inspection tells that the {Speed_limit=70, Urban_or_Rural_Area=Rural,  Road_Type=Dual carriageway, Junction_Detail=Slip road} presents a strong lift (Co-occur frequently).

##### Analysis: Also the {Speed_limit=70, Light_Conditions=Darkness - no lighting, Urban_or_Rural_Area=Rural, Road_Type=Dual carriageway}  strong lift (Co-occur frequently).

##### Analisys: Also  {Speed_limit=70,  Road_Type=Dual carriageway, Junction_Detail=Slip road, Junction_Control=Give way or uncontrolled} making the juctio control type uncotroleld co-occur.

##### Analisys: Also crossing areas with {Pedestrian_Crossing-Physical_Facilities=Pedestrian phase at traffic signal junction,Road_Type=Dual carriageway, Junction_Detail=Crossroads, Junction_Control=Auto traffic signal} co-occur toguether, what makes sense in term of accidents.

##### Analisys -> Also is possible to identify the (Co-occur frequently) {Month=nov., Road_Surface_Conditions=Wet or damp, Weather_Conditions=Raining + high winds, Season=Fall} making sense, november, raining , fall.


#### 2.4 - Lets refine our frequent itemsets generations combing other more refined items of interest.

## Generate the subset transactions containing only Fatal Accidents
trans_sub <- subset(trans, items %in% "Accident_Severity=Fatal")

## Generate the itemsets
itemsets_subset <- apriori(trans_sub, parameter = list(target = "frequent",
                                                       supp=min_supp, minlen = min_len, maxlen=max_len))

## Inspect the 10 most frequent item
inspect(head(sort(itemsets_subset), n=10))




##### 2.4.1 - Adding a extra measure of quality

quality(itemsets_subset)$lift <- interestMeasure(itemsets_subset,
                                                 measure="lift", trans = trans_sub)

## Lets analyse
inspect(head(sort(itemsets_subset, by = "lift"), n=10))


## Lets plot
plot(head(sort(itemsets_subset, by = "lift"), n=10), method = "graph", control=list(cex=.8))


##### Analysis: A quick analisys shows that {Urban_or_Rural_Area=Urban, Pedestrian_Crossing-Physical_Facilities=Pedestrian phase at traffic signal junction, Junction_Detail=Crossroads,  Junction_Control=Auto traffic signal}  have a high lift, making co-occurr toguether frequently what makes sense, since in dense areas and crossroads there are many people crossing rods and frequantly dont respect the traffic signs.

  

#### 2.5- Subset the overwellming class

## Generate the subset transactions containing only overwellming number casualties
trans_sub <- subset(trans, items %in% "Casualties_Class=Overweelming")

## Generate the itemsets
itemsets_subset <- apriori(trans_sub, parameter = list(target = "frequent",
                                                       supp=min_supp, minlen = min_len, maxlen=max_len))

## Inspect the 10 most frequent item
inspect(head(sort(itemsets_subset), n=10))

## attain only thos with large number of casualties involved
itemsets_subset <- subset(itemsets_subset, subset = items %in% "Casualties_Class=Overweelming")

quality(itemsets_subset)$lift <- interestMeasure(itemsets_subset,
                                                 measure="lift", trans = trans_sub)

## Lets analyse
inspect(head(sort(itemsets_subset, by = "lift"), n=10))


## Lets plot
plot(head(sort(itemsets_subset, by = "lift"), n=10), method = "graph", control=list(cex=.8))
  

##### Analysis: {Accident_Severity=Fatal, Month=feb., Casualties_Class=Overweelming,Weather_Conditions=Fog or mist}  We may find that fog, february, Fatal accidnet and number of casuaties high co-occur frequently. An typical conditions to have major accidents




  #### 3.1 - Mine Association Rules
  
rules <- apriori(trans, parameter = list(supp=min_supp, maxlen=max_len, target="rules"))

## Quick inspection
inspect(head(sort(rules, by="lift"), n=10))

## Plot the scatterplot
plot(rules)

## Iteractevely
#plot(rules, interactive=TRUE)



#### 3.2 - Lower the support value

(200/nrow(trans))

min_supp = 0.0015
## Generate the items
rules <- apriori(trans, parameter = list(supp=min_supp, maxlen=max_len, target ="rules"))

## Quick inspection
inspect(head(sort(rules, by="lift"), n=10))

## Plot the scatterplot
plot(rules)

## Iteractevely
#plot(rules, interactive=TRUE)



##### Analysis: We may find some rules that area obvious to us such as Speed_limit=60, Month=Oct, Day_Period=Evening} => {Light_Conditions=Darkness - no lighting}. In order to obtain more relevant ones we gona focus in the cases that acciddents are fatal.

#### 3.3 - Subset the rules for analysis
#### 3.3.1 - ## Subset the Accident_Severity=Fatal


r_subset <- subset(rules, subset = items %in% "Accident_Severity=Fatal")

## Inspect
inspect(head(sort(r_subset, by="lift"), 10))

## Lets plot
itemFrequencyPlot(items(r_subset), topN=10, cex.names=.5)

## Plot graph by lift
plot(head(sort(r_subset, by="lift"), 10),
     method="graph", control=list(cex=.7))




##### Analysis: We may find that {Accident_Severity=Fatal, Speed_limit=60, Day Period=Afternoon} => {Urban_or_Rural_Area=Rural} Co-occur together the lift is not very high, but is bigger than one what may suggest that they are positive co-related.



####3.3.2 - Subset by Light conditions to find some padrons

r_subset <- subset(rules, subset = items %in% "Light_Conditions=Darkness - no lighting")

## Inspect
inspect(head(sort(r_subset, by="lift"), n=10))

## Plot itemfrequency
itemFrequencyPlot(items(r_subset), topN=10, cex.names=0.5)

## Show graph
plot(head(sort(r_subset, by="lift"), 10),
     method="graph", control=list(cex=.7))





##### Analysis -> From an quick analisys, Speed_limit=60, Month=Oct, Day_Period=Evening} => {Light_Conditions=Darkness - no lighting} is a coerent rule but not very usefull.



#### 3.3.3 - Subset by Road Surface conditions

r_subset <- subset(rules, subset = items %in% "Road_Surface_Conditions=Wet or damp")

## Inspect
inspect(head(sort(r_subset, by="lift"), n=10))

## Plot itemfrequency
itemFrequencyPlot(items(r_subset), topN=10, cex.names=0.5) #(Too big to plot)

## Show graph
plot(head(sort(r_subset, by="lift"), 10),
     method="graph", control=list(cex=.7))




##### Analysis: We may find that Light_Conditions=Darkness - no lighting, Road_Type=Dual carriageway,  Road_Surface_Conditions=Wet or damp} => {Speed_limit=70} have an lift value > 1 suggesting that they co-occur together (of course wet conditions and Darkness leads to more accidents).



#### 3.4 - Subset by road type.

r_subset <- subset(rules, subset = items %in% "Road_Type=Dual carriageway")

## Inspect
inspect(head(sort(r_subset, by="lift"), n=10))

## Plot itemfrequency
itemFrequencyPlot(items(r_subset), topN=10,  cex.names=0.5) #(Too big to plot)

## Show graph
plot(head(sort(r_subset, by="lift"), 10),
     method="graph", control=list(cex=.7))





##### Analysis: We may find the rule {Light_Conditions=Darkness - no lighting,  Road_Type=Dual carriageway, Month=dec.} => {Speed_limit=70} contains a co-occurence of high number of car involved. Clearely a auto -way.


#### 3.4 - From our generated rules lets find out those with more conviction (High Values of conviction (indicates that the consequent depends strongly in the antecendent)

rules_convi <- cbind(as(rules, "data.frame"),
                     conviction=interestMeasure(rules,
                                                "conviction", trans))
## Order by descreasing value of conviction
rules_convi <- rules_convi[order(rules_convi$conviction),]

## Get only the top ones%
per <-0.0005 # define percentage
top_rules <- head(rules_convi[order(rules_convi$conviction, decreasing = T),], per*nrow(rules_convi))

# Show then (they are ordered by conviction)
(top_rules$rules)




##### Analysis: Of course the most commun ones appears first with strong conviction, namelly data out of range. However we are insterested in a more grain set of groups of itemsets and rules, namelly those that contain Fatal accidents as consequant.




#### 3.4.1 - Try to find rules that have in the consequent Accident_Severity=Fatal

## From our rules, subset then
rules_subset <- subset(rules, subset=(rhs %in% c("Accident_Severity=Fatal")))

## Check if any rules were generated
summary(rules_subset)



##### Analysis -> The current parameters values were not able to generate the items that correspond to the fatal accidents as consequent. (Rare Cases)




#### 3.4.2 -> Lets lower our standards trying to catch the fatal ones

## In order to save our memory we gonna subset the transactions that contain fatal ones
trans_sub <- subset(trans, items %in% "Accident_Severity=Fatal")

## Set new parameters
(1/nrow(trans_sub))
min_supp <- 50/nrow(trans_sub)
min_conf <- 0.00025

## Generate the rules with consequent as Accident_Severity=Fatal"
rules_subset <- apriori(trans_sub, parameter = list(supp=min_supp, conf=min_conf,
                                                    target ="rules"),
                        appearance = list(rhs=c("Accident_Severity=Fatal")))


## Check if any rules were generated
summary(rules_subset)


## Ok some rules where generated,  inspect then
inspect(head(sort(subset(rules_subset, subset= rhs %pin% "Accident_Severity=Fatal")), by="lift"), n=5)


## Plot itemfrequency
itemFrequencyPlot(items(rules_subset), topN=10, cex.names=0.5) #(Too big to plot)

## Show graph
plot(head(sort(rules_subset, by="lift"), 10),
     method="graph", control=list(cex=.7))



##### Analysis: The diagram may suggest that the fatal accidents hapens with no particular positive correlations (lift =1 -> Independent) Discuss this with Rita.


#### 3.5 - Lest find out in witch conditions accurs major accidents with many cars involved

## In order to save our memory we gonna subset the transactions that Casualties_Class=Large
trans_sub <- subset(trans, items %in% "Casualties_Class=Large")

## Set new parameters
(500/nrow(trans_sub))
min_supp <- 500/nrow(trans_sub)
min_conf <- 0.00025

## Generate the rules with consequent as Accident_Severity=Fatal"
rules_subset <- apriori(trans_sub, parameter = list(supp=min_supp, conf=min_conf,
                                                    target ="rules"),
                        appearance = list(rhs=c("Casualties_Class=Large")))


## Check if any rules were generated
summary(rules_subset)


## Ok some rules where generated,  inspect then
inspect(head(sort(subset(rules_subset, subset= rhs %pin% "Casualties_Class=Large")), by="lift"), n=5)

##### Analisys: Clearely an rule regarding an autoway conditions


#### 4.1 - For The subset obtained with the entropy method, lets do a fine analisys
subset <- subset_entropy
## Remove so save memory
rm(subset_entropy)

#### Convert to transactions
trans <- as(subset, "transactions")
summary(trans)

(500/nrow(trans))
min_supp <- 0.0036
min_conf <- 0.5
min_lift <- 0.6
min_len <- 2
max_len <- 4

## Generate frequent itemsets
itemsets <- apriori(trans, parameter = list(target = "frequent",
                                            supp=min_supp, minlen = min_len, maxlen =max_len))

## Inspect the 10 most frequent item
inspect(head(sort(itemsets), n=10))




##### 2.3.1 - Adding a extra measure of quality
quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = trans)

## Lets analyse
inspect(head(sort(itemsets, by = "lift"), n=10))

trans_sub <- subset(trans, items %in% "Accident_Severity=Fatal")

## Generate the itemsets
itemsets_subset <- apriori(trans_sub, parameter = list(target = "frequent",
                                                       supp=min_supp, minlen = min_len, maxlen=max_len))

## Inspect the 10 most frequent item
inspect(head(sort(itemsets_subset), n=10))

##### Analysis: Police always attend fatal accindents, normal

quality(itemsets_subset)$lift <- interestMeasure(itemsets_subset,
                                                 measure="lift", trans = trans_sub)

## Lets analyse
inspect(head(sort(itemsets_subset, by = "lift"), n=10))


## Generate the rules with consequent as Accident_Severity=Fatal"
rules_subset <- apriori(trans_sub, parameter = list(supp=min_supp, conf=min_conf,
                                                    target ="rules"),
                        appearance = list(rhs=c("Accident_Severity=Fatal")))


## Check if any rules were generated
summary(rules_subset)


## Ok some rules where generated,  inspect then
inspect(head(sort(subset(rules_subset, subset= rhs %pin% "Accident_Severity=Fatal")), by="lift"), n=5)






