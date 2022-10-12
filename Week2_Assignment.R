#Setup for question 1 
library(tidyverse)
library(Stat2Data)
data("Hawks")
#question 1 
# Q 1.1.1

hsf = select(filter(Hawks, Species == "RT" & Weight >= 1000 ), Wing, Weight, Tail)

#Q 1.2.1
df_hsfSorted = arrange(hsf,Wing)
head(df_hsfSorted)

#Q 1.3 all sub questions included
species_code = c("CH","RT","SS")
full_name = c("Cooper's","Red-tailed","Sharp-shinned")
hawkSpeciesNameCode= data.frame(species_code,full_name)
hawkSpeciesNameCode=rename(hawkSpeciesNameCode,Species = species_code)
hawks_joined = left_join(Hawks,hawkSpeciesNameCode)
hawks_joined = select(hawks_joined, -Species)
hawks_joined = rename(hawks_joined,Species = full_name)
output_1 = select(hawks_joined,Species, Wing , Weight)
head(output_1)

# Q 1.4

hawks_joined = mutate(hawks_joined, bird_bmi = 1000 * Weight /(Wing*Wing))
?arrange
output_14 = arrange(select(hawks_joined,Species, bird_bmi), desc(bird_bmi))
head(output_14)

# Q 1.5.1

group_by(hawks_joined,Species) %>% summarize( num_rows = n(),wing_mn= mean(Wing,na.rm=TRUE), wing_md = median(Wing,na.rm=TRUE), t_mn_wing = mean(Wing, trim = 0.1,na.rm=TRUE), b_wt_wing=max(Wing/Tail, na.rm = TRUE))

# Q 1.5.2

group_by(hawks_joined,Species) %>% summarize(across(everything(),~sum(is.na(.x))))


# Q 2.1

# we need purrr library to run Maps
?map

# Q 2.1.2
impute_by_median<-function(x){
  mu<-median(x,na.rm=1) # first compute the median of x
  impute_f<-function(z){ # coordinate-wise imputation
    if(is.na(z)){
      return(mu) # if z is na replace with median
    }else{
      return(z) # otherwise leave in place
    }
  }
  return(map_dbl(x,impute_f)) # apply the map function to impute across vector
}

x <- c (1,2,NA,4)

impute_by_median(x)

# Q 2.1.3 

x = seq(0,10,0.1)
y_computer = function (x){
  y= 5*x +1
  return(y)
 
}
y = map_dbl(x,y_computer)

df_xy = data.frame(x,y)

# Q 2.1.4

sometimes_missing = function(index,value){
  if (index %% 5 == 0 & value %% 5 == 0){
    return(NA)
  }
  else if (index %% 5 == 0){
    return(index)
  }
  else if(value %% 5 == 0){
    return(value)
  }
}
sometimes_missing(10,20)
sometimes_missing(14,20)
sometimes_missing(10,11)

#map 2 dbl part is pending and 2.1.5 is pending


# Q 2.2
?Separate
install.packages("tidyr")
library(tidyr)
install.packages("readxl")
library(readxl)
wins_data_frame = read_excel("C:\\Users\\sragh\\Downloads\\HockeyLeague(3).xlsx",sheet = 1)
losses_data_frame = read_excel("C:\\Users\\sragh\\Downloads\\HockeyLeague(3).xlsx",sheet = 2)

#wins_separate = separate(wins_data_frame,col=X1990,into=c("wins_1990","total_games_1990"),sep ="of")
wins_data_frame=pivot_longer(wins_data_frame,as.character(seq(1990,2020)),names_to= 'years',values_to = 'games' )
wins_separate = separate(wins_data_frame,col=games,into=c("wins","total_games"),sep ="of",convert = TRUE)

losses_data_frame=pivot_longer(losses_data_frame,as.character(seq(1990,2020)),names_to= 'years',values_to = 'games' )
losses_separate = separate(losses_data_frame,col=games,into=c("losses","total_games"),sep ="of",convert = TRUE)
master_set = inner_join(wins_separate,losses_separate)
master_set=rename(master_set, teams = ...1)
master_set=mutate(master_set,draws = total_games - (wins+losses), wins_rt = wins/total_games , loss_rt = losses/total_games, draw_rt = (total_games - (wins+losses))/total_games)
master_set %>% group_by(teams) %>% summarize(W_md= median(wins,na.rm = TRUE),W_mn=mean(wins, na.rm = TRUE), L_md=median(losses,na.rm=TRUE),L_mn = mean(losses,na.rm = TRUE), D_md = median(draws, na.rm = TRUE), D_mn = mean(draws, na.rm= TRUE) )


# Q3
library(ggplot2)
ducks_wins = select(filter(master_set, teams == "Ducks"), wins)
# Histogram for ducks wins
ggplot(data=ducks_wins,aes(x=wins))+xlab("Ducks wins 1990-2020")+geom_histogram(binwidth = 3)+ ylab("Count")
# Density graphs for Ducks wins
ggplot(data=ducks_wins,aes(x=wins))+xlab("Ducks wins 1990-2020")+geom_density(adjust=0.5)+ ylab("Count")
ggplot(data=ducks_wins,aes(x=wins))+xlab("Ducks wins 1990-2020")+geom_density(adjust=2)+ ylab("Count") 

#Bivariate plot set up

wins_separate = rename(wins_separate,teams = ...1)
wins_team = pivot_wider(wins_separate,names_from = teams,values_from = wins)
wins_team = select(wins_team,-years)

#bivariate plot
ggplot(data= wins_team, aes(x=Ducks, y = Eagles))+geom_point()

 


