#SML Project
#FIFA player ratings datasets

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(rio)
library(modelr)
library(purrr)

#loading the datasets:
#FIFA20 players dataset:

fifa20 <- fread('D:/NEU/Spring 2020/SML/Project/Datasets/players_20.csv')
class(fifa20)
View(fifa20)
#fifa20 as a tibble
fifa20 <- as_tibble(fifa20)

#EDA and visualization:
#Basic visualization:
#Histogram of player age
fifa20 %>% ggplot(aes(age))+geom_bar()+labs(x="player age", y="number of players",
                                           title="Number of players by age" )
#Most common age for players is 22.


#Histogram of player rating
fifa20 %>% ggplot(aes(overall))+geom_bar()+labs(x="player rating", y="number of players",
                                            title="Number of players by rating" )
#most common rating range for players is 64-66.


#Which country has the most players on FIFA20:
fifa20 %>% group_by(nationality)%>% count()%>%
  arrange(desc(n))
#The country with most players on FIFA20 is England.


#Histogram of weight in kgs
fifa20 %>% ggplot(aes(weight_kg))+geom_bar()+labs(x="player weight", y="number of players",
                                                title="Number of players by weight in kgs" )
#70 and 75 kgs seem to be the most and 2nd most common player weights on FIFA20.


#Histogram of height in cms
fifa20 %>% ggplot(aes(height_cm))+geom_bar()+labs(x="player height", y="number of players",
                                                  title="Number of players by height in cms" )
#180 cms seems to be the most common player height on fifa20.


#Top 10 players with highest value in euros
fifa20 %>% arrange(desc(value_eur)) %>% select(short_name, club, nationality, 
                                               overall, value_eur)
#Tibble of 10 most valuable players on fifa20.


#Top 10 players with highest weekly wage in euros
fifa20 %>% arrange(desc(wage_eur)) %>% select(short_name, club, nationality, 
                                               overall, wage_eur)
#Tibble of players with highest weekly wage in euros.


#Top 10 players with best rating
fifa20 %>% arrange(desc(overall)) %>% select(short_name, club, nationality, overall)
#Tibble of top 10 rated players on fifa20.


#####ABSTRACT HYPOTHESIS: there exists a positive
#correlation between player overall and value in euros
#Is there correlation b/w pairs of features in {overall, value_eur, wage_eur}
ovr_val_wag <- fifa20 %>% select(overall, value_eur, wage_eur)
cor_ovr_val_wag <- cor(ovr_val_wag)
round(cor_ovr_val_wag, 2)
#one would expect value and wage to be highly and positively correlated with overall but
#the correlation isnt that strong. That being said,overall and wage are +vely correlated.
#High +ve correlation exists b/w value and wage though.
#Therefore, our hypothesis is true.


#Top 10 players with the best potential overall
fifa20 %>% arrange(desc(potential))  %>% select(short_name, club, 
                                                nationality, overall, potential)
#top 10 players with best potential on fifa20.


#Players aged 25 or less with best potential:
fifa20 %>% 
  filter(age <= 25) %>%
  arrange(desc(potential))  %>% 
  select(short_name, club, nationality, overall, potential)


#Players aged 21 or less with best potential:
fifa20 %>% 
  filter(age <= 21) %>%
  arrange(desc(potential))  %>% 
  select(short_name, club, nationality, overall, potential)


#Player with highest scope for increase in overall
# ie Potental-Overall
pot_inc <- fifa20 %>%
  mutate(pot_increase = potential-overall) %>%
  arrange(desc(pot_increase)) %>%
  select(short_name, club, nationality, age, overall, potential, pot_increase)
pot_inc
#we see that young players have scope for highest increase in rating.
#so are pot_increase and age positively correlated?
pot_inc_age_pot_inc <- pot_inc %>% select(age, pot_increase)
cor_pot_inc_age_pot_inc <- cor(pot_inc_age_pot_inc)
round(cor_pot_inc_age_pot_inc, 2)
#age and pot_increase have high -ve correlation.


#####ABSTRACT HYPOTHESIS:
##### WAGE AND AGE CORRELATION:
#scatter plot of wage and age
fifa20 %>% ggplot(aes(age, wage_eur))+geom_point()+geom_jitter()+
  labs(x="player age", y="player wage in euros", 
       title = "wage vs age scatter plot")
### our hypothesis:
#player wage and age are positively correlated upto the age of 31 and 
#negatively correlated after that
players31_andless <- fifa20 %>% filter(age <= 31)%>% select(age, wage_eur, value_eur)
players_over31 <- fifa20 %>% filter(age > 31)%>% select(age, wage_eur, value_eur)

cor_31andless <- cor(players31_andless)
round(cor_31andless, 2)
#age and wage are +vely correlated but the correlation is not very high.

players_over31 <- cor(players_over31)
round(cor_over31, 2)
#age and wage are -vely correlated but the correlation is not very high.
#Therefore we dont reject the hypothesis.



#####ABSTRACT HYPOTHESIS
#left footed players have a higher overall rating compared to right footed players
fifa20 %>% filter(preferred_foot=="Left")%>%
  summarise(avg_player_overall = mean(overall))
#average overall rating of left footed player on fifa20 is 66.7

fifa20 %>% filter(preferred_foot=="Right")%>%
  summarise(avg_player_overall = mean(overall))
#average overall rating of left footed player on fifa20 is 66.1
#Our hypothesis is true but not by a huge margin.


####6 PHYSICAL ATTRIBUTES EDA
#EDA on pace, shooting, passing, dribbling, defending and physique
#Top 10 fastest players on fifa20
fifa20 %>% arrange(desc(pace)) %>% select(short_name, club, nationality, overall,
                                      team_position, pace)

#Top 10 best shooters of the ball on fifa20
fifa20 %>% arrange(desc(shooting)) %>% select(short_name, club, nationality, overall,
                                              team_position, shooting)
#Strikers are the best shooters 

#Top 10 best passers on fifa20
fifa20 %>% arrange(desc(passing)) %>% select(short_name, club, nationality, overall,
                                             team_position, passing)
#midfielders are the best passers with the exception of messi who is a winger.

#Top 10 best dribblers on fifa20
fifa20 %>% arrange(desc(dribbling)) %>% select(short_name, club, nationality, overall,
                                               team_position, dribbling)
#wingers are the best dribblers.

#Top 10 best defenders on fifa20
fifa20 %>% arrange(desc(defending)) %>% select(short_name, club, nationality, overall,
                                               team_position, defending)
#Its surprising to see a midfielder who is very good at defending.

#Top 10 most physical players
fifa20 %>% arrange(desc(physic)) %>% select(short_name, club, nationality, overall,
                                               team_position, physic)
#midfielders dominate the top10 list.


####Goalkeeping stats
#Top 10 best goalkeepers on fifa20
fifa20 %>% filter(team_position=="GK") %>%
  arrange(desc(overall)) %>%
  select(short_name, club, nationality, overall)

#EDA on gk stats like gk_diving, gk_handling, gk_kicking, gk_reflexes, 
#gk_speed, gk_positioning

#Top 10 best divers on fifa20
fifa20 %>% arrange(desc(gk_diving)) %>% select(short_name, club, nationality, overall,
                                            gk_diving)

#Top 10 best handlers on fifa20
fifa20 %>% arrange(desc(gk_handling)) %>% select(short_name, club, nationality, overall,
                                               gk_handling)

#Top 10 best kickers on fifa20
fifa20 %>% arrange(desc(gk_kicking)) %>% select(short_name, club, nationality, overall,
                                               gk_kicking)

#Top 10 best reflexes on fifa20
fifa20 %>% arrange(desc(gk_reflexes)) %>% select(short_name, club, nationality, overall,
                                               gk_reflexes)

#Top 10 best speed on fifa20
fifa20 %>% arrange(desc(gk_speed)) %>% select(short_name, club, nationality, overall,
                                               gk_speed)

#Top 10 best positioning on fifa20
fifa20 %>% arrange(desc(gk_positioning)) %>% select(short_name, club, nationality, overall,
                                               gk_positioning)




####ATTACKING STATS
#stats like attacking_crossing, attacking_finishing and attacking_heading_accuracy
#Top 10 best crossers on fifa20
fifa20 %>% arrange(desc(attacking_crossing)) %>% 
  select(short_name, club, nationality, overall,attacking_crossing)

#Top 10 best finishers on fifa20
fifa20 %>% arrange(desc(attacking_finishing)) %>% 
  select(short_name, club, nationality, overall,attacking_finishing)
#ARE FINISHING AND SHOOTING CORRELATED?

#Top 10 players most likely to score a goal via a header on fifa20
fifa20 %>% arrange(desc(attacking_heading_accuracy)) %>% 
  select(short_name, club, nationality, overall,attacking_heading_accuracy)



####MOVEMENT STATS
#stats like movement_acceleration and movement_balance
#Top 10 players with best acceleration
fifa20 %>% arrange(desc(movement_acceleration)) %>% 
  select(short_name, club, nationality, overall,movement_acceleration)

#Top 10 players with best balance
fifa20 %>% arrange(desc(movement_balance)) %>% 
  select(short_name, club, nationality, overall,movement_balance)
#ARE BALANCE AND DRIBBLING CORRELATED?



####POWER STATS
#stats like power_shot_power, power_jumping, power_strength
#Top 10 players with best shot power on fifa20
fifa20 %>% arrange(desc(power_shot_power)) %>% 
  select(short_name, club, nationality, overall,power_shot_power)
#ARE SHOOTING AND SHOT POWER CORRELATED?

#Top 10 jumpers on fifa20
fifa20 %>% arrange(desc(power_jumping)) %>% 
  select(short_name, club, nationality, overall,power_jumping)
#ARE JUMPING AND ATTACKING_HEADING_ACCURACY CORRELATED?

#Top 10 players with best power strength on fifa20
fifa20 %>% arrange(desc(power_strength)) %>% 
  select(short_name, club, nationality, overall,power_strength)




####MENTALITY STATS
#stats like mentality_positioning, menatlity_penalties and mentality_vision
#Top 10 players with best position sense on fifa20
fifa20 %>% arrange(desc(mentality_positioning)) %>% 
  select(short_name, club, nationality, team_position,overall,mentality_positioning)

#Top 10 best penalty takes on fifa20
fifa20 %>% arrange(desc(mentality_penalties)) %>% 
  select(short_name, club, nationality, overall,mentality_penalties)
#ARE ATTACKING FINISHING AND PENALTIES CORRELATED?

#Top 10 players with best vision on fifa20
fifa20 %>% arrange(desc(mentality_vision)) %>% 
  select(short_name, club, nationality, overall,mentality_vision)
#ARE VISION AND PASSING CORRELATED?




####DEFENDING STATS
#stats like defending_marking, defending_standing_tackle and 
#defending_sliding_tackle
#Top 10 best markers on fifa20
fifa20 %>% arrange(desc(defending_marking)) %>% 
  select(short_name, club, nationality, team_position,overall,defending_marking)

#Top 10 best tacklers on fifa20
fifa20 %>% arrange(desc(defending_standing_tackle)) %>% 
  select(short_name, club, nationality, team_position,overall,defending_standing_tackle)

#Top 10 best sliding tacklers on fifa20
fifa20 %>% arrange(desc(defending_sliding_tackle)) %>% 
  select(short_name, club, nationality, team_position,overall,defending_sliding_tackle)





####ABSTRACT HYPOTHESIS
#tall, short and strong players are
#statistically good at heading, dribbling and tackling respectively

#Tall players and heading
#scatter plot of height vs heading
fifa20 %>% ggplot(aes(height_cm, attacking_heading_accuracy))+geom_point()+
  geom_jitter()+labs(x="player height in cm", y="heading score out of 100",
                     title = "Plot of height vs heading score")
#THIS IS A VERY INTERESTING FIND. WHY?
cor(fifa20$height_cm, fifa20$attacking_heading_accuracy,  
    method = "spearman", use = "complete.obs")
#There exists a weak positive correlation between height and heading.


#Short players and dribbling
#scatter plot of height vs dribbling
fifa20 %>% ggplot(aes(height_cm, dribbling))+geom_point()+
  geom_jitter()+labs(x="player height in cm", y="dribbling score out of 100",
                     title = "Plot of height vs dribbling score")
cor(fifa20$height_cm, fifa20$dribbling,  
    method = "spearman", use = "complete.obs")
#There is a weak negative correlation b/w height and dribbling ie short players
#are good at dribbling.

#Dribbling and movement attributes:
cor(fifa20$dribbling, fifa20$movement_agility,
    method = "pearson", use = "complete.obs")
#Dribbling and movement_agility are highly +vely correlated

cor(fifa20$dribbling, fifa20$movement_balance,
    method = "pearson", use = "complete.obs")
#Dribbling and movement_balance are moderately +vely correlated

#correlation matrix for dribbling and movement attributes:
library(corrplot)
library(rquery)
drib_move <- fifa20 %>% select(dribbling,movement_acceleration,
                            movement_sprint_speed, movement_agility,
                            movement_reactions, movement_balance)
source("http://www.sthda.com/upload/rquery_cormat.r")
require("corrplot")
rquery.cormat(drib_move)
#we can see from the corrplot that dribbling is highly +vely
#correlated with the movement attributes.


#Strong players and tackling(both standing and sliding)
#scatter plot of physique vs standing tackling
fifa20 %>% ggplot(aes(physic, defending_standing_tackle))+geom_point()+
  geom_jitter()+labs(x="physique of players", y="standing tackling",
                     title = "physique vs standing tackling")
cor(fifa20$physic, fifa20$defending_standing_tackle,
    method = "pearson", use = "complete.obs")
#moderate +ve correlation

#scatter plot of physique vs sliding tackling
fifa20 %>% ggplot(aes(physic, defending_sliding_tackle))+geom_point()+
  geom_jitter()+labs(x="physique of players", y="sliding tackling",
                     title = "physique vs sliding tackling")
cor(fifa20$physic, fifa20$defending_sliding_tackle,
    method = "pearson", use = "complete.obs")
#moderate +ve correlation



####TEAM STATS:
#Top 10 teams with highest average player overall
fifa20 %>% filter(!club %in% c("Uruguay","Colombia","Mexico","Netherlands"))%>%
  group_by(club) %>%
  summarise(avg_player_ovr = mean(overall)) %>%
  arrange(desc(avg_player_ovr))
#These are 10 best teams or the top 10 teams with the highest average player
#overall out of 100.

#Top 10 teams with highest average player overall in the starting 11
#exclude subs and reserves
best_teams <- fifa20 %>% filter(!club %in% c("Uruguay","Colombia","Mexico","Netherlands"))%>%
  filter(!team_position %in% c("SUB","RES")) %>%
  group_by(club) %>%
  summarise(avg_player_ovr = mean(overall)) %>%
  arrange(desc(avg_player_ovr))
best_teams
#view(best_teams)
#These are the top 10 teams with the highest avg. starting 11 player overall.

#Worst teams:
worst_teams <- fifa20 %>% filter(!club %in% c("Uruguay","Colombia","Mexico","Netherlands","India"))%>%
  filter(!team_position %in% c("SUB","RES")) %>%
  group_by(club) %>%
  summarise(avg_player_ovr = mean(overall)) %>%
  arrange(avg_player_ovr)
worst_teams
#View(worst_teams)

#Teams with most valuable squads:
fifa20 %>% group_by(club)%>%
  summarise(squad_value_eur = sum(value_eur))%>%
  arrange(desc(squad_value_eur))

#Teams with highest weekly wage:
fifa20 %>% group_by(club)%>%
  summarise(wage_value_eur = sum(wage_eur))%>%
  arrange(desc(wage_value_eur))


####BEST 11 ON FIFA20
unique(fifa20$team_position)
#Assuming a 4231 formation, we come up with the best team on fifa20 or the 
#fifa world 11
#striker:
#positions to be considered: ST, CF, LS, RS, LF, RF
fifa20 %>% filter(team_position %in% c("ST","CF","LS","RS","LF","RF")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#can pick either of Kane, Suarez, Aguero or Lewandowski.

#Wingers: 2 wingers to be picked, one on the left and one on the right.
#positions to be considered: RW, LW
fifa20 %>% filter(team_position %in% c("LW","RW")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#we pick Messi on the right and Ronaldo on the left

#one AM, positions: CAM, LAM, RAM
fifa20 %>% filter(team_position %in% c("CAM","LAM","RAM")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#we pick Neymar

#2 midfielders, one on the right and one on the left
#right midfielder: RCM, CDM, RDM, CM
fifa20 %>% filter(team_position %in% c("RCM","CDM","RDM","CM")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#we pick De Bruyne
#left midfielder: positions to be considered: LCM, CDM, LDM, CM
fifa20 %>% filter(team_position %in% c("LCM","CDM","LDM","CM")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#we pick Busquets


#2wing backs, one on the right and one on the left:
#left back, positions to be considered: LB, LWB
fifa20 %>% filter(team_position %in% c("LWB","LB")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
# we pick Jordi Alba
#right back, positions to be considered: RB, RWB
fifa20 %>% filter(team_position %in% c("RWB","RB")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#we pick Joshua Kimmich

#2 centre backs
#positions to be considered: LCB, RCB
fifa20 %>% filter(team_position %in% c("LCB","RCB")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
# we pick VVD and one of Koulibaly, Chiellini and Ramos

#Goalkeeper, positions: GK
fifa20 %>% filter(team_position %in% c("GK")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#we pick Jan Oblak

#Therefore the team comprises of Lewandowski, Messi, Ronaldo, De Bruyne, Busquets,
#Neymar, Alba, Van Dijk, Ramos, Kimmich and Oblak.



#####HIGHEST RATED PLAYERS FOR FAMOUS JERSEY NUMBERS: 1,2,3,4,5,6,7,8,9,10,11
fifa20 %>% filter(team_jersey_number==1) %>%
  arrange(desc(overall))%>%
  select(short_name, team_jersey_number, overall)
#Ter Stegen

fifa20 %>% filter(team_jersey_number==2) %>%
  arrange(desc(overall))%>%
  select(short_name, team_jersey_number, overall)
#Godin

fifa20 %>% filter(team_jersey_number==3) %>%
  arrange(desc(overall))%>%
  select(short_name, team_jersey_number, overall)
#Chiellini

fifa20 %>% filter(team_jersey_number==4) %>%
  arrange(desc(overall))%>%
  select(short_name, team_jersey_number, overall)
#Van Dijk

fifa20 %>% filter(team_jersey_number==5) %>%
  arrange(desc(overall))%>%
  select(short_name, team_jersey_number, overall)
#Busquets

fifa20 %>% filter(team_jersey_number==6) %>%
  arrange(desc(overall))%>%
  select(short_name, team_jersey_number, overall)
#Pogba

fifa20 %>% filter(team_jersey_number==7) %>%
  arrange(desc(overall))%>%
  select(short_name, team_jersey_number, overall)
#Ronaldo

fifa20 %>% filter(team_jersey_number==8) %>%
  arrange(desc(overall))%>%
  select(short_name, team_jersey_number, overall)
#Toni Kroos

fifa20 %>% filter(team_jersey_number==9) %>%
  arrange(desc(overall))%>%
  select(short_name, team_jersey_number, overall)
#Lewandowski

fifa20 %>% filter(team_jersey_number==10) %>%
  arrange(desc(overall))%>%
  select(short_name, team_jersey_number, overall)
#Messi

fifa20 %>% filter(team_jersey_number==11) %>%
  arrange(desc(overall))%>%
  select(short_name, team_jersey_number, overall)
#Salah


#BEST TEAMS AGED 23 OR LESS AND MORE THAN 23:
#<=23
fifa20leq23 <- fifa20 %>% filter(age <= 23)
#Assuming a 4231 formation, we come up with the best team on fifa20 or the 
#fifa world 11
#striker:
#positions to be considered: ST, CF, LS, RS, LF, RF
fifa20leq23 %>% filter(team_position %in% c("ST","CF","LS","RS","LF","RF")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#Havertz

#Wingers: 2 wingers to be picked, one on the left and one on the right.
#positions to be considered: RW, LW
fifa20leq23 %>% filter(team_position %in% c("LW","RW")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#Mbappe, Coman

#one AM, positions: CAM, LAM, RAM
fifa20leq23 %>% filter(team_position %in% c("CAM","LAM","RAM")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#Alli

#2 midfielders, one on the right and one on the left
#right midfielder: RCM, CDM, RDM, CM
fifa20leq23 %>% filter(team_position %in% c("RCM","CDM","RDM","CM")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#Torreira
#left midfielder: positions to be considered: LCM, CDM, LDM, CM
fifa20leq23 %>% filter(team_position %in% c("LCM","CDM","LDM","CM")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#Frenkie De Jong

#2wing backs, one on the right and one on the left:
#left back, positions to be considered: LB, LWB
fifa20leq23 %>% filter(team_position %in% c("LWB","LB")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#Grimaldo
#right back, positions to be considered: RB, RWB
fifa20leq23 %>% filter(team_position %in% c("RWB","RB")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#Alexander Arnold

#2 centre backs
#positions to be considered: LCB, RCB
fifa20leq23 %>% filter(team_position %in% c("LCB","RCB")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#Sule and Lucas Hernandez

#Goalkeeper, positions: GK
fifa20leq23 %>% filter(team_position %in% c("GK")) %>%
  arrange(desc(overall))%>%
  select(short_name, team_position, overall)
#Donnarumma

#Therefore the best 11 under-23 or 23 is: Havertz, Mbappe, Coman, Torreira,
#Frenkie De Jong, Alli, Grimaldo, TAA, Sule, Lucas Hernandez and Donnarumma

#ONE INTERESTING OBSERVATION MADE IS THAT NO PLAYER AGED 23 OR LESS MADE IT 
#INTO THE FIFA WORLD 11 SO THE BEST TEAM AGED MORE THAN 23 IS THE SAME AS THE 
#FIFA WORLD 11.


####BEST PLAYER FOR TOP 10 RANKED COUNTRIES IN THE FIFA RANKINGS
#Current FIFA rankings:
# 1.Belgium, 2.France, 3.Brazil, 4.England, 5.Uruguay, 6.Croatia, 7.Portugal,
#8.Spain, 9.Argentina, 10.Colombia
#Belgium:
fifa20 %>% filter(nationality=="Belgium")%>%arrange(desc(overall))%>%
  select(short_name, club, overall) %>% top_n(1)
#Hazard and De bruyne
#France
fifa20 %>% filter(nationality=="France")%>%arrange(desc(overall))%>%
  select(short_name, club, overall) %>% top_n(1)
#Mbappe, Kante and Griezmann
#Brazil
fifa20 %>% filter(nationality=="Brazil")%>%arrange(desc(overall))%>%
  select(short_name, club, overall) %>% top_n(1)
#Neymar
#England
fifa20 %>% filter(nationality=="England")%>%arrange(desc(overall))%>%
  select(short_name, club, overall) %>% top_n(1)
#Harry Kane
#Uruguay
fifa20 %>% filter(nationality=="Uruguay")%>%arrange(desc(overall))%>%
  select(short_name, club, overall) %>% top_n(1)
#Luis Suarez
#Croatia
fifa20 %>% filter(nationality=="Croatia")%>%arrange(desc(overall))%>%
  select(short_name, club, overall) %>% top_n(1)
#Luka Modric
#Portugal
fifa20 %>% filter(nationality=="Portugal")%>%arrange(desc(overall))%>%
  select(short_name, club, overall) %>% top_n(1)
#Cristiano Ronaldo
#Spain
fifa20 %>% filter(nationality=="Spain")%>%arrange(desc(overall))%>%
  select(short_name, club, overall) %>% top_n(1)
#David De Gea, Sergio Ramos and Sergio Busquets
#Argentina
fifa20 %>% filter(nationality=="Argentina")%>%arrange(desc(overall))%>%
  select(short_name, club, overall) %>% top_n(1)
#Lionel Messi
#Colombia
fifa20 %>% filter(nationality=="Colombia")%>%arrange(desc(overall))%>%
  select(short_name, club, overall) %>% top_n(1)
#James Rodriguez



####PLAYERS WITH THE BEST SKILL MOVES WITH OVERALL >=85
fifa20%>% filter(overall>=85,skill_moves==5)%>%arrange(desc(skill_moves))%>%
  select(short_name, club, nationality, team_position,overall,skill_moves)%>%
  group_by(skill_moves) %>% top_n(1)%>%ungroup()

####PLAYERS WITH BEST WEAK FOOT WITH OVERALL >=85
fifa20%>% filter(overall>=85,weak_foot==5)%>%arrange(desc(weak_foot))%>%
  select(short_name, club, nationality, team_position,overall,weak_foot)%>%
  group_by(weak_foot) %>% top_n(1)%>%ungroup()


##Are skill_moves and dribbling correlated?
#Boxplot of skill_moves vs dribbling
fifa20 %>% ggplot(aes(as.factor(skill_moves),dribbling))+geom_boxplot()+
  labs(x="skill_moves out of 5",y="dribbling score out of 100",
       title = "Boxplot of skill_moves vs dribbling")
#They are positively correlated
#Strength of correlation:
cor(fifa20$skill_moves, fifa20$dribbling, method = "spearman",
               use = "complete.obs")
#high positive correlation.

##Are players with a strong weak foot good at shooting?
#Boxplot of weak foot vs shooting
fifa20 %>% ggplot(aes(as.factor(weak_foot),shooting))+geom_boxplot()+
  labs(x="weak_foot out of 5",y="shooting score out of 100",
       title = "Boxplot of weak foot vs shooting")
#Strength of correlation:
cor(fifa20$weak_foot, fifa20$shooting, method = "spearman",
    use = "complete.obs")
#weak positive correlation


####split work rate into 2 columns: attack and defence work rate:
fifa20 <- separate(fifa20, work_rate, into = c("attack_workrate","defence_workrate"),
         sep = "/")
#good players with high attack and defence workrates:
fifa20%>%filter(overall>=85,attack_workrate=="High",defence_workrate=="High")%>%
  arrange(desc(overall))%>%
  select(short_name, club, nationality, team_position,
         attack_workrate, defence_workrate, overall)%>%group_by(overall)%>%
  top_n(1)%>%ungroup()

#Laziest players on fifa20
fifa20%>%filter(attack_workrate=="Low",defence_workrate=="Low")%>%
  arrange(desc(overall))%>%
  select(short_name, club, nationality, team_position,
         attack_workrate, defence_workrate, overall)%>%group_by(overall)%>%
  top_n(1)%>%ungroup()


####SUMMARIES OF FEW COLUMNS
sum_stats <- fifa20 %>% select(overall, potential, value_eur, wage_eur,
                               release_clause_eur,
                    pace, shooting, passing,
                   dribbling, physic, defending)
summary(sum_stats)

#release_clause_eur is NA for 1298 players, this is because these players do not 
#have a release clause inlcuded in their current contract.

#pace, shooting, passing, dribbling, defending and physic is NA for 2036 players,
#lets find out why
pace_na <- fifa20 %>% filter(is.na(pace),is.na(dribbling),is.na(shooting),
                             is.na(passing),is.na(defending),is.na(physic))%>%
  select(short_name, club, nationality, team_position, overall)
dim(pace_na) #2036 players with NAs.
#team positions of these players:
unique(pace_na$team_position)
#We can see that goalkeepers, subs and reserve team players do not have 
#values for these 6 attributes.
pace_na %>% filter(team_position=="")
#There are 40 rows in this dataframe where the position is "". These players
#are fictional and do not belong to any club.




#RELEASE CLAUSE EDA
#players with the highest release clauses in euros:
fifa20 %>% arrange(desc(release_clause_eur)) %>%
  select(short_name, club, nationality, overall, wage_eur, 
         value_eur, release_clause_eur)
#Messi and Neymar have the highest release clauses in euros

#Are value and release clause correlated?
cor(fifa20$value_eur, fifa20$release_clause_eur, method = "pearson",
    use = "complete.obs")
#We see that value and release clause have very high positive correlation,
#correlation coeeficient almost equal to 1.

##Are wage and release clause correlated?
cor(fifa20$wage_eur, fifa20$release_clause_eur, method = "pearson",
    use = "complete.obs")
#We also see that wage and release clause have high positive correlation.

#Do talented young players have high release clauses?
elite_young <- fifa20 %>% filter(age <= 23)%>%
  mutate(elite_pot_inc = potential-overall)%>%
  select(short_name, club, nationality, overall, potential, 
         elite_pot_inc, release_clause_eur) %>%
  arrange(desc(elite_pot_inc))
elite_young
#Check if 'elite_pot_inc' and 'release_clause_eur' are correlated
cor(elite_young$elite_pot_inc, elite_young$release_clause_eur,
    method = "pearson",use = "complete.obs")
#They have a low negative correlation. One would expect, higher possibility of
#increase in overall would result in the player having a bigger release clause
#but that is not the case here.
#elite_young %>% ggplot(aes(as.factor(elite_pot_inc), release_clause_eur))+
# geom_boxplot()
 

#Do older players have smaller release clauses?
cor(fifa20$age, fifa20$release_clause_eur, method = "pearson",
    use = "complete.obs")
#There is negligable correlation between these 2 variables which is surprising.


####ATTACK STATS:
attack_stats <- fifa20%>% select(shooting, passing, attacking_crossing,
                                 attacking_finishing, attacking_heading_accuracy,
                                 attacking_short_passing, attacking_volleys)
source("http://www.sthda.com/upload/rquery_cormat.r")
require("corrplot")
rquery.cormat(attack_stats)
#Almost all pairs are positively correlated to each other.
#One exception is the correlation b/w heading and crossing. There is a -ve 
#correlation b/w these 2 variables and it makes sense since a good crosser of the
#ball is the one delivering the crosses to the player whos good at heading.



#GOALKEEPING STATS:
gk_stats <- fifa20%>% filter(team_position=="GK")%>%
  select(overall, goalkeeping_diving, goalkeeping_handling,goalkeeping_kicking,
         goalkeeping_positioning, goalkeeping_reflexes)
source("http://www.sthda.com/upload/rquery_cormat.r")
require("corrplot")
rquery.cormat(gk_stats)
#kicking is not as highly correlated to GK overall when compapred to 
#other goalkeeping attributes.


#DEFENDING STATS:
defence_stats <- fifa20%>% select(defending, defending_marking,
                                  defending_standing_tackle, defending_sliding_tackle,
                                  physic)
source("http://www.sthda.com/upload/rquery_cormat.r")
require("corrplot")
rquery.cormat(defence_stats)
#Though physique is not highly correlated to defence stats, having a high
#physique rating out of 100 implies that the player is good at defending.


#POWER STATS:
power_stats <- fifa20%>%select(physic, power_shot_power, power_jumping,
                               power_stamina, power_strength, power_long_shots)
source("http://www.sthda.com/upload/rquery_cormat.r")
require("corrplot")
rquery.cormat(power_stats)
#long shots and sho power have high +ve correlation which makes sense
#power strength and physique have high +ve correlation too which makes sense.


######INTERCEPTIONS AND DEFENDING:
cor(fifa20$defending, fifa20$mentality_interceptions, method = "pearson",
    use = "complete.obs")
#Very high +ve correlation

#INTERCEPTIONS FOR DEFNSIVE MIDS, WING BACKS AND CENTRE BACKS:
unique(fifa20$team_position)
#DEFENSIVE MIDS:
mid_int_def <- fifa20%>%filter(team_position %in% c("CDM","LDM","RCM","LCM","CM"))
cor(mid_int_def$defending, mid_int_def$mentality_interceptions)
#Very high positive correlation(.93)

#WING BACKS:
wb_int_def <- fifa20%>%filter(team_position %in% c("LB","RB","LWB","RWB"))
cor(wb_int_def$defending, wb_int_def$mentality_interceptions)

#CENTRE BACKS:
cb_int_def <- fifa20%>%filter(team_position %in% c("LCB","RCB"))
cor(cb_int_def$defending, cb_int_def$mentality_interceptions)
#mid way between defensive mids and wingbacks(.92)


######POSITIONING AND FINISHING:
cor(fifa20$mentality_positioning, fifa20$attacking_finishing, method = "pearson",
    use = "complete.obs")
#very high positive correlation.


#####STANDING TACKLING AND DEFENDING
#STANDING TACKLING FOR DEFNSIVE MIDS, WING BACKS AND CENTRE BACKS:
unique(fifa20$team_position)
#DEFENSIVE MIDS:
mid_sttack_def <- fifa20%>%filter(team_position %in% c("CDM","LDM","RCM","LCM","CM"))
cor(mid_sttack_def$defending, mid_sttack_def$defending_standing_tackle)
#Very high positive correlation(.95)

#WING BACKS:
wb_sttack_def <- fifa20%>%filter(team_position %in% c("LB","RB","LWB","RWB"))
cor(wb_sttack_def$defending, wb_sttack_def$defending_standing_tackle)

#CENTRE BACKS:
cb_sttack_def <- fifa20%>%filter(team_position %in% c("LCB","RCB"))
cor(cb_sttack_def$defending, cb_sttack_def$defending_standing_tackle)
#best(.955)


#####SLIDING TACKLING AND DEFENDING
#SLIDING TACKLING FOR DEFNSIVE MIDS, WING BACKS AND CENTRE BACKS:
unique(fifa20$team_position)
#DEFENSIVE MIDS:
mid_sltack_def <- fifa20%>%filter(team_position %in% c("CDM","LDM","RCM","LCM","CM"))
cor(mid_sltack_def$defending, mid_sltack_def$defending_sliding_tackle)
#Very high positive correlation(.917)

#WING BACKS:
wb_sltack_def <- fifa20%>%filter(team_position %in% c("LB","RB","LWB","RWB"))
cor(wb_sltack_def$defending, wb_sltack_def$defending_sliding_tackle)

#CENTRE BACKS:
cb_sltack_def <- fifa20%>%filter(team_position %in% c("LCB","RCB"))
cor(cb_sltack_def$defending, cb_sltack_def$defending_sliding_tackle)
#similar to wingbacks (.91)




####VISION AND PASSING:
cor(fifa20$passing, fifa20$mentality_vision, method = "pearson",
    use = "complete.obs")
#very high +ve correlation.


####FINISHING AND PENALTIES:
cor(fifa20$attacking_finishing, fifa20$mentality_penalties, method = "pearson",
    use = "complete.obs")
#very high +ve correlation.



######DIFFERENCES BETWEEN FORWARD LINE, MIDFIELD AND DEFENCE:
unique(fifa20$team_position)
###PACE
#wingers:
fifa20 %>% filter(team_position %in% c("RW","LW")) %>% 
  summarise(avg_pace = mean(pace))   #78.6

#Strikers and forwards:
fifa20 %>% filter(team_position %in% c("ST","CF","LS","RS","RF","LF")) %>% 
  summarise(avg_pace = mean(pace))   #70.4

#Attacking mids:
fifa20 %>% filter(team_position %in% c("CAM","RM","LM","RAM","LAM")) %>% 
  summarise(avg_pace = mean(pace))   #75.9

#Defensive mids:
fifa20 %>% filter(team_position %in% c("RCM","CDM","LDM","LCM","RDM","CM")) %>% 
  summarise(avg_pace = mean(pace))   #64.7

#Wing backs:
fifa20 %>% filter(team_position %in% c("RB","LB","LWB","RWB")) %>% 
  summarise(avg_pace = mean(pace))   #73.7

#Centre backs:
fifa20 %>% filter(team_position %in% c("RCB","LCB","CB")) %>% 
  summarise(avg_pace = mean(pace))   #57.5
#Wingers are the fastest and cbs are the slowest.


####SHOOTING:
#wingers:
fifa20 %>% filter(team_position %in% c("RW","LW")) %>% 
  summarise(avg_shooting = mean(shooting))   #65.7

#Strikers and forwards:
fifa20 %>% filter(team_position %in% c("ST","CF","LS","RS","RF","LF")) %>% 
  summarise(avg_shooting = mean(shooting))   #68.6

#Attacking mids:
fifa20 %>% filter(team_position %in% c("CAM","RM","LM","RAM","LAM")) %>% 
  summarise(avg_shooting = mean(shooting))   #63.1

#Defensive mids:
fifa20 %>% filter(team_position %in% c("RCM","CDM","LDM","LCM","RDM","CM")) %>% 
  summarise(avg_shooting = mean(shooting))   #57.8

#Wing backs:
fifa20 %>% filter(team_position %in% c("RB","LB","LWB","RWB")) %>% 
  summarise(avg_shooting = mean(shooting))   #47.8

#Centre backs:
fifa20 %>% filter(team_position %in% c("RCB","LCB","CB")) %>% 
  summarise(avg_shooting = mean(shooting))   #38.3
#strikers have the best shooting and centre backs have the worst shooting.



###PASSING
#wingers:
fifa20 %>% filter(team_position %in% c("RW","LW")) %>% 
  summarise(avg_passing = mean(passing))   #63.8

#Strikers and forwards:
fifa20 %>% filter(team_position %in% c("ST","CF","LS","RS","RF","LF")) %>% 
  summarise(avg_passing = mean(passing))   #57.6

#Attacking mids:
fifa20 %>% filter(team_position %in% c("CAM","RM","LM","RAM","LAM")) %>% 
  summarise(avg_passing = mean(passing))   #64.9

#Defensive mids:
fifa20 %>% filter(team_position %in% c("RCM","CDM","LDM","LCM","RDM","CM")) %>% 
  summarise(avg_passing = mean(passing))   #65.5

#Wing backs:
fifa20 %>% filter(team_position %in% c("RB","LB","LWB","RWB")) %>% 
  summarise(avg_passing = mean(passing))   #60.6

#Centre backs:
fifa20 %>% filter(team_position %in% c("RCB","LCB","CB")) %>% 
  summarise(avg_passing = mean(passing))   #50.8
#defensive mids have the best passing and centre backs have the worst passing.



###DRIBBLING
fifa20 %>% filter(team_position %in% c("RW","LW")) %>% 
  summarise(avg_dribbling = mean(dribbling))   #72.3

#Strikers and forwards:
fifa20 %>% filter(team_position %in% c("ST","CF","LS","RS","RF","LF")) %>% 
  summarise(avg_dribbling = mean(dribbling))   #67.7

#Attacking mids:
fifa20 %>% filter(team_position %in% c("CAM","RM","LM","RAM","LAM")) %>% 
  summarise(avg_dribbling = mean(dribbling))   #71.1

#Defensive mids:
fifa20 %>% filter(team_position %in% c("RCM","CDM","LDM","LCM","RDM","CM")) %>% 
  summarise(avg_dribbling = mean(dribbling))   #67.3

#Wing backs:
fifa20 %>% filter(team_position %in% c("RB","LB","LWB","RWB")) %>% 
  summarise(avg_dribbling = mean(dribbling))   #65.3

#Centre backs:
fifa20 %>% filter(team_position %in% c("RCB","LCB","CB")) %>% 
  summarise(avg_dribbling = mean(dribbling))   #51.9
#wingers have the best dribbling and centre backs have the worst dribbling.



###DEFENDING
fifa20 %>% filter(team_position %in% c("RW","LW")) %>% 
  summarise(avg_defending = mean(defending))   #37.0

#Strikers and forwards:
fifa20 %>% filter(team_position %in% c("ST","CF","LS","RS","RF","LF")) %>% 
  summarise(avg_defending = mean(defending))   #33.4

#Attacking mids:
fifa20 %>% filter(team_position %in% c("CAM","RM","LM","RAM","LAM")) %>% 
  summarise(avg_defending = mean(defending))  #42.6

#Defensive mids:
fifa20 %>% filter(team_position %in% c("RCM","CDM","LDM","LCM","RDM","CM")) %>% 
  summarise(avg_defending = mean(defending))   #61.8

#Wing backs:
fifa20 %>% filter(team_position %in% c("RB","LB","LWB","RWB")) %>% 
  summarise(avg_defending = mean(defending))   #64.3

#Centre backs:
fifa20 %>% filter(team_position %in% c("RCB","LCB","CB")) %>% 
  summarise(avg_defending = mean(defending))   #68.4
#Centre backs have the best defense and forwards/strikers have the worst defense.



###PHYSIC
fifa20 %>% filter(team_position %in% c("RW","LW")) %>% 
  summarise(avg_physic = mean(physic))   #61.5

#Strikers and forwards:
fifa20 %>% filter(team_position %in% c("ST","CF","LS","RS","RF","LF")) %>% 
  summarise(avg_physic = mean(physic))   #68.9

#Attacking mids:
fifa20 %>% filter(team_position %in% c("CAM","RM","LM","RAM","LAM")) %>% 
  summarise(avg_physic = mean(physic)) #61.6

#Defensive mids:
fifa20 %>% filter(team_position %in% c("RCM","CDM","LDM","LCM","RDM","CM")) %>% 
  summarise(avg_physic = mean(physic))  #69.4

#Wing backs:
fifa20 %>% filter(team_position %in% c("RB","LB","LWB","RWB")) %>% 
  summarise(avg_physic = mean(physic))  #68.5

#Centre backs:
fifa20 %>% filter(team_position %in% c("RCB","LCB","CB")) %>% 
  summarise(avg_physic = mean(physic))  #73.6
#Centre backs have the best physic and wingers/attacking mids have the worst 
#physic.



####Very good players who are slow:
fifa20 %>% filter(overall >= 85) %>%
  arrange(pace)%>%
  select(short_name, club, team_position, overall, pace)
#Dani Parejo, Busquets, Kroos 
####very fast players who are bad overall
fifa20 %>% filter(overall <= 70) %>%
  arrange(desc(pace))%>%
  select(short_name, club, team_position, overall, pace)

####good players who are physically weak:
fifa20 %>% filter(overall >= 85) %>%
  arrange(physic)%>%
  select(short_name, club, team_position, overall, physic)
#Insigne, Mertens
####strong players who are bad overall:
fifa20 %>% filter(overall <= 70) %>%
  arrange(desc(physic))%>%
  select(short_name, club, team_position, overall, physic)


######centre backs attack the goal during set pieces to chip in with headers
#which centre backs are good at attacking the goal with their heads?
fifa20 %>% filter(team_position %in% c("LCB","RCB","CB"))%>%
  arrange(desc(attacking_heading_accuracy))%>%
  select(short_name, club, overall, attacking_heading_accuracy)
#Sergio Ramos expected.


####wing backs overlap with the wingers while attacking to deliver
#key crosses into the box
#which wing/full backs are the best crossers:
fifa20 %>% filter(team_position %in% c("RB","RWB","LWB","LB"))%>%
  arrange(desc(attacking_crossing))%>%
  select(short_name, club, overall, attacking_crossing)
#Joshua Kimmich expected.


####wingers who are very good crossers:
fifa20 %>% filter(team_position %in% c("RW","LW"))%>%
  arrange(desc(attacking_crossing))%>%
  select(short_name, club, overall, attacking_crossing)


####Target men are strikers who are tall, strong and good at heading.
#Best target men:
fifa20 %>% filter(height_cm >= 180, physic >= 75)%>%
  filter(team_position %in% c("ST","CF","LS","RS","RF","LF","SUB"))%>%
  arrange(desc(attacking_heading_accuracy))%>%
  select(short_name, club, overall, attacking_heading_accuracy)


####Very good players who are poor at penalties: excluding fbs, wbs, cbs, gks and subs
#since they dont generally take penalties
fifa20 %>% filter(overall >=85) %>%
  filter(!team_position %in% c("RB","RWB","LWB","LB","GK","LCB","RCB","CB","SUB"))%>%
  arrange(mentality_penalties) %>%
  select(short_name, club, overall, mentality_penalties)
#Frenkie De Jong


####Wingers who cut in and shoot to score goals: They are false wingers
#ie right wingers with preferred foot as left and have good shooting and vice versa
fifa20 %>% filter(team_position=="RW", preferred_foot=="Left")%>%
  arrange(desc(shooting))%>%
  select(short_name, club, shooting, overall)
#Leo Messi, Salah
fifa20 %>% filter(team_position=="LW", preferred_foot=="Right")%>%
  arrange(desc(shooting))%>%
  select(short_name, club, shooting, overall)
#Ronaldo, Hazard




###########COMPARISON BETWEEN MESSI AND RONALDO: MAJOR STATS OUT OF 100
#BASIC STATS:
messi_ronaldo_basic <- fifa20 %>% 
  filter(short_name =="L. Messi" | short_name=="Cristiano Ronaldo")%>%
  select(age, height_cm, weight_kg, overall, value_eur, wage_eur, release_clause_eur,
         international_reputation,
         weak_foot, skill_moves)

#6 MAIN ATTRIBUTES:
messi_ronaldo_6main <- fifa20 %>% 
  filter(short_name =="L. Messi" | short_name=="Cristiano Ronaldo")%>%
  select(pace, shooting, passing, dribbling, defending, physic)

#ATTACK ATTRIBUTES:
messi_ronaldo_attack <- fifa20 %>% 
  filter(short_name =="L. Messi" | short_name=="Cristiano Ronaldo")%>%
  select(attacking_crossing, attacking_finishing, attacking_heading_accuracy,
         attacking_short_passing, attacking_volleys)

#MOVEMENT ATTRIBUTES:
messi_ronaldo_move <- fifa20 %>% 
  filter(short_name =="L. Messi" | short_name=="Cristiano Ronaldo")%>%
  select(movement_acceleration, movement_sprint_speed, movement_agility,
         movement_reactions, movement_balance)

#POWER ATTRIBUTES:
messi_ronaldo_power <- fifa20 %>% 
  filter(short_name =="L. Messi" | short_name=="Cristiano Ronaldo")%>%
  select(power_shot_power, power_jumping, power_stamina,
         power_strength, power_long_shots)

#DEFENSE ATTRIBUTES:
messi_ronaldo_def <- fifa20 %>% 
  filter(short_name =="L. Messi" | short_name=="Cristiano Ronaldo")%>%
  select(defending_marking, defending_standing_tackle, defending_sliding_tackle)

#MENTALITY ATTRIBUTES:
messi_ronaldo_ment <- fifa20 %>% 
  filter(short_name =="L. Messi" | short_name=="Cristiano Ronaldo")%>%
  select(mentality_aggression, mentality_interceptions, mentality_positioning, 
         mentality_penalties, mentality_vision, mentality_composure)

#Difference in basic stats:
diff_basic <- data.frame(diff(as.matrix(messi_ronaldo_basic)))
diff_basic

#Difference in 6 main attributes:
diff_6main <- data.frame(diff(as.matrix(messi_ronaldo_6main)))
diff_6main
#Difference in attack attributes:
diff_attack <- data.frame(diff(as.matrix(messi_ronaldo_attack)))
diff_attack
#Difference in movement attributes:
diff_move <- data.frame(diff(as.matrix(messi_ronaldo_move)))
diff_move
#Difference in defence attributes:
diff_def <- data.frame(diff(as.matrix(messi_ronaldo_def)))
diff_def
#difference in power attributes:
diff_power <- data.frame(diff(as.matrix(messi_ronaldo_power)))
diff_power
#difference in mentality attributes:
diff_ment <- data.frame(diff(as.matrix(messi_ronaldo_ment)))
diff_ment
#These are the differences between messi and ronaldo. 
#Who has got the best overall stats?
total_diff <- bind_cols(diff_6main, diff_attack, diff_def, diff_ment, 
          diff_move, diff_power)
total_diff
#adding values across columns:
set.seed(4321)
sum <- 0
for (i in 1:length(total_diff)) 
{
 sum <- sum + total_diff[[i]] 
}
print(sum)
#######OVERALL RONALDO LEADS MESSI BY 20 POINTS IN NUMERIC STATS.



