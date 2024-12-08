source("0_functions.R")

load(file = "saved/2_Overview.Rda") #Henter endelige version af omkodet data
#load(file = "saved/2.1_Overview.Rda") #Henter første version af ukodet data

# Udvælg supplerende variabler
sup    <- set %>% select(Age, Year, `Fathers education`, `Mothers education`, Education, `Father born in country`, Gender, Arbejdsløs, `Har modtaget dagpenge`)
# Udvælg aktive variabler. 
# Jeg starter med blot at vælge ale de kolonner som IKKE er supplerende variabler.
active <- set %>% select(-any_of(colnames(sup)))
map.ind(soc.mca(active,passive ="Missing"))

#Efter førsteanalyse foretager jeg omkodning ----
#Kategorier jeg har fjernet:
#Bekymring ældre
#"Staten bør sikre behov" = set$`Vigtigt at sikre behov`,
# "Demokratisk deltagelse" = deltagelse,
#deltagelse <- data.frame(
#  "Stemmer til ft.valg" = set$national_vote,
#  "Demokrati er vigtigt" = set$demo_important,
#  check.names = FALSE)
# "Tillid i offentligt ansatte" = set$Civil_service_conf,
#"Bekymrer sig om immigranter" = set$Bekymring_immigration,
#"Holdning til immigration" = set$`Immigration effekt på velfærd`,
#   "Staten bør sikre behov" = set$`Vigtigt at sikre behov`,

#deltagelse <- data.frame(
#  "Stemmer til ft.valg" = set$national_vote,
#  "Demokrati er vigtigt" = set$demo_important,
#  "Militær styre" = set$polsys_army,
#  "Stærk leder" = set$polsys_strong_leader,
#  "Teknokrati" = set$polsys_experts,
#  check.names = FALSE)
#"Demokratisk deltagelse" = deltagelse,

#tillid <- data.frame(
#  "Tillid i omsorgssystemet" = set$Social_security_conf,
#  "Tillid i offentligt ansatte" = set$Civil_service_conf,  
#  "Tillid til regeringen" = set$Government_conf,
#  check.names = FALSE)
#"Tillid til institutioner" = tillid,
# 
#"Bekymrer sig om ældre" = set$Bekymring_ældre,
#"Bekymrer sig om svage" = set$Bekymring_svage,
#"Bekymrer sig om immigranter" = set$Bekymring_immigration,
#"Holdning til immigration" = set$`Immigration effekt på velfærd`,

#Headings Brutto liste ----

goder <- data.frame(
  "Bekymrer sig om arbejdsløse" = set$Bekymring_arbejdsløse,
  "Holdning til sociale ydelser" = set$`Sociale ydelser er ydmygende`,
  check.names = FALSE)

soc.dem_neo.lib <- data.frame(
  "Holdning til arbejdsmarkedet" = set$`Aktiverende eller aktiv AM`,
  "Bør staten sikre lighed eller lige indkomst" = set$`Ligehed eller lige adgang`,
  "Individ vs. Stat forsørgelsesansvar" = set$`Individ versus Stat`,
  "Hvor foreneligt er demokrati med arbejdsløshedsstøtte" =set$`Demo=dagpenge`, check.names = FALSE)

headings <- list("offentlige goder" = goder,
                 "Socialdemokratisme / Neoliberalisme" = soc.dem_neo.lib)

list.of.headings <- headings %>% map(~ .x %>% table())
list.of.headings
summary(soc.dem_neo.lib$`Holdning til arbejdsmarkedet`)
#----
#headings <- list("Offentlige goder" = set %>% select(Bekymring_arbejdsløse, ----
#                                                     Bekymring_ældre,
#                                                     Bekymring_immigration,
#                                                     `Dagpenge er godt`,
#                                                     `Sociale ydelser er ydmygende`,
#                                                     `Immigration effekt på velfærd`),
#                 
#                 "Tillid til institutioner" = set %>% select(Civil_service_conf,
#                                                             Social_security_conf,
#                                                             Government_conf),
#                 
#                 "Politisk system" = set %>% select(polsys_army,
#                                                    polsys_strong_leader,
#                                                    polsys_experts,
#                                                    polsys_demo),
#                 
#                 "Demokratisk deltagelse" = set %>% select(demo_important,
#                                                           national_vote),
#                 
#                 "Socialdemokratisme/Neoliberalisme" = set %>% select(`Individ versus Stat`,
#                                                                      `Ligehed eller lige adgang`,
#                                                                      `Vigtigt at sikre lighed`,
#                                                                      `Vigtigt at sikre behov`,
#                                                                      `Aktiverende eller aktiv AM`,
#                                                                      `Demo=dagpenge`))
#
# ----

# My_first_analysis ----
r <- soc.mca(headings, passive = "Missing")
r
#Sæt Missing kategorier som passive

map.ind(r)
map.density(r, size = 0.8)


contribution(r, 1) #Hvilke svar-kategorier bidrager mest til fordelingen langs x-aksen?
contribution(r, 2) #Hvilke svar-kategorier bidrager mest til fordelingen langs y-aksen?

#----
# Hvordan fordeler kategorierne sig på koordinatsystemer?
map.ctr(r,ctr.dim =1:2, label.size = 2.5, point.size = 1)

# Er der variabler der bidrager for meget til dimensionerne?
contribution(r, dim=1:2, mode= "variable")
# dimensionerne ser bedre ud.

#KVALITETTSKONTROL

#mulige omkodninger
# Indeholder min analyse for små kategorier? ----
too.small <- extract_mod(r, dim =1:2)
view(too.small)

tiny <- too.small %>%filter(Frequency <=  r$n.ind *0.05)
tiny
#Jeg finder 0 svarkategorier som 5% eller mindre af respondenterne har udtrykt
#----
#Indeholder min analyse meget overlappende kategori-par? Eigen.check ----

check <- active %>% mca.eigen.check()
view(check)

check %>%arrange(-`First eigen`)
check %>%arrange(-`Max overlap (%)`)


a <- table(Strongman = active$polsys_strong_leader, Military = active$polsys_army)
a
chisq.test(a)
#----

#Følgende variabel par udlades af de aktive variabler
# National vote = Local vote
#Tillid kategori
#Bekymringer på tværs af kategorier










