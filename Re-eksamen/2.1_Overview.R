#Jeg skal danne mig et overblik over de basale træk ved kategorierne i datasættet
source("0_functions.R")
load(file = "saved/1_data.Rda")

freq.tab <- function(x, title = deparse(substitute(x))){
  tp <- table(x) %>% enframe(name = "label", value="n")
  tp <- tp %>% mutate("share" = round(n/sum(n),2), "Cum. share"= cumsum(share)) %>% flextable() %>% set_caption(title)
  tp
}

#Jeg skal udvælge mine aktive og mine baggrunds variabler ----
#Hvilke variabler kan indikere noget om ens holdning til demokrati?
#Hvilke variabler kan indikere noget om ens holdning til offentlige goder?
#Jeg prøver med 2 sæt af variabler:
# Angående demokrati:
#v122 v131 v133 v134 v135 v136 v137 v138 v139 v140 v141 v148 v171 v172
#Angående Offentlige goder
#v103 v106 v219 v220 v221 v222 v223 v224
#-----

#Total liste over variabler med frekvenstabel til hver ----
#list.of.tabs <- evs.set %>% map(~ .x %>% freq.tab) # This may take a while
#save_as_html(values = list.of.tabs, path = "output/tables.html")

# ----

#Jeg vil have en tabel liste over de variable jeg vil arbejde med, ----
#Først baggrundsvariabler og demografi, herefter variabler udvalgt udfra min rammeteori

set <- evs.set %>% select("Age" = `age_r - age recoded (6 intervals) (Q64)`,
                          "Year" = `year - survey year`,                                                             
                          
                          "Fathers education" =`v262_r - educational level father: recoded (Q99)`,   
                          "Mothers education" =`v263_r - educational level mother: recoded (Q100)`,
                          "Education" =`v243_r - educational level respondent: recoded (Q81)`,
                          "Father born in country" = `v230 - father born in [country] (Q68)`,
                          "Gender" = `v225_weight - CALIBRATION variable: Gender`,
                          "Arbejdsløs" =`v259 - respondent experienced unemployment longer than 3 months (Q96)`,
                          "Har modtaget dagpenge" = `v260 - dependency on social security during last 5 years respondent (Q97)`,
                          "Indkomst" = `v261 - households total net income (Q98) (standardized)`,
                          
                          "Civil_service_conf" = `v122 - how much confidence in: civil service (Q38H)`,
                          "Social_security_conf" = `v123 - how much confidence in: social security system (Q38I)`,
                          "Government_conf" = `v131 - how much confidence in: government (Q38Q)`,
                          "Gender_equality.im_demo" = `v141 - democracy: women have the same rights as men (Q39I)`,
                          "demo_important" = `v142 - importance of democracy (Q40)`,
                          "demo_own_country_important" = `v143 - democracy in own country (Q41)`,
                          "polsys_strong_leader" = `v145 - political system: strong leader (Q43A)`,
                          "polsys_experts" = `v146 - political system: experts making decisions (Q43B)`,
                          "polsys_army" = `v147 - political system: the army ruling (Q43C)`,
                          "polsys_demo" = `v148 - political system: democratic (Q43D)`,
                          "local_vote" = `v171 - vote in elections: local level (Q48A)`,
                          "national_vote" = `v172 - vote in elections: national level (Q48B)`,
                          
                          "Individ versus Stat" = `v103 - individual vs. state responsibility for providing (Q32A)`,
                          "Ligehed eller lige adgang" = `v106 - equalize incomes vs. incentives for individual effort (Q32D)`,
                          "Demo=dagpenge" = `v136 - democracy: people receive state aid for unemployment (Q39D)`,
                          "Bekymring_arbejdsløse" = `v218 - are you concerned with: unemployed people (Q61B)`,
                          "Bekymring_ældre" = `v217 - are you concerned with: elderly people (Q61A)`,
                          "Bekymring_immigration" = `v219 - are you concerned with: immigrants (Q61C)`,
                          "Bekymring_svage" = `v220 - are you concerned with: sick and disabled (Q61D)`,
                          "Vigtigt at sikre lighed" = `v221 - important: eliminating income inequalities (Q62A)`,
                          "Vigtigt at sikre behov" = `v222 - important: basic needs for all (Q62B)`,
                          "Dagpenge er godt" = `v149 - do you justify: claiming state benefits (Q44A)`,
                          "Immigration effekt på velfærd" = `v187 - immigrants are a strain on welfare system (Q52C)`,
                          "Aktiverende eller aktiv AM" = `v104 - take any job vs. right to refuse job when unemployed (Q32B)`,
                          "Sociale ydelser er ydmygende" = `v47 - humiliating receiving money without working (Q12B)`)

set %>% map(~table(.x, useNA = "always") %>% prop.table %>% round(2))

#list.of.tabs <- set %>% map(~ .x %>% freq.tab)
#save_as_html(values = list.of.tabs, path = "output/teori_tables.html")



#Omkod variabler ----
#så for små kategorier lægges sammen med de nærmeste >0.05)
#brug Mutate og recode



#Omkod N/A til "missing"
set <- set %>% map_df(~fct_na_value_to_level(.x, level = "Missing"))
#----

#Jeg gennemgår min opdaterede liste over de aktive variabler jeg vil inddrage i min MCA analyse
#list.of.tabs <- set %>% map(~ .x %>% freq.tab)
#save_as_html(values = list.of.tabs, path = "output/teori_tables.html")



#----
save(evs, set, file ="saved/2.1_Overview.Rda")


# Sammenhæng mellem holdning til demokrati og holdning til offentlige goder
