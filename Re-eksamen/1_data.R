#Indlæs det forrige script
source("0_functions.R")
# 1_data.R Indlæs data og formatér

# I projektmappen ligger ESV filen i .sav
# Jeg importerer filen med read_sav() funktionen og binder den til variablen evs

evs <- read_sav(file='data/EVS.sav')
evs

# Gør kategorierne læsbare

evs <- evs %>% as_factor(only_labelled = TRUE)             # Convert from SPSS format to regular factors
labels <- evs %>% map_chr(~.x %>% attr("label"))           # Extract variable labels from each column
evs <- droplevels(evs)                                     # We throw away all the empty labels
new.colnames <- paste(colnames(evs), "-", labels)          # Pasting the short orignal variable name and the longer label together
colnames(evs) <- new.colnames                              # Assigning the new variable names

#Jeg finder de variable som har et overvæld af levels (svar kategorier (mere end 15)), og som kan føre til unødig kompleksistet i analysen ----
#Jeg udelader disse variabler fra datasættet

#evs.set <- evs %>% select(where(is.factor))
#evs.set$`country - country code (ISO 3166-1 numeric code)`
#nlev <- evs.set %>% map_int(~.x %>% nlevels)
#head(nlev) # With head we get the first 10 values
#evs.set <- evs.set %>% select(which(nlev < 15))
#-----


# Gem filen
save(evs, file = "saved/1_data.Rda")
