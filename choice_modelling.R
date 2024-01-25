library(dplyr)
library(extraDistr)
library(mlogit)
library(car)

####                       ####
####  Duomenų generavimas  ####
####                       ####

# Generuojamos visos įmanomos atributų lygių kombinacijos

attributes <- expand.grid(
  company = c("Metalac", "NikolaTeslaAirport", "Jedinstvo", "Energoprojekt"),
  return_rate = c(0, 0.05, 0.10, 0.15),
  dividend = c(0, 1.5, 3.0, 4.5, 6),
  trend = c("Trend1", "Trend2", "Trend3")
)

set.seed(123) # nustatomas 'seed' tam, jog kas kartą generuojant alternatyvas bei paklaidas, jie išliktų tokie patys

# Sukuriamas masyvas ir sudedamos reikšmės

gen <- sample(c("male", "female"), size = 100, prob = c(0.3669, 0.6331), replace = TRUE)

# Lyčių procentai paimti tiesiogiai iš straipsnio

df <- data.frame(
  ID = rep(1:100, each = 3), # respondento nr.; kartojama 3x, nes yra 3 alternatyvos kiekvienam respondentui
  alternative = as.factor(rep(1:3, times = 100)), # alternatyvos Nr.
  choice = 0, # priskiriama pradinė reikšmė 0 kiekvienai alternatyvai
  gender = rep(gen, each = 3), # lytis kartojama tris kartus vienam respondentui, kadangi yra 3 alternatyvos
  attributes[sample(nrow(attributes), size = 100 * 3, replace = TRUE), ],
  epsilon = rgumbel(300) # at. Gumbel paklaidos
)

# Kintamųjų manipuliacija

df <- df %>% mutate(
  gender = as.factor(gender),
  company = as.factor(company),
  return_rate = as.numeric(return_rate),
  dividend = as.numeric(dividend),
  trend = as.factor(trend)
)


# Priskiria reikšmę 1 kintamajam 'choice' atsitiktinai

#  df <- df %>% 
#  group_by(ID) %>%
#  slice_sample(n = 1) %>%
#  mutate(temp = TRUE) %>%
#  full_join(df) %>% 
#  mutate(choice = case_when(temp ~ 1, TRUE ~ choice)) %>%
#  arrange(ID, alternative) %>% select(-temp)

###############################################################


####                           ####
####  Naudingumų skaičiavimas  ####
####                           ####


# Įvertintos 'beta' (part-worths) reikšmės iš straipsnio

# Priskiriami pavadinimai tam, jog algoritmas galėtų susieti reikšmes, esančias duomenų lentelėje 
# su atitinkamomis 'beta' reikšmėmis pagal indeksus

beta_company <- c("Metalac" = 3.35, "NikolaTeslaAirport" = 3.00, "Jedinstvo" = -6.14, "Energoprojekt" = -0.21)
beta_return <- c("0" = -4.13, "0.05" = -1.57, "0.1" = 0.90, "0.15" = 4.80)
beta_dividend <- c("0" = -28.14, "1.5" = -6.57, "3" = 1.57, "4.5" = 12.30, "6" = 20.84)
beta_trend <- c("Trend1" = 12.25, "Trend2" =  8.10, "Trend3" = -20.35)

v_values <- numeric(nrow(df)) # sukuriamas tuščias vektorius į kuri bus saugojamos reikšmės

for (i in 1:nrow(df)) {
  # Ištraukiami atributo lygiai alternatyvoms nuo 'i' iki duomenų lent. pab.
  company <- df$company[i]
  return_rate <- as.character(df$return_rate[i])
  dividend <- as.character(df$dividend[i])
  trend <- df$trend[i]
  
  # Apskaičiuojama 'V' reikšmė kiekvienai iš alternatyvų
  
  v_values[i] <- beta_company[company] + beta_return[return_rate] + beta_dividend[dividend] + beta_trend[trend]
}

df$V <- v_values # sukuriamas naujas stulpelis ir pridedamos V reikšmės kiekvienai alternatyvai
df$U <- df$V + df$epsilon # apskaičiuojamas kiekvienos alternatyvos naudingumas pagal straipsnyje nurodytą f-lę

# Jei naudingumas (U) didžiausias, priskiriama reikšmė 1 kintamajam 'choice' (daroma ta pati prielaida kaip ir
# straipsnyje, jog individas alternatyvą renkasi tada ir tik tada, jei jos naudingumas yra didžiausias)

df <- df %>%
  group_by(ID) %>%
  mutate(choice = ifelse(U == max(U), 1, 0))


####                                                 ####
####  Modelio pritaikymas bei aprašomoji statistika  ####
####                                                 ####


# Dazniu lentele (%)

xtabs(choice ~ company, data = df)
xtabs(choice ~ return_rate, data = df)
xtabs(choice ~ dividend, data = df)
xtabs(choice ~ trend, data = df)

####


fit <- mlogit(choice ~ company + return_rate + dividend + return_rate:trend, data = df)
summary(fit)

fit2 <- mlogit(choice ~ company + return_rate + dividend + return_rate:trend 
               + dividend:gender + trend:gender, data = df)
summary(fit2)

# Naudojant choice ~ company + return_rate + dividend + trend - visi kint. tampa stat. nereikšm. Išbandžius visas kitas
# interakcijas nepasiekiamas toks rezultatas kaip su 'return_rate:trend'. Taip pat, naudojant išraišką 'x1 * x2' apskritai
# yra gaunami labai prasti rezultatai kur praktiškai visi kint. yra stat. nereikšm.

# Žymenys:
# 'x1:x2' includes only the interaction term between x1 and x2.
# 'x1*x2' includes the main effects of x1 and x2 along with their interaction term.

# Frequencies of alternatives:choice -> alternatyvos pasirinkimo tikimybė