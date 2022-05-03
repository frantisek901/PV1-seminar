#### SKript pro naètení, vyèištìní a uložení dat pro semináø KSS/PV1

## Encoding: windows-1250
## Vytvoøil: 2022-05-02 FrK
## Upravil:  2022-05-03 FrK

## Notes:
#  1) v první øadì naètu všechna data
#  2) zkusím sice vše, ale když bude problém, pøednost mají ta troje data, která doprovází otázky
#  3) data využiju na zadání úkolu è. 8 a snad i 9 pro KSS/KA1
#



# Hlavièka ----------------------------------------------------------------

# Promazání
rm(list = ls())

# Packages
library(readr)
library(readxl)
library(writexl)
library(dplyr)
library(forcats)
library(tidyr)
library(tibble)
library(ggplot2)

# Vlastni funkce
prejmenuj = function(data, pozice, jmena) {
  names(data)[pozice] = jmena
  data
}


# Naèítání ----------------------------------------------------------------

## data1
df1 = read_xlsx("data1.xlsx") %>%
  select(3, 4, 6, 10, 13) %>%
  prejmenuj(1:5, c("Pohlaví", "Vìk", "Dìti", "Adopce", "Adopce postiženého")) %>%
  mutate(across(.cols = everything(), ~factor(.x)))

# Uložení vyèištìných dat pro KSS/KA1
write_xlsx(df1, "../KSS-KA1/!data/df1.xlsx")



## data2
df2 = read_xlsx("data2.xlsx") %>%
  select(4, 10, 5:7) %>%
  prejmenuj(1:5, c("Vzdìlání", "Informovanost", "Svìdek", "Zpùsobeno neinformovaností", "Dùležitý problém"))%>%
  mutate(
    Vzdìlání =
      recode(
        Vzdìlání,
        `Odborné uèilištì s výuèním listem` = "Vyuèen/a", `Støední škola s maturitou` = "SŠ",
        `Vysoká škola (bakaláøské studium)` = "VŠ", `Vysoká škola (magisterské studium)` = "VŠ",
        `Vyšší odborné vzdìlání` = "SŠ", `Základní škola` = "ZŠ") %>%
      factor(levels = c("ZŠ", "Vyuèen/a", "SŠ", "VŠ")),
    Informovanost = factor(Informovanost, levels = c("Ano", "Spíše ano", "Spíše ne", "Ne")),
    `Zpùsobeno neinformovaností` = factor(`Zpùsobeno neinformovaností`, levels = c("Ano", "Spíše ano", "Spíše ne", "Ne")),
    across(.cols = everything(), ~factor(.x))
  )

# Uložení vyèištìných dat pro KSS/KA1
write_xlsx(df2, "../KSS-KA1/!data/df2.xlsx")



## df3
df3 = read_csv("data3.csv") %>%
  select(2:6, 8) %>%
  prejmenuj(1:6, c("Vìk", "Pohlaví", "Vzdìlání", "Ví", "Co", "Prevence")) %>%
  rowid_to_column("ID") %>%
  separate(Co, into = paste0("pol", 1:4), sep = ";") %>%
  pivot_longer(6:9, values_to = "Uvedeno") %>%
  select(-name) %>%
  filter(!is.na(Uvedeno))

df3 %>%
  ggplot() +
  aes(x = Uvedeno, fill = Uvedeno) +
  geom_bar()


