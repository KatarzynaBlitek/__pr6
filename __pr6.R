# Do badan przyjeto poziom istotnosci alfa = 0,05


#usuwanie kolumn gdzie sa same brakami------------------------------------------------------------------------------------------------------------------
dane <- dane %>% select_if(function(x){!all(is.na(x))})


library(dplyr)


#DANE zawierajace regon typ rodziny i poszczegolne kategorie produktow zbozowych---------------------------------------------------------------------------
danezRodzinyRegiony <- dane %>% select (Region=Regione, Typ.Rodziny=TIPFAM, Chleby=C_1101, Herbatniki=C_1102, Makarony=C_1103, Ryz=C_1104, Maka=C_1105, Pozostale=C_1106, Wyroby_cukiernicze_i_ciasta=C_1107)


#tabelka ze zbozami i chlebem - OGOLEM
zboza <- dane %>% select(C_1101, C_1102, C_1103, C_1104, C_1105, C_1106, C_1107)
#nazwy wierszy
colnames(zboza) <- c("Chleby","Herbatniki","Makarony","Ryz","Maka","Pozostale","Wyroby_cukiernicze_i_ciasta")


#USUWANIE WARTOSCI ODSTAJACYCH-----------------------------------------------------------------------------------------------------------------------
#chleb
iqrCHLEB<-IQR(zboza$Chleby) 
q1CHLEB<- quantile(zboza$Chleby,0.25) 
q3CHLEB<-quantile(zboza$Chleby,0.75) 
mild_lowCHLEB<-q1CHLEB-(1.5*iqrCHLEB) 
mild_highCHLEB<-q3CHLEB+(1.5*iqrCHLEB) 
NOWEchleby<-zboza$Chleby[zboza$Chleby>mild_lowCHLEB & zboza$Chleby<mild_highCHLEB]



#tabelka
#CHLEB <- table(zboza$Chleby[zboza$Chleby>mild_lowCHLEB & zboza$Chleby<mild_highCHLEB])
#CHLEB <-CHLEB %>% select(Chleby=Var1) 
#CHLEB <- as.numeric(CHLEB$Chleby)


#herbatniki
iqrHERB<-IQR(zboza$Herbatniki) 
q1HERB<- quantile(zboza$Herbatniki,0.25) 
q3HERB<-quantile(zboza$Herbatniki,0.75) 
mild_lowHERB<-q1HERB-(1.5*iqrHERB) 
mild_highHERB<-q3HERB+(1.5*iqrHERB) 
NOWEherbatniki<-zboza$Herbatniki[zboza$Herbatniki>mild_lowHERB & zboza$Herbatniki<mild_highHERB]

#tabelka
#HERBATNIKI <- as.data.frame(table(zboza$Herbatniki[zboza$Herbatniki>mild_lowHERB & zboza$Herbatniki<mild_highHERB]))
#HERBATNIKI <-HERBATNIKI %>% select(Herbatniki=Var1) 


#makarony
iqrMAKAR<-IQR(zboza$Makarony) 
q1MAKAR<- quantile(zboza$Makarony,0.25) 
q3MAKAR<-quantile(zboza$Makarony,0.75) 
mild_lowMAKAR<-q1HERB-(1.5*iqrMAKAR) 
mild_highMAKAR<-q3HERB+(1.5*iqrMAKAR) 
NOWEmakarony<-zboza$Makarony[zboza$Makarony>mild_lowMAKAR & zboza$Makarony<mild_highMAKAR]

#tabelka
#MAKARONY <- as.data.frame(table(zboza$Makarony[zboza$Makarony>mild_lowMAKAR & zboza$Makarony<mild_highMAKAR]))
#MAKARONY <- MAKARONY %>% select(Makarony=Var1)


#ryz
iqrRYZ<-IQR(zboza$Ryz) 
q1RYZ<- quantile(zboza$Herbatniki,0.25) 
q3RYZ<-quantile(zboza$Herbatniki,0.75) 
mild_lowRYZ<-q1RYZ-(1.5*iqrRYZ) 
mild_highRYZ<-q3RYZ+(1.5*iqrRYZ) 
NOWEryz<-zboza$Ryz[zboza$Ryz>mild_lowRYZ & zboza$Ryz<mild_highRYZ]

#tabelka
#RYZ <- as.data.frame(table(zboza$Ryz[zboza$Ryz>mild_lowRYZ & zboza$Ryz<mild_highRYZ]))
#RYZ <- RYZ %>% select(Ryz=Var1)



#maka
iqrMAKA<-IQR(zboza$Maka) 
q1MAKA<- quantile(zboza$Maka,0.25) 
q3MAKA<-quantile(zboza$Maka,0.75) 
mild_lowMAKA<-q1HERB-(1.5*iqrMAKA) 
mild_highMAKA<-q3HERB+(1.5*iqrMAKA) 
NOWEmaka<-zboza$Herbatniki[zboza$Maka>mild_lowMAKA & zboza$Maka<mild_highMAKA]  

#tabelka
#MAKA <- as.data.frame(table(zboza$Herbatniki[zboza$Maka>mild_lowMAKA & zboza$Maka<mild_highMAKA]))
#MAKA <- MAKA %>% select(Maka=Var1)



#pozostale
iqrPOZ<-IQR(zboza$Pozostale) 
q1POZ<- quantile(zboza$Pozostale,0.25) 
q3POZ<-quantile(zboza$Pozostale,0.75) 
mild_lowPOZ<-q1POZ-(1.5*iqrPOZ) 
mild_highPOZ<-q3POZ+(1.5*iqrPOZ) 
NOWEpozostale <- zboza$Pozostale[zboza$Pozostale>mild_lowPOZ & zboza$Pozostale<mild_highPOZ]

#POZOSTALE <- as.data.frame(table(zboza$Pozostale[zboza$Pozostale>mild_lowPOZ & zboza$Pozostale<mild_highPOZ]))
#POZOSTALE <- POZOSTALE %>% select(Pozostale=Var1)



#wyroby cukiernicz i ciasta

iqrCIASTA<-IQR(zboza$Wyroby_cukiernicze_i_ciasta) 
q1CIASTA<- quantile(zboza$Wyroby_cukiernicze_i_ciasta,0.25) 
q3CIASTA<-quantile(zboza$Wyroby_cukiernicze_i_ciasta,0.75) 
mild_lowCIASTA<-q1CIASTA-(1.5*iqrCIASTA) 
mild_highCIASTA<-q3CIASTA+(1.5*iqrCIASTA) 
NOWEciasta<-zboza$Wyroby_cukiernicze_i_ciasta[zboza$Wyroby_cukiernicze_i_ciasta>mild_lowCIASTA & zboza$Wyroby_cukiernicze_i_ciasta<mild_highCIASTA]

#CIASTA <- as.data.frame(table(zboza$Wyroby_cukiernicze_i_ciasta[zboza$Wyroby_cukiernicze_i_ciasta>mild_lowCIASTA & zboza$Wyroby_cukiernicze_i_ciasta<mild_highCIASTA]))
#CIASTA <- CIASTA %>% select(Wyroby_cukiernicze_i_ciasta=Var1)

summary(NOWEciasta)

  
#ANALIZA TYPOW RODZIN----------------------------------------------------------------------------------------------------------------------------------


#osoby samotne < 35-letnie w regionach 
Osoby_samotne_ponizej35 <- danezRodzinyRegiony %>% filter(Typ.Rodziny == "1") %>% select(Region,Chleby, Herbatniki, Makarony, Ryz, Maka, Pozostale, Wyroby_cukiernicze_i_ciasta)
#sumowane wedlug regionow
Osoby_samotne_ponizej35SUMA <- Osoby_samotne_ponizej35 %>% arrange(Region) %>% count(Region)


summary(Osoby_samotne_ponizej35SUMA$n)


#osoby samotne w wieku 65 lat i starsze
Osoby_samotne65 <- danezRodzinyRegiony %>% filter(Typ.Rodziny == "3") %>% select(Region,Chleby, Herbatniki, Makarony, Ryz, Maka, Pozostale, Wyroby_cukiernicze_i_ciasta)
#sumowane wedlug regionow
Osoby_samotne65SUMA <- Osoby_samotne65 %>% arrange(Region) %>% count(Region)

summary(Osoby_samotne65SUMA$n)


#pary bez dzieci z osob¹ kontaktowa poni¿ej 35 roku zycia
Pary_bez_Dzieci_ponizej35 <- danezRodzinyRegiony %>% filter(Typ.Rodziny == "4") %>% select(Region,Chleby, Herbatniki, Makarony, Ryz, Maka, Pozostale, Wyroby_cukiernicze_i_ciasta)
#sumowane wedlug regionow
Pary_bez_Dzieci_ponizej35SUMA <- Pary_bez_Dzieci_ponizej35 %>% arrange(Region) %>% count(Region)

summary(Pary_bez_Dzieci_ponizej35SUMA$n)


#Pary bez dzieci z osoba kontaktowa w wieku 65 lat i starsza
Pary_bez_Dzieci65 <- danezRodzinyRegiony %>% filter(Typ.Rodziny == "6") %>% select(Region,Chleby, Herbatniki, Makarony, Ryz, Maka, Pozostale, Wyroby_cukiernicze_i_ciasta)
#sumowane wedlug regionow
Pary_bez_Dzieci65SUMA <- Pary_bez_Dzieci65 %>% arrange(Region) %>% count(Region)

summary(Pary_bez_Dzieci65SUMA$n)


#Pary z 1 dzieckiem
Pary_1Dziecko <- danezRodzinyRegiony %>% filter(Typ.Rodziny == "7") %>% select(Region,Chleby, Herbatniki, Makarony, Ryz, Maka, Pozostale, Wyroby_cukiernicze_i_ciasta)
#sumowane wedlug regionow
Pary_1DzieckoSUMA <- Pary_1Dziecko %>% arrange(Region) %>% count(Region)

summary(Pary_1DzieckoSUMA$n)


#pary z 2 dzieci
Pary_2Dzieci <- danezRodzinyRegiony %>% filter(Typ.Rodziny == "8") %>% select(Region,Chleby, Herbatniki, Makarony, Ryz, Maka, Pozostale, Wyroby_cukiernicze_i_ciasta)
#sumowane wedlug regionow
Pary_2DzieciSUMA <- Pary_2Dzieci %>% arrange(Region) %>% count(Region)

summary(Pary_2DzieciSUMA$n)


#samotny rodzic
Samotny_rodzic <- danezRodzinyRegiony %>% filter(Typ.Rodziny == "10") %>% select(Region,Chleby, Herbatniki, Makarony, Ryz, Maka, Pozostale, Wyroby_cukiernicze_i_ciasta)
#sumowane wedlug regionow
Samotny_rodzicSUMA <- Samotny_rodzic %>% arrange(Region) %>% count(Region)

summary(Samotny_rodzicSUMA$n)




#SPRAWDZENIE CZY PROBY POCHODZA Z ROZKLADU NORMALNEGO
#TEST NORMALNOSCI ROZKLADOW Shapiro-Wilka w poszczegolnych podgrupach
# Hipoteza zerowa (H0) - podgrupa ma rozkad normalny
# Hipoteza alternatywna (H1) - podgrupa nie ma rozkadu normalnego


shapiro.test(Osoby_samotne_ponizej35SUMA$n)
#WNIOSEK - Odzucic H0 na rzecz hipotezy alternatywnej H1
#grupa nie ma rozkladu normalnego


shapiro.test(Osoby_samotne65SUMA$n)
#WNIOSEK - Odzucic H0 na rzecz hipotezy alternatywnej H1
#grupa nie ma rozkladu normalnego


shapiro.test(Pary_1DzieckoSUMA$n)
#WNIOSEK - Brak podstaw do odrzucenia HO
#grupa pochodzi z populacji o rozkladzie normalnym


shapiro.test(Pary_2DzieciSUMA$n)
#WNIOSEK - Brak podstaw do odrzucenia HO
#grupa pochodzi z populacji o rozkladzie normalnym


shapiro.test(Pary_bez_Dzieci_ponizej35SUMA$n)
#WNIOSEK - Odzucic H0 na rzecz hipotezy alternatywnej H1
#grupa nie ma rozkladu normalnego


shapiro.test(Pary_bez_Dzieci65SUMA$n)
#WNIOSEK - Odzucic H0 na rzecz hipotezy alternatywnej H1
#grupa nie ma rozkladu normalnego


shapiro.test(Samotny_rodzicSUMA$n)
#WNIOSEK - Brak podstaw do odrzucenia HO
#grupa pochodzi z populacji o rozkladzie normalnym


#zalozenie testow istotnosci (rozklad normalny) spelnione dla niektorych typow rodzin dla ich wydatkow na produkty zbozowe


#HIPOTEZA 1 - Wariancje dla wydatkow na zboza dla par z jednym i z dwojka dzieci sa rowne

#TEST ISTOTNOSCI DWOCH WARIANCJI
# Hipoteza zerowa (H0) - wariancje w obu grupach sa rowne
# Hipoteza alternatywna (H1) - wariancje w obu grupach sa istotnie rozne
testDwochWariancji<-var.test(Pary_1DzieckoSUMA$n, Pary_2DzieciSUMA$n)
testDwochWariancji
#WNIOSEK - Brak podstaw do odrzucenia H0
#Mozna stwierdzic, ze wariancje dla wydatkow na produkty zbozowe dla par z jednym i z dwojka dzieci sa sobie rowne


#HIPOTEZA 2 - Srednie wydatki samotnego rodzica na produkty zbozowe sa istotnie mniejsze niz 77 zl

#TEST ISTOTNOŒCI WARTOŒCI OCZEKIWANEJ
# H0 - Samotny rodzic wydaje srednio 77 zl na produkty zbozowe
# H1 - Przecietny samotny rodzic wydaje istotnie mniej niz 120 zl na produkty zbozowe
testSrednieja<-t.test(Samotny_rodzicSUMA$n, mu=120, conf.level = 0.95, alternative = 'less')
testSrednieja
#WNIOSEK - Brak podstaw do odrzucenia H0
#Mozna stwierdzic, ze przecietny samotny rodzic wydaje istotnie mniej niz 77 zl na produkty zbozowe


#HIPOTEZA 3 - Srednie wydatki na produkty zbozowe dla par z jednym i z dwojka dzieci sa takie same

#TEST ISTOTNOSCI DWOCH WARTOSCI OCZEKIWANYCH
# HO - Srednie wydatki na produkty zbozowe dla par z jednym i z dwojka dzieci sa takie same
# H1 - Srednie wydatki na na produkty zbozowe dla par z jednym i z dwojka dzieci istotnie sie roznia
testDwochSrednich<-t.test(Pary_1DzieckoSUMA$n,Pary_2DzieciSUMA$n,var.equal = F)
testDwochSrednich
#WNIOSEK - Nie ma podstaw do odrzucenia H0
#Mozna stwierdzic, ze srednie wydatki na produkty zbozowe dla par z jednym i z dwojka dzieci sa takie same



summary(NOWEchleby)
summary(NOWEherbatniki)
summary(NOWEpozostale)
summary(NOWEryz)
summary(NOWEmakarony)
summary(NOWEmaka)



#wariancje
var(zboza)
wariancje<-as.data.frame(apply(zboza, 2, var))
View(wariancje)

#odchylenia standardowe
odchylenieStd<-as.data.frame(sqrt(wariancje))
View(odchylenieStd)


#korelacja miedzy wydatkami na zboza miedzy: Parami bez dzieci z osoba kontaktowa 65letnia lub wiecej oraz parami z 1 dzieckiem
#korelacja = cor(NOWEchleby,NOWEherbatniki,method = "spearman")

#plot(Pary_2DzieciSUMA$n,Pary_1DzieckoSUMA$n, xlab = "Pary z dwojka dzieci ", ylab = "Pary z jednym dzieckiem", title("Wydatki na zboza dla dwoch typow rodzin"))




#jakby trzeba bylo zsumowac wszystkie wydatki xd
#wydatkiCALOSCIOWO <-dane %>% select(starts_with("C_"))





