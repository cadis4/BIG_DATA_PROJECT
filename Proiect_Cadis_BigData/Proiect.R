  library(rpart)
  library(rpart.plot)
  library(tidyverse)
  library(ISLR)
  library(rsample)
  library(caret)
  library(partykit)
  library(ggplot2)
  

  data <- read.csv("cars.csv") 
  # regresie liniară între anul de productie si pretul masinii
  lm_year <- lm(priceUSD ~ year, data = data)
  summary(lm_year)
  
  # regresie liniară între kilometraj si pretul masinii
  lm_mileage <- lm(priceUSD ~ mileage.kilometers., data = data)
  summary(lm_mileage)
  
  # regresie liniară între tipul de combustibil si pretul masinii
  lm_fuel_type <- lm(priceUSD ~ fuel_type, data = data)
  summary(lm_fuel_type)
  
  
  # grafic cu linie de regresie pentru relatia dintre anul de productie si pretul masinii
  ggplot(data, aes(x = year, y = priceUSD)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") + 
    labs(x = "Anul", y = "Pretul masinii") +
    ggtitle("Relatia dintre anul si pretul masinii")
  
  # grafic cu linie de regresie pentru relatia dintre kilometraj si pretul masinii
  ggplot(data, aes(x = mileage.kilometers., y = priceUSD)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(x = "Kilometrajul", y = "Pretul masinii") +
    ggtitle("Relatia dintre kilometraj si pretul masinii")
  
  # grafic cu linie de regresie pentru relatia dintre volumul motorului si pretul masini
  ggplot(data, aes(x = volume.cm3., y = priceUSD)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "green") +
    labs(x = "Volum", y = "Pretul masinii") +
    ggtitle("Relatia dintre volum si pretul masinii")
  
  # grafic pentru media pretului masinilor în functie de tipul de combustibil
  ggplot(data, aes(x = fuel_type, y = priceUSD)) +
    stat_summary(fun.y = "mean", geom = "bar", fill = "blue") +
    labs(x = "Tipul de combustibil", y = "Pretul mediu al masinii") +
    ggtitle("Pretul mediu al masinii în functie de tipul de combustibil")
  
  # arbore de decizie pentru predictia pretului masinii pe baza anului de productie si kilometrajului
  ArborePret <- rpart(priceUSD ~ year + mileage.kilometers., data = data)
  rpart.plot(ArborePret, box.palette = "Blues", shadow.col = "gray")
  
  # arbore de decizie pentru predictia anului masinii pe baza tipului de combustibil si kilometrajului
  ArboreAn <- rpart(year ~ fuel_type + mileage.kilometers., data = data)
  rpart.plot(ArboreAn, box.palette = "Greens", shadow.col = "gray")
  
  # arbore de decizie pentru predictia kilometrajului pe baza anului de productie
  ArboreTipComb <- rpart(mileage.kilometers. ~ year, data = data)
  rpart.plot(ArboreTipComb, box.palette = "Oranges", shadow.col = "gray")

  
