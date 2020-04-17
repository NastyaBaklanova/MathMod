#Бакланова Анастасия регион 57 Орловская область рассчитать урожайность пшеницы в 2005 году, взяв для рассчета средние суммы активных температур за предыдущие 10 лет, с метеостанций в радиусе не более 90 км

#Установка (и проверка) рабочей директории

setwd("C:/R/Zad 1")
getwd()

#Устанавливаем нужные пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)

#Скачиванием список метеостанций 
#station_data = ghcnd_stations()
#write.csv(station_data, file = "station_data.csv"
# Загружаем список всех метеостанций

station_data = read.csv("station_data.csv")

#Формируем список метеостанций ближайших к столице региона
#Создадим таблицу с именем региона и его координатами

orel = data.frame(id = "orel", latitude = 52.9650800,  longitude = 36.0784900)

#прочитаем справку командой meteo_nearby_stations
?meteo_nearby_stations
#Найдем метеостанции, соответствующие критериям
orel_around = meteo_nearby_stations(lat_lon_df = orel, station_data = station_data, var = c("TAVG"), year_min = 1995, year_max = 2005)
#идентификатор метеостанции
orel_id = orel_around[["orel"]][["id"]][1]
#отфильтруем все станции, на расстоянии более 90 км при помощи фунции filter
orel_stations=filter(orel_table, distance<=90)
str(orel_stations)
orel_stations$id
#создаем таблицу, в которой будем сохранять данные со всех станций в радиусе 90 км
orel_table = orel_around[[1]]; orel_table
#список необходимых станций
orel_table$id
#cкачиваем погодные данные для выбранных метеостанций
#получаем все данные с метеостанций
all_orel_data = meteo_tidy_ghcnd(stationid = orel_id)
#создаем объект, куда скачаем все данные всех метеостанций
all_i = data.frame()
all_orel_meteodata = data.frame()
#создаем цикл, чтобы скачать все данные с двух метостанций
for(i in 1:2)
{
  all_i=meteo_tidy_ghcnd(stationid = orel_table$id[i]) 
  all_i=all_i[,c("id","date","tavg")]
  all_orel_meteodata=bind_rows(all_orel_meteodata, all_i)
}
#записываем полученные результаты
write.csv(all_orel_meteodata,file = "all_orel_meteodata.csv")
#Разбиваем даты на составляющие (год, месяц, года) 
#Добавляем столбцы год, месяц, день в таблицу
all_orel_meteodata = mutate(all_orel_meteodata, year = year(date),
                            month = month(date), day = day(date))
#фильтруем данные с 1995-2005 года
years_orel_meteodata = filter(all_orel_meteodata, year >1995&year<2005)
#приводим средние суммы температур в подходящую форму, при помощи делиния на 10
years_orel_meteodata[,"tavg"]= years_orel_meteodata$tavg / 10 
#превращаем все NA и tavg <5 в нули 
years_orel_meteodata[is.na(years_orel_meteodata$tavg),"tavg"] = 0
years_orel_meteodata[years_orel_meteodata$tavg<5, "tavg"] = 0
#группируем по метеостанциям, годам и месяцам при помощи функции group_by
alldays = group_by(years_orel_meteodata, id, year, month)
#просуммируем температуру по этим группам с помощью sum
sum_t=summarize(alldays, tsum = sum(tavg))
#сгруппируем данные по месяцам  
group_months=group_by(sum_t,month)
#найдем для всех метеостанций среднее по месяцам
sum_t_months=summarize(group_months , St = mean(tsum))
#вводим констатны по урожайность
afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
di=c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
y=1.0
Kf=300
Qj=1600
Lj=2.2
Ej=25 
# рассчитаем Fi по месяца
sum_t_months = mutate(sum_t_months, Fi = afi+bfi*y*St)
#рассчитаем Yi
sum_t_months = mutate(sum_t_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))
##  расчитываем урожай как сумму по месяцам
Yield = sum(sum_t_months$Yi)
Yield
#урожайность 17,61 ц/га