library(weatherData)
#library("devtools")
#install_github("Ram-N/weatherData")
#getStationCode("India")
#showAvailableColumns("VOBG", "2016-01-01", opt_detailed=T)
bangalore <- getWeatherForDate("VOBL", start_date="2010-01-01",end_date="2016-11-26", opt_detailed=T, opt_custom_columns=T, custom_columns=c(2,7,8,13))
bangalore$City <- rep("bangalore",nrow(bangalore))
kolkata <- getWeatherForDate("VECC", start_date="2010-01-01",end_date="2016-11-26", opt_detailed=T, opt_custom_columns=T, custom_columns=c(2,7,8,13))
kolkata$City <- rep("kolkata",nrow(kolkata))

weather <- rbind(bangalore,kolkata,malda)
remove(bangalore,kolkata,malda)