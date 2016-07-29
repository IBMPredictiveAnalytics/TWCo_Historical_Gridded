#inputdate
#fake inputdata
latitudeData <- c("48.139311","48.832953","48.874145","48.868832")
longitudeData <- c("11.580559","2.740986","2.331364","2.322407")
startDateData <- c("20150615","20150615","20150615","20150615")
endDateData <- c("20150915","20150915","20150915","20150915")

modelerData <- data.frame(lat = latitudeData,
                          long = longitudeData,
                          startDate = startDateData,
                          endDate = endDateData)

packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(plyr)

#input
input_apikey <- "b3d40ed5d347cad47e5a7a3528c68255"
input_userKey <- input_apikey
input_col_lat <- "lat"
input_col_long <- "long"
input_col_zipcode <- "zipcode"
input_col_startDate <- "startDate"
input_col_endDate <- "endDate"
input_interval <- "daily"
input_units <- "metric"
input_station <- "csfr"
input_time <- "gmt"
input_text_startDate <- "startDate"
input_text_endDate <- "endDate"
input_radio_date <- "is_variableinput"
input_radio_location <- "is_latlong"

#constant input
input_format <- "csv"
input_delivery <- "file"

url_basic <- "http://cleanedobservations.wsi.com/CleanedObs.svc/GetObs?version=2&format=csv"
funGenerateURL <- function(url_Location,sStartDate, sEndDate){
  sUrlResult <- paste(
    url_basic, url_Location,
    "&startDate=", sStartDate,
    "&endDate=", sEndDate,
    "&userKey=", input_apikey,
    "&interval=", input_interval,
    "&units=", input_units,
    "&station=", input_station,
    "&time=", input_time,
    sep=""
  )
  return(sUrlResult)
}
retriveDataFromTWC_OneMonth <- function(url){
  fRemoteDate <- file(url,"r")
  resultData <- read.csv(fRemoteDate,header=TRUE)
  close(fRemoteDate)
  return(resultData)
}

retriveDataFromTWC <- function(modelerDataIter){
  funGenerateLocationURL <- function(modelerDataIter){
    #sUrlResult = ""
    if(input_radio_location == "is_geocode"){
      sUrlResult <- paste(
        "&zipcode=", modelerDataIter[input_col_zipcode], sep=""
      )
    }else{
      sUrlResult <- paste(
        "&lat=", modelerDataIter[input_col_lat],
        "&long=", modelerDataIter[input_col_long],
        sep=""
      )
    }
    return(sUrlResult)
  }
  print(modelerDataIter)
  if(input_radio_date == "is_variableinput"){
    sStartDate <- as.Date(modelerDataIter[input_col_startDate],"%Y%m%d")
    sEndDate <- as.Date(modelerDataIter[input_col_endDate],"%Y%m%d")
  }else{
    sStartDate <- as.Date(input_text_startDate,"%Y%m%d")
    sEndDate <- as.Date(input_text_endDate,"%Y%m%d")
  }
  resultData <- data.frame()
  for(day in format(seq(sStartDate,sEndDate,365),"%m/%d/%Y")){
    dayNumber <- as.Date(day,"%m/%d/%Y")
    sMonthStartDate <- day#format(day,"%Y%m%d")
    if(dayNumber + 365 > sEndDate )
      sMonthEndDate <- format(sEndDate,"%m/%d/%Y")
    else
      sMonthEndDate <- format(dayNumber + 365,"%m/%d/%Y")
    sRequestURL <- funGenerateURL(
      funGenerateLocationURL(modelerDataIter),
      sMonthStartDate,
      sMonthEndDate
    )
    print(sRequestURL)
    resultData <- rbind(resultData,retriveDataFromTWC_OneMonth(sRequestURL))
    print(nrow(resultData))
  }
  print(nrow(resultData))
  return(data.frame(resultData))
}
modelerData <- ldply(apply(modelerData, 1, FUN = retriveDataFromTWC))

varSiteId <- c(fieldName="SiteId",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varLatitude <- c(fieldName="Latitude",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varLongitude <- c(fieldName="Longitude",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varDateHrGmt <- c(fieldName="DateHrGmt",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",  fieldRole="")
varDateHrLwt <- c(fieldName="DateHrLwt",fieldLabel="",fieldStorage="string",fieldMeasure="",fieldFormat="",  fieldRole="")
varSurfaceTemperatureCelsius <- c(fieldName="SurfaceTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varSurfaceDewpointTemperatureCelsius <- c(fieldName="SurfaceDewpointTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varSurfaceWetBulbTemperatureCelsius <- c(fieldName="SurfaceWetBulbTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varRelativeHumidity <- c(fieldName="RelativeHumidity",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varSurfaceAirPressureKilopascals <- c(fieldName="SurfaceAirPressureKilopascals",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varCloudCoverage <- c(fieldName="CloudCoverage",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varWindChillTemperatureCelsius <- c(fieldName="WindChillTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varApparentTemperatureCelsius <- c(fieldName="ApparentTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varWindSpeedKph <- c(fieldName="WindSpeedKph",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varWindDirection <- c(fieldName="WindDirection",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varPrecipitationPreviousHourCentimeters <- c(fieldName="PrecipitationPreviousHourCentimeters",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varDownwardSolarRadiation <- c(fieldName="DownwardSolarRadiation",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varDiffuseHorizontalRadiation <- c(fieldName="DiffuseHorizontalRadiation",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varDirectNormalIrradiance <- c(fieldName="DirectNormalIrradiance",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varMslPressure <- c(fieldName="MslPressure",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varHeatIndexCelsius <- c(fieldName="HeatIndexCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varSnowfallCentimeters <- c(fieldName="SnowfallCentimeters",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varSurfaceWindGustsKph <- c(fieldName="SurfaceWindGustsKph",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")

varSurfaceTemperatureFahrenheit <- c(fieldName="SurfaceTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varSurfaceDewpointTemperatureFahrenheit <- c(fieldName="SurfaceDewpointTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varSurfaceWetBulbTemperatureFahrenheit <- c(fieldName="SurfaceWetBulbTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varWindChillTemperatureFahrenheit <- c(fieldName="WindChillTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varApparentTemperatureFahrenheit <- c(fieldName="ApparentTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")
varHeatIndexFahrenheit <- c(fieldName="HeatIndexFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",  fieldRole="")

varDay<-c(fieldName="Day",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMonth<-c(fieldName="Month",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varYear<-c(fieldName="Year",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinSurfaceTemperatureCelsius<-c(fieldName="MinSurfaceTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxSurfaceTemperatureCelsius<-c(fieldName="MaxSurfaceTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgSurfaceTemperatureCelsius<-c(fieldName="AvgSurfaceTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinSurfaceDewpointTemperatureCelsius<-c(fieldName="MinSurfaceDewpointTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxSurfaceDewpointTemperatureCelsius<-c(fieldName="MaxSurfaceDewpointTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgSurfaceDewpointTemperatureCelsius<-c(fieldName="AvgSurfaceDewpointTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinSurfaceWetBulbTemperatureCelsius<-c(fieldName="MinSurfaceWetBulbTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxSurfaceWetBulbTemperatureCelsius<-c(fieldName="MaxSurfaceWetBulbTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgSurfaceWetBulbTemperatureCelsius<-c(fieldName="AvgSurfaceWetBulbTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinRelativeHumidity<-c(fieldName="MinRelativeHumidity",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxRelativeHumidity<-c(fieldName="MaxRelativeHumidity",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgRelativeHumidity<-c(fieldName="AvgRelativeHumidity",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinSurfaceAirPressureKilopascals<-c(fieldName="MinSurfaceAirPressureKilopascals",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxSurfaceAirPressureKilopascals<-c(fieldName="MaxSurfaceAirPressureKilopascals",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgSurfaceAirPressureKilopascals<-c(fieldName="AvgSurfaceAirPressureKilopascals",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinCloudCoverage<-c(fieldName="MinCloudCoverage",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxCloudCoverage<-c(fieldName="MaxCloudCoverage",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgCloudCoverage<-c(fieldName="AvgCloudCoverage",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinWindChillTemperatureCelsius<-c(fieldName="MinWindChillTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxWindChillTemperatureCelsius<-c(fieldName="MaxWindChillTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgWindChillTemperatureCelsius<-c(fieldName="AvgWindChillTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinApparentTemperatureCelsius<-c(fieldName="MinApparentTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxApparentTemperatureCelsius<-c(fieldName="MaxApparentTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgApparentTemperatureCelsius<-c(fieldName="AvgApparentTemperatureCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinWindSpeedKph<-c(fieldName="MinWindSpeedKph",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxWindSpeedKph<-c(fieldName="MaxWindSpeedKph",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgWindSpeedKph<-c(fieldName="AvgWindSpeedKph",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinWindDirection<-c(fieldName="MinWindDirection",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxWindDirection<-c(fieldName="MaxWindDirection",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgWindDirection<-c(fieldName="AvgWindDirection",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinPrecipitationPreviousHourCentimeters<-c(fieldName="MinPrecipitationPreviousHourCentimeters",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxPrecipitationPreviousHourCentimeters<-c(fieldName="MaxPrecipitationPreviousHourCentimeters",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgPrecipitationPreviousHourCentimeters<-c(fieldName="AvgPrecipitationPreviousHourCentimeters",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varSumPrecipitationPreviousHourCentimeters<-c(fieldName="SumPrecipitationPreviousHourCentimeters",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinDownwardSolarRadiation<-c(fieldName="MinDownwardSolarRadiation",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxDownwardSolarRadiation<-c(fieldName="MaxDownwardSolarRadiation",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgDownwardSolarRadiation<-c(fieldName="AvgDownwardSolarRadiation",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varSumDownwardSolarRadiation<-c(fieldName="SumDownwardSolarRadiation",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinDiffuseHorizontalRadiation<-c(fieldName="MinDiffuseHorizontalRadiation",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxDiffuseHorizontalRadiation<-c(fieldName="MaxDiffuseHorizontalRadiation",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgDiffuseHorizontalRadiation<-c(fieldName="AvgDiffuseHorizontalRadiation",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varSumDiffuseHorizontalRadiation<-c(fieldName="SumDiffuseHorizontalRadiation",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinDirectNormalIrradiance<-c(fieldName="MinDirectNormalIrradiance",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxDirectNormalIrradiance<-c(fieldName="MaxDirectNormalIrradiance",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgDirectNormalIrradiance<-c(fieldName="AvgDirectNormalIrradiance",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varSumDirectNormalIrradiance<-c(fieldName="SumDirectNormalIrradiance",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinMslPressure<-c(fieldName="MinMslPressure",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxMslPressure<-c(fieldName="MaxMslPressure",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgMslPressure<-c(fieldName="AvgMslPressure",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinHeatIndexCelsius<-c(fieldName="MinHeatIndexCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxHeatIndexCelsius<-c(fieldName="MaxHeatIndexCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgHeatIndexCelsius<-c(fieldName="AvgHeatIndexCelsius",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinSnowfallCentimeters<-c(fieldName="MinSnowfallCentimeters",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxSnowfallCentimeters<-c(fieldName="MaxSnowfallCentimeters",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgSnowfallCentimeters<-c(fieldName="AvgSnowfallCentimeters",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varSumSnowfallCentimeters<-c(fieldName="SumSnowfallCentimeters",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinSurfaceWindGustsKph<-c(fieldName="MinSurfaceWindGustsKph",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxSurfaceWindGustsKph<-c(fieldName="MaxSurfaceWindGustsKph",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgSurfaceWindGustsKph<-c(fieldName="AvgSurfaceWindGustsKph",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")

varMinSurfaceTemperatureFahrenheit<-c(fieldName="MinSurfaceTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxSurfaceTemperatureFahrenheit<-c(fieldName="MaxSurfaceTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgSurfaceTemperatureFahrenheit<-c(fieldName="AvgSurfaceTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinSurfaceDewpointTemperatureFahrenheit<-c(fieldName="MinSurfaceDewpointTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxSurfaceDewpointTemperatureFahrenheit<-c(fieldName="MaxSurfaceDewpointTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgSurfaceDewpointTemperatureFahrenheit<-c(fieldName="AvgSurfaceDewpointTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinSurfaceWetBulbTemperatureFahrenheit<-c(fieldName="MinSurfaceWetBulbTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxSurfaceWetBulbTemperatureFahrenheit<-c(fieldName="MaxSurfaceWetBulbTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgSurfaceWetBulbTemperatureFahrenheit<-c(fieldName="AvgSurfaceWetBulbTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinWindChillTemperatureFahrenheit<-c(fieldName="MinWindChillTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxWindChillTemperatureFahrenheit<-c(fieldName="MaxWindChillTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgWindChillTemperatureFahrenheit<-c(fieldName="AvgWindChillTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinApparentTemperatureFahrenheit<-c(fieldName="MinApparentTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxApparentTemperatureFahrenheit<-c(fieldName="MaxApparentTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgApparentTemperatureFahrenheit<-c(fieldName="AvgApparentTemperatureFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMinHeatIndexFahrenheit<-c(fieldName="MinHeatIndexFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varMaxHeatIndexFahrenheit<-c(fieldName="MaxHeatIndexFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")
varAvgHeatIndexFahrenheit<-c(fieldName="AvgHeatIndexFahrenheit",fieldLabel="",fieldStorage="real",fieldMeasure="",fieldFormat="",fieldRole="")

if(input_interval == "hourly"){
  if(input_units == "imperial"){
    modelerDataModel <- data.frame(varSiteId,varLatitude,varLongitude,varDateHrGmt,varDateHrLwt,
                                   varSurfaceTemperatureCelsius,varSurfaceDewpointTemperatureCelsius,
                                   varSurfaceWetBulbTemperatureCelsius,varRelativeHumidity,
                                   varSurfaceAirPressureKilopascals,varCloudCoverage,
                                   varWindChillTemperatureCelsius,varApparentTemperatureCelsius,varWindSpeedKph,
                                   varWindDirection,varPrecipitationPreviousHourCentimeters,varDownwardSolarRadiation,
                                   varDiffuseHorizontalRadiation,varDirectNormalIrradiance,varMslPressure,
                                   varHeatIndexCelsius,varSnowfallCentimeters,varSurfaceWindGustsKph)
  }else if(input_units == "metric"){
    modelerDataModel <- data.frame(varSiteId,varLatitude,varLongitude,varDateHrGmt,varDateHrLwt,
                                   varSurfaceTemperatureFahrenheit,varSurfaceDewpointTemperatureFahrenheit,
                                   varSurfaceWetBulbTemperatureFahrenheit,varRelativeHumidity,
                                   varSurfaceAirPressureKilopascals,varCloudCoverage,
                                   varWindChillTemperatureFahrenheit,varApparentTemperatureFahrenheit,varWindSpeedKph,
                                   varWindDirection,varPrecipitationPreviousHourCentimeters,varDownwardSolarRadiation,
                                   varDiffuseHorizontalRadiation,varDirectNormalIrradiance,varMslPressure,
                                   varHeatIndexFahrenheit,varSnowfallCentimeters,varSurfaceWindGustsKph)  
  }
}else if(input_interval == "daily"){
  if(input_units == "imperial"){
    modelerDataModel <- data.frame(varSiteId,
                                   varLatitude,
                                   varLongitude,
                                   varDay,
                                   varMonth,
                                   varYear,
                                   varMinSurfaceTemperatureCelsius,
                                   varMaxSurfaceTemperatureCelsius,
                                   varAvgSurfaceTemperatureCelsius,
                                   varMinSurfaceDewpointTemperatureCelsius,
                                   varMaxSurfaceDewpointTemperatureCelsius,
                                   varAvgSurfaceDewpointTemperatureCelsius,
                                   varMinSurfaceWetBulbTemperatureCelsius,
                                   varMaxSurfaceWetBulbTemperatureCelsius,
                                   varAvgSurfaceWetBulbTemperatureCelsius,
                                   varMinRelativeHumidity,
                                   varMaxRelativeHumidity,
                                   varAvgRelativeHumidity,
                                   varMinSurfaceAirPressureKilopascals,
                                   varMaxSurfaceAirPressureKilopascals,
                                   varAvgSurfaceAirPressureKilopascals,
                                   varMinCloudCoverage,
                                   varMaxCloudCoverage,
                                   varAvgCloudCoverage,
                                   varMinWindChillTemperatureCelsius,
                                   varMaxWindChillTemperatureCelsius,
                                   varAvgWindChillTemperatureCelsius,
                                   varMinApparentTemperatureCelsius,
                                   varMaxApparentTemperatureCelsius,
                                   varAvgApparentTemperatureCelsius,
                                   varMinWindSpeedKph,
                                   varMaxWindSpeedKph,
                                   varAvgWindSpeedKph,
                                   varMinWindDirection,
                                   varMaxWindDirection,
                                   varAvgWindDirection,
                                   varMinPrecipitationPreviousHourCentimeters,
                                   varMaxPrecipitationPreviousHourCentimeters,
                                   varAvgPrecipitationPreviousHourCentimeters,
                                   varSumPrecipitationPreviousHourCentimeters,
                                   varMinDownwardSolarRadiation,
                                   varMaxDownwardSolarRadiation,
                                   varAvgDownwardSolarRadiation,
                                   varSumDownwardSolarRadiation,
                                   varMinDiffuseHorizontalRadiation,
                                   varMaxDiffuseHorizontalRadiation,
                                   varAvgDiffuseHorizontalRadiation,
                                   varSumDiffuseHorizontalRadiation,
                                   varMinDirectNormalIrradiance,
                                   varMaxDirectNormalIrradiance,
                                   varAvgDirectNormalIrradiance,
                                   varSumDirectNormalIrradiance,
                                   varMinMslPressure,
                                   varMaxMslPressure,
                                   varAvgMslPressure,
                                   varMinHeatIndexCelsius,
                                   varMaxHeatIndexCelsius,
                                   varAvgHeatIndexCelsius,
                                   varMinSnowfallCentimeters,
                                   varMaxSnowfallCentimeters,
                                   varAvgSnowfallCentimeters,
                                   varSumSnowfallCentimeters,
                                   varMinSurfaceWindGustsKph,
                                   varMaxSurfaceWindGustsKph,
                                   varAvgSurfaceWindGustsKph)
  }else if(input_units == "metric"){
    modelerDataModel <- data.frame(varSiteId,
                                   varLatitude,
                                   varLongitude,
                                   varDay  ,
                                   varMonth,
                                   varYear,
                                   varMinSurfaceTemperatureFahrenheit,
                                   varMaxSurfaceTemperatureFahrenheit,
                                   varAvgSurfaceTemperatureFahrenheit,
                                   varMinSurfaceDewpointTemperatureFahrenheit,
                                   varMaxSurfaceDewpointTemperatureFahrenheit,
                                   varAvgSurfaceDewpointTemperatureFahrenheit,
                                   varMinSurfaceWetBulbTemperatureFahrenheit,
                                   varMaxSurfaceWetBulbTemperatureFahrenheit,
                                   varAvgSurfaceWetBulbTemperatureFahrenheit,
                                   varMinRelativeHumidity,
                                   varMaxRelativeHumidity,
                                   varAvgRelativeHumidity,
                                   varMinSurfaceAirPressureKilopascals,
                                   varMaxSurfaceAirPressureKilopascals,
                                   varAvgSurfaceAirPressureKilopascals,
                                   varMinCloudCoverage,
                                   varMaxCloudCoverage,
                                   varAvgCloudCoverage,
                                   varMinWindChillTemperatureFahrenheit,
                                   varMaxWindChillTemperatureFahrenheit,
                                   varAvgWindChillTemperatureFahrenheit,
                                   varMinApparentTemperatureFahrenheit,
                                   varMaxApparentTemperatureFahrenheit,
                                   varAvgApparentTemperatureFahrenheit,
                                   varMinWindSpeedKph,
                                   varMaxWindSpeedKph,
                                   varAvgWindSpeedKph,
                                   varMinWindDirection,
                                   varMaxWindDirection,
                                   varAvgWindDirection,
                                   varMinPrecipitationPreviousHourCentimeters,
                                   varMaxPrecipitationPreviousHourCentimeters,
                                   varAvgPrecipitationPreviousHourCentimeters,
                                   varSumPrecipitationPreviousHourCentimeters,
                                   varMinDownwardSolarRadiation,
                                   varMaxDownwardSolarRadiation,
                                   varAvgDownwardSolarRadiation,
                                   varSumDownwardSolarRadiation,
                                   varMinDiffuseHorizontalRadiation,
                                   varMaxDiffuseHorizontalRadiation,
                                   varAvgDiffuseHorizontalRadiation,
                                   varSumDiffuseHorizontalRadiation,
                                   varMinDirectNormalIrradiance,
                                   varMaxDirectNormalIrradiance,
                                   varAvgDirectNormalIrradiance,
                                   varSumDirectNormalIrradiance,
                                   varMinMslPressure,
                                   varMaxMslPressure,
                                   varAvgMslPressure,
                                   varMinHeatIndexFahrenheit,
                                   varMaxHeatIndexFahrenheit,
                                   varAvgHeatIndexFahrenheit,
                                   varMinSnowfallCentimeters,
                                   varMaxSnowfallCentimeters,
                                   varAvgSnowfallCentimeters,
                                   varSumSnowfallCentimeters,
                                   varMinSurfaceWindGustsKph,
                                   varMaxSurfaceWindGustsKph,
                                   varAvgSurfaceWindGustsKph)  
  }  
}else if(input_interval == "monthly"){
  if(input_units == "imperial"){
    modelerDataModel <- data.frame(varSiteId,
                                   varLatitude,
                                   varLongitude,
                                   varMonth,
                                   varYear,
                                   varMinSurfaceTemperatureCelsius,
                                   varMaxSurfaceTemperatureCelsius,
                                   varAvgSurfaceTemperatureCelsius,
                                   varMinSurfaceDewpointTemperatureCelsius,
                                   varMaxSurfaceDewpointTemperatureCelsius,
                                   varAvgSurfaceDewpointTemperatureCelsius,
                                   varMinSurfaceWetBulbTemperatureCelsius,
                                   varMaxSurfaceWetBulbTemperatureCelsius,
                                   varAvgSurfaceWetBulbTemperatureCelsius,
                                   varMinRelativeHumidity,
                                   varMaxRelativeHumidity,
                                   varAvgRelativeHumidity,
                                   varMinSurfaceAirPressureKilopascals,
                                   varMaxSurfaceAirPressureKilopascals,
                                   varAvgSurfaceAirPressureKilopascals,
                                   varMinCloudCoverage,
                                   varMaxCloudCoverage,
                                   varAvgCloudCoverage,
                                   varMinWindChillTemperatureCelsius,
                                   varMaxWindChillTemperatureCelsius,
                                   varAvgWindChillTemperatureCelsius,
                                   varMinApparentTemperatureCelsius,
                                   varMaxApparentTemperatureCelsius,
                                   varAvgApparentTemperatureCelsius,
                                   varMinWindSpeedKph,
                                   varMaxWindSpeedKph,
                                   varAvgWindSpeedKph,
                                   varMinWindDirection,
                                   varMaxWindDirection,
                                   varAvgWindDirection,
                                   varMinPrecipitationPreviousHourCentimeters,
                                   varMaxPrecipitationPreviousHourCentimeters,
                                   varAvgPrecipitationPreviousHourCentimeters,
                                   varSumPrecipitationPreviousHourCentimeters,
                                   varMinDownwardSolarRadiation,
                                   varMaxDownwardSolarRadiation,
                                   varAvgDownwardSolarRadiation,
                                   varSumDownwardSolarRadiation,
                                   varMinDiffuseHorizontalRadiation,
                                   varMaxDiffuseHorizontalRadiation,
                                   varAvgDiffuseHorizontalRadiation,
                                   varSumDiffuseHorizontalRadiation,
                                   varMinDirectNormalIrradiance,
                                   varMaxDirectNormalIrradiance,
                                   varAvgDirectNormalIrradiance,
                                   varSumDirectNormalIrradiance,
                                   varMinMslPressure,
                                   varMaxMslPressure,
                                   varAvgMslPressure,
                                   varMinHeatIndexCelsius,
                                   varMaxHeatIndexCelsius,
                                   varAvgHeatIndexCelsius,
                                   varMinSnowfallCentimeters,
                                   varMaxSnowfallCentimeters,
                                   varAvgSnowfallCentimeters,
                                   varSumSnowfallCentimeters,
                                   varMinSurfaceWindGustsKph,
                                   varMaxSurfaceWindGustsKph,
                                   varAvgSurfaceWindGustsKph)
  }else if(input_units == "metric"){
    modelerDataModel <- data.frame(varSiteId,
                                   varLatitude,
                                   varLongitude,
                                   #varDay  ,
                                   varMonth,
                                   varYear,
                                   varMinSurfaceTemperatureFahrenheit,
                                   varMaxSurfaceTemperatureFahrenheit,
                                   varAvgSurfaceTemperatureFahrenheit,
                                   varMinSurfaceDewpointTemperatureFahrenheit,
                                   varMaxSurfaceDewpointTemperatureFahrenheit,
                                   varAvgSurfaceDewpointTemperatureFahrenheit,
                                   varMinSurfaceWetBulbTemperatureFahrenheit,
                                   varMaxSurfaceWetBulbTemperatureFahrenheit,
                                   varAvgSurfaceWetBulbTemperatureFahrenheit,
                                   varMinRelativeHumidity,
                                   varMaxRelativeHumidity,
                                   varAvgRelativeHumidity,
                                   varMinSurfaceAirPressureKilopascals,
                                   varMaxSurfaceAirPressureKilopascals,
                                   varAvgSurfaceAirPressureKilopascals,
                                   varMinCloudCoverage,
                                   varMaxCloudCoverage,
                                   varAvgCloudCoverage,
                                   varMinWindChillTemperatureFahrenheit,
                                   varMaxWindChillTemperatureFahrenheit,
                                   varAvgWindChillTemperatureFahrenheit,
                                   varMinApparentTemperatureFahrenheit,
                                   varMaxApparentTemperatureFahrenheit,
                                   varAvgApparentTemperatureFahrenheit,
                                   varMinWindSpeedKph,
                                   varMaxWindSpeedKph,
                                   varAvgWindSpeedKph,
                                   varMinWindDirection,
                                   varMaxWindDirection,
                                   varAvgWindDirection,
                                   varMinPrecipitationPreviousHourCentimeters,
                                   varMaxPrecipitationPreviousHourCentimeters,
                                   varAvgPrecipitationPreviousHourCentimeters,
                                   varSumPrecipitationPreviousHourCentimeters,
                                   varMinDownwardSolarRadiation,
                                   varMaxDownwardSolarRadiation,
                                   varAvgDownwardSolarRadiation,
                                   varSumDownwardSolarRadiation,
                                   varMinDiffuseHorizontalRadiation,
                                   varMaxDiffuseHorizontalRadiation,
                                   varAvgDiffuseHorizontalRadiation,
                                   varSumDiffuseHorizontalRadiation,
                                   varMinDirectNormalIrradiance,
                                   varMaxDirectNormalIrradiance,
                                   varAvgDirectNormalIrradiance,
                                   varSumDirectNormalIrradiance,
                                   varMinMslPressure,
                                   varMaxMslPressure,
                                   varAvgMslPressure,
                                   varMinHeatIndexFahrenheit,
                                   varMaxHeatIndexFahrenheit,
                                   varAvgHeatIndexFahrenheit,
                                   varMinSnowfallCentimeters,
                                   varMaxSnowfallCentimeters,
                                   varAvgSnowfallCentimeters,
                                   varSumSnowfallCentimeters,
                                   varMinSurfaceWindGustsKph,
                                   varMaxSurfaceWindGustsKph,
                                   varAvgSurfaceWindGustsKph)  
  } 
}