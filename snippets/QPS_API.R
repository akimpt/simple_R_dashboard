##  This code contains functions to query the QPS crime statistics website
##  http://www.police.qld.gov.au/forms/CrimeStatsDesktop.asp and extract a
##  dataframe of suburb-level geo-coded crime statistics.   This is based on
##  an unoffical API interpretation, and the veracity of the results is
##  subject to the QPS's own caveats as listed on the website.

##  tmp<-getURLContent("https://data.police.qld.gov.au/api/
##    qpsoffences?boundarylist=1_4852&startdate=1325376000&
##    enddate=1356912000&offences=1,8,14,17,21,27,28,29,30,35,39,45,47,51,52,54,55")

##  Load required packages RCurl and rjson.  The wesbite uses a REST API and
##  exports results as JSON.

require(RCurl)
require(rjson)

##  Dates are in UNIX Epoch time, or seconds since January 1, 1970 GMT. The
##  function requires dates in the "YYYY-MM-DD" format.  See the default
##  encoding below.

##  Start Date 1325376000 --- for January 1, 2012
##  End Date   1356912000 --- for December 31, 2012

start<-as.Date("01/01/2012",format="%m/%d/%Y")
end<-as.Date("02/01/2012",format="%m/%d/%Y")

##  This function is used internally to convert output from a list to a
##  dataframe and extract the lat-long coordinates.

list.to.data.frame<-function(x)
{
  if(length(x)>1 && length(x$Result)>0)
  {
    tmpx<-lapply(x$Result, function(x) lapply(x,function(x) ifelse(is.null(x), NA, x)))
    tmp<-unlist(tmpx)
    tmp.matrix<-data.frame(matrix(tmp,ncol=11,byrow=T))
    colnames(tmp.matrix)<-names(x$Result[[1]])
    tmp.coords<-
      lapply(tmp.matrix$GeometryWKT,function(x) gsub("POINT ","",x))
    tmp.coords<-
      lapply(tmp.coords,function(x) gsub("[(]","",x))
    tmp.coords<-
      lapply(tmp.coords,function(x) gsub("[)]","",x))
    tmp.coords<-
      lapply(tmp.coords,function(x) strsplit(as.character(tmp.coords)," "))[[1]]
    tmp.coords.x<-lapply(tmp.coords,function(x) x[1])
    tmp.coords.y<-lapply(tmp.coords,function(x) x[2])
    tmp.matrix$x.coord<-as.numeric(unlist(tmp.coords.x))
    tmp.matrix$y.coords<-as.numeric(unlist(tmp.coords.y))
    return(tmp.matrix)
  }
  else tmp.matrix<-data.frame()
  return(tmp.matrix)
}

##  This is the main function, right now it only takes suburb name and start and end
##  dates as arguments, returing data for all offenses.  There is no real reason why
##  this can't be added in the future if desired.

get.qps.data<-function(x,start,end)
{
  qps.html<-"https://data.police.qld.gov.au/api/"

  start.date<-as.character(as.numeric(start)*24*3600)
  end.date<-as.character(as.numeric(end)*24*3600)
  offences<-"1,8,14,17,21,27,28,29,30,35,39,45,47,51,52,54,55"

  ##  Gets Suburb Level Information including encoding for the API

  suburb<-fromJSON(getURL(paste0(qps.html,"suburbs?name=",x)))

  ##  Extract Suburb Id number

  if(length(suburb$Result)>0)  ## Crude Error Handling makes sure it is a valid suburb
  {
    suburb.id<-suburb$Result[[1]]$QldSuburbId

    ##  Make Query Argument

    location<-paste0("qpsoffences?boundarylist=1_",as.character(suburb.id))
    dates<-paste0("&startdate=",start.date,"&enddate=",end.date)
    off<-paste0("&offences=",offences)
    query<-paste0(qps.html,location,dates,off)

    ##  This extracts the total number of offences for the suburb

    tmp<-getURL(query)

    return(list.to.data.frame(fromJSON(tmp)))
  }
  else return(data.frame())
}

##  The resulting output contains the variables: "QpsOffenceId"   "OccurrenceNo"
##  "QpsOffenceCode" "ReportDate"     "StartDate"     "EndDate"        "Suburb"         "Postcode"       "Solved"         "GeometryWKT"
##  "GeometryWKID"   "x.coord"        "y.coords"
##
##  The offence codes are not neccesarily clear but I believe are (in order):
##  1 = Homicide
##  8 = Assault
##  14 = Robbery
##  17 = Other Offence Against Person
##  21 = Unlawful Entry
##  27 = Arson
##  28 = Other Property Damage
##  29 = Unlawful Use of A Motor Vehicle
##  30 = Other Theft
##  35 = Fraud
##  39 = Handling Stolen Goods
##  45 = Drug Offence
##  47 = Liquor (excl. Drunkeness)
##  51 = Weapons Act Offences
##  52 = Good Order Offence
##  54 = Traffic and Related Offences
##  55 = Other

##  An example extracting several suburbs and binding them as a single data frame.
##  This is written as a loop, but may also work using sapply().  Large queries may
##  take an excessive amount of time, the QPS server can take a while to run.

sample.suburbs<-c("Moggill","Kenmore","Brookfield")

result.dataframe<-numeric()
for(i in 1:length(sample.suburbs))
{
  result.dataframe<-rbind(result.dataframe,get.qps.data(sample.suburbs[i],start,end))
}