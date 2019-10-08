season_calc <- function(a){
  if (a < "2011-07-01") {output = "2010-2011"}
  else if (a < "2012-07-01") {output = "2011-2012"}
  else if (a < "2013-07-01") {output = "2012-2013"}
  else if (a < "2014-07-01") {output = "2013-2014"}
  else if (a < "2015-07-01") {output = "2014-2015"}
  else if (a < "2016-07-01") {output = "2015-2016"}
  else if (a < "2017-07-01") {output = "2016-2017"}
  else if (a < "2018-07-01") {output = "2017-2018"}
  else if (a < "2019-07-01") {output = "2018-2019"}
  output
}
