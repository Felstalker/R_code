# TODO: Add comment
# 
# Author: bao1827
###############################################################################
#

library(quantmod)

getSymbols("IBM")
IBM

names(IBM)
attributes(IBM)
class(IBM)

to.weekly(IBM)
weeklyReturn(IBM)

Hi(IBM)

chartSeries(IBM)

barChart(IBM)
candleChart(IBM)

##########
getSymbols("YHOO")
chartSeries(YHOO)
chartSeries(YHOO, subset='last 4 months')
chartSeries(YHOO, subset='2007::2008-01')
chartSeries(YHOO,theme=chartTheme('white'))
chartSeries(YHOO,TA=NULL)   #no volume
chartSeries(YHOO,TA=c(addVo(),addBBands()))  #add volume and Bollinger Bands from TTR

addMACD()   #  add MACD indicator to current chart

# The chart must be up for these to work #
listTA() 
setTA()

chartSeries(YHOO)   #draws chart a


help(getSymbols)

getFinancials("IBM")

print("IBM Balance Sheet")
viewFinancials(IBM.f, type = c("BS"), "A")
print("IBM Income Statement")
viewFinancials(IBM.f, type = c("IS"), "A")
print("IBM Cash Flow")
viewFinancials(IBM.f, type = c("CF"), "A")



