
# installing/loading the package: Preferably Run on R GUI Not Rstudio
if(!require(installr)) {
  install.packages("installr"); require(installr)} #load / install+load installr

# using the package:
updateR() # this will start the updating process of your R installation.  
#It will check for newer versions, and if one is available, will guide you
#through the decisions you'd need to make.


require(installr)
# using the package:
updateR()