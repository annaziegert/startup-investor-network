# Create an account on shiniapps.io to deploy your first app. This
# procedure together will take approximately 10 minutes.

# - Go to: https://www.shinyapps.io/admin/#/signup
# 
# - Create an account (name + password). You can use whatever email
#   you like.
# 
# - Fill in your tokens (accountname + token + secret) in the code
#   below
# 



install.packages("shiny")
install.packages("rsconnect") # used to deploy
library(igraph)
library(shiny)
library(data.table)
library(rsconnect) # open libraries that you will use today

setAccountInfo(name='q7pufs-anna0sophie-ziegert',
               token='3D2954CBB26BA7ABE46B05078FB25A33',
               secret='k4U1rz1pDZB3V0LLJ4AtkrLW/SvqDLSjxvGFNway')
deployApp('shiny/base_directory')

# https://q7pufs-anna0sophie-ziegert.shinyapps.io/base_directory/
