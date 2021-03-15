#-----------------------------------------------------------------------------------------------------------------#
#                                                                                                                 #
# PROJECT: Z-DRUGS - Q1 2017 - Q1 2020	                                                                          #
# AUTHOR: MICHAEL MAGUIRE, MS, DMA II                                                                             #
# INSTITUTION: UNIVERSITY OF FLORIDA, COLLEGE OF PHARMACY                                                         #
# DEPARTMENT: PHARMACEUTICAL OUTCOMES AND POLICY                                                                  #
# SUPERVISORS: AMIE GOODIN, PHD, MPP | JUAN HINCAPIE-CASTILLO, PHARMD, PHD, MS                                    #
# SCRIPT: 01_create-directories.R                                                                          			  #
#                                                                                                                 #
#-----------------------------------------------------------------------------------------------------------------#

if (dir.exists("./data")) {
  print("Directory already exists!")
} else {
  dir.create("./data")
}

if (dir.exists("./data/clean/")) {
  print("Directory already exists!")
} else {
  dir.create("./data/clean/")
}

if (dir.exists("./data/raw/")) {
  print("Directory already exists!")
} else {
  dir.create("./data/raw/")
}

if (dir.exists("./output")) {
  print("Directory already exists!")
} else {
  dir.create("./output")
}

if (dir.exists("./functions")) {
  print("Directory already exists!")
} else {
  dir.create("./functions")
}

if (dir.exists("./plots")) {
  print("Directory already exists!")
} else {
  dir.create("./plots")
}