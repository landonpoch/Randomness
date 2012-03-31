#This file is for random testing of python32 syntax/features

Days = ['Sunday',
  'Monday',
  'Tuesday',
  'Wednesday',
  'Thursday',
  'Friday',
  'Saturday']

def AddDays():
  day = int(Days.index(input('Please choose a day of the week: ').capitalize()))
  daysToAdd = int(input('How many days would you like to add? '))
  print(Days[(day + daysToAdd) % 7])
  
AddDays()