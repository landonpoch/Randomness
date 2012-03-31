#This file is for random testing of python32 syntax/features

Months = dict(January=31,
  February=28,
  March=31,
  April=30,
  May=31,
  June=30,
  July=31,
  August=31,
  September=30,
  October=31,
  November=30,
  December=31)

Days = ['Sunday',
  'Monday',
  'Tuesday',
  'Wednesday',
  'Thursday',
  'Friday',
  'Saturday']
  
class MyDate:
  def __init__(self):
    pass

def daysInYear():
  print(str(sum(Months.values())))
  
def addDays():
  day = int(Days.index(input('Please choose a day of the week: ').capitalize()))
  daysToAdd = int(input('How many days would you like to add? '))
  print(Days[(day + daysToAdd) % 7])
  
#addDays()
daysInYear()