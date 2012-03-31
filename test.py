#This file is for random testing of python32 syntax/features

class MyDate:
  def __init__(self):
    self.months = dict(January=31, 
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
    self.days = ['Sunday', 
      'Monday', 
      'Tuesday', 
      'Wednesday', 
      'Thursday', 
      'Friday', 
      'Saturday']

  def daysInYear(self):
    print(str(sum(self.months.values())))
  
  def addDays(self):
    day = int(self.days.index(input('Please choose a day of the week: ').capitalize()))
    daysToAdd = int(input('How many days would you like to add? '))
    print(self.days[(day + daysToAdd) % 7])
    
date = MyDate()
date.daysInYear()