#This file is for random testing of python32 syntax/features

class MyDate:
  months = dict(January=31, 
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
  days = ['Sunday', 
    'Monday', 
    'Tuesday', 
    'Wednesday', 
    'Thursday', 
    'Friday', 
    'Saturday']
  def __init__(self, month, day, year):
    self.month = month
    self.day = day
    self.year = year

  def days_in_year(self):
    print(str(sum(MyDate.months.values())))
  
  def day_calculator(self):
    day = int(MyDate.days.index(input('Please choose a day of the week: ').capitalize()))
    daysToAdd = int(input('How many days would you like to add? '))
    print(MyDate.days[(day + daysToAdd) % 7])
    
date = MyDate(3, 31, 2012)
