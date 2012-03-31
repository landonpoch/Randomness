#This file is for random testing of python32 syntax/feature
import traceback

class MyDate:
  months = [('January', 31), 
    ('February', 28), 
    ('March', 31), 
    ('April', 30), 
    ('May', 31), 
    ('June', 30), 
    ('July', 31), 
    ('August', 31), 
    ('September', 30), 
    ('October', 31), 
    ('November', 30), 
    ('December', 31)]
  days = ['Sunday', 
    'Monday', 
    'Tuesday', 
    'Wednesday', 
    'Thursday', 
    'Friday', 
    'Saturday']
  def __init__(self, month, day, year):
    self.month = MyDate.months[month - 1]
    if day <= self.month[1]:
      self.day = day
    else:
      raise ValueError('Not that many days in the month')
    self.year = year

  def days_in_year(self):
    return sum(month[1] for month in MyDate.months)
  
  def day_calculator(self):
    day = int(MyDate.days.index(input('Please choose a day of the week: ').capitalize()))
    daysToAdd = int(input('How many days would you like to add? '))
    print(MyDate.days[(day + daysToAdd) % 7])

try:    
  date = MyDate(3, 31, 2012)
  print(date.days_in_year())
except:
  traceback.print_exc()