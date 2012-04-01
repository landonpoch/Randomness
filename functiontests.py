from pylab import plot, show, grid
import math

def f(x):
  return ((3 * (x**2)) + (2 * x) + 5)
  #return x
  #return math.sin(x)
  
def my_map(function, list):
  return [function(item) for item in list]
  
#def plot(function, list):
#  return [(item, function(item)) for item in list]

def my_range(low, high):
  increments = (high - low) / 10000.0
  return my_map(lambda x: x + increments, range(low, low + 500))

x_list = my_range(-100, 100)
y_list = my_map(f, x_list)

plot(x_list, y_list)
grid(True)
show()