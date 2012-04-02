import random

def get_random():
  return str(random.randint(0,9))
  
def make_chaos(length):
  chaosString = ""
  for i in range(length):
    chaosString = chaosString + get_random()
  return chaosString
  
while True:
  print make_chaos(80)