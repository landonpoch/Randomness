import uuid

'''
All aggregates have ids
'''
class AggregateAbstract(object):
  def __init__(self):
    self.id = uuid.uuid1()
  
class AccountAbstract(AggregateAbstract):
  def __init__(self, customer_id, initial_balance):
    super(AccountAbstract, self).__init__()
    self._customer_id = customer_id
    self._balance = initial_balance
    
class SavingsAccount(AccountAbstract):
  initial_balance = 20.0
  def __init__(self, customer_id):
    super(SavingsAccount, self).__init__(customer_id, SavingsAccount.initial_balance)
    
class Customer(AggregateAbstract):
  def __init__(self, first_name, last_name):
    super(Customer, self).__init__()
    self._first_name = first_name
    self._last_name = last_name