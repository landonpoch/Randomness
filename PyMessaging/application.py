from messages import Command, Event
from domain import Customer, SavingsAccount

'''
Command handlers receive commands, act on the commands and issue events
'''
class CommandHandlers:
  def __init__(self, bus):
    self._bus = bus
    self.register_subscriptions()
    
  def create_customer(self, message):
    CreateAccountSaga(self._bus) # TODO: figure out where saga should actually be created
    customer_id = Customer(message.first_name, message.last_name)
    self._bus.publish(customer_id, Event.CUSTOMER_CREATED)
    
  def create_savings_account(self, customer_id):
    a = SavingsAccount(customer_id)
    self._bus.publish('Savings account created successfully! Account Id: %s' % a.id, Event.SAVINGS_ACCOUNT_CREATED)
      
  def confirm_account_creation(self, message):
    print message
    self._bus.publish(None, Event.ACCOUNT_CREATION_CONFIRMED)

  def register_subscriptions(self):
    self._bus.subscribe(self.create_customer, Command.CREATE_CUSTOMER)
    self._bus.subscribe(self.create_savings_account , Command.CREATE_SAVINGS_ACCOUNT)
    self._bus.subscribe(self.confirm_account_creation, Command.CONFIRM_ACCOUNT_CREATION)

'''
Sagas manage state, receive events and issue commands
'''    
class CreateAccountSaga:
  class SagaState:
    INITIAL = 'INITIAL'
    CUSTOMER_CREATED = 'CUSTOMER_CREATED'
    ACCOUNT_CREATED = 'ACCOUNT_CREATED'
    FINISHED = 'FINISHED'
  
  def __init__(self, bus):
    self._bus = bus
    self._state = self.SagaState.INITIAL
    self.register_subscriptions()
    
  def handle_customer_created(self, customer_id):
    if self._state == self.SagaState.INITIAL:
      self._state = self.SagaState.CUSTOMER_CREATED
      self._bus.publish(customer_id, Command.CREATE_SAVINGS_ACCOUNT)
      
  def handle_account_created(self, message):
    if self._state == self.SagaState.CUSTOMER_CREATED:
      self._state = self.SagaState.ACCOUNT_CREATED
      self._bus.publish(message, Command.CONFIRM_ACCOUNT_CREATION)
      
  def handle_account_confirmed(self, message):
    if self._state == self.SagaState.ACCOUNT_CREATED:
      self._state = self.SagaState.FINISHED
      
  def register_subscriptions(self):
    self._bus.subscribe(self.handle_customer_created, Event.CUSTOMER_CREATED)
    self._bus.subscribe(self.handle_account_created, Event.SAVINGS_ACCOUNT_CREATED)
    self._bus.subscribe(self.handle_account_confirmed, Event.ACCOUNT_CREATION_CONFIRMED)