from messages import Message, Command
from infrastructure import Bus
from application import CommandHandlers

'''
Simple presentation layer (console app) that
pretty much does nothing but allow us to test
a super simplified architecture.
'''
class Presentation:
  def __init__(self):
    self._bus = Bus()
    CommandHandlers(self._bus) #usually run as a separate thread
    
  def create_account(self):
    # use this as a simple way to create a command instead of some gui just for now
    print 'Please create a new customer first!'
    first_name = raw_input('First Name: ')
    last_name = raw_input('Last Name: ')
    m = Message()
    m.first_name = first_name
    m.last_name = last_name
    self._bus.publish(m, Command.CREATE_CUSTOMER)
    print 'finished command'

Presentation().create_account()