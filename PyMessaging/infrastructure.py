'''
Bus currently just contains a registry of messages and 
actions to invoke when those messages are published.
Basic pub/sub
'''
class Bus:
  def __init__(self):
    self._subscriptions = {}
    
  def publish(self, message, message_type):
    if self._subscriptions.has_key(message_type):
      actions = self._subscriptions[message_type]
      for a in actions:
        a(message)
    
  def subscribe(self, action, message_type):
    if self._subscriptions.has_key(message_type):
      self._subscriptions[message_type].append(action)
    else:
      self._subscriptions[message_type] = [action]