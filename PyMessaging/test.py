class BaseTest(object):
  def __init__(self):
    raise NotImplementedError
  
  def set_id(self, id):
    self.id = id
    
class Concrete(BaseTest):
  def __init__(self):
    self.set_id(1)
    
c = Concrete()