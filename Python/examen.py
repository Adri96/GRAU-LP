

def i_func(n, l, dic):
  for x in l:
    if not isinstance(x, list):
      if not n in dic: dic[n] = 0
      dic[n] += 1
    else:
      i_func(n+1, x, dic)
        

def func(l):
  dic = {}
  i_func(1, l, dic)
  return dic


d = func([ [1], [2, [3, 4, 5]] ])
for x in d:
  print "{} {}".format(x, d[x])




class Tree(object):
  def __init__(self, x):
    self.rt = x
    self.child = []
    self.cnt = 0
  def addChild(self, a):
    self.child.append(a)
  def root(self):
    return self.rt
  def ithChild(self, i):
    return self.child[i]
  def numChildren(self):
    return self.cnt


class Pre(Tree):
  def i_preorder(a):
    return reduce(lambda x, y: x + i_preorder(y), a.child, [a.root])
  def preorder(self):
    return i_preorder(self)



a = Pre(2)
a.addChild(Pre(3))
a.addChild(Pre(4))
print a.preorder()
















