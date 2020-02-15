from graphviz import Digraph
dot = Digraph(comment='Iteration: 20')
dot.node('0', '* : 0001')
dot.node('1', '+ : ')
dot.node('2', '~ : 0')
dot.node('3', '* : ')
dot.node('4', 'a : ')

dot.node('5', 'b : ')
dot.node('6', '~ : 1')
dot.node('7', '* : ')
dot.node('8', 'a : ')

dot.node('9', 'c : ')




dot.edge('0', '1')
dot.edge('1', '2')
dot.edge('2', '3')
dot.edge('3', '4')

dot.edge('2', '5')
dot.edge('1', '6')
dot.edge('6', '7')
dot.edge('7', '8')

dot.edge('6', '9')



dot.render('left-multiply-bug/it20.gv', view=True)