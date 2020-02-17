from graphviz import Digraph
dot = Digraph(comment='Iteration: 200')
dot.node('0', '+ : 0')
dot.node('1', '* : 001')
dot.node('2', '+ : ')
dot.node('3', '~ : 0')
dot.node('4', '* : ')
dot.node('5', 'a : ')


dot.node('6', 'b : ')

dot.node('7', '~ : 1')
dot.node('8', '* : ')
dot.node('9', 'a : ')


dot.node('10', 'c : ')






dot.edge('0', '1')
dot.edge('1', '2')
dot.edge('2', '3')
dot.edge('3', '4')
dot.edge('4', '5')


dot.edge('3', '6')

dot.edge('2', '7')
dot.edge('7', '8')
dot.edge('8', '9')


dot.edge('7', '10')





dot.render('left-multiply-bug/it200.gv', view=True)