from graphviz import Digraph
dot = Digraph(comment='Iteration: 40')
dot.node('0', '* : 000011')
dot.node('1', '+ : ')
dot.node('2', '~ : 00')
dot.node('3', 'a : ')

dot.node('4', 'c : ')

dot.node('5', '~ : 01')
dot.node('6', 'a : ')

dot.node('7', 'd : ')

dot.node('8', '~ : 10')
dot.node('9', 'b : ')

dot.node('10', 'c : ')

dot.node('11', '~ : 11')
dot.node('12', 'b : ')

dot.node('13', 'd : ')





dot.edge('0', '1')
dot.edge('1', '2')
dot.edge('2', '3')

dot.edge('2', '4')

dot.edge('1', '5')
dot.edge('5', '6')

dot.edge('5', '7')

dot.edge('1', '8')
dot.edge('8', '9')

dot.edge('8', '10')

dot.edge('1', '11')
dot.edge('11', '12')

dot.edge('11', '13')




dot.render('left-right-multiply-bug/it40.gv', view=True)