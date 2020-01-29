from graphviz import Digraph
dot = Digraph(comment='The Round Table')
dot.node('0', '* : 110')
dot.node('1', '+ : ')
dot.node('2', '~ : 0')
dot.node('3', 'a : ')
dot.node('4', 'b : ')
dot.node('5', 'c : 1')

dot.edge('0', '1')
dot.edge('1', '2')
dot.edge('2', '3')
dot.edge('2', '4')
dot.edge('1', '5')

dot.render('trees/round-table.gv', view=True)