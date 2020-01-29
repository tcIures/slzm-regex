from graphviz import Digraph
dot = Digraph(comment='The Round Table')

dot.node('A', 'King Arthur')
dot.node('B', 'Sir Bedevere the Wise')
dot.edge('A', 'B')
dot.edge('A', 'B')
dot.node('L', 'Sir Lancelot the Brave')


dot.edge('B', 'L', constraint='false')

dot.render('trees/round-table.gv', view=True)