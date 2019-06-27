import networkx as nx
import matplotlib.pyplot as plt

def get_graph(nodes):
    G = nx.Graph()
    for edge in nodes:
        G.add_edge(edge[0], edge[1])
    return G

def diameter(nodes):
    G = get_graph(nodes)
    return nx.diameter(G)

def clustering(nodes):
    G = get_graph(nodes)
    return nx.average_clustering(G)

def visualize(nodes):
    G = get_graph(nodes)
    plt.subplot(121)
    nx.draw(G, with_labels=True, font_weight='bold')
    plt.show()
