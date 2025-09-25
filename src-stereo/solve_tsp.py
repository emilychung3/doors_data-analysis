'''
lydia barnes, april 2024
from: https://www.geeksforgeeks.org/traveling-salesman-problem-tsp-implementation/
see also https://gis.stackexchange.com/questions/420940/shortest-path-touching-all-points

'''

from sys import maxsize 
from itertools import permutations
import numpy as np
 
# implementation of traveling Salesman Problem 
def travelling_salesman(graph,nodes): 

    # list all vertices
    V = np.shape(graph)[0]
    vertex = [] 
    for i in range(V): 
        vertex.append(i) 

    # permute the vertices
    next_permutation=permutations(vertex)
 
    # iterate through permutations, recording the distance covered by each path
    min_distance = maxsize #start with a big number
    paths = []; distance = []; #make some empty lists
    for i in next_permutation:
 
        # compute current path distance 
        current_distance = 0
        for j,node in enumerate(i): 
            previous_node = i[j-1]
            current_distance += graph[previous_node][node] #add to the loop distance
 
        # do we have a new minimum distance?
        min_distance = min(min_distance, current_distance) 

        # record all paths and distances
        paths.append(nodes[list(i)])
        distance.append(current_distance)
    
    # use the minimum distance to select all shortest paths
    paths = np.asarray(paths)
    idx = np.where(np.round(distance,4) == np.round(min_distance,4))[0]
    paths = paths[idx,:]

    return [paths,min_distance]


def hamiltonian_path(graph,nodes,source_vertex): 
 
    # store all vertices apart from source vertex 
    V = np.shape(graph)[0]
    vertex = [] 
    for i in range(V): 
        if i != source_vertex: 
            vertex.append(i) 

    # generate permutations
    next_permutation=permutations(vertex)
 
    # iterate over permutations
    min_distance = maxsize 
    paths = []; distances = []
    for i in next_permutation:
 
        # compute current path weight 
        current_distance = 0
        previous_node = source_vertex 
        for node in i: 
            current_distance += graph[previous_node][node] 
            previous_node = node
 
        # update minimum 
        min_distance = min(min_distance, current_distance) 
        
        i = (source_vertex,) + i #make sure you record the starting point
        paths.append(nodes[list(i)])
        distances.append(current_distance)
    
    paths = np.asarray(paths)
    idx = np.where(np.round(distances,4) == np.round(min_distance,4))[0]
    paths = paths[idx,:]
    distances = np.asarray(distances)
    distances = list(distances[idx])
         
    return [paths,distances] 