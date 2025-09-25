'''
lydia barnes, april 2024

gets shortest paths between doors in doors project. doors are arranged in a 
four by four grid and numbered so that column 1 has 1:4, column 2 has 5:8, and 
so on. we calculate the shortest path only for relevant doors, of which there 
are four in each of context 1 and context 2.

inputs:
    graph, a numpy file (nrows = ncolumns = nnodes) of connection weights. in
    this case, connection weights are physical distances between doors in a.u.,
    with 1 = the distance between two adjacent doors. 
    
    sub_infos, a .mat file of trial codes for the subjects whose paths you want
    to analyse. each row is one subject. columns 2:5 (zero-indexed) indicate 
    target door identities in Context A, 6:9 Context B. 

outputs:
    travelling salesman solution: the shortest path that touches all target 
    doors once and returns to the start. the start is irrelevant, because the 
    path is a loop. there will be at least eight solutions: four start points 
    and two directions. these are fundamentally the same solution, but we print
    all of them for clarity. 

    shortest hamiltonian path: the shortest path that touches all target 
    doors, visiting each only once (no loop). here, where you start is 
    important, as it allows you to exclude one section of the loop. there will 
    be at least two solutions, because all solutions can go in either direction. 
    some doors will give more solutions. for example, doors [6 8 13 15] give 
    two U-shapes and one zig-zag solution, each in two directions, for a total 
    of six.

'''
#https://stackoverflow.com/questions/75114841/debugger-warning-from-ipython-frozen-modules
#TODO: export dependencies to .yaml 

## get libraries
import numpy as np
import scipy
import json
from solve_tsp import travelling_salesman, hamiltonian_path

#  read the graph
graph = np.load('graph.npy') 

#   read the trial list from sub_infos.mat
trial_list = scipy.io.loadmat('sub_infos')
trial_list = trial_list['sub_infos']

travelling_solutions = {}
hamiltonian_solutions = {}
context_names = ['A','B']
for subject in range(0,np.shape(trial_list)[0]):

    these_travelling_solutions = {}
    these_hamiltonian_solutions = {}
    for context in range(0,2):

        #   select the context-relevant doors
        if context==0:
            idx = range(2,6)
        else:
            idx = range(6,10)
        doors = trial_list[subject,idx]

        #   get their connections
        this_graph = np.empty((len(doors),len(doors)))
        for i,door in enumerate(doors):
            this_graph[i,:] = graph[door-1,doors-1]

        #   get the shortest path(s), requiring a full loop
        [this_travelling_path,this_travelling_distance] = travelling_salesman(this_graph,doors)

        #   get the shortest path(s), allowing a single visit to each node
        #       first, get the shortest path for each start point
        this_hamiltonian_path = []; this_hamiltonian_distance = []
        for i in range(0,len(doors)):
            [paths,distance] = hamiltonian_path(this_graph,doors,i)
            for j in range(0,len(paths)):
                this_hamiltonian_path.append(paths[j])
                this_hamiltonian_distance.append(distance[j])

        #       some start points will allow for shorter paths than others. 
        #       find those:
        min_idx = np.where(this_hamiltonian_distance == np.min(this_hamiltonian_distance))[0]
        this_hamiltonian_path = np.asarray(this_hamiltonian_path)[min_idx,:]
        
        # append the shortest path to the output
        this_travelling_path = this_travelling_path.tolist()
        this_travelling_solution = [this_travelling_path,this_travelling_distance]
        this_hamiltonian_path = this_hamiltonian_path.tolist()
        this_hamiltonian_solution = [this_hamiltonian_path,np.min(this_hamiltonian_distance)]

        these_travelling_solutions[context_names[context]] = this_travelling_solution
        these_hamiltonian_solutions[context_names[context]] = this_hamiltonian_solution

    travelling_solutions[str(subject+1)] = these_travelling_solutions
    hamiltonian_solutions[str(subject+1)] = these_hamiltonian_solutions

f = open('travelling_solutions.json','w')
json.dump(travelling_solutions,f)
f.close()

f = open('hamiltonian_solutions.json','w')
json.dump(hamiltonian_solutions,f)
f.close()

print()

