TOWARDS AGENT-BASED MODEL SPECIFICATION OF SMART GRID:
A COGNITIVE AGENT-BASED COMPUTING APPROACH

Note: 
a. The routing for the three networks(Random, scale-free, 
   small-world) is unfinished.
	i. Only for Random Network works(random and centrality based), 
	   since its graph is not based on polygon.
	ii. The scale-free and Small-world's path were not finished. I made a
	    self algorithm(compute_random_regular function) for its path as an alternative,
            due to builtin functions incompatability, however I cannot finished it on time. 
    	    But the networks can still view its sources and targets moving that will marked 
            as visited. 
b. Click the general button on the right to show the parameters and click 
   the "draw the network" button to show the networks. 
c. When running the graph, please do adjust the minimum cycle to
   1.0s (lowest) to see the path for either random or centrality-based
   routing.
d. The experiment will stop if all the consumers are visited. 
e. Provided a .gaml and a .txt versions just in case. 