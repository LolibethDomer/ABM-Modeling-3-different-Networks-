/**
* Name: TABMSSmartGrid
* Author: Lolibeth Domer
* Tags: 
*/


model TABMSSmartGrid

global {
	graph g_graph;
	
	int nb_nodes <-20 parameter:"Number of Nodes:" min:20 max:500;
    int power_sources <-5 parameter: "Power Sources:" among:[5,10, 50,100, 150];
    int walker <-5 parameter: "Walkers:" among:[5,10, 50,100, 150];
    int power_consumer <-10 parameter: "Power Consumer:" among:[10,50,100, 150,200,250,300,350];
	string network <-"Random Network" parameter: "Network type:"  among: ["Random Network", "Scale-Free", "Small-World"];
	string routing <- "Random Routing"parameter: "Routing:" among: ["Random Routing", "Centrality-Based Routing"];
	//internal variables of nodes
	list<int> list_nodes_degrees;
	list<point> list_nodes_visited;
	list<point> list_all_nodes;
	list list_regular_nodes;//not point
	list<point> list_all_edges;
	list list_regular_edges;
	
	//points of nodes
	list<point> list_sources;
	list<point> list_consumers;
	list<point> list_walkers;
	list<int> list_connected_index;

	//shortest path variables
	point source;
    point target;
    path random_walk_path;
    path centrality_based_path;
    list<path> paths;    
    
	//WALKERS REFLEX	
	reflex pick_two_points {
		
	    if (g_graph = nil) {
	        g_graph <- one_of(g_graph.vertices);
	    }
	    
	  //write list_nodes_visited;   
		if (routing="Centrality-Based Routing"){
			   centrality_based_path <- nil;
			    loop while: centrality_based_path = nil {
			        target <- any (list_consumers);     
			        source <-any (list_walkers);
			        
			          //stopping simulation	
	  			 	if ( empty(list_nodes_visited)){
	   					write "ALL NODES ARE VISITED";
	   					do pause;
	   					break;
	   					}
	   				else {// mark the node as visited 
			        	remove target from: list_nodes_visited;
			        }
			      	g_graph <- g_graph with_weights (g_graph.edges as_map (each::geometry(each).perimeter));
			        if (source != target) {
			        	if (network="Random Network"){//builtin-node
			        		centrality_based_path <- path_between(g_graph, source, target);
			        	}
			        	else{// (scale-free and small world) regular_agent_node
			        		centrality_based_path <- path_between(g_graph, source, target);
			        		//centrality_based_path <- compute_random_regular();		
			       		}
			        }
			    }
		    }//if end
	    else{//Random Routing
	    	random_walk_path <- nil;
		    loop while: random_walk_path = nil {
		        source <-any (list_walkers);
		        target <- any (list_consumers);
		        
		         //stopping simulation	
	  			 if ( empty(list_nodes_visited)){
	   					write "ALL NODES ARE VISITED";
	   					do pause;
	   					break;
	   					}
	   			else {// mark the node as visited 
			        remove target from: list_nodes_visited;
			     }
		       if (source != target) {
			        	if (network="Random Network"){//builtin-node
			        		random_walk_path <- path_between(g_graph, source, target);
			        	}	
			        	else{
			        		random_walk_path <- path_between(g_graph, source, target);
			        		//random_walk_path <- compute_random_regular();		
			       		}
			     }
		    } 	
	    }//else end
	
    }//reflex end
    
    //Unfinish algorithm for path (small-world and scale-free)
    action compute_random_regular{
    	list all_node_passed<-nil;
		list path_way<-nil;
		list distance<-nil;
		list degrees<-nil;
		list edge<-nil;
		list neigh<-nil;
		list rej<-nil;
		
		point node<-target;
		list a<-nil;
		loop v over: list_regular_nodes {
			 if (node=point(v)){
			  	a<-v;
			  }
		}
		//record the node
		add a[0] to: all_node_passed;
		
	//	loop while: point(node) != source {
		loop s from:0 to:1 {
		//scan next node
		 loop f over: list_regular_nodes{
		 	float d<-point(f) distance_to point(node);
			add d to: distance;
			add f to: neigh;
			add g_graph degree_of f to: degrees;
			//write f;
		 }//scanning next node end
		 
		 write distance;
		 //arrange order
		 list temp2<-[regular_agent_node(0)];
		 float temp; 
		 int i;
		 int j; 
		 int k;
		 loop i from:0 to:nb_nodes-1{
		 	 loop j from:i+1 to:nb_nodes-1{
		 	 	if j=nb_nodes{
		 	 		break;
		 	 	}
		 	 	float x<-float(distance[i]);
		 	 	float y<-float(distance[j]);
		 	 	
		 	 	if ( x>y ) {
                    temp <- float(distance[i]);
                   distance[i] <- distance[j];
                   distance[j] <-temp;
                   
                    temp2[0] <- neigh[i];
                   neigh[i] <- neigh[j];
                   neigh[j] <-temp2[0];
                }
		 	 	j<-j+1;
		 	 }
		 	 i<-i+1;
		 }
		 //add 0 at: 0 to: workingList ;
 		list next_node;
 		list backup_node;
 		pair pairs;
 		pair backup_pairs;
 		//scanning what edge and node is next
		loop i from:0 to:length(neigh)-1{
			list z<-nil;
			loop v over: list_regular_nodes {
			 if (node=point(v)){
			  	z<-v;
			  }
			}
			if (g_graph edge_between(neigh[i]::z[0])!=nil){
				list edge<-g_graph edge_between(neigh[i]::z[0]);}
			if (distance[i]!=0.0 and i!=0 and (float (distance[i])>float(distance[i-1]))  and (g_graph degree_of neigh[i]>=3)  and !empty(edge) ){
				next_node<-neigh[i];
				pairs<-(z[0]::neigh[i]);
				break;
			}
			if (distance[i]!=0.0  and g_graph degree_of neigh[i]>=2 and !empty(edge) ){
				backup_node<-neigh[i];
				backup_pairs<-(z[0]::neigh[i]);
			}
			
			if (i=length(neigh)-1 and empty(next_node)){
				next_node<-backup_node;
				pairs<-backup_pairs;
			}	
		}
		 
		
		 if (!empty(next_node)){
		 	 	node<-next_node[0];
		 	 	pairs<-node::neigh[0];
		 		 edge<-g_graph edge_between(pairs);
		 }
		 else{
		 	 node<-neigh[0];
		 	pairs<-node::neigh[0];
		 	 edge<-g_graph edge_between(pairs);
		 }
		 
		
		 if (point(node) distance_to point(target)=0){
			break;
		}
		 add edge[0] to: path_way;
		}
		 return path(path_way);
		  
    }//endfunction
    

	//CLEANING GRAPHS, VARIABLES ETC.
	action clean {
		//species
		
		ask regular_agent_node {
			do die;
		}
		ask regular_agent_edge {
			do die;
		}

		ask builtin_edge {
			do die;
		}

		ask builtin_node {
			do die;
		}
		
		
		//list of walkers/producers/consumers
		list_sources<-nil;
		list_consumers<-nil;
		list_walkers<-nil;
		
		//reflex walkers
		centrality_based_path <- nil;
		random_walk_path <- nil;
		list_connected_index <- nil;
		
		//internal variables/nodes
		list_nodes_degrees<-nil;
		list_nodes_visited<-nil;
		list_all_nodes<-nil;
		list_regular_nodes<-nil;
		
	}
	
	
	//RANDOMIZE PRODUCER, CONSUMER, AND WALKERS, GET CONSUMERS TO VISIT
	action assigning_nodes{
		int count_sources<-0;
		int count_consumer<-0;
		int count_walker<-0;
		bool flag<-false;
		list<point> list_check<-nil;
		list<point> list_check_sources<-nil;
		
		point rand;
		
		list components_edge<-connected_components_of(g_graph,true);//true=edges, false=nodes
		list components_node<-connected_components_of(g_graph,false);//true=edges, false=nodes

		loop v over: g_graph.vertices {
			add v to: list_check;//for randomize
			add v to: list_all_nodes;//all nodes(point)
			add v to: list_regular_nodes;
			add g_graph degree_of v to: list_nodes_degrees;//(degree of nodes)
		}
		
		loop v over: g_graph.edges {
			add v to: list_all_edges;//all nodes(point)
			add v to: list_regular_edges;
		}
		
		loop p from:0 to: nb_nodes-1{
			rand<-any (list_check);
			//write length(list_check);
			if (count_sources!=power_sources){
				count_sources<-count_sources + 1;
				remove rand from: list_check;
				add rand to: list_sources;
			}
			else if (count_consumer!=power_consumer){
				count_consumer<-count_consumer + 1;
				remove rand from: list_check;
				add rand to: list_consumers;
			}	
			
		}//loop
		//make the rest a consumer
		if (!empty(list_check)){
				loop p from:0 to: length(list_check)-1{
					rand<-any (list_check);
					remove rand from: list_check;
					add rand to: list_consumers;
				}
			}
		
			//LIST OF CONSUMERS TO VISIT
			list_nodes_visited<-list_consumers;
			
			loop v over: list_sources {
			add v to: list_check_sources;}
			
			loop p from:0 to: walker-1{
				rand<-any (list_check_sources);
				 if (count_walker!=walker){
					count_walker<-count_walker + 1;
					remove rand from: list_check_sources;
					add rand to: list_walkers;
				}	
				
			}//loop walker
			g_graph <- g_graph with_weights (g_graph.edges as_map (each::geometry(each).perimeter));
	}//randomized

	//SMALL WORLD
	action small_world {//regular node
		write "Small-World Network";
		do clean;
		float rewirering_probability <- 0.1;
		int fake_lattice_start_degree <- 4; 
		g_graph <- generate_watts_strogatz(regular_agent_node, regular_agent_edge, nb_nodes, rewirering_probability, fake_lattice_start_degree, true);
	}
	
	//for small world layout
	action circle_layout {
		g_graph <- layout_circle(g_graph, world.shape, false );
		//g_graph <- spatial_graph(g_graph, world.shape, false );
	}
	
	//SCALE-FREE
	action scale_free {
		write "Scale-Free Network";
		do clean;
		int new_edges_addition_per_node_introduction <- 1;
		g_graph <- generate_barabasi_albert(regular_agent_node, regular_agent_edge, nb_nodes,	new_edges_addition_per_node_introduction, true);
		//g_graph <- spatial_graph(g_graph, world.shape,regular_agent_node, regular_agent_edge,nb_nodes );
}
	
	//for scale-free layout
	action hubs_layout {
		g_graph <- layout_force(g_graph, world.shape,0.5,0.01, 100 );
	}
	
	//RANDOM 
	action random_network {//builtin 
		write "Random Network";
		do clean;
		create builtin_node number: nb_nodes;
		g_graph <- first(builtin_node).my_graph;
		//g_graph <- layout_force(g_graph, world.shape, 0.5, 0.01, 100 );
		create people number: 1000{
			//People agents are located anywhere in one of the building
			location <- one_of(g_graph);
      	}
	}
	
}// global end

//BUILTIN_NODE SPECIES
species builtin_node parent: graph_node edge_species: builtin_edge {
   
    init {
    int i <- 0;
    loop g over: builtin_node {
        if (flip(0.1)) {
        add i to: list_connected_index;
        }
        i <- i + 1;
    }
    }

    bool related_to (builtin_node other) {
    using topology(world) {
        return (self.location distance_to other.location < 50);
    }
    }

   aspect base {
    	point the_node <-self;
		string class<-nil;
		
		loop s from:0 to: length(list_sources)-1 {
			if (list_sources[s]=the_node){
				class<-"source";}}
		loop c from:0 to: length(list_consumers)-1 {
			if (list_consumers[c]=the_node){
				class<-"consumer";}}
		
		if (class="source"){
			draw circle(1) color: #red;}
		else if(class="walker"){
			draw circle(1) color: #yellow;}
		else{//consumers
    		draw circle(1) color: #black;}
    	
    	
    	//draw string(g_graph degree_of self) color: #black size: 4 at: {self.location.x-1, self.location.y-2};
      
      	bool flag<-false;
	    loop v over: list_nodes_visited {
	      	if (the_node=v){
	      		flag<-true;
	      	}
	      }
	     //except sources
	     loop v over: list_sources {
	      	 if (the_node=v){
	      		flag<-true;
	      	}
	     }
	      	if (flag=false){
	      		draw string("Visited") color: #black size: 4 at: {self.location.x-1, self.location.y-2};	
	      	}
      	}//aspect
}//end builtin node 

species builtin_edge parent: base_edge {
   aspect base {
   		 draw shape color: #skyblue;
    }
}


//REGULAR_AGENT_NODE SPECIES
species regular_agent_node /*parent: graph_node*/ edge_species: regular_agent_edge{
	
  init {
    int i <- 0;
    loop g over: regular_agent_node {
        if (flip(0.1)) {
        add i to: list_connected_index;
        }
        i <- i + 1;
    }
    }

		
	aspect base {
    	point the_node <-(self);
		string class<-nil;

		loop s from:0 to: length(list_sources)-1 {
			if (list_sources[s]=the_node){
				class<-"source";}}
		loop c from:0 to: length(list_consumers)-1 {
			if (list_consumers[c]=the_node){
				class<-"consumer";}}
		
		if (class="source"){
			draw circle(1) color: #red;}
		else if(class="walker"){
			draw circle(1) color: #yellow;}
		else{//consumers
    		draw circle(1) color: #black;}
    	
    	bool flag<-false;
	    loop v over: list_nodes_visited {
	      	if (the_node=v){
	      		flag<-true;
	      	}
	      }
	     //except sources
	     loop v over: list_sources {
	      	 if (the_node=v){
	      		flag<-true;
	      	}
	     }
	      	if (flag=false){
	      		draw string("Visited") color: #black size: 4 at: {self.location.x-1, self.location.y-2};	
	      	}
	     
      	}//aspect
}//regular_agent_node end

species regular_agent_edge parent: base_edge{
	 aspect base {
   		 draw shape color: #gray;
    }	
	aspect default {
		draw circle(5) color: color;
		
	}
}

//FOR MOVING WALKERS
  species people skills: [moving]{
}
//FOR MOVING WALKERS

//NETWORKS
experiment Graph type: gui {
	user_command "Draw the Network" {
		switch network {
			match "Scale-Free" {
				ask world {
					do scale_free;
					do hubs_layout;
					do assigning_nodes;
				}

			}

			match "Small-World" {
				ask world {
					do small_world;
					do circle_layout;
					do assigning_nodes;
				}
			}

			match "Random Network" {
				ask world {
					do random_network;
					do assigning_nodes;	
				}
			}
		}

	}
	
	
	output {
		display MyDisplay type: java2D {
		species builtin_node aspect: base;
        species builtin_edge aspect: base;
        species regular_agent_node aspect: base;
       	species regular_agent_edge aspect: base;
        graphics "path" { 	
        if (centrality_based_path != nil) {
            draw circle(2) at:  point(centrality_based_path.source)color: #red border: #black;
             draw string("SOURCE") color: #black size: 4 at: {source.location.x-1, source.location.y-2};	
            draw circle(2) at:  point(centrality_based_path .target) color: #yellow border: #black;
            draw (centrality_based_path.shape + 0.5) color: #yellow;
        }
         if (random_walk_path != nil) {
            draw circle(2) at: source color: #red border: #black;
            draw string("SOURCE") color: #black size: 4 at: {source.location.x-1, source.location.y-2};	
          	draw circle(2) at: target color: #yellow border: #black;
			draw (random_walk_path.shape + 0.5) color: #yellow;
        }
        }
    }
   }
	

}//experiment end


