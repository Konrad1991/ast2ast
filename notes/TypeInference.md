# Creation of graph

- The entire function is handled as one huge expression
- Each node contains an ID
- all nodes are stored in an ID-Node instance list
- Create a list last_assignments with an entry for each variable.
  The list is initialised with a Root node.
  In case the variable is an argument to the function the root node contains the specified type.
  Otherwise the root node is a mock.
  Thereby, one can detect non initialised variables when the last assignment is the mock node.
- Everytime, an assignment is found it is stored in the list last_assignments.
  Thus, for each variable the last assignment is known.
- Everytime, an if node is found the last assignments are set as following:
    * First one traverses the nodes within true block, false block and else if blocks and gathers all assignments and stores them.
      It is crucial that the assignments are stored within a separate last_assignments_if_node list.
    * If an assignment for a variable is found a so called merge node is created and set as last assignment for the respective variable
    * The merge node contains two ids the one referring to the if_node and another one to the last assignment before the if node.
    * If no assignment for a variable is found the last assignment before the if_node is set as last assignment for this variable.

# Extracting and sorting the edges

- Traverse over the graph and gather for all variable nodes:
  * From: the last assignment (when was the variable updated the last time).
  * To: the id of the variable itself.
  * As one recursively traverses the AST one goes bottom to top.
    Therefore, the slightly confusing naming for From and To
  * Finally one stores the From and To in a Matrix
- Afterwards, one conduct a topological search which results
  in a list of IDs describing the path from last assignment to the first one
  in bottom up direction.
  Now one follows this path for each variable.
