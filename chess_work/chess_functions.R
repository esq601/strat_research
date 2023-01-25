# function to initialize the MCTS algorithm
mcts_init <- function(board) {
  # initialize the tree with the current board state
  tree <- list(board=board, children="")
  
  # return the initialized MCTS algorithm
  return(list(tree=tree))
}

# function to run the MCTS algorithm for a given number of iterations
mcts_run <- function(mcts, iterations) {
  # get the current tree from the MCTS algorithm
  tree <- mcts$tree
  
  # loop through the specified number of iterations
  for (i in 1:iterations) {
    # select a node in the tree using the UCB1 algorithm
    selected_node <- mcts_ucb1_select(tree)
    
    # expand the selected node by adding all possible child nodes
    selected_node <- mcts_expand(selected_node)
    
    # simulate random playouts from the selected node to the end of the game
    win_prob <- mcts_simulate(selected_node)
    
    # update the selected node with the results of the playout
    selected_node <- mcts_update(selected_node, win_prob)
  }
  
  # return the updated MCTS algorithm
  return(list(tree=tree))
}

# function to select the best move using the MCTS algorithm
mcts_best_move <- function(mcts) {
  # get the current tree from the MCTS algorithm
  tree <- mcts$tree
  
  # find the child node with the highest win probability
  best_node <- max(tree$children, key=function(child) child$win_prob)
  
  # return the move that leads to the best node
  return(best_node$move)
}


# function to select a node in the tree using the UCB1 algorithm
mcts_ucb1_select <- function(node) {
  # if the node has no children, return the node
  if (length(node$children) == 0) return(node)
  
  # calculate the UCB1 value for each child node
  ucb1_values <- sapply(node$children, function(child) {
    # calculate the exploitation term
    exploitation <- child$win_prob / child$visits
    
    # calculate the exploration term
    exploration <- sqrt(2 * log(node$visits) / child$visits)
    
    # return the UCB1 value for the child node
    return(exploitation + exploration)
  })
  
  # find the child node with the highest UCB1 value
  best_child_index <- which.max(ucb1_values)
  best_child <- node$children[best_child_index]
  
  # if the best child node is a leaf node, return the node
  if (length(best_child$children) == 0) return(best_child)
  
  # otherwise, recursively select a node using the UCB1 algorithm
  return(mcts_ucb1_select(best_child))
}

# function to expand a node by adding all possible child nodes
mcts_expand <- function(node) {
  # get all possible moves from the current board state
  moves <- chess_moves(node$board)
  
  # add a child node for each possible move
  node$children <- lapply(moves, function(move) {
    # make the move on the board
    board <- chess_move(node$board, move)
    
    # create a new node for the resulting board state
    return(list(board=board, move=move, visits=0, win_prob=0, children=""))
  })
  
  # return the expanded node
  return(node)
}

# function to simulate random playouts from the selected node to the end of the game
mcts_simulate <- function(node) {
  # get the current board state
  board <- node$board
  
  # simulate random playouts until the game ends
  while (TRUE) {
    # get all possible moves from the current board state
    moves <- chess_moves(board)
    
    # if there are no possible moves, the game is over
    if (length(moves) == 0) break
    
    # choose a random move
    move <- sample(moves, 1)
    
    # make the move on the board
    board <- chess_move(board, move)
  }
  
  # check the winner of the game
  winner <- chess_winner(board)
  
  # calculate the win probability for the selected node
  # (1 if the selected node's player won, 0 if the opponent won, 0.5 if it was a draw)
  win_prob <- ifelse(winner == 1, 1, ifelse(winner == -1, 0, 0.5))
  
  # return the win probability
  return(win_prob)
}

# function to update the selected node with the results of the playout
mcts_update <- function(node, win_prob) {
  # increment the number of visits for the node
  node$visits <- node$visits + 1
  
  # update the win probability for the node using incremental average
  node$win_prob <- (node$win_prob * (node$visits - 1) + win_prob) / node$visits
  
  # return the updated node
  return(node)
}
