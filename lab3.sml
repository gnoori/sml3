(* left subtree, right subtree, key, value *)
datatype BST = Empty | Node of BST * BST * int * int;

fun parsePost [] = Empty
|   parsePost lst =
    let
        fun pP (stack, (0,k,v)::str) = pP(Node(Empty, Empty, k, v)::stack, str)
        |   pP (L::stack, (1,k,v)::str) = pP(Node(L, Empty, k, v)::stack, str)
        |   pP (R::stack, (2,k,v)::str) = pP(Node(Empty, R, k, v)::stack, str)
        |   pP (R::L::stack, (3,k,v)::str) = pP(Node(L, R, k, v)::stack, str)
        |   pP (T::stack, []) = T;
    in
        pP([],lst)
    end;

(*Guzel Noori
Lab 3 - 2/7/2021*)
val exTree0 = []
val exTree1 = [(0,1,1),(0,3,3),(3,2,2)];
val exTree2 = [(0,2,2),(2,1,1),(0,4,4),(3,3,3),(0,6,6),(1,7,7),(3,5,5)];
val exTree3 = [(0,1,1),(0,4,4),(1,5,5),(3,2,2),(1,8,8),(0,15,15),(2,14,14),(3,11,11)];

(*Inserts a key-value pair into a given tree and returns 
the resulting tree.*)
fun insert (Empty, key, value) = Node(Empty, Empty, key, value)
  | insert(Node(left, right, i, e), key, value) =
    if key < i
      then Node(insert(left, key, value), right, i, e)
    else Node(left, insert(right, key, value), i, e);

(*Searches for a node with the given key. 
Returns a list containing the corresponsing 
value if such node exists, or returns an empty list.*)
fun find (Empty, key) = Empty
  | find (Node(left, right, i, e), key) =
    if key < i
      then find(left, key)
    else if key > i
      then find(right, key)
    else Node(left, right, i, e);

(*Deletes the node with the given key from the given 
tree and returns the resulting tree.If does not exist, 
then it returns the unchanged tree*)
fun delete (Empty, key) = Empty
  | delete (Node(left, right, i, e), key) =
    if key < i then Node(delete(left, key), right, i, e)
    else if i < key then Node(left, delete(right, key), i, e)
    else
      if right = Empty
        then left
      else if left = Empty
        then right
      else
        let
          fun min(Empty) = nil
            | min(Node(Empty, Empty, i, e)) = [i, e]
            | min(Node(Empty, right, i, e)) = min right
            | min(Node(left, Empty, i, e)) = min left
            | min(Node(left, right, i, e)) =
              let val least = min left
                  val most = min right
              in if hd least < hd most then least else most end
        in
          Node(left, delete(right, hd (min(right))), hd (min(right)), hd (tl (min(right))))
        end;

(*Returns a postorder of the given tree in the format described above.*)
fun postOrder Empty = nil
  | postOrder (Node(Empty, Empty, i, e)) = [(0,i,e)]
  | postOrder (Node(left, Empty, i, e)) = postOrder(left) @ [(1,i,e)]
  | postOrder (Node(Empty, right, i, e)) = postOrder(right) @ [(2,i,e)]
  | postOrder (Node(left, right, i, e)) = postOrder(left) @ postOrder(right) @ [(3,i,e)];

(*Subtree function that trims the given tree such that all 
the keys in the new tree are between minKey and maxKey inclusively*)
fun subTree (Empty, minKey, maxKey) = Empty
  | subTree (Node(left, right, i, e), minKey, maxKey) =
    if i >= minKey andalso i <= maxKey
      then Node(subTree(left, minKey, maxKey), subTree(right, minKey, maxKey), i, e)
    else if i < minKey
      then subTree(right, minKey, maxKey)
    else subTree(left, minKey, maxKey);
