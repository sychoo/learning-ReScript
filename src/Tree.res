// Tue May  3 23:41:11 EDT 2022
// Simon Chu

type rec tree =
  | Leaf
  | Node(int, tree, tree);


let rec sum = (t) => {
  switch (t) {
  | Leaf => 0
  | Node(v, lst, rst) => v + sum(lst) + sum(rst);
  }
}

let rec depth = (t) => {
	switch (t) {
  | Leaf => 0
  | Node (_, lst, rst) =>
    if (depth(lst) > depth(rst)) {
      1 + depth(lst)
    } else {
      1 + depth(rst)
    }
  }
}

let rec search = (t, k) => {
  switch (t) {
  | Leaf => false
  | Node (v, lst, rst) =>
    if (v == k) {
      true
    } else if (k < v) {
      search(lst, k)
    } else {
      search(rst, k)
    }
  }
}

let leaf = Leaf

// 7
let single = Node(7, Leaf, Leaf)

//     8
//     /\
//    3  10
//   /\   \
//  1 6    14
//    /\   /
//   4  7  13
let bst =
  Node(
    8,
    Node(3,
      Node(1, Leaf, Leaf),
      Node(6,
        Node(4, Leaf, Leaf),
        Node(7, Leaf, Leaf)
      )
    ),
    Node(10,
      Leaf,
      Node(14,
        Node(13, Leaf, Leaf),
        Leaf
      )
    )
  );

//     1
//     /\
//    2  3
//   /\  /\
//  4 6  5 7
let bt =
  Node(
    1,
    Node(2,
      Node(4, Leaf, Leaf),
      Node(6, Leaf, Leaf)
    ),
    Node(3,
      Node(5, Leaf, Leaf),
      Node(7, Leaf, Leaf)
    )
  );

Js.log(depth(bst));
Js.log(sum(bst));
Js.log(search(bst, 100));
Js.log(search(bst, 14));