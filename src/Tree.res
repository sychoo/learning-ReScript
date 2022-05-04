type rec tree = Leaf | Node(int, tree, tree);

let rec sum = (item) => {
  switch (item) {
  | Leaf => 0
  | Node(value, left, right) => value + sum(left) + sum(right);
  }
};

// hello
let myTree =
  Node(
    1,
    Node(2, Node(4, Leaf, Leaf), Node(6, Leaf, Leaf)),
    Node(3, Node(5, Leaf, Leaf), Node(7, Leaf, Leaf))
  );

let rec depth = (item) => {
	switch (item) {
  | Leaf => 1
   | Node (value, left, right) => if (depth(left) > depth(right)) {
      	1+depth(left)
      } else {
      	1+depth(right)
      }
  }
}
Js.log(depth(myTree));
Js.log(sum(myTree));
Js.log("Hello, World!")
