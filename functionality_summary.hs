beginner's_first_function x y = x+y
first_string = "there are no variables, only functions"
i_am_a_function = "yes, I indeed am, and I return the string that you are reading"
concatenate = ["strings", "are", "lists"] ++ ["and can be", "concatenated"]
better_concatenate = 'T':'h':'i':'s':' ':"is faster"

list_operations_1 xs = (head xs, tail xs, last xs, init xs, length xs, reverse xs, null xs, minimum xs, maximum xs, sum xs, product xs)
list_operations_2 xs n = (take n xs, drop n xs, elem n xs, xs !! n)
ranges = ([1..20], [1, 5..99], [99, 98..1], ['A'..'Z'], [0.01..10.01])
infinite_shit = (cycle [1, 2, 3], repeat 5)

math_notation = [x*10 | x <- [1..10]]
holy_shit = [x*y | x <- [1..10], y <-[2..5], mod (x*y) 3 == 0]
