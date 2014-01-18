let is_palindrome lst = (lst = List.rev lst);;

assert(is_palindrome [`a;`b;`c] = false);;
assert(is_palindrome [`a;`b;`a] = true);;
assert(is_palindrome [`a;`b;`b;`a] = true);;
assert(is_palindrome [`a;`a] = true);;
assert(is_palindrome [`a] = true);;
assert(is_palindrome [] = true);; 
