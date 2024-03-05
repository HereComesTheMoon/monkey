

Types:
- Primitive types:
	- Integers, bool. Decide whether a single bigint integer type or to follow Rust's model. 
	- No strings at first, too much baggage associated with that!
	- A 'None' type.
- All other types should purely be given by a pattern matching. Ideas:
	- (a: int, b: int) a struct/tuple of two integers named a and b
	- _ the any type
	- (a: int, ..) a struct with an integer a which may contain other fields
	- [int..] a vector/list/array of ints
	- (int -> int) a function which takes an int and returns an int
	- a: int | b: None
	- int | None

- Ideas:
	- Each function takes EXACTLY ONE argument.
		- If this sounds insane, remember that the default argument will be a tuple with fields.
	- 'Destructuring' should be super easy.
	- For now ALL variables have to be explicitly typed.
		- In particular, all functions HAVE to be typed. Follow Rust's "Golden Rule" here, do not compromise.
	- Type checking happens by seeing if all the patterns match up.

	- What if we did not have any vectors or array types? Maybe just aim for a 'better lisp', and ignore all the vector stuff. Instead: 
	

Questions:
- How to handle field names? 
- How to handle enum names? Is there any 'natural' solution to this?
- Not sure if type inference is possible
- Are generics possible? Can these be handled via pattern matching? 
	- Surely a vector of generic type T matches the Vec<_> pattern.
- How to affirm that two types are the same, without explicitly naming them?
	- Should this even be possible?
- If we'd like to destructure structs to access the internal variables, how do we distinguish between destructuring and merely typing the variable?
	- Example: 
		Imagine we call a function foo() which takes as input the type: (int, (int, int)). The function signature might look like:
		foo(a: int, b: (c: int, d: int)). So, what's the problem? In this case we seem like we'd be able to address b, c, and d. But that's not great. Alternatives: 
			- foo(a: int, _: (c: int, d: int)).
				- Problem: This might not be uniquely determined, or it might require allowing very 'loose' inputs?

	- Motivating example:
		```
		fn foo((x1: int, y1: int), (x2: int, y2: int)) -> (int, int) {
			return (x1 + x2, y1 + y2)
		}

		let p: (int, int) = (1, 2);
		let q: (int, int) = (3, 4);
		foo(p, q);
		```
		This should be possible.

	- Alternative: NO struct field names, NO enum variant names WHATSOEVER. Instead: All of these names are determined at definition-site, and it HAS to be possible to uniquely bind the pattern to the incoming variable...Or alternatively, make it all order-dependent, which might be a mess. When it is necessary to use two types at the same time, use the newtype pattern. 



Motivating examples:
```
fn foo((x1: int, y1: int), (x2: int, y2: int)) -> (int, int) {
	return (x1 + x2, y1 + y2)
}

let p: (int, int) = (1, 2); // type annotation may not be necessary? Difficult to say
let q: (int, int) = (3, 4); 
foo(p, q);
```

```
// Hypothetical function signature for hand-rolled map on Vector. 
// In practice: Want dot/method syntax
fn map<T, S>(v: Vec<T>, f: T -> S) -> Vec<S> {
	...
}

```

```
fn find_by<T>(v: Vec<T>, key: fn(T) -> bool) -> T | None {
	for x in v {
		if key(x) {
			return x;
		}
	}
	return None;
}
```

```
fn 
```


```
let v = vec![1, 2, 3, 4];
let f = |
v.iter()
 .map()

```
