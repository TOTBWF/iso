# iso - an isomorphic programming language

iso is an interpreted programming language that explores the computational power of pattern matching.
In this language we don't define functions, we define isomorphisms. They can execute in either direction.

This has some interesting consequences. Imagine you are writing Python like this:

```python
[a, b] = [1, 2]
```

This assigns 1 to a and 2 to b. Nothing that special, many languages have this feature.

Now imagine you could write this in Python:

```python
def fn(x):
    return x+1

[fn(a), fn(b)] = [1, 2]
```

If this did work, it would have to answer the question: what value of a gives 1 when passed to fn()?

Now try this in iso, which does work.

You can define fn with a bit of boilerplate as we haven't added integer math as a builtin yet:
```
type Int4 = Bool * Bool * Bool * Bool

iso fn :: Int4 <-> Int4
| a,b,c,0 <-> a,b,c,1
| a,b,0,1 <-> a,b,1,0
| a,0,1,1 <-> a,1,0,0
| 0,1,1,1 <-> 1,0,0,0
| 1,1,1,1 <-> 0,0,0,0
```

Then you can run it forwards:
```
> :evall fn (0,0,0,1)
(0,0,1,0)
```

And backwards:
```
> :evalr fn (0,0,1,0)
(0,0,0,1)
```
# Ideas/Future Plans
- Parametric polymorphism has some interesting consequences (though it is not implemented)
For example, consider the isomorphism `if :: Bool <-> Either a b`. Both sides actually hold the same
amount of "information", even though they may not have the same "size". This could be an interesting
area for further research.

# Compiling and Running
```
stack build && stack exec iso
```
