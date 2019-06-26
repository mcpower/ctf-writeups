# Code Golf

Our task was to write a Haskell function `g :: [String] -> String` that takes in
a list of "transparencies" and produces a decrypted string according to the
given rules:

> For example, given the strings `"ha  m"` and `"ck e"`:
> 
> If you overlay them:
> 
>     "ha  m" 
>     "ck e" 
> 
> Shift them by an appropriate offset:
> 
>     "ha  m" 
>       "ck e"
> 
> And combine them, you get `"hackme"`.
> 
> For the data we're working with, the following rules seem to always hold:
> 
> 1. The correct offset will never cause two characters to occupy the same column.
> 2. The correct offset will minimize the length of the final text after trimming
>    leading and trailing spaces.
> 3. If there are multiple possible decryptions that follow the above rules, the
>    lexicographically first one is correct.

However, the twist is that the source code must be less than 181 bytes! A very
nice code golf challenge.
The following modules have already been imported for use:

    Prelude
    Control.Applicative
    Control.Arrow
    Control.Monad
    Data.Array
    Data.Bits
    Data.Bool
    Data.Char
    Data.Complex
    Data.Dynamic
    Data.Either
    Data.Eq
    Data.Fixed
    Data.Function
    Data.Graph
    Data.Int
    Data.Ix
    Data.List
    Data.Maybe
    Data.Monoid
    Data.Ord
    Data.Ratio
    Data.Tree
    Data.Tuple
    Data.Typeable
    Data.Word
    Debug.SimpleReflect
    Text.Printf
    ShowFun

Code could be submitted to a judging server by base64 encoding it and `nc`ing
the encoded source to the server.

## First (failed) attempt

Our first attempt was to generate **all** possible offsets. That is, assign each
transparency with an offset in the range
`[0, ..., sum of lengths of transparencies]`, combine them together, then take the
best of the valid combinations.

Our initial code looked a bit like:

```haskell
-- Takes a list of non-spaces in a column and turns it into a character.
-- If there's no non-spaces, i.e. the column is all spaces, turn it into a space.
-- If there's only one non-space, turn it into that non-space character.
-- Otherwise, there's more than one non-space, so the column is invalid.
nonSpacesToChar :: [Char] -> Maybe Char
nonSpacesToChar [] = Just ' '
nonSpacesToChar [x] = Just x
nonSpacesToChar _ = Nothing

strip :: String -> String
strip = dropWhile (==' ') . dropWhileEnd (==' ')

leftPad :: String -> Int -> String
leftPad s n = (replicate n ' ') ++ s

compareFunc :: String -> (Int, String)
compareFunc x = (length x, x)

g :: [String] -> String
g transparencies = minimumBy (comparing compareFunc) (do
    let totalLength = sum $ map length transparencies
    -- Generate all combinations of padded strings.
    paddedStrings <- sequence $ map (\s -> map (leftPad s) [0..totalLength]) transparencies
    -- Turn them into a list of "columns" which has spaces and non-spaces.
    let columns = transpose paddedStrings
    -- Turn all columns into a Maybe Char and sequences them together.
    Just combined <- return $ sequence $ map (nonSpacesToChar . filter (/= ' ')) columns
    return $ strip combined
    )
```

We unfortunately don't have any version history before our first recorded golf
below, but here are a few optimisations we made prior to it:

- The Maybe monad can be simulated in the List monad, where `Just x` becomes
  `[x]` and `Nothing` becomes `[]`. This saves on writing out `Just` and
  `Nothing`, and saves a few more chars as we also use the List monad in `g`.
- Because `printf` is imported for us, we can rewrite `leftPad` as
  `flip (printf "%*s")`. This doesn't perfectly replicate our existing
  `leftPad` function, as it pads it *to* a length of `n` rather than padding it
  with `n` spaces, but the range of values we pad the strings with means our
  solution still exhaustively tries every possible offset.
- In-lining one use functions and `let` bindings saves a few characters.
- Rewriting functions to be point-free often saves a few characters here and
  there.
- `map` can be rewritten as `(<$>)`, saving a few characters of whitespace and
  allowing nicer operation sectioning.
- `sequence $ map` can be rewritten as `mapM`. Thanks,
  [Code Golf Stack Exchange][cgse]! The tips there are very helpful for golfing.

With these optimisations, we have our first recorded golf (**196/181**):

```haskell
d[]=" "
d[x]=[x]
d _=[]
g a=minimumBy(comparing(\x->(length x,x)))[dropWhile(==' ')$dropWhileEnd(==' ')$c|w<-mapM((<$>[0..sum$length<$>a]).flip(printf"%*s"))a,c<-mapM(d.filter(/=' '))$transpose w]
```

We decided to use a list comprehension as we were using the List monad anyway.
However, upon closer inspection, the list comprehension seems like a nice
monadic "pipeline". Replacing the list comprehension with a bind and a map
saves a few characters (**191/181**):

```haskell
d[]=" "
d[x]=[x]
d _=[]
g a=minimumBy(comparing(\x->(length x,x)))$dropWhile(==' ').dropWhileEnd(==' ')<$>(mapM(d.filter(/=' ')).transpose=<<mapM((<$>[0..sum$length<$>a]).flip(printf"%*s"))a)
```

The outer map, used for stripping the final string with spaces, can also be
replicated by stripping the *columns* before the `nonSpacesToChar` stage.
This does mean that the `filter(/=' ')` needed to be mapped instead of
going through the `mapM`, but allows us to remove the outer map. This doesn't
save any characters by itself, but cuts down the `dropWhile` predicate by one
character, netting us two characters saved (**189/181**):

```haskell
d[]=" "
d[x]=[x]
d _=[]
g a=minimumBy(comparing(\x->(length x,x)))$mapM d.dropWhile(=="").dropWhileEnd(=="").(filter(/=' ')<$>).transpose=<<mapM((<$>[0..sum$length<$>a]).flip(printf"%*s"))a
```

From there... we were stumped. Making `g` point-free didn't save any characters,
factoring out the `dropWhile` predicate didn't save any characters...

What if we assumed the input strings are nice? If the input strings don't have
any leading spaces, we don't need to strip leading spaces as all correct answers
must have one offset being 0. This got us down to **173/181**:

```haskell
d[]=" "
d[x]=[x]
d _=[]
g a=minimumBy(comparing(\x->(length x,x)))$mapM d.dropWhileEnd(=="").(filter(/=' ')<$>).transpose=<<mapM((<$>[0..sum$length<$>a]).flip(printf"%*s"))a
```

We submitted it... and the server didn't give us any response after a minute or
so.
Was our code too slow? We submitted the example solution
`g a = "This is probably not the right answer"`
and received "incorrect answer" response after a few seconds.

Considering how many operations the code performs (`totalLength^n`), it became
obvious that our first attempt was simply too slow to run, and we had to
tackle the problem in a non-brute force manner.

## Exfiltration

We received a "incorrect answer" after submitting the example solution.
Are there any other responses for solutions which are wrong?
Submitting `g = undefined` gives us a "runtime error" response... so we could
potentially exfiltrate information from the server by causing runtime errors
under certain conditions.

For example, submitting

```haskell
g a = if any ((==' ') . last) a then undefined else ""
```

gives us a runtime error, while

```haskell
g a = if any ((==' ') . head) a then undefined else ""
```

gives us an incorrect answer.

We have just determined that the transparencies in the first test case run by
the judge have no leading spaces, but do have trailing spaces.

As the judge could potentially run our code with multiple inputs - which would
be almost impossible to exfiltrate information from - we decided to stop there
and focus on a second attempt at the task.

## Second (failed?) attempt

"What could be a better, non-brute force way of solving this?"

One idea we had was a dynamic programming-like solution. In Python:

```python
from itertools import zip_longest
from typing import List, Optional

def merge(a: str, b: str) -> Optional[str]:
    out = ""
    for x, y in zip_longest(a, b, fillvalue=" "):
        if x == " ":
            out += y
        elif y == " ":
            out += x
        else:
            return None
    return out

def solve(current_output: str, remaining_strings: List[str]) -> str:
    if remaining_strings == []:
        return current_output
    candidates: List[str] = []
    # case 1: insert any string here, if possible
    # it is guaranteed that this loop body is executed at least once
    for s in remaining_strings:
        merged = merge(current_output, s)
        # if current_output == "", merged is guaranteed to be not None
        # therefore candidates is always non-empty by the end.
        if merged is None:
            continue
        remaining_without_s = list(remaining_strings)
        remaining_without_s.remove(s)
        candidates.append(solve(merged, remaining_without_s))
    # case 2: don't insert anything here, move onto the next character
    if current_output != "":
        head, *tail = current_output
        candidates.append(head + solve("".join(tail), remaining_strings))
    return min(candidates, key=lambda x: (len(x), x))

def g(a: List[str]) -> str:
    return solve("", list(map(str.strip, a)))
```

This solution keeps a running "current output" string in calculations, and takes
the minimum of inserting a string at the current location, or moving onto the
next character. Instead of merging all strings at once in the first attempt,
we instead merge two strings at a time.

The first draft for this golf was written on a phone:

```haskell
r ' ' c = [c]
r c ' ' = [c]
r _ _ = []
q=(++cycle" ")

-- maybe change q to be _?s=s++cycle" "

l=length
solve cur [] = cur
solve cur remaining = minimumBy (comparing(\x->(l x,x))) $ [solve w$delete d remaining|d <- remaining, w <- mapM id $ take (max (l d) $ l cur) $ zipWith r (q d) (q cur)]++[x:solve xs remaining|(x:xs)<-[cur]]
g = solve "".(dropWhile(==' ').dropWhileEnd(==' ')<$>)

-- maybe mapM id -> mapM r, zipWith r -> zip, r to (Char, Char) -> [Char]? use pattern guards? r(a,b)|' '==a=[b]|' '==b=[a]|1>0=[]
-- maybe change solve to be an infix symbol (!)?
```

Surprisingly, this code actually type-checks and worked just fine!
The main problem here is that unlike Python, Haskell does not have a
`zip_longest` function, so we had to emulate that by using `q` (which appends
an infinite number of spaces at the end of a string) and `take`.

By removing whitespace and renaming variables, we got **244/181**:

```haskell
r ' 'c=[c]
r c ' '=[c]
r _ _=[]
q=(++cycle" ")
l=length
s c[]=c
s c z=minimumBy(comparing(\x->(l x,x)))$[s w$delete d z|d<-z,w<-mapM id$take(max(l d)$l c)$zipWith r(q d)$q c]++[x:s y z|(x:y)<-[c]]
g=s"".(dropWhile(==' ').dropWhileEnd(==' ')<$>)
```

Going backwards through the comments in the draft, we changed `solve` or `s` to
be an infix symbol, cutting it down to **239/181**:

```haskell
r ' 'c=[c]
r c ' '=[c]
r _ _=[]
q=(++cycle" ")
l=length
c?[]=c
c?z=minimumBy(comparing(\x->(l x,x)))$[w?delete d z|d<-z,w<-mapM id$take(max(l d)$l c)$zipWith r(q d)$q c]++[x:y?z|(x:y)<-[c]]
g=(""?).(dropWhile(==' ').dropWhileEnd(==' ')<$>)
```

Another comment from the draft - using pattern guards - didn't end up saving us
any characters, but the
`mapM id -> mapM r, zipWith r -> zip, r to (Char, Char) -> [Char]` refactoring
did - cutting it down to **233/181**:

```haskell
r(' ',b)=[b]
r(a,' ')=[a]
r _=[]
q=(++cycle" ")
l=length
c?[]=c
c?z=minimumBy(comparing(\x->(l x,x)))$[w?delete d z|d<-z,w<-mapM r$take(max(l d)$l c)$zip(q d)$q c]++[x:y?z|(x:y)<-[c]]
g=(""?).(dropWhile(==' ').dropWhileEnd(==' ')<$>)
```

The first comment, changing `q` to be an infix operator, only ended up making
the code longer.

We don't seem like we're getting anywhere close to the target 181 characters.
What's especially annoying is the amount of code we have for doing the merge
operation...

What if we wrote our own `zip_longest` function from scratch? In fact, as we're
`mapM`'ing `r` onto the result of the `zipLongest`, we may as well write a
function `String -> String -> [String]` which does the zipLongest as well as
the "merge" operation that `r` does.

Writing this naively gets us a nice **221/181**:

```haskell
(' ':b)#(c:d)=(c:)<$>b#d
(a:b)#(' ':d)=(a:)<$>b#d
""#x=[x]
x#""=[x]
_#_=[]
c?[]=c
c?z=minimumBy(comparing(\x->(length x,x)))$[w?delete d z|d<-z,w<-c#d]++[x:y?z|(x:y)<-[c]]
g=(""?).(dropWhile(==' ').dropWhileEnd(==' ')<$>)
```

The list unpacking in `(#)` takes a few characters, so replacing that with a
guard gets us a **218/181**:

```haskell
(a:b)#(c:d)|a==' '=(c:)<$>b#d|c==' '=(a:)<$>b#d|1>0=[]
""#x=[x]
x#_=[x]
c?[]=c
c?z=minimumBy(comparing(\x->(length x,x)))$[w?delete d z|d<-z,w<-c#d]++[x:y?z|(x:y)<-[c]]
g=(""?).(dropWhile(==' ').dropWhileEnd(==' ')<$>)
```

We seem to be comparing against `' '` a lot, so let's factor that out for a
**212/181**:

```haskell
q=(==' ')
(a:b)#(c:d)|q a=(c:)<$>b#d|q c=(a:)<$>b#d|1>0=[]
""#x=[x]
x#_=[x]
c?[]=c
c?z=minimumBy(comparing(\x->(length x,x)))$[w?delete d z|d<-z,w<-c#d]++[x:y?z|(x:y)<-[c]]
g=(""?).(dropWhile q.dropWhileEnd q<$>)
```

Now that we have the `(#)` function, we can combine the two cases of `(?)`
(our solve function) together with a few observations!

The first case goes through every remaining string and sees which ones can be
merged with the current output. The only way for a string to be merged here is
if the current output starts with a space. If it started with a non-space,
no string can be merged as they will "conflict" at the first character - as all
input strings are stripped of spaces.
If a string can be merged, `(?)` is called again with the merged string and the
remaining strings. However, this recursive call will **never** use the first
case as the current output can never start with a space - as we just merged a
string into it. Therefore, the second case is always chosen after the first
case.

The second case advances the current output forward one character... so the
first case can be re-written as "merge a string and advance output forward".

If we made every case advance the current output forward one character, we can
combine the two cases together! As the `delete` function is a no-op if the given
needle is not in the haystack, and our `(#)` function is also a no-op if one of
the strings is empty, we can append on the empty string to the possible values
of `d` to simulate the second case in the first case. This gives us a nice
**198/181**:

```haskell
q=(==' ')
(a:b)#(c:d)|q a=(c:)<$>b#d|q c=(a:)<$>b#d|1>0=[]
""#x=[x]
x#_=[x]
c?[]=c
c?z=minimumBy(comparing(\x->(length x,x)))[x:p?delete d z|d<-"":z,x:p<-c#d]
g=(""?).(dropWhile q.dropWhileEnd q<$>)
```

We were a bit stuck here, so we took a look at the
[Code Golf Stack Exchange thread](cgse) for a bit of inspiration.
There's a [neat trick](iflist) when a conditional returns either an empty list
or another list - list comprehensions! This helps a lot with `(#)`, and is
doubly useful as we needed to map over the remaining `b#d` list anyway.
However, what do we put in the hole (`_`) here?

```haskell
(a:b)#(c:d)=[_:x|q a||q c,x<-b#d]
```

It needs to be either `a` or `c`, depending on which one is a space or not.
We're guaranteed that one of them *is* a space... so is there a nice way of
representing the hole? Recalling [another answer](spaceless) from the Code Golf
Stack Exchange thread, space is less than all printable ASCII!
Therefore, taking the `max` of `a` and `c` is what we need in that case.
This brings us down to **189/181**:

```haskell
q=(==' ')
(a:b)#(c:d)=[max a c:x|q a||q c,x<-b#d]
""#x=[x]
x#_=[x]
c?[]=c
c?z=minimumBy(comparing(\x->(length x,x)))[x:p?delete d z|d<-"":z,x:p<-c#d]
g=(""?).(dropWhile q.dropWhileEnd q<$>)
```

The use of `max` here gives us a bit of inspiration for the other branch of
`(#)`! As empty string is less than all other strings, we can rewrite the
other branches of `(#)` with a `max` as well - giving us **186/181**:

```haskell
q=(==' ')
(a:b)#(c:d)=[max a c:x|q a||q c,x<-b#d]
a#b=[max a b]
c?[]=c
c?z=minimumBy(comparing(\x->(length x,x)))[x:p?delete d z|d<-"":z,x:p<-c#d]
g=(""?).(dropWhile q.dropWhileEnd q<$>)
```

As space is less than all printable ASCII, checking whether a character is equal
to space may as well be the same as whether it's less than or equal to it.
That's the same as whether it's less than its successor - which is `'!'` -
saving one character in the process (**185/181**):

```haskell
q=(<'!')
(a:b)#(c:d)=[max a c:x|q a||q c,x<-b#d]
a#b=[max a b]
c?[]=c
c?z=minimumBy(comparing(\x->(length x,x)))[x:p?delete d z|d<-"":z,x:p<-c#d]
g=(""?).(dropWhile q.dropWhileEnd q<$>)
```

In the second branch of `(#)`, we realised that one of `a` and `b` must be the
empty string... so concatenating `a` and `b` gives us the same result as using
`max`. Whoops! That gets us a **182/181** - so close!

```haskell
q=(<'!')
(a:b)#(c:d)=[max a c:x|q a||q c,x<-b#d]
a#b=[a++b]
c?[]=c
c?z=minimumBy(comparing(\x->(length x,x)))[x:p?delete d z|d<-"":z,x:p<-c#d]
g=(""?).(dropWhile q.dropWhileEnd q<$>)
```

To finally get us down below the 181 character mark, we notice that the code
used for stripping the input strings doesn't need to be run until we actually
use the strings. In fact, we only need to strip the strings when merging!
That saves the two characters, bringing us down to ***179/181***!

```haskell
q=(<'!')
(a:b)#(c:d)=[max a c:x|q a||q c,x<-b#d]
a#b=[a++b]
c?[]=c
c?z=minimumBy(comparing(\x->(length x,x)))[x:p?delete d z|d<-"":z,x:p<-c#(dropWhile q$dropWhileEnd q$d)]
g=(""?)
```

We submitted this solution... and got back an "incorrect answer" response.

## Debugging the second attempt

We wrote a few hard test cases to ensure that we've covered everything the
task asks for:

```haskell
examples = [
    (["ha  m", "ck e"], "hackme"),
    (["    b    d  ", " ce  "], "b  ced"),
    (["   a c ", " b d "], "abcd"),
    ([" a ", "   bsdf sadsd  "], "bsdfasadsd"),
    ([" b    d ", "    a ", "  c"], "b  acd"),
    ([" a c e  ", "d f  ", " b"], "abcdef")
    ]

-- should be empty
bad = [(c, a, g c) | (c,a)<-examples,g c /= a]
```

Our solution passed all of those test cases.

We thought about inputs which could break our solution. If the input has a
string which is entirely spaces, the solution *crashes* when it tries to run
`""?[" "]`. The merge operation will always return an empty string, and as we
pattern match on a non-empty string, the list comprehension in `(?)` is empty.
As `minimumBy` expects a non-empty list, this crashes the solution.

The verdict we got back from the judge was "incorrect answer", not
"runtime error" - so that's not the problem here.

We thought about any flaws in our logic, but we convinced ourselves that the
logic is fine through an inductive sketch.

We were stumped.

## Third (failed) attempt

Our morale was low, but we couldn't give up just yet. We thought of another way
of solving it, similar to our first brute-force approach but recursive.

Given two strings `"ha  m"` and `"ck e"`, we can "place" the second one in any
of these places to form a valid solution:

        ha  m
    ck e
     ck e
      ck e
         ...
           ck e
            ck e
             ck e   

Therefore, given two strings `s` and `a`, we can offset `a` beneath `s` in
anywhere in the range of `[-length a, ..., length s]`. Let `o` be this offset -
if `o` is negative, we left pad `s` by `-o` spaces, if `o` is positive we left
pad `a` by `o` spaces.

By repeating this process for all strings, starting off with an empty string,
we can enumerate all possible interleavings. We can then take the minimum of
all of them. This should be much faster than our first attempt, as this
solution only considers *valid* interleavings, instead of possibly invalid
interleavings too.

Reusing some of our code from our second attempt, we managed to get a solution
down to **221/181**:

```haskell
q=(<'!')
l=length
a¡b=([1..a]>>" ")++b
(a:b)#(c:d)=[max a c:x|q a||q c,x<-b#d]
a#b=[a++b]
s?(a:b)=[-l a..l s]>>=(\o->((-o)¡s)#(o¡a))>>=(?b)
s?_=[dropWhile q$dropWhileEnd q$s]
g=minimumBy(comparing(\x->(length x,x))).(""?)
```

We realised it was probably too hard to cut the solution down to size, and
gave up there.

## Postmortem

After the CTF concluded, we found only one other writeup of this problem by
@ldruschk, which you can find [here](ldruschkwriteup). One thing which surprised
us was the following quote:

> However, as it turned out the checker service does not actually supply strings
> with leading or trailing spaces, so we can throw the whole stripping logic
> away...

We previously exfiltrated the fact that at least one input has a trailing space!
Something seems a bit fishy here...

@ldruschk mentioned that their solution had a few flaws and does not cover all
edge cases, but managed to obtain the flag for this task,
`CTF{since-brevity-is-the-soul-of-wit-I-will-be-brief}`.
@ldruschk also has a "fixed" version of their code which covers the edge cases,
but they could not submit it as it was 5 characters over the limit.

Under close inspection, @ldruschk seems to use a similar function to our `(#)`
which we've golfed down quite a bit. Using a few of our own observations, we
managed to golf @ldruschk "fixed" version down to something submittable
(**168/181**):

```haskell
(x:a)#(y:b)=max x y:a#b
x#y=x++y
u=reverse.dropWhile(<'!')
g=head.sortOn(0<$).sort.map(foldl(\r->(#r).until(and.zipWith(((<'!').).min)r)(' ':))"").permutations.map(u.u)
```

We submitted this solution... and got back an "incorrect answer" response.
Something definitely seems fishy here - @ldruschk claims that the code *fixed*
some edge cases, not introduced them!

Comparing it to our golfed version of @ldruschk's "flawed" code (**132/181**)...

```haskell
(x:a)#(y:b)=max x y:a#b
x#y=x++y
g=head.sortOn(0<$).sort.map(foldl1(\r->(#r).until(and.zipWith(((<'!').).min)r)(' ':))).permutations
```

...we see that the only difference is the removal of `.map(u.u)` - the
stripping of the input strings - and the `foldl1` instead of the `foldl` which
fails to deal with the empty list case.

We submitted this solution as well, to make sure our modifications did not
change anything important... and got the flag.

As an experiment, we decided to remove our stripping code from our second
attempt (**161/181**):

```haskell
q=(<'!')
(a:b)#(c:d)=[max a c:x|q a||q c,x<-b#d]
a#b=[a++b]
c?[]=c
c?z=minimumBy(comparing(\x->(length x,x)))[x:p?delete d z|d<-"":z,x:p<-c#d]
g=(""?)
```

We submitted this solution... **and got the flag**.

Did we misread the task wrong? It mentioned:

> The correct offset will minimize the length of the final text after trimming
> leading and trailing spaces.

What if we only trim leading and trailing spaces when calculating the length of
the strings, and not trim spaces of the strings themselves? We need to
use @ldruschk's solution for this, as our solution cannot handle minimising
the length of trimmed strings (**164/181**):

```haskell
(x:a)#(y:b)=max x y:a#b
x#y=x++y
u=reverse.dropWhile(<'!')
g=head.sortOn((0<$).u.u).sort.map(foldl1(\r->(#r).until(and.zipWith(((<'!').).min)r)(' ':))).permutations
```

The judge returns "incorrect output" for this solution. Therefore, the task
description is definitely misleading - solutions should *never* trim leading and
trailing spaces.

### Exfiltration with a working solution

The judge **does** judge on more than one input. Changing `g` to be
`g a=[x|length a<=4,x<-""?a]` on our working **161/181** solution returns the
flag, `g a=[x|length a<=3,x<-""?a]` gives an incorrect answer, and
`g a=[x|length a==4,x<-""?a]` gives an incorrect answer too. Therefore, all
inputs have length less than 4, there is at least one input with length 4,
and there is at least one input with length not equal to 4.

### Further improvements to our solution

Using some observations from @ldruschk's writeup, we can get our (presumably 
working) **179/181** solution down smaller. Instead of
`minimumBy(comparing(\x->(length x,x)))`, using the head of a sorted list is
much shorter, shortening our solution down to **162/181**:

```haskell
q=(<'!')
(a:b)#(c:d)=[max a c:x|q a||q c,x<-b#d]
a#b=[a++b]
c?[]=c
c?z=head$sortOn(0<$)$sort[x:p?delete d z|d<-"":z,x:p<-c#(dropWhile q$dropWhileEnd q$d)]
g=(""?)
```

Additionally, their "string trim" function is a bit shorter, saving a character
(**161/181**):

```haskell
q=(<'!')
u=reverse.dropWhile q
(a:b)#(c:d)=[max a c:x|q a||q c,x<-b#d]
a#b=[a++b]
c?[]=c
c?z=head$sortOn(0<$)$sort[x:p?delete d z|d<-"":z,x:p<-c#(u$u$d)]
g=(""?)
```

As `q` is not used as often now, we can remove the definition of `q` and do a
bit of tweaking to get it down to **159/181**:

```haskell
u=reverse.dropWhile(<'!')
(a:b)#(c:d)=[max a c:x|min a c<'!',x<-b#d]
a#b=[a++b]
c?[]=c
c?z=head$sortOn(0<$)$sort[x:p?delete d z|d<-"":z,x:p<-c#(u$u$d)]
g=(""?)
```

With these character savings, we can fix the crash mentioned in "Debugging the
second attempt" and still be inside the character limit with **174/181**:

```haskell
u=reverse.dropWhile(<'!')
(a:b)#(c:d)=[max a c:x|min a c<'!',x<-b#d]
a#b=[a++b]
c?[]=c
c?z=head$sortOn(0<$)$sort[x:p?delete d z|d<-"":z,x:p<-c#d]
g=(""?).filter(>"").(u.u<$>)
```

Unsurprisingly, this code still fails to pass the judge.

[cgse]: https://codegolf.stackexchange.com/questions/19255/tips-for-golfing-in-haskell
[iflist]: https://codegolf.stackexchange.com/a/150792
[spaceless]: https://codegolf.stackexchange.com/a/52942
[ldruschkwriteup]: https://github.com/ldruschk/ctf-writeups/blob/master/2019_googlectf_code_golf.md
