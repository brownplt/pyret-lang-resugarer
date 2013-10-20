#lang scribble/manual

@(require
  racket/list
  racket/file
  "common.rkt")

@title[#:tag "s:forms"]{Language Constructs}

@section[#:tag "s:program"]{Programs}

Programs consist of a sequence of import or provide statements, followed by a
block:

@justcode{
program: prelude block
prelude: (import-stmt|provide-stmt)*
block: stmt*
}

@section{Import Statements}

@margin-note{Import and provide statements aren't available in Captain Teach;
the page is responsible for providing all the needed libraries.}

Import statements come in two forms:

@justcode{
import-stmt: "import" (import-name | import-string) "as" NAME
import-name: NAME
import-string: STRING
}

Both forms bind the value provided by the target (either @prod{import-name} or
@prod{import-string}) to the @prod{NAME} after @prod{as}.

The form with @prod{STRING} as a target transforms the import into a Racket require
statement, using the string as the module path.  For example, given this
@in-code{"m.arr"}:

@justcode{
  provide m end
  m = 22
}

Another file in the same directory could use it:

@justcode{
  import "m.arr" as m
  print(m) # prints 22
}

It is an error if the file does not exist, or does not have a provide
statement.

The form that uses a @prod{NAME} production looks for a file with that name in the
built-in libraries of Pyret.  These are currently found in the @in-code{lang/racket-ffi/}
directory of Pyret, and are maintained by the Pyret authors.

Example:

@justcode{
  import io as IO
  IO.read-line()
}

It is an error if there is no such named file in @in-code{lang/racket-ffi/}, or if the
file does not provide an identifier named @in-code{%PYRET-PROVIDE}.

@section{Provide Statements}

A provide statement comes in one of two forms:

@justcode{
provide-stmt: "provide" stmt "end" | "provide" "*"
}

Both forms have no effect when the program is run as the top-level program
(e.g. in a Captain Teach editor, DrRacket buffer, or as the target of @in-code{raco
pyret}).

When the program is in a file that is evaluated via @tt{import}, the program is
run, and then the @tt{provide} statement is run in top-level scope to determine
the value bound to the identifier in the import statement.

In the first form, the @tt{stmt} internal to the provide is evaluated, and the
resulting value is provided.

The second form is syntactic sugar for:

@justcode{
provide {
  id: id,
  ...
} end
}


@section{Blocks}

A block's syntax is a list of statements:

@justcode{
block: stmt*
}

Blocks serve two roles in Pyret:

@itemlist[
  @item{Sequencing of operations}
  @item{Units of lexical scope}
]

The @tt{let-expr}, @tt{fun-expr}, @tt{data-expr}, and @tt{var-expr} forms are
handled specially and non-locally within blocks.  They all define names that
are in scope for all expressions in the block.

Example:

@justcode{
  fun f(): x end # x is defined when f evaluates
  x = 12
  f() # evaluates to 12
}

Example:

@justcode{
  data Node:
    | node(in :: Edge, out :: Edge) # Edge is visible
  end

  data Edge:
    | edge(weight :: Number)
  end
}

There is one gotcha, which is that if identifiers are evaluated before they are
defined, as opposed to just being closed over, Pyret throws an exception:

@justcode{
  x = y
  y = 9
}

(Note: Right now this program runs but it shouldn't.  The value of @tt{x} is
@tt{#<undefined>}, which is an issue with a designed but un-implemented fix.)

The sections on the individual statements describe which names they introduce
into the scope.

Blocks evaluate each of their statements in order, and evaluate to the value of
the final statement in the block.

@section{Statements}

There are a number of forms that can only appear as statements in @tt{block}s
and @tt{provide} expressions:

@justcode{
stmt: let-expr | fun-expr | data-expr | when-expr
    | var-expr | assign-expr | binop-expr
}

@subsection[#:tag "s:let-expr"]{Let Expressions}

Let expressions are written with an equals sign:

@justcode{
let-expr: binding "=" binop-expr
}

A let statement causes the name in the @tt{binding} to be put in scope in the
current block, and upon evaluation sets the value to be the result of
evaluating the @tt{binop-expr}.  The resulting binding cannot be changed via an
@tt{assign-expr}, and cannot be shadowed by other bindings within the same or
nested scopes:

@justcode{
x = 5
x := 10
# Error: x is not assignable

}

@justcode{
x = 5
x = 10
# Error: x defined twice

}

@justcode{
x = 5
fun f():
  x = 10
end
# Error: can't use the name x in two nested scopes

}

@justcode{
fun f():
  x = 10
end
fun g():
  x = 22
end
# Not an error: x is used in two scopes that are not nested
}

@subsection[#:tag "s:graph-expr"]{Graph Declarations}

Graph declarations look like a series of let statements:

@justcode{
graph-expr: "graph:" let-expr* "end"
}

They behave like let statements, binding variables in the block in which they
appear.  However, they allow for the creation of mutual references between
data.  For example:

@justcode{
check:
  graph:
    BOS = [PVD, WOR]
    WOR = [BOS]
    PVD = [BOS]
  end
  BOS.first is PVD
  BOS.rest.first is WOR
  WOR.first is BOS
  PVD.first is BOS
end
}

If we wrote this with a let expression, we would run in the gotcha about using
an identifier before we defined it.  But @tt{graph:} keeps track of these
mutual references and causes the right relationships to hold at the end of the
@tt{graph:} block.  Fields that use a graph identifier need to be declared as
@tt{cyclic} in their data declaration, otherwise an error results.


@subsection[#:tag "s:fun-expr"]{Function Declaration Expressions}

Function declarations have a number of pieces:

@justcode{
fun-expr: "fun" fun-header ":" doc-string block where-clause "end"
fun-header: ty-params NAME args return-ann
ty-params:
  ["<" list-ty-param* NAME ">"]
list-ty-param: NAME ","
args: (PARENSPACE|PARENNOSPACE) [list-arg-elt* binding] ")"
list-arg-elt: binding ","
return-ann: ["->" ann]
doc-string: ["doc:" STRING]
where-clause: ["where:" block]
}

A function expression is syntactic sugar for a let and an anonymous function
expression.  The statement:

@justcode{
"fun" ty-params NAME args return-ann ":"
  doc-string
  block
  where-clause
"end"
}

Is equivalent to

@justcode{
NAME "=" "fun" ty-params args return-ann ":"
  doc-string
  block
"end"
}

With the @tt{where-clause} registered in @seclink["s:check/where" "check
mode"].  Concretely:

@justcode{
fun f(x, y):
  x + y
end
}

is equivalent to

@justcode{
f = fun(x, y):
  x + y
end
}

See the documentation for @tt{lambda-exprs} for an explanation of arguments'
and annotations' behavior, as well as @tt{doc-strings}.

@subsection[#:tag "s:data-expr"]{Data Declarations}

Data declarations define a number of related functions for creating and
manipulating a data type.  Their grammar is:

@justcode{
data-expr: "data" NAME ty-params data-mixins ":"
    data-variant*
    data-sharing
    where-clause
  "end"
data-mixins: ["deriving" mixins]
data-variant: "|" NAME variant-members data-with | "|" NAME data-with
variant-members: (PARENSPACE|PARENNOSPACE) [list-variant-member* variant-member] ")"
list-variant-member: variant-member ","
variant-member: ["mutable"|"cyclic"] binding
data-with: ["with:" fields]
data-sharing: ["sharing:" fields]
}

A @tt{data-expr} causes a number of new names to be bound in the scope of the
block it is defined in:

@itemlist[
  @item{The @tt{NAME} of the data definition}
  @item{@tt{NAME}, for each variant of the data definition}
  @item{@tt{is-NAME}, for each variant of the data definition}
]

For example, in this data definition:

@justcode{
data BTree:
  | node(value :: Number, left :: BTree, right :: BTree)
  | leaf(value :: Number)
end
}

These names are defined, with the given types:

@justcode{
BTree :: (Any -> Bool)
node :: (Number, BTree, BTree -> BTree)
is-node :: (Any -> Bool)
leaf :: (Number -> BTree)
is-leaf :: (Any -> Bool)
}

We call @tt{node} and @tt{leaf} the @emph{constructors} of @tt{BTree}, and they
construct values with the named fields.  They will refuse to create the value
if fields that don't match the annotations are given.  As with all annotations,
they are optional.  The constructed values can have their fields accessed with
@seclink["s:dot-expr" "dot expressions"] and @seclink["s:colon-expr" "colon
expressions"].

The function @tt{BTree} is a @emph{detector} for values created from this data
definition, and can be used as an annotation to check for values created by the
constructors of @tt{BTree}.  @tt{BTree} returns true when provided values
created by @tt{node} or @tt{leaf}, but no others.

The functions @tt{is-node} and @tt{is-leaf} are detectors for the values
created by the individual constructors: @tt{is-node} will only return @tt{true}
for values created by calling @tt{node}, and correspondingly for @tt{leaf}.

Here is a longer example of the behavior of detectors, field access, and
constructors:

@justcode{
data BTree:
  | node(value :: Number, left :: BTree, right :: BTree)
  | leaf(value :: Number)
where:
  a-btree = node(1, leaf(2), node(3, leaf(4), leaf(5)))

  BTree(a-btree) is true
  BTree("not-a-tree") is false
  BTree(leaf(5)) is false
  is-leaf(leaf(5)) is true
  is-leaf(a-btree) is false
  is-leaf("not-a-tree") is false
  is-node(leaf(5)) is false
  is-node(a-btree) is true
  is-node("not-a-tree") is false

  a-btree.value is 1
  a-btree.left.value is 2
  a-btree.right.value is 3
  a-btree.right.left.value is 4
  a-btree.right.right.value is 4

end
}

A data definition can also define, for each instance as well as for the data
definition as a whole, a set of methods.  This is done with the keywords
@tt{with:} and @tt{sharing:}.  Methods defined on a variant via @tt{with:} will
only be defined for instances of that variant, while methods defined on the
union of all the variants with @tt{sharing:} are defined on all instances.  For
example:

@justcode{
data BTree:
  | node(value :: Number, left :: BTree, right :: BTree) with:
    size(self): 1 + self.left.size() + self.right.size() end
  | leaf(value :: Number) with:
    size(self): 1 end,
    increment(self): leaf(self.value + 1) end
sharing:
  values-equal(self, other):
    self.value == other.value
  end
where:
  a-btree = node(1, leaf(2), node(3, leaf(4)))
  a-btree.values-equal(leaf(1)) is true
  leaf(1).values-equal(a-btree) is true
  a-btree.size() is 3
  leaf(0).size() is 1
  leaf(1).increment() is leaf(2)
  a-btree.increment() # raises error: field increment not found.
end
}

A data definition also sets up some special methods that are used by other
constructs in the language.  Most of the time, you shouldn't need to call these
directly, but they are present on each instance:

@itemlist[
  @item{@tt{tostring} is a method that produces a string
        representation of the value}

  @item{@tt{_torepr} is a method that produces a string that represents the
  value in ``constructor form''.  This is distinct from @tt{tostring} in that,
  for example, the @tt{tostring} of @tt{"a-str"} is the string value
  @tt{"a-str"}, but the @tt{_torepr} is @tt{"\"a-str\""}.  This produces more
  meaningful REPL output, among other things.}
 
  @item{@tt{_equals} is a method used to check equality with other values.  It
  is called implicitly by the @tt{==} operator.}

  @item{@tt{_match} is a method that is used by @seclink["s:cases-expr" "cases
  expressions"].}

]

@;{ TODO: singleton variants and mixins }

@(subsection-title (emph "Mutable and Cyclic Fields") "s:mutable-data") 

@margin-note{In the future, we plan to support both @tt{cyclic} and
@tt{mutable} on the same field.  For now, only one is allowed.}

Fields of variants can be optionally be defined with one of the special
modifiers @tt{cyclic} or @tt{mutable}.  These allow for different kinds of
stateful update of those fields.

In the variant constructor, a field marked @tt{mutable} is wrapped in a
@seclink["s:mutables" @tt{Mutable}] container.  The @tt{Mutable} container is
initialized with both read and write guards for the annotation on the variant
member, if one is present.  That field can be subsequently updated with
@seclink["s:update-expr" "update expressions"] and accessed with
@seclink["s:get-bang-expr" "mutable lookup"].

Example:

@justcode{
data Node:
  | node(mutable in :: List<Node>)
where:
  n1 = node([])
  n2 = node([n1])
  n1!{in : [n2]}
  n1!in.first is n2
  n2!in.first is n1
end
}

Accessing a @tt{mutable} field with a @seclink["s:dot-expr" "dot expression"]
will result in an error.

Cyclic fields allow for mutually-referential data to be created without
allowing it to be mutable for its entire lifetime.  Variant constructors with
@tt{cyclic} fields do no extra work when handed most values.  However, if a
@seclink["s:placeholders" @tt{Placeholder}] value is passed for a @tt{cyclic}
field, it is @emph{guarded} with the annotation on the field (if any).  Most
Pyret programs should not construct @tt{Placeholders} directly, but should
instead use the @seclink["s:graph-expr" @tt{graph}] form, which creates
@tt{Placeholders} and sets up mutual references between @tt{cyclic} fields.

Example:

@justcode{
data Node:
  | node(cyclic in :: Node)
where:
  graph:
  n1 = node(n2)
  n2 = node(n1)
  end

  n1.in is n2
  n2.in is n1
end
}

@margin-note{Pedantically, @tt{cyclic} and @tt{mutable} fields in Pyret only
have @emph{intensional} equality, while objects with no mutable fields enjoy
@emph{extensional} equality.  With more sophisticated equality algorithms, it
is meaningful to provide extensional equality for @tt{cyclic} fields.  It is
unclear if extensional equality is meaningful for @tt{mutable} fields, because
of temporal concerns.}

@bold{A note on equality.}  Both @tt{cyclic} and @tt{mutable} allow the
creation of objects that create cyclic data structures.  This is problematic
from the point of view of equality for two reasons:

@itemlist[

@item{If a naïve structural equality algorithm compared two such objects, it
might not terminate.  A more sophisticated graph-isomorphism algorithm quickly
becomes computationally complex.}

@item{For @tt{mutable} data, an equality algorithm that compares the contents
of two @tt{Mutable} containers could return @tt{true} at one point in time and
@tt{false} at another.}

]

For these reasons, equality on @tt{mutable} and @tt{cyclic} fields only
succeeds if the @tt{Mutable} or @tt{Placeholder} values are @emph{the same
value}, rather than checking the equality of their contents.  This has the
benefit of being efficient, and supporting object-identity style equality in
one case where it is often relevant: graph algorithms.

@subsection[#:tag "s:var-expr"]{Variable Declarations}

Variable declarations look like @seclink["s:let-expr" "let bindings"], but
with an extra @tt{var} keyword in the beginning:

@justcode{
var-expr: "var" binding "=" expr
}

A @tt{var} expression creates a new @emph{assignable variable} in the current
scope, initialized to the value of the expression on the right of the @tt{=}.
It can be accessed simply by using the variable name, which will always
evaluate to the last-assigned value of the variable.  @seclink["s:assign-expr"
"Assignment statements"] can be used to update the value stored in an
assignable variable.

If the @tt{binding} contains an annotation, the initial value is checked
against the annotation, and all @seclink["s:assign-expr" "assignment
statements"] to the variable check the annotation on the new value before
updating.

@subsection[#:tag "s:assign-expr"]{Assignment Statements}

Assignment statements have a name on the left, and an expression on the right
of @tt{:=}:

@justcode{
assign-expr: NAME ":=" binop-expr
}

If @tt{NAME} is not declared in the same or an outer scope of the assignment
expression with a @tt{var} declaration, the program fails with a static error.

At runtime, an assignment expression changes the value of the assignable
variable @tt{NAME} to the result of the right-hand side expression.

@section{Expressions}

@subsection[#:tag "s:get-bang-expr"]{Mutable Lookup Expressions}

A mutable dot expression looks like a @seclink["s:dot-expr" "dot expression"],
but with the dot replaced with an exclamation point (pronounced ``bang''):

@justcode{
get-bang-expr: expr "!" NAME
}

A mutable dot expression first evaluates the @tt{expr} on the left to an
object, and then looks to see if @tt{NAME} is present in the fields of that
object.  If it is, and if the value of the field is a @seclink["s:mutables"
@tt{Mutable}], the expression evaluates to the value currently in that
@tt{Mutable} (after annotation checking).  If the value is not a @tt{Mutable}
or is not present, an exception is signalled.

@subsection[#:tag "s:update-expr"]{Update Expressions}

An update expression looks like a @seclink["s:extend-expr" "object extension"]
expression, but with the dot replaced with an exclamation point:

@justcode{
update-expr: expr "!" "{" fields "}"
}

An update expression first evaluates the expression to the left of the @tt{!}.
It then evaluates @emph{all} of the expressions in @tt{fields}, to produce a
list of field names mapping to values.  It then, in order from left to right,
checks that each name is present on the object, and is a @seclink["s:mutables"
@tt{Mutable}] value.  If any value is not present or not a @tt{Mutable}, an
exception is signalled.  If @emph{all} values are @tt{Mutable}, they are all
set to the corresponding new values in the field list.  If the name-to-value
mapping appears more than once in the list, the right-most value ``wins,'' and
is the resulting value in the @tt{Mutable} after the update.

Each update also checks the annotations for the field (if any were provided),
and signals an exception if any were violated.  @seclink["s:mutables"
@tt{Mutables}] has some additional technical details on this annotation
checking.

@subsection[#:tag "s:lam-expr"]{Lambda Expressions}

The grammar for a lambda expression is:

@justcode{
lambda-expr: "fun" ty-params [args] return-ann ":"
    doc-string
    block
    where-clause
  "end"
fun-header: ty-params NAME args return-ann
ty-params:
  ["<" list-ty-param* NAME ">"]
list-ty-param: NAME ","
args: (PARENSPACE|PARENNOSPACE) [list-arg-elt* binding] ")"
list-arg-elt: binding ","
return-ann: ["->" ann]
doc-string: ["doc:" STRING]
}

@margin-note{
The @tt{ty-params} and @tt{where-clause} of lambda expressions are currently not
interpreted by Pyret.  The @tt{ty-params} will be used when Pyret has more
complete support for checking polymorphic functions.  The @tt{where-clause} is
included for homogeneity with @seclink["s:fun-expr" "function statements"]. 
}

A lambda expression creates a function value that can be applied with
@seclink["s:app-expr" "application expressions"].  The arguments in @tt{args}
are bound to their arguments as immutable identifiers as in a
@seclink["s:let-expr" "let expression"].  These identifiers follow the same
rules of no shadowing and no assignment.

If the arguments have @seclink["s:annotations" "annotations"] associated with
them, they are checked before the body of the function starts evaluating, in
order from left to right.  If an annotation fails, an exception is thrown.

@justcode{
add1 = fun(x :: Number):
  x + 1
end
add1("not-a-number")
# Error: expected a Number and got "not-a-number"
}

Functions values are created with a @tt{"_doc"} field which holds the string
value of the @tt{doc-string} written in the function expression.  So:

@justcode{
documented = fun():
  doc: "Evaluates to a standards-compliant random number"
  4
end

check:
  documented._doc is "Evaluates to a standards-compliant random number"
end
}

@subsection[#:tag "s:app-expr"]{Application Expressions}

Function application expressions have the following grammar:

@justcode{
app-expr: expr app-args
app-args: PARENNOSPACE [app-arg-elt* binop-expr] ")"
app-arg-elt: binop-expr ","
}

An application expression is an expression (usually expected to evaluate to a
function), followed by a comma-separated list of arguments enclosed in
parentheses.  It first evaluates the arguments in left-to-right order, then
evaluates the function position.  If the function position is a function value,
the number of provided arguments is checked against the number of arguments
that the function expects.  If they match, the arguments names are bound to the
provided values.  If they don't, an exception is thrown.

Note that there is @emph{no space} allowed before the opening parenthesis of
the application.  If you make a mistake, Pyret will complain:

@justcode{
f(1) # This is the function application expression f(1)
f (1) # This is the id-expr f, followed by the paren-expr (1)
# The second form yields a well-formedness error that there
# are two expressions on the same line
}

@subsection[#:tag "s:left-apply-expr"]{Caret Application Expressions}

@margin-note{
The grammar of @tt{left-app-fun-expr} is restricted to avoid confusing
constructions, like:

@justcode{
obj^f(1)(2)
}

in which it would be unclear if the function to call is @tt{f} or @tt{f(1)}.}
An application can also be written with a caret symbol @tt{^}:

@justcode{
left-app-expr: expr "^" left-app-fun-expr app-args
left-app-fun-expr: id-expr | id-expr "." NAME
}

This is merely syntactic sugar for putting the initial @tt{expr} as the first
argument of an application to the @tt{left-app-fun-expr}.  These are equivalent:

@justcode{
obj^f(1, 2, 3)
}

@justcode{
f(obj, 1, 2, 3)
}

This allows for patterns like method-chaining on values that do not have
methods defined.  For example, one could write a function @tt{add-each} that
adds to each element of a list, and another function @tt{square} that
squares them, and chain them linearly:

@justcode{
check:
  [1,2,3]^inc(1)^square() is [4, 9, 16]
end
}

@subsection[#:tag "s:curried-apply-expr"]{Curried Application Expressions}

Suppose a function is defined with multiple arguments:

@justcode{
fun f(v, w, x, y, z): ... end
}

Sometimes, it is particularly convenient to define a new function that
calls @tt{f} with some arguments pre-specified:

@justcode{
call-f-with-123 = fun(y, z): f(1, 2, 3, y, z) end
}

Pyret provides syntactic sugar to make writing such helper functions
easier:

@justcode{
call-f-with-123 = f(1, 2, 3, _, _) # same as the fun expression above
}

Specifically, when Pyret code contains a function application some of
whose arguments are underscores, it constructs an anonymous function
with the same number of arguments as there were underscores in the
original expression, whose body is simply the original function
application, with the underscores replaced by the names of the
arguments to the anonymous function.

This syntactic sugar also works with
@seclink["s:left-apply-expr" "caret application expressions"], and
with operators.  For example, the following are two ways to sum a list
of numbers:

@justcode{
[1, 2, 3, 4].foldl(fun(a, b): a + b end, 0)

[1, 2, 3, 4].foldl(_ + _, 0)
}

Likewise, the following are two ways to compare two lists for
equality:

@justcode{
list.map_2(fun(x, y): x == y end, first-list, second-list)

list.map_2(_ == _, first-list, second-list)
}

Note that there are some limitations to this syntactic sugar.  You
cannot use it with the @tt{is} or @tt{raises} expressions in
@seclink["s:checkers" "check: blocks"], since both test expressions and expected
outcomes are known when writing tests.  Also, note that the sugar is
applied only to one function application at a time.  As a result, the
following code:

@justcode{
_ + _ + _
}

desugars to

@justcode{
fun(z):
  (fun (x, y): x + y end) + z
end
}

which is probably not what was intended.  You can still write the
intended expression manually:

@justcode{
fun(x, y, z): x + y + z end
}

Pyret just does not provide syntactic sugar to help in this case
(or other more complicated ones).

@subsection[#:tag "s:obj-expr"]{Object Expressions}

Object expressions map field names to values:

@justcode{
obj-expr: "{" fields "}" | "{" "}"
fields: list-field* field [","]
list-field: field ","
field: key ":" binop-expr
     | key args return-ann ":" doc-string block where-clause "end"
key: NAME | "[" binop-expr "]"
}

@margin-note{The ability to define fields as computed strings using
@tt{["brack" + "ets"]} is deprecated, and will be replaced in the future by
other reflective operations on objects and dictionaries.}

A comma-separated sequence of fields enclosed in @tt{{}} creates an object; we
refer to the expression as an @emph{object literal}.  There are two types of
fields: @emph{data} fields and @emph{method} fields.  A data field in an object
literal simply creates a field with that name on the resulting object, with its
value equal to the right-hand side of the field.  A method field

@justcode{
key args return-ann ":" doc-string block where-clause "end"
}

is syntactic sugar for:

@justcode{
key ":" "method" args return-ann ":" doc-string block where-clause "end"
}

That is, it's just special syntax for a data field that contains a method
value.

@margin-note{The overriding of later fields is expected to be deprecated and
replaced with an error.}

The fields are evaluated in order.  If the same field appears more than once,
the later use overrides the earlier use, but both field expressions are still
evaluated.

@subsection[#:tag "s:list-expr"]{List Expressions}

A list expression is a sequence of comma-separated expressions enclosed in
@tt{[]}:

@justcode{
list-elt: binop-expr ","
list-expr: "[" [list-elt* binop-expr] "]"
}

An empty list literal @tt{[]} is syntactic sugar for @tt{list.empty}.

A list with elements in it is transformed into a sequence of nested calls to
@tt{list.link}, ending in @tt{list.empty}.  For example:

@justcode{
[1,2,3]
}

becomes

@justcode{
list.link(1, list.link(2, list.link(3, list.empty)))
}

See the documentation for @seclink["s:lists" "lists"] for more information on
the values created by @tt{list.link} and @tt{list.empty}.

@subsection[#:tag "s:dot-expr"]{Dot Expressions}

A dot expression is any expression, followed by a dot and name:

@justcode{
dot-expr: expr "." NAME
}

A dot expression evaluates the @tt{expr} to a value @tt{val}, and then does one
of five things:

@itemlist[
  @item{Raises an exception, if @tt{NAME} is not a field of @tt{expr}}

  @item{Evaluates to the value stored in @tt{NAME}, if @tt{NAME} is present and
  not a method, mutable, or placeholder value}

  @item{Raises an exception, if the value stored in @tt{NAME} is a @tt{mutable} field.}

  @item{Evaluates to the value stored in the placeholder stored in @tt{NAME},
  if the value is a placeholder value.}

  @item{
  
    If the @tt{NAME} field is a method value, evaluates to a function that is
    the @emph{method binding} of the method value to @tt{val}.  For a method 
    
    @justcode{
      m = method(self, x): body end
    }

    The @emph{method binding} of @tt{m} to a value @tt{v} is equivalent to:

    @justcode{
      (fun(self): fun(x): body end end)(v)
    }

    What this detail means is that you can look up a method and it
    automatically closes over the value on the left-hand side of the dot.  This
    bound method can be freely used as a function.

    For example:

    @justcode{
      o = { m(self, x): self.y + x end, y: 22 }
      check:
        the-m-method-closed-over-o = o.m
        m(5) is 27
      end
    }
  }
]

@subsection[#:tag "s:colon-expr"]{Colon Expressions}

The colon expression is like the dot expression, but does not perform method
binding.  It is written as a dot expression, but with @tt{:} rather than @tt{.}.

@justcode{
colon-expr: expr ":" NAME
}

A colon expression evaluates @tt{expr} and then evaluates to the @tt{NAME}
value from the resulting object.  It signals an error if @tt{NAME} is absent.
The key distinction between @tt{:} and @tt{.} is that a colon expression does
not pre-apply methods, signal an exception on @seclink["s:mutables"
@tt{Mutables}], or automatically unwrap @seclink["s:placeholders"
@tt{Placeholder}] values.  This has value in more sophisticated patterns, like
those used by Pyret's generic equality and @tt{tostring} algorithms.

@subsection[#:tag "s:extend-expr"]{Extend Expressions}

The extend expression consists of an base expression and a list of fields to
extend it with:

@justcode{
extend-expr: expr "." "{" fields "}"
}

The extend expression first evaluates @tt{expr} to a value @tt{val}, and then
creates a new object with all the fields of @tt{val} and @tt{fields}.  If a
field is present in both, the new field is used.

Examples:

@justcode{
check:
  o = {x : "original-x", y: "original-y"}
  o2 = o.{x : "new-x", z : "new-z"}
  o2.x is "new-x"
  o2.y is "original-y"
  o2.z is "new-z"
end
}

@subsection[#:tag "s:if-expr"]{If Expressions}

@subsection[#:tag "s:cases-expr"]{Cases Expressions}

A cases expression consists of a datatype (in parentheses), an expression to
inspect (before the colon), and a number of branches.  It is intended to be
used in a structure parallel to a data definition.

@justcode{
cases-expr: "cases" (PARENSPACE|PARENNOSPACE) expr-check ")" expr-target ":"
    cases-branch*
    ["|" "else" "=>" block]
  "end"
cases-branch: "|" NAME [args] "=>" block
}

A @tt{cases} expression first evaluates @tt{expr-check} to get a checker for
the type of the value to branch on.  Typically, this should be the name of a
datatype like @tt{list.List}.  The expression then evaluates @tt{expr-target},
and checks if it matches the given annotation.  If it does not, an exception is
raise, otherwise it proceeds to match it against the given cases.

@margin-note{ Under the hood, @tt{cases} is calling the @tt{_match} function of
the target value, which is defined for each variant and performs the
appropriate dispatch.}

Cases should use the names of the variants of the given data type as the
@tt{NAME}s of each branch.  The branches will be tried, in order, checking if
the given value is an instance of that variant.  If it matches, the fields of
the variant are bound, in order, to the provided @tt{args}, and the right-hand
side of the @tt{=>} is evaluated in that extended environment.  An exception
results if the wrong number of arguments are given.

An optional @tt{else} clause can be provided, which is evaluated if no cases
match.  If no @tt{else} clause is provided, a default is used that raises an
exception.

For example, a cases expression on lists looks like:

@justcode{
check:
  result = cases(list.List) [1,2,3]:
    | empty => "empty"
    | link(f, r) => "link"
  end
  result is "link"

  result2 = cases(list.List) [1,2,3]:
    | empty => "empty"
    | else => "else"
  end
  result2 is else

  result3 = cases(list.List) empty:
    | empty => "empty"
    | else => "else"
  end
  result3 is "empty"
end
}

@subsection[#:tag "s:for-expr"]{For Expressions}

For expressions consist of the @tt{for} keyword, followed by a list of
@tt{binding from expr} clauses in parentheses, followed by a block:

@justcode{
for-expr: "for" expr PARENNOSPACE [for-bind-elt* for-bind] ")" return-ann ":"
  block
"end"
for-bind-elt: for-bind ","
for-bind: binding "from" binop-expr
}

The for expression is just syntactic sugar for a
@seclink["s:lam-expr"]{@tt{lam-expr}} and a @seclink["s:app-expr"]{@tt{app-expr}}.  An expression

@justcode{
for fun-expr(arg1 :: ann1 from expr1, ...) -> ann-return:
  block
end
}

is equivalent to:

@justcode{
fun-expr(fun(arg1 :: ann1, ...) -> ann-return: block end, expr1, ...)
}

Using a @tt{for-expr} can be a more natural way to call, for example, list
iteration functions because it puts the identifier of the function and the
value it draws from closer to one another.  Use of @tt{for-expr} is a matter of
style; here is an example that compares @tt{fold} with and without @tt{for}:

@justcode{
for fold(sum from 0, number from [1,2,3,4]):
  sum + number
end

fold(fun(sum, number): sum + number end, [1,2,3,4])
}

@subsection[#:tag "s:try-expr"]{Try Expressions}



@section[#:tag "s:annotations"]{Annotations}

Annotations in Pyret express intended types values will have at runtime.
They appear next to identifiers anywhere a @tt{binding} is specified in the
grammar, and if an annotation is present adjacent to an identifier, the program
is compiled to raise an error if the value bound to that identifier would
behave in a way that violates the annotation.  The annotation provides a
@emph{guarantee} that either the value will behave in a particular way, or the
program will raise an exception.

@subsection[#:tag "s:name-ann"]{Name Annotations}

Some annotations are simply names.  For example, a
@seclink["s:data-expr"]{@tt{data declaration}} binds the name of the
declaration as a value suitable for use as a name annotation.  There are
built-in name annotations, too:

@justcode{
Any
Number
String
Bool
}

Each of these names represents a particular type of runtime value, and using
them in annotation position will check each time the identifier is bound that
the value is of the right type.

@justcode{
x :: Number = "not-a-number"
# Error: expected Number and got "not-a-number"
}

@tt{Any} is an annotation that allows any value to be used.  It semantically
equivalent to not putting an annotation on an identifier, but it allows a
program to clearly signal that no restrictions are intended for the identifier
it annotates.

@subsection[#:tag "s:arrow-ann"]{Arrow Annotations}

An arrow annotation is used to describe the behavior of functions.  It consists
of a list of comma-separated argument types followed by an ASCII arrow and
return type:

@justcode{
arrow-ann: (PARENSPACE|PARENNOSPACE) arrow-ann-elt* ann "->" ann ")"
arrow-ann-elt: ann ","
}

When an arrow annotation appears in a binding, that binding position
@emph{wraps} values that are bound to it in a new function.  This new function,
when applied, checks that the arguments match the provided list of argument
annotations, and if any fail, raises an exception.  When (or if) the function
finishes evaluating to a value, it checks that the resulting value matches the
@emph{return annotation}, which appears after the arrow, again signalling an
exception if it does not.

@justcode{
# This line does not cause an error yet
f :: (Number -> String) = fun(x): x + 1 end

# This raises an exception "Expected Number, got 'not-a-number'"
f("not-a-number")

# This raises an exception "Expected String, got 4"
f(3)
}

@section{Complete Grammar}

@(apply joincode (rest (file->lines (collection-file-path "lang/grammar.rkt" "pyret"))))
