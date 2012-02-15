/* Compare
   If v is an ADT, return its constructor. If it's a primitive value, return
   the value itself.
*/
function C(v) {
    return v instanceof Array ? v[0] : v;
}

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.

   When a thunk is evaluated, by reading the member 'x' of the "pointer," the
   closure is evaluated and the getter removed, to be replaced with the value
   returned by the thunk, and the getter finally returns the return value of
   the closure.
*/
function T(f) {
    var t = {t: true};
    t.__defineGetter__("x", function() {
        delete this['x'];
        delete this['t'];
        this.x = f();
        return this.x;
    });
    return t;
}

/* Eval
   Evaluate the given thunk t into head normal form.
   Start by evaluating the thunk; if a value is returned, return it.
   Things get more complex if the thunk returns another thunk; in that case,
   evaluate that thunk and update t to point to the value it returns; loop
   until we stop getting thunks for result.
   This way, t is always memoized fully evaluated into HNF. If we couple this
   with making Call f x => T(function() {return f(T(x));}) we get tail call
   elimination without any AST mangling. We also get slow function calls.
*/
function E(t) {
    if(t.x == undefined) {
        return t;
    }
    for(; t.x.t; t.x = t.x.x);
    return t.x;
}

/* Data constructor
   Create a data constructor function for the constructor with the given tag.
   The strict parameter contains an array of strictness notations; 1/true in
   position n means that the nth argument to the constructor should be
   evaluated before the constructor is applied.
*/
function D(tag, strict) {
    var data = [tag].concat(Array.prototype.slice.call(arguments, 2));
    for(var i = 1; i < data.length; ++i) {
        if(strict[i-1]) {
            E(data[i]);
        }
    }
    var f = function() {
        for(var i in arguments) {
            data.push(arguments[i]);
            if(strict[data.length-2]) {
                E(data[data.length-1]);
            }
        }
        if(data.length > strict.length) {
            return data;
        }
        return f;
    };
    return data.length > strict.length ? data : f;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function addC(a, b) {
    var x = a+b;
    return [1, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [1, x & 0xffffffff, x < -2147483648];
}
