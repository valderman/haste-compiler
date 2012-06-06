/* Eval
   Evaluate the given thunk t into head normal form; trampoline version.
   Start by evaluating the thunk; if a value is returned, return it.
   Things get more complex if the thunk returns another thunk; in that case,
   evaluate that thunk and update t to point to the value it returns; loop
   until we stop getting thunks for result.
   This way, t is always memoized fully evaluated into HNF. If we couple this
   with making Call f x => T(function() {return f(T(x));}) we get tail call
   elimination without any AST mangling. We also get slow function calls.
*/
function E(t) {
    if(t instanceof Thunk) {
        if(t.f) {
            t.x = t.f();
            t.f = 0;
            while(t.x.f) {
                t.x = t.x.f();
            }
        }
        return t.x;
    }
    return t;
}
