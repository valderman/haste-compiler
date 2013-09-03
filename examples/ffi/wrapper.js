// jQuery object selector. Note that the FFI marshals Int, Double, JSString,
// Ptr-wrapped types and other primitives for us, so there's no need to do
// anything special with such arguments or return values.
var js_jquery = $;

function js_click(obj, callback) {
    // Callbacks must be called using the A() Haste RTS operation, taking a
    // function and an array of arguments. Note that arguments must be
    // explicitly wrapped ([0, evt.button] rather than just evt.button) and
    // that the argument list must always be end with a dummy argument;
    // in this case 0.
    obj.click(function(evt) {A(callback, [[0, evt.button], 0]);});
}

function js_hide(obj) {
    obj.hide();
}
