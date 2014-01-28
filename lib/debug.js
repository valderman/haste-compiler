/* debug.js
   Debugging utilities, tracing, etc.
 */

if(typeof console != 'undefined') {
    var __h_debug = function(x) {console.log(x)};
} else {
    var __h_debug = alert;
}

/* Trace something with arguments and a return value */
function __h_trace(msg, args, res) {
    var str = "TRACE:     " + msg + "\n"
            + "  ARGS:    " + args + "\n"
            + "  RETURNS: " + res;
    __h_debug(str);
    return res;
}
