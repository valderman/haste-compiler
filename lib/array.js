// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    var b = new ArrayBuffer(n);
    b.i8  = new Int8Array(b);
    b.i16 = new Int16Array(b);
    b.i32 = new Int32Array(b);
    b.w8  = new Uint8Array(b);
    b.w16 = new Uint16Array(b);
    b.w32 = new Uint32Array(b);
    b.f32 = new Float32Array(b);
    b.f64 = new Float64Array(b);
    return b;
}
