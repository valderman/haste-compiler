// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    return {
        off:  addr.off + off,
        b:    addr.b,
        i8:   addr.i8,
        i16:  addr.i16,
        i32:  addr.i32,
        w8:   addr.w8,
        w16:  addr.w16,
        w32:  addr.w32,
        f32:  addr.f32,
        f64:  addr.f64
    };
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr[type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr[type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {return a.b == b.b && a.off == b.off;}
function addrLT(a, b) {return a.off < b.off;}
function addrGT(a, b) {return a.off > b.off;}
