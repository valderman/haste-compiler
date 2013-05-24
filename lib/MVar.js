// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return [1, realWorld, {empty: true}];
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [1, st, 0, undefined];
    } else {
        mv.empty = true;
        mv.x = null;
        return [1, realWorld, 1, mv.x];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    mv.empty = true;
    mv.x = null;
    return [1,realWorld,mv.x];
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
    return [1,realWorld];
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return [1, realWorld, 0];
    } else {
        mv.empty = false;
        mv.x = val;
        return [1, realWorld, 1];
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return [1, realWorld, mv.empty ? 1 : 0];
}
