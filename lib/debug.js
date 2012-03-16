function jsprint(val, theWorld) {
    print(val);
    return [1,0];
}

function jsget(theWorld) {
    var l = readline();
    return [1,0,Number(l)];
}

function jsid(val) {
    return [1,0,val];
}
