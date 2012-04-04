function jsAlert(val,_) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
    return [1,0];
}

function jsLog(val,_) {
    console.log(val);
    return [1,0];
}

function jsPrompt(str,_) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return [1,0,val == undefined ? '' : val.toString()];
}

function jsEval(str,_) {
    var x = eval(str);
    return [1,0,x == undefined ? '' : x.toString()];
}

function isNull(obj,_) {
    return [1,0,[obj === null]];
}

function jsRead(str,_) {
    return [1,0,Number(str)];
}

function jsShowI(val, _) {return [1,0,val.toString()];}
function jsShow(val, _) {
    var ret = val.toString();
    return [1,0,val == Math.round(val) ? ret + '.0' : ret];
}

function jsSetCB(id, evt, cb, _) {
    var elem = document.getElementById(id);
    if(elem != null) {
        if(elem.addEventListener) {
            elem.addEventListener(evt, function() {cb(0);}, false);
            return [1,0,true];
        } else if(elem.attachEvent) {
            elem.attachEvent('on'+evt, function() {cb(0);});
            return [1,0,true];
        }
    }
    return [1,0,false];
}

function jsSetTimeout(msecs, cb, _) {
    window.setTimeout(function() {cb(0);}, msecs);
    return [1,0];
}

// Round a Float/Double.
function rintDouble(d, _) {
    return [1,0,Math.round(d)];
}
var rintFloat = rintDouble;

// Degenerate versions of u_iswspace, u_iswalnum and u_iswalpha.
function u_iswspace(c, _) {
    return [1,0,[c==9 || c==10 || c==13 || c==32]];
}

function u_iswalnum(c, _) {
    return [1,0,[(c >= 48 && c <= 57) || u_iswalpha(c)[0]]];
}

// [a-zA-ZåäöÅÄÖ]
function u_iswalpha(c, _) {
    return [1,0,[(c >= 65 && c <= 90) || (c >= 97 && c <= 122) ||
                 c == 229 || c == 228 || c == 246 ||
                 c == 197 || c == 196 || c == 214]];
}

function jsGet(elem, prop, _) {
    var elem = document.getElementById(elem);
    return [1,0,elem[prop]];
}

function jsSet(elem, prop, val, _) {
    var elem = document.getElementById(elem);
    elem[prop] = val;
    return [1,0];
}

function jsFind(elem, _) {
    if(document.getElementById(elem)) {
        return [1,0,true];
    }
    return [1,0,false];
}

function jsRand(_) {
    return [1,0,Math.random()];
}

// Concatenate a Haskell list of JS strings
function jsCat(strs, _) {
    var arr = [];
    while(strs[0] != 1) {
        arr.push(E(E(strs)[1])[1]);
        strs = strs[2];
    }
    return [1,0,arr.join('')];
}
