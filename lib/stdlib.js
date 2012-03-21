function jsAlert(val,_) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
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
    if(elem == null) {
        return [1,0,[false]];
    } else {
        elem[evt] = cb;
        return [1,0,[true]];
    }
}

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
