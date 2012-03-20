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

function jsShow(val, _) {
    return [1,0,val.toString()];
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