function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
    return [1,realWorld];
}

function jsLog(val) {
    console.log(val);
    return [1,realWorld];
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return [1,realWorld,val == undefined ? '' : val.toString()];
}

function jsEval(str) {
    var x = eval(str);
    return [1,realWorld,x == undefined ? '' : x.toString()];
}

function isNull(obj) {
    return [1,realWorld,[obj === null]];
}

function jsRead(str) {
    return [1,realWorld,Number(str)];
}

function jsShowI(val) {return [1,realWorld,val.toString()];}
function jsShow(val) {
    var ret = val.toString();
    return [1,realWorld,val == Math.round(val) ? ret + '.0' : ret];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n') {
                A(cb,[[1,k.keyCode], realWorld]);
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {A(cb,[[1,x.button], realWorld]);};
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[1,x.keyCode], realWorld]);};
        break;        
    default:
        fun = function() {A(cb,[realWorld]);};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return [1,realWorld,true];
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return [1,realWorld,true];
    }
    return [1,realWorld,false];
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {A(cb,[realWorld]);}, msecs);
    return [1,realWorld];
}

// Round a Float/Double.
function rintDouble(d) {
    return [1,realWorld,Math.round(d)];
}
var rintFloat = rintDouble;

// Degenerate versions of u_iswspace, u_iswalnum and u_iswalpha.
function u_iswspace(c) {
    return [1, realWorld, c==9 || c==10 || c==13 || c==32];
}

function u_iswalnum(c) {
    return [1, realWorld, (c >= 48 && c <= 57) || u_iswalpha(c)[2]];
}

// [a-zA-ZåäöÅÄÖ]
function u_iswalpha(c, _) {
    return [1,0, (c >= 65 && c <= 90) || (c >= 97 && c <= 122) ||
                  c == 229 || c == 228 || c == 246 ||
                  c == 197 || c == 196 || c == 214];
}

function jsGet(elem, prop) {
    return [1,realWorld,elem[prop].toString()];
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
    return [1,realWorld];
}

function jsGetStyle(elem, prop) {
    return [1,realWorld,elem.style[prop].toString()];
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
    return [1,realWorld];
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
    return [1,realWorld];
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
    return [1,realWorld];
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,realWorld,[2,[1,e]]];
    }
    return [1,realWorld,[1]];
}

function jsCreateElem(tag) {
    return [1,realWorld,document.createElement(tag)];
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,realWorld,[2,[1,elem]]];
        }
        elem = elem.previousSibling;
    }
    return [1,realWorld,[1]];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,realWorld,[2,[1,elem.childNodes[i]]]];
        }
    }
    return [1,realWorld,[1]];
}

function jsGetChildren(elem) {
    var children = [1];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [2, [1,elem.childNodes[i]], children];
        }
    }
    return [1,realWorld,children];
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 2) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
    return [1,realWorld];
}

function jsAppendChild(child, container) {
    container.appendChild(child);
    return [1,realWorld];
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
    return [1,realWorld];
}

function jsRand() {
    return [1,realWorld,Math.random()];
}

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0] != 1) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return [1,realWorld,arr.join(sep)];
}

// Escape all double quotes in a string
function jsUnquote(str) {
    return [1,realWorld,str.replace(/"/, '\\"')];
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [1,realWorld,[1]];
    }
    return [1,realWorld,[2,hs]];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [1, [1, jsRead(obj)[2]]];
    case 'string':
        return [2, [1, obj]];
        break;
    case 'boolean':
        return [3, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [4, arr2lst(obj, 0)];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [1];
            for(var i in ks) {
                xs = [2, [1, [1,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [5, xs];
        }
    }
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [1];
    }
    return [2, toHS(arr[elem]), T(function() {return arr2lst(arr,elem+1);})]
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);
    xhr.setRequestHeader('Cache-control', 'no-cache');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                A(cb,[[1,xhr.responseText], realWorld]);
            } else {
                A(cb,[[1,""], realWorld]); // Nothing
            }
        }
    }
    xhr.send(postdata);
    return [1,realWorld];
}

function u_towlower(charCode) {
    return [1, realWorld, String.fromCharCode(charCode).toLowerCase().charCodeAt(0)];
}

function u_towupper(charCode) {
    return [1, realWorld, String.fromCharCode(charCode).toUpperCase().charCodeAt(0)];
}
