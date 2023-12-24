function permutations(array){
    var result = [];
    if (array.length === 1) {
        result.push(array);
        return result;
    }
    for (var i = 0; i < array.length; i++) {
        var first = array[i];
        var rest = array.slice(0, i).concat(array.slice(i + 1));
        var inner = permutations(rest);
        for (var j = 0; j < inner.length; j++) {
        result.push([first].concat(inner[j]));
        }
    }
    return result;
}
function concat(array){
    return array.reduce(function (acc, x) { return acc.concat(x); }, []);
}

function permutations(array){

    if (array.length === 0) {
        return [[]];
    }
    return concat(array.map(function (x, i) {
        var rest = array.slice(0, i).concat(array.slice(i + 1));
        return permutations(rest).map(function (p) { return [x].concat(p); });
    }));
}