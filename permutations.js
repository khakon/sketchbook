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

function permutations(array){

    if (array.length === 0) {
        return [[]];
    }
    return array.map(function (x, i)  {
        console.log(x); 
        var rest = array.filter(function (_, index) { return index !== i; });
        console.log(rest); 
        return [[x].concat(permutations(rest))]; 
    });
}