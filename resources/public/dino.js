globalObject = {a: 1, b: 2, c: 3};

makeDino = function () {
  var dino = {};
  dino.desc = "blue dino-dog";
  dino.says = function(number) {
    var result = "";
    for (i=0; i<(number-1); i++) { result += "Ruff-"; }
    result += "Ruff!";
    return result };
  return dino;
};
