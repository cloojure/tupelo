
// doesn't seem to work with figwheel-main
goog.provide( 'wilmaPhony' );  // 'wilmaPhony' is a "virtual-namespace" that doesn't really exist

wilmaPhony.stats  = {  lipstick : "red",  height : 5.5 };  // both are the same
wilmaPhony.stats2 = { "lipstick": "red", "height": 5.5 };

wilmaPhony.makeWilma = function () {
  var wilma = {};
  wilma.desc = "patient housewife";
  wilma.says = function(name) {
    result = "Hello, " + name;
    return result };
  return wilma;
};
