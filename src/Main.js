const prestoDayum = require("presto-ui").doms;
const parseParams = require("presto-ui").helpers.web.parseParams;


function domAll(elem) {
  for (var i = 0; i < elem.children.length; i++) {
    elem.children[i] = domAll(elem.children[i]);
  }
  return prestoDayum(elem.type, elem.props, elem.children);
}

function applyProp(element, attribute) {
  var prop = {
    id: element.props.id
  }
  prop[attribute.value0] = attribute.value1.value0;
  Android.runInUI(parseParams("linearLayout", prop, "set"));
}

window.removeChild = removeChild;
window.addChild = addChild;
window.addAttribute = addAttribute;
window.removeAttribute = removeAttribute;
window.updateAttribute = updateAttribute;
window.addAttribute = addAttribute;
window.insertDom = insertDom;

function removeChild (child, parent, index) {
  console.log("removeChild");
  Android.removeView(child.props.id);
  console.log(child, parent, index);
}

function addChild (child, parent, index) {
  console.log("addChild");
  Android.addViewToParent(parent.props.id, domAll(child), index);
  console.log(child, parent, index);
}

window.__screenSubs = {};

function addAttribute (element, attribute) {
  applyProp(element, attribute);
}

function removeAttribute (element, attribute) {
  console.log(element, attribute);
}

function updateAttribute (element, attribute) {
  applyProp(element, attribute);
}

exports.click = function() {}

function insertDom(root) {
  return function(dom) {
    return function() {
      root.props.height = "match_parent";
      root.props.width = "match_parent";
      root.props.id = "GodFather";
      root.type = "relativeLayout";

      root.children.push(dom);
      dom.parentNode = root;
      window.N = root;

      Android.Render(domAll(root));
    }
  }
}

exports.move = function(sub) {
  addEventListener("keydown", function(e) {
    sub({ keyCode: e.keyCode })
  })
}

exports.randomInt = function (min) {
  return function (max) {
    return Math.floor(Math.random() * (max + 1)) + min;
  }
}
