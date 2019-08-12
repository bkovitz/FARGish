// TODO Get a correct polyfill for Object.values
function values(o) {
  a = [];
  for (k in o)
    a.push(o[k]);
  return a;
}

function eqSets(a, b) {
  if (a.size !== b.size)
    return false;
  for (var x of a)
    if (!b.has(x))
      return false;
  return true;
}

function index(d) {
  return d.index;
}

function find(nodeById, nodeId) {
  var node = nodeById.get(nodeId);
  if (!node) throw new Error("missing: " + nodeId);
  return node;
}

function constant(x) {
  return function() {
    return x;
  };
}

function jiggle() {
  return (Math.random() - 0.5) * 1e-6;
}

function isTag(node) {
  return node["tag?"];
}

function area(node) {
  return node.height * node.width;
}

var graph = {}
var nodesArray = [] // graph.nodes as an array, sorted by order in which
                    // they should be drawn (i.e. increasing Z-order).

function clearGraph() {
  graph = {
    "nodes": {},
    "links": [],  // TODO this right
    "t": undefined
  };
  nodesArray = [];
}

var scaleSlider;

window.onload = function() {
  scaleSlider = document.getElementById("scale-slider");
  scaleSlider.oninput = function() {
    const ws = document.getElementById("ws")
    const bbox = ws.getBBox();
    ws.setAttribute("viewBox",       Math.min(-500, (bbox.x-10)) +
                               " " + Math.min(-500, (bbox.y-10)) +
                               " " + (this.value*(2*bbox.width+20)) +
                               " " + (this.value*(2*bbox.height+20)));
      
  };
  reset_button();
}

var gg; // The raw JSON received from the server; only for debugging.

function step_button() {
  $.get("step", updateGraphFromJSON);
}

function reset_button() {
  clearGraph();
  node.remove();
  link.remove();
  $.get("reset", updateGraphFromJSON);
}

//function get_model() {
//  $.get("get-model", function(data) {
//    updateGraph(JSON.parse(data));
//    restart();
//  });
//}

function updateGraphFromJSON(data) {
  updateGraph(JSON.parse(data));
}

function updateGraph(g) {
  gg = g;
  $('#t').text(g.t);
  for (var i = 0; i < g.nodes.length; i++) {
    const node = g.nodes[i];
    if (!graph.nodes.hasOwnProperty(node.id)) {
      graph.nodes[node.id] = 
        Object.assign(node, {
          width: node.d3width * nodeWidthMultiplier,
          height: node.d3height * nodeHeightMultiplier,
          members: new Set(node.members),
          membersRecursive: new Set(node.membersRecursive),
          memberOf: new Set(node.memberOf)
        });
    }
  }
  // Poor man's topological sort, putting containers first so they draw
  // before, hence under, the nodes that they contain.
  nodesArray = values(graph.nodes).sort(compareLengthMembersRecursive);

  graph.links = g.links.filter(l =>
    !graph.nodes[l.source].members.has(l.target)
    &&
    !graph.nodes[l.target].members.has(l.source));

  restart();
}

function compareLengthMembersRecursive(node1, node2) {
  return node2.membersRecursive.size - node1.membersRecursive.size;
}

var zoom = d3.zoom()
    .on("zoom", function() {
      console.log(d3.event);
      svg.attr("transform", d3.event.transform);
    });

// Set up known HTML elements
var svg = d3.select("svg")
    .attr("width", "100%")
    .attr("height", "100%")
    .call(zoom)
    .append("g");
    //.attr("transform", "translate(200, 200) scale(0.6)");
//    width = +svg.attr("width"),
//    height = +svg.attr("height");

//svg.call(zoom.transform, d3.zoomIdentity.translate(200, 200).scale(0.6));

//var zoom = d3.zoom()
//  .on("zoom", function () {
//    console.log("HERE");
//    svg.attr("transform", d3.event.transform)
//  });

//zoom.translateTo(svg, -200, -200);
//zoom.scaleTo(svg, 0.6);
//
//svg.call(zoom)
//  .append("g");

//svg.attr('height', '100%')
//    .attr('width', '100%')
//    .attr('viewBox', '0 0 960 300')
//    .attr('preserveAspectRatio', 'xMinYMin');

var nodeg = svg.append("g")
    .attr("class", "nodes");

var node = nodeg.selectAll("g");

var linkg = svg.append("g")
    .attr("class", "links");

var link = linkg.selectAll("line");


// Panning and zooming

//var zoom = d3.zoom()
//  .on("zoom", zoomed);
//
//function zoomed() {
//  svg.attr("transform", d3.event.transform);
//}


// TODO Make three groups, in this order: container nodes, links, non-container
// nodes. Then the lines should show up correctly on the screen.


function isContainer(node) {
  return node.members.size > 0;
}

//var color = d3.scaleOrdinal(d3.schemeCategory10);

//const nodeWidth = 60;
//const nodeHeight = 40;

const nodeWidthMultiplier = 30;
const nodeHeightMultiplier = 20;

var simulation = d3.forceSimulation()
    .force('link', d3.forceLink()
//                       .distance(function(l) {
//                         if (l.source["tag?"] || l.target["tag?"])
//                           return 100;
//                         else if (eqSets(l.source.memberOf, l.target.memberOf))
//                           return nodeWidthMultiplier;
//                         else
//                           return 200;
//                       })
                       //.strength(0.00)
                       //.iterations(2)
                       .id(function(d) { return d.id; }))
    //.force('link', forceCustomLink().id(function(d) { return d.id; }))
    .force('charge', d3.forceManyBody().strength(0))
    //.force('center', d3.forceCenter(width / 2, height / 2))
    .force('collide', forceRectCollide())
    .force('containment', forceContainment())
    .alphaDecay(1 - Math.pow(0.001, 1 / 300))
    .stop()
    .on('tick', ticked);

var defaultLinkStrength = simulation.force('link').strength();


//d3.json("g.json").then(function(g) {
//  graph = g
//  restart();
//})

function restart() {
  node = nodeg.selectAll("g").data(nodesArray, function(d) { return d.id; });

  nodeEnter = node.enter().append("g")
      .call(d3.drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended));
    
  nodeEnter.append("rect")
      //.attr('width', nodeWidth)
      //.attr('height', nodeHeight)
      .attr('width', function(d) { return d.width; })
      .attr('height', function(d) { return d.height; })
//      .attr('transform', function(d) {
//        return 'translate(' + (-d.width / 2) + ',' + (-d.height / 2) + ')';
//      })
      .attr('rx', 1.5)
      .attr('ry', 1.5)
      //.attr('transform', 'translate(-15,-10)')
      //.attr("fill", function(d) { return color(d.group); })
      .each(function(d) { this.classList.add("node", d.class); });

  nodeEnter.append("text")
      .text(function(d) {
        //console.log(d);
        return d["display-name"] + "";
      })
      .style("text-anchor", function (d) {
                              if (isContainer(d))
                                return "start";
                              else
                                return "middle";
                            })
      .classed('svgText', true)
      .attr('x', function(d) { if (isContainer(d))
                                 return 5;
                               else
                                 return d.width / 2;
                             })
      .attr('y', function(d) { if (isContainer(d))
                                 return 17;
                               else
                                 return d.height / 2 + 4;
                             })

  node = nodeEnter.merge(node);

  link = linkg.selectAll("line").data(graph.links)
    .enter().append("line")
      .attr("stroke-width", function(d) { return Math.sqrt(d.value); })
      .merge(link);

  //node = node.merge(node);
  //link = link.merge(link);

//  //This makes a tool-tip
//  node.append("title")
//      .text(function(d) { return d.id; });

  //simulation.force('charge').strength(-30); // HACK
  simulation.nodes(nodesArray);
  simulation.force("link")
      .links(graph.links)
  //simulation.force('link').links(graph.links);
  var ls = graph.links;
  var n = node.data().length;
  var m = ls.length;
  var count = new Array(n);
  for (let i = 0; i < m; i++) {
    l = ls[i];
    count[l.source.index] = (count[l.source.index] || 0) + 1;
    count[l.target.index] = (count[l.target.index] || 0) + 1;
  }

  simulation.force('link')
      .distance(function(l) {
        if (l.source.memberOf.size > 0 && l.target.memberOf.size > 0 &&
            eqSets(l.source.memberOf, l.target.memberOf))
          return nodeWidthMultiplier;
        else if (isTag(l.source) && count[l.source.index] < 2 ||
                 isTag(l.target) && count[l.target.index] < 2)
          return 80;
        else if (count[l.source.index] >= 2 || count[l.target.index] >= 2)
          return 300;
        else
          return 100;
      })
      .strength(function(l, i) {
        if (l.source.members.size > 0 || l.target.members.size > 0)
          return 0;
        else if (l.source.memberOf.size > 0 && l.target.memberOf.size > 0) {
          if (eqSets(l.source.memberOf, l.target.memberOf))
            return 0.1;
          else
            return 0.0;
        } else
//        } else if (count[l.source.index] >= 2 || count[l.target.index] >= 2)
//          return 0.02;
//        else
//          return 0.5;
          return Math.pow(defaultLinkStrength(l, i) / 3, 2);
      });
  simulation.alpha(1).restart();
}

function addn(x) {
  var n = Object.assign({}, graph.nodes[0]);
  n.id = x;
  n["display-name"] = x;
  graph.nodes.push(n);
  restart();
}

function getX(d) { return d.x; }
function getY(d) { return d.y; }

function ticked() {
  //node.attr('x', getX).attr('y', getY);
  node
      .attr("transform", function(d) {
        //console.log(d);
        return "translate(" + (d.x - d.width/2) + "," + (d.y - d.height/2) + ")";
      })

  link
      .attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });

  // Update size of SVG so scroll-bars appear when needed
//  const ws = document.getElementById("ws")
//  const bbox = ws.getBBox();
//  ws.setAttribute("viewBox",       Math.min(-500, (bbox.x-10)) +
//                             " " + Math.min(-500, (bbox.y-10)) +
//                             " " + (2*bbox.width+20) +
//                             " " + (2*bbox.height+20));
//  //ws.setAttribute("viewBox", "-500 -500 2000 2000");
//  ws.setAttribute("width", (bbox.width+20) + "px");
//  ws.setAttribute("height", (bbox.height+20) + "px");

}

var oldChargeStrength;

function dragstarted(d) {
  oldChargeStrength = simulation.force('charge').strength();
  if (!d3.event.active) {
    simulation.force('charge').strength(0);
    simulation.alphaTarget(0.3).restart();
  }
  d.fx = d.x;
  d.fy = d.y;
  for (let k of d.membersRecursive) {
    n = graph.nodes[k];
    n.fx = n.x;
    n.fy = n.y;
  }
}

function dragged(d) {
//  d.fx = d3.event.x;
//  d.fy = d3.event.y;
  //console.log(d3.event.dx, d3.event.dy);
  d.fx += d3.event.dx;
  d.fy += d3.event.dy;
//  d3.select(this).attr("transform", function(d) {
//    return "translate(" + [d.fx - d.width/2, d.fy - d.height/2] + ")";
//  });
  for (let k of d.membersRecursive) {
    n = graph.nodes[k];
    n.fx += d3.event.dx;
    n.fy += d3.event.dy;
  }
}

function dragended(d) {
  if (!d3.event.active) {
    simulation.alphaTarget(0);
    simulation.on('end', function() {
      simulation.force('charge').strength(oldChargeStrength);
    });
  }
  //if (!d3.event.active) simulation.alpha(0);
  d.fx = null;
  d.fy = null;
  for (let k of d.membersRecursive) {
    n = graph.nodes[k];
    n.fx = null;
    n.fy = null;
  }
}

//// Based on https://github.com/d3/d3-force/blob/master/src/link.js by
//// Michael Bostock.
//function customLinkForce() {
//  var links = [],
//      nodes,
//      id = index;
//
//  function force(alpha) {
//    var n = links.length;
//    for (var i = 0, link, source, target; i < n; i++) {
//      link = links[i], source = link.source, target = link.target;
//      if (isTag(source) && isTag(target)) {
//        // nothing
//      } else if (!isTag(source) && !isTag(target)) {
//        // nothing
//      } else {
//        // exactly one of the nodes is a tag
//        var 
//      }
//    }
//  }
//
//  function initialize() {
//    if (!nodes) return;
//
//    var i,
//        n = nodes.length,
//        m = links.length,
//        nodeById = new Map(nodes.map((d, i) => [id(d, i, nodes), d])),
//        link;
//
//    for (i = 0; i < m; ++i) {
//      link = links[i], link.index = i;
//      if (typeof link.source !== "object")
//        link.source = find(nodeById, link.source);
//      if (typeof link.target !== "object")
//        link.target = find(nodeById, link.target);
//    }
//  }
//
//  force.initialize = function(_) {
//    nodes = _;
//    initialize();
//  }
//
//  force.links = function(_) {
//    return arguments.length ? (links = _, initialize(), force) : links;
//  }
//
//  force.id = function(_) {
//    return arguments.length ? (id = _, force) : id;
//  }
//
//  return force;
//}

function forceRectCollide() {
  var nodes;

  function force(alpha) {
    if (alpha < 1000) {
      var qtree = d3.quadtree(nodes, getX, getY);
      for (var i = 0; i < nodes.length; i++) {
        qtree.visit(oldCollide(qtree, alpha, nodes[i]))
      }
    }
  }

  force.initialize = function(_) {
    nodes = _;
  }

  return force;
}

function forceContainment() {
  var nodes;

  function force(alpha) {
    if (alpha < 1000) {
      var qtree = d3.quadtree(nodes, getX, getY);
      for (var i = 0; i < nodes.length; i++) {
        qtree.visit(containment(qtree, alpha, nodes[i]))
      }
    }
  }

  force.initialize = function(_) {
    nodes = _;
  }

  return force;
}

// Based on http://bl.ocks.org/natebates/273b99ddf86e2e2e58ff
function oldCollide(qtree, alpha, node) {
  return function(quadnode, x1, y1, x2, y2) {
    var updated = false;

    if (isLeafNode(quadnode) && quadnode.data !== node) {
      var qnode = quadnode.data;
      if (!node["tag?"] &&
          !qnode["tag?"] &&
          !qnode.membersRecursive.has(node.id) &&
          !node.membersRecursive.has(qnode.id) &&
          eqSets(node.memberOf, qnode.memberOf)) {
        var dx = node.x - qnode.x;
        var dy = node.y - qnode.y;
        var xSpacing = (qnode.width + node.width) / 2;
        var ySpacing = (qnode.height + node.height) / 2;
        var absDx = Math.abs(dx);
        var absDy = Math.abs(dy);
        var l, lx, ly;

        if (absDx < xSpacing && absDy < ySpacing) {
          // The lower bound here prevents collisions between nearly coincident
          // nodes from causing them to be moved gigantic distances.
          //l = Math.max(1.0, Math.min(2.0, Math.sqrt(dx * dx, dy * dy))) * alpha;
          l = Math.max(1.0, Math.sqrt(dx * dx + dy * dy))
          lx = (absDx - xSpacing) / l;
          ly = (absDy - ySpacing) / l;

          // The axis that's barely within the bounds probably triggered the
          // collision.
          if (Math.abs(lx) > Math.abs(ly)) {
            lx = 0;
          } else {
            ly = 0;
          }

          //console.log(node.id, qnode.id, l, lx, ly);

          node.x -= dx *= lx;
          node.y -= dy *= ly;

          qtree.remove(qnode);
          qnode.x += dx;
          qnode.y += dy;
          qtree.add(qnode);

          updated = true;
        }
      }
    }
    return updated;
  };
}

function collide(qtree, alpha, node) {
  return function(quadnode, x1, y1, x2, y2) {
    var updated = false;

    if (isLeafNode(quadnode) && quadnode.data !== node) {
      var qnode = quadnode.data;
      if (!node["tag?"] &&
          !qnode["tag?"] &&
          !qnode.membersRecursive.has(node.id) &&
          !node.membersRecursive.has(qnode.id) &&
          eqSets(node.memberOf, qnode.memberOf)) {
        var k = 200 * alpha;
        var dx = node.x - qnode.x;
        var dy = node.y - qnode.y;
        var xSpacing = (qnode.width + node.width) / 2;
        var ySpacing = (qnode.height + node.height) / 2;
        var absDx = Math.abs(dx);
        var absDy = Math.abs(dy);

        if (absDx < xSpacing && absDy < ySpacing) { // collision!
          if (node.x + xSpacing < qnode.x) {
            node.vx -= k;
            qnode.vx += k;
          } else {
            node.vx += k;
            qnode.vx -= k;
          }

          if (node.y + ySpacing < qnode.y) {
            node.vy -= k;
            qnode.vy += k;
          } else {
            node.vy += k;
            qnode.by -= k;
          }
          qtree.remove(qnode);
          qtree.add(qnode);

          updated = true;
        }
      }
    }
    return updated;
  };
}
function containment(qtree, alpha, node) {
  return function(quadnode, x1, y1, x2, y2) {
    var updated = false;

    if (isLeafNode(quadnode) && quadnode.data !== node) {
      var k = 50 * alpha;
      var qnode = quadnode.data;
      if (!node["tag?"] &&
          !qnode["tag?"] &&
          node.members.has(qnode.id)) {
        if (qnode.x - qnode.width/2 < node.x - node.width/2) {
          qnode.x = node.x - node.width/2 + qnode.width/2;
          //qnode.vx = Math.max(k, qnode.vx);
        } else if (qnode.x + qnode.width/2 > node.x + node.width/2) {
          qnode.x = node.x + node.width/2 - qnode.width/2;
          //qnode.vx = Math.min(-k, qnode.vx);
        }

        if (qnode.y - qnode.height/2 < node.y - node.height/2) {
          qnode.y = node.y - node.height/2 + qnode.height/2;
          //qnode.vy = Math.max(k, qnode.vy);
        } else if (qnode.y + qnode.height/2 > node.y + node.height/2) {
          qnode.y = node.y + node.height/2 - qnode.height/2;
          //qnode.vy = Math.min(-k, qnode.vy);
        }

        updated = true;
      }
    }
    return updated;
  };
}

function isLeafNode(quadnode) {
  return !quadnode.length;
}

////////////////////////////////////////////////////////////////////////

// Like d3.forceLink, except tags can't drag other nodes.
function forceCustomLink(links) {
  var id = index,
      strength = defaultStrength,
      strengths,
      distance = constant(30),
      distances,
      nodes,
      count,
      bias,
      iterations = 1;

  if (links == null) links = [];

  function defaultStrength(link) {
    return 1 / Math.min(count[link.source.index], count[link.target.index]);
  }

  function force(alpha) {
    for (var k = 0, n = links.length; k < iterations; ++k) {
      for (var i = 0, link, source, target, x, y, l, b; i < n; ++i) {
        link = links[i], source = link.source, target = link.target;
        x = target.x + target.vx - source.x - source.vx || jiggle();
        y = target.y + target.vy - source.y - source.vy || jiggle();
        l = Math.sqrt(x * x + y * y);
        l = (l - distances[i]) / l * alpha * strengths[i];
        x *= l, y *= l;
        target.vx -= x * (b = bias[i]);
        target.vy -= y * b;
        source.vx += x * (b = 1 - b);
        source.vy += y * b;
      }
    }
  }

  function initialize() {
    if (!nodes) return;

    var i,
        n = nodes.length,
        m = links.length,
        nodeById = new Map(nodes.map((d, i) => [id(d, i, nodes), d])),
        link;

    for (i = 0, count = new Array(n); i < m; ++i) {
      link = links[i], link.index = i;
      if (typeof link.source !== "object") link.source = find(nodeById, link.source);
      if (typeof link.target !== "object") link.target = find(nodeById, link.target);
      count[link.source.index] = (count[link.source.index] || 0) + 1;
      count[link.target.index] = (count[link.target.index] || 0) + 1;
    }

    // CUSTOMIZED PART: changed bias from Bostock's original
    for (i = 0, bias = new Array(m); i < m; ++i) {
      link = links[i];
      if (isTag(link.source) && !isTag(link.target))
        bias[i] = 0.0;
      else if (!isTag(link.source) && isTag(link.target))
        bias[i] = 1.0;
      else
        bias[i] = count[link.source.index] /
                    (count[link.source.index] + count[link.target.index]);
    }

    strengths = new Array(m), initializeStrength();
    distances = new Array(m), initializeDistance();
  }

  function initializeStrength() {
    if (!nodes) return;

    for (var i = 0, n = links.length; i < n; ++i) {
      strengths[i] = +strength(links[i], i, links);
    }
  }

  function initializeDistance() {
    if (!nodes) return;

    for (var i = 0, n = links.length; i < n; ++i) {
      distances[i] = +distance(links[i], i, links);
    }
  }

  force.initialize = function(_) {
    nodes = _;
    initialize();
  };

  force.links = function(_) {
    return arguments.length ? (links = _, initialize(), force) : links;
  };

  force.id = function(_) {
    return arguments.length ? (id = _, force) : id;
  };

  force.iterations = function(_) {
    return arguments.length ? (iterations = +_, force) : iterations;
  };

  force.strength = function(_) {
    return arguments.length ? (strength = typeof _ === "function" ? _ : constant(+_), initializeStrength(), force) : strength;
  };

  force.distance = function(_) {
    return arguments.length ? (distance = typeof _ === "function" ? _ : constant(+_), initializeDistance(), force) : distance;
  };

  return force;
}
