// TODO Get a correct polyfill for Object.values
function values(o) {
  a = [];
  for (k in o)
    a.push(o[k]);
  return a;
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

window.onload = function() {
  reset_button();
}

var gg; // The raw JSON received from the server; only for debugging.

function step_button() {
  $.get("step");
  get_model();
}

function reset_button() {
  $.get("reset");
  clearGraph();
  node.remove();
  link.remove();
  get_model();
}

function get_model() {
  $.get("get-model", function(data) {
    update_graph(JSON.parse(data));
    restart();
  });
}

function update_graph(g) {
  gg = g;
  $('#t').text(g.t);
  for (var i = 0; i < g.nodes.length; i++) {
    const node = g.nodes[i];
    //console.log(node);
    if (!graph.nodes.hasOwnProperty(node.id)) {
      //console.log("HERE", node.id);
      graph.nodes[node.id] = 
        Object.assign(node, {
          width: node.d3width * nodeWidthMultiplier,
          height: node.d3height * nodeHeightMultiplier,
          members: new Set(node.members),
          membersRecursive: new Set(node.membersRecursive)
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
}

function compareLengthMembersRecursive(node1, node2) {
  return node2.membersRecursive.size - node1.membersRecursive.size;
}

// Set up known HTML elements
var svg = d3.select("#ws"),
    width = +svg.attr("width"),
    height = +svg.attr("height");

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
                       .distance(function(l) {
                         if (l.source["tag?"] || l.target["tag?"])
                           return 70;
                         else
                           return 90;
                       })
                       .strength(0.05)
                       .id(function(d) { return d.id; }))
    .force('charge', d3.forceManyBody().strength(-2))
    .force('center', d3.forceCenter(width / 2, height / 2))
    .force('collide', forceRectCollide())
    .force('containment', forceContainment())
    .alphaDecay(1 - Math.pow(0.001, 1 / 100))
    .stop()
    .on('tick', ticked);


var forceLinks = []

//d3.json("g.json").then(function(g) {
//  graph = g
//  restart();
//})

function restart() {
  node = nodeg.selectAll("g").data(nodesArray, function(d) { return d.id; });

  nodeEnter = node.enter().append("g");
    
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
      .each(function(d) { this.classList.add("node", d.class); })
      .call(d3.drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended));

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

  simulation.nodes(nodesArray);
  simulation.force("link").links(graph.links);
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
  const ws = document.getElementById("ws")
  const bbox = ws.getBBox();
  ws.setAttribute("viewBox", (bbox.x-10) + " " + (bbox.y-10) +
                             " " + (bbox.width+20) + " " + (bbox.height+20));
  ws.setAttribute("width", (bbox.width+20) + "px");
  ws.setAttribute("height", (bbox.height+20) + "px");

}

function dragstarted(d) {
  if (!d3.event.active) simulation.alphaTarget(0.3).restart();
  d.fx = d.x;
  d.fy = d.y;
}

function dragged(d) {
  d.fx = d3.event.x;
  d.fy = d3.event.y;
}

function dragended(d) {
  if (!d3.event.active) simulation.alphaTarget(0);
  d.fx = null;
  d.fy = null;
}

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
          !node.membersRecursive.has(qnode.id)) {
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
          l = Math.max(10.0, Math.sqrt(dx * dx, dy * dy));
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
          !node.membersRecursive.has(qnode.id)) {
        var k = 200 * alpha;
        var dx = node.x - qnode.x;
        var dy = node.y - qnode.y;
        var xSpacing = (qnode.width + node.width) / 2;
        var ySpacing = (qnode.height + node.height) / 2;
        var absDx = Math.abs(dx);
        var absDy = Math.abs(dy);
        var l, lx, ly;

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
