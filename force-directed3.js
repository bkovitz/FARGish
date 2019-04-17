var graph = {
  "nodes": {},
  "links": [],  // TODO this right
  "t": undefined
}

var gg;

function step_button() {
  $.get("step", function(data) {
    update_graph(JSON.parse(data));
    restart();
  });
}

function reset_button() {
  $.get("reset", function(data) {
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
        Object.assign(node, { width: node.d3width * nodeWidthMultiplier,
                              height: node.d3height * nodeHeightMultiplier });
    }
  }
  graph.links = g.links;
}

// Set up known HTML elements
var svg = d3.select("#ws"),
    width = +svg.attr("width"),
    height = +svg.attr("height");

//svg.attr('height', '100%')
//    .attr('width', '100%')
//    .attr('viewBox', '0 0 960 300')
//    .attr('preserveAspectRatio', 'xMinYMin');

var linkg = svg.append("g")
    .attr("class", "links");

var link = linkg.selectAll("line");

var nodeg = svg.append("g")
    .attr("class", "nodes");

var node = nodeg.selectAll("g");

//var color = d3.scaleOrdinal(d3.schemeCategory10);

//const nodeWidth = 60;
//const nodeHeight = 40;

const nodeWidthMultiplier = 30;
const nodeHeightMultiplier = 20;

var simulation = d3.forceSimulation()
    .force('link', d3.forceLink()
                       .distance(400)  // 90
                       .id(function(d) { return d.id; }))
    .force('charge', d3.forceManyBody())
    .force('center', d3.forceCenter(width / 2, height / 2))
    .force('collide', forceRectCollide())
    .stop()
    .on('tick', ticked);


var forceLinks = []

//d3.json("g.json").then(function(g) {
//  graph = g
//  restart();
//})

function restart() {
  link = linkg.selectAll("line").data(graph.links)
    .enter().append("line")
      .attr("stroke-width", function(d) { return Math.sqrt(d.value); })
      .merge(link);

  var nodes_array = Object.values(graph.nodes);
  node = nodeg.selectAll("g").data(nodes_array, function(d) { return d.id; });

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
      .style("text-anchor", "middle")
      .classed('svgText', true)
      .attr('x', function(d) { return d.width / 2; })
      .attr('y', function(d) { return d.height / 2; })

  node = nodeEnter.merge(node);

  //node = node.merge(node);
  //link = link.merge(link);

//  //This makes a tool-tip
//  node.append("title")
//      .text(function(d) { return d.id; });

  simulation.nodes(nodes_array);
  //forceLinks = JSON.parse(JSON.stringify(graph.links))
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
  var nodes_array = Object.values(graph.nodes);
  //var qtree = d3.quadtree(nodes_array, getX, getY);

//  for (var i = 0; i < nodes_array.length; i++) {
//    qtree.visit(collide(qtree, nodes_array[i]));
//  }

  link
      .attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });

  //node.attr('x', getX).attr('y', getY);
  node
      .attr("transform", function(d) {
        //console.log(d);
        return "translate(" + (d.x - d.width/2) + "," + (d.y - d.height/2) + ")";
      })

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
    var qtree = d3.quadtree(nodes, getX, getY);
    for (var i = 0; i < nodes.length; i++) {
      qtree.visit(collide(qtree, nodes[i]))
    }
  }

  force.initialize = function(_) {
    nodes = _;
  }

  return force;
}

// Based on http://bl.ocks.org/natebates/273b99ddf86e2e2e58ff
function collide(qtree, node) {
  return function(quadnode, x1, y1, x2, y2) {
    var updated = false;

    //console.log(quadnode);
    if (isLeafNode(quadnode) && quadnode.data !== node) {
      var qnode = quadnode.data;
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

        // The one that's barely within the bounds probably triggered the
        // collision.
        if (Math.abs(lx) > Math.abs(ly)) {
          lx = 0;
        } else {
          ly = 0;
        }

        console.log(node.id, qnode.id, l, lx, ly);

        node.x -= dx *= lx;
        node.y -= dy *= ly;

        qtree.remove(qnode);
        qnode.x += dx;
        qnode.y += dy;
        qtree.add(qnode);

        updated = true;
      }
    }
    return updated;
  };
}

function isLeafNode(quadnode) {
  return !quadnode.length;
}
