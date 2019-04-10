// Set up known HTML elements
var svg = d3.select("#ws"),
    width = +svg.attr("width"),
    height = +svg.attr("height");

var linkg = svg.append("g")
    .attr("class", "links");

var link = linkg.selectAll("line");

var nodeg = svg.append("g")
    .attr("class", "nodes");

var node = nodeg.selectAll("g");

//var color = d3.scaleOrdinal(d3.schemeCategory10);

const nodeWidth = 60;
const nodeHeight = 40;

var simulation = d3.forceSimulation()
    .force("link", d3.forceLink()
                       .distance(90)
                       .id(function(d) { return d.id; }))
    .force("charge", d3.forceManyBody().strength(-10))
    .force("center", d3.forceCenter(width / 2, height / 2))
    .stop()
    .on("tick", ticked);


var graph = {}

var forceLinks = []

d3.json("g.json").then(function(g) {
  graph = g
  restart();
})

function restart() {
  
  link = linkg.selectAll("line").data(graph.links)
    .enter().append("line")
      .attr("stroke-width", function(d) { return Math.sqrt(d.value); })
      .merge(link);

  node = nodeg.selectAll("g").data(graph.nodes, function(d) { return d.id; })

  nodeEnter = node.enter().append("g")
    
  nodeEnter.append("rect")
      .attr('width', nodeWidth)
      .attr('height', nodeHeight)
      .attr('rx', 1.5)
      .attr('ry', 1.5)
      //.attr('transform', 'translate(-15,-10)')
      //.attr("fill", function(d) { return color(d.group); })
      .each(function(d) { this.classList.add("node", d.class); })
      .call(d3.drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended))

  nodeEnter.append("text")
      .text(function(d) {
        //console.log(d);
        return d["display-name"] + "";
      })
      .style("text-anchor", "middle")
      .classed('svgText', true)
      .attr('x', nodeWidth / 2)
      .attr('y', nodeHeight / 2)

  node = nodeEnter.merge(node);

  //node = node.merge(node);
  //link = link.merge(link);

//  //This makes a tool-tip
//  node.append("title")
//      .text(function(d) { return d.id; });

  simulation.nodes(graph.nodes);
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

function ticked() {
  link
      .attr("x1", function(d) { return d.source.x + nodeWidth / 2; })
      .attr("y1", function(d) { return d.source.y + nodeHeight / 2; })
      .attr("x2", function(d) { return d.target.x + nodeWidth / 2; })
      .attr("y2", function(d) { return d.target.y + nodeHeight / 2; });

  node
      .attr("transform", function(d) {
        //console.log(d);
        return "translate(" + d.x + "," + d.y + ")";
      })
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
