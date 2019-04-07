var svg = d3.select("#ws"),
    width = +svg.attr("width"),
    height = +svg.attr("height");

var color = d3.scaleOrdinal(d3.schemeCategory10);

const nodeWidth = 60;
const nodeHeight = 40;

var simulation = d3.forceSimulation()
    .force("link", d3.forceLink().id(function(d) { return d.id; }))
    .force("charge", d3.forceManyBody())
    .force("center", d3.forceCenter(width / 2, height / 2));

var graph = {}

d3.json("g.json").then(function(g) {
  graph = g

  var link = svg.append("g")
      .attr("class", "links")
    .selectAll("line")
    .data(graph.links)
    .enter().append("line")
      .attr("stroke-width", function(d) { return Math.sqrt(d.value); });

  var node = svg.append("g")
      .attr("class", "nodes")
    .selectAll("g")
    .data(graph.nodes)
    .enter().append("g")
    
  node.append("rect")
      .attr('width', nodeWidth)
      .attr('height', nodeHeight)
      .attr('rx', 1.5)
      .attr('ry', 1.5)
      .attr('transform', 'translate=(-15,-10)')
      //.attr("fill", function(d) { return color(d.group); })
      .each(function(d) { this.classList.add("node", d.class); })
      .call(d3.drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended));

  node.append("text")
      .text(function(d) {
        console.log(d);
        return d["display-name"] + "";
      })
      .style("text-anchor", "middle")
      .classed('svgText', true)
      .attr('x', nodeWidth / 2)
      .attr('y', nodeHeight / 2)

//  //This makes a tool-tip
//  node.append("title")
//      .text(function(d) { return d.id; });

  simulation
      .nodes(graph.nodes)
      .on("tick", ticked);

  simulation.force("link")
      .distance(80)
      .links(graph.links);

  function ticked() {
    link
        .attr("x1", function(d) { return d.source.x + nodeWidth / 2; })
        .attr("y1", function(d) { return d.source.y + nodeHeight / 2; })
        .attr("x2", function(d) { return d.target.x + nodeWidth / 2; })
        .attr("y2", function(d) { return d.target.y + nodeHeight / 2; });

    node
        .attr("transform", function(d) {
          return "translate(" + d.x + "," + d.y + ")";
        })
  }
});

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
