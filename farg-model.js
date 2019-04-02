var svg = d3.select("svg"),
    width = +svg.attr("width"),
    height = +svg.attr("height");

var fill = d3.scale.category20();

var force = d3.layout.force()
    .size([width, height])
    .nodes([{}]) // a single node
    .linkDistance(30)
    .charge(-60)
    .on("tick", tick);

svg.on("mousemove", mousemove)
    .on("mousedown", mousedown);

svg.append("rect")
  .classed("workspace", true)
  .attr("width", width)
  .attr("height", height);

var nodes = force.nodes(),
    links = force.links(),
    node = svg.selectAll(".node"),
    link = svg.selectAll(".link");

var cursor = svg.append("circle")
    .attr("r", 30)
    .attr("transform", "translate(-100,-100)")
    .attr("class", "cursor");

restart();

function add_node(x, y) {
  var node = {x: x, y: y};
  nodes.push(node);

  // add links to any nearby nodes
  nodes.forEach(function(target) {
    var x = target.x - node.x,
        y = target.y - node.y;
    if (Math.sqrt(x * x + y * y) < 30) {
      links.push({source: node, target: target});
    }
  })
}

function add_link(source, target) {
  links.push({source: source, target: target});
}

function mousemove() {
  cursor.attr("transform", "translate(" + d3.mouse(this) + ")");
}

function mousedown() {
  var point = d3.mouse(this);
  add_node(point[0], point[1]);

  restart();
}

function tick() {
  link.attr("x1", function(d) { return d.source.x; })
      .attr("y1", function(d) { return d.source.y; })
      .attr("x2", function(d) { return d.target.x; })
      .attr("y2", function(d) { return d.target.y; });

  node.attr("x", function(d) { return d.x; })
      .attr("y", function(d) { return d.y; });
}

function restart() {
  link = link.data(links);

  link.enter().insert("line", ".node")
      .attr("class", "link");

  node = node.data(nodes);

  node.enter().insert("rect", ".cursor")
      .attr("class", "node")
      .attr("width", 25)
      .attr("height", 25)
      .attr("r", 25)
      .call(force.drag);

  force.start();
}

// Buttons

function step_button() {
  add_node(200, 200);
  restart();
}
