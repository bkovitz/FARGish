// fargcyto.js  --  Javascript code to display a FARG model
//
// Goes with fargcyto.html. Works with cytoscript.js and cola.js.

lightAquamarine = '#89D3AD'
lightUnsaturatedBlue = '#BAECE9'

function getCircularReplacer() {
  const seen = new WeakSet();
  return (key, value) => {
    if (typeof value === "object" && value !== null) {
      if (seen.has(value)) {
        return;
      }
      seen.add(value);
    }
    return value;
  };
};


var cy;  // The cytoscape.js object

var stringStyleSheet = 'node { background-color: cyan; }'

// Layout object that gives constraints and other parameters to cola.js
/*
const layout = {
  name: 'cola',
  directed: true,
  fit: false,
  handleDisconnected: false,
  // alignment: function(n) { return alignments[n.id()]; },
  //gapInequalities: gaps,
  //refresh: 0.00000001
  refresh: 10
}
*/

var alignments = {
  // bricks
  4: {y: 0},
  6: {y: 0},
  8: {y: 0},
  10: {y: 0},
  12: {y: 0},
  14: {y: 0},
  // target
  2: { y: 100 }
}

var alignments1 = {
  // bricks
  //4: {y: 0},
  //6: {y: 0},
  //8: {y: 0},
  10: {y: 0, x: 0},
  12: {y: 0, x: 100},
  14: {y: 0, x: 200},
  // target
  //2: { y: 100 }
}

var alignments2 = {
  // bricks
  4: {y: 0},
  6: {y: 0},
  8: {y: 0},
  //10: {y: 0},
  //12: {y: 0},
  //14: {y: 0},
  // target
  //2: { y: 100 }
}


var layout;  // The current cytoscape.js layout object

function makeLayout(n) {
  var alignmentFunc;
  if (n) {
    alignmentFunc = function(n) { return alignments2[n.id()]; }
  } else {
    alignmentFunc = function(n) { return alignments1[n.id()]; }
  }
  return {
    name: 'cola',
    directed: true,
    fit: false,
    nodeDimensionsIncludeLabels: true,
    //nodeSpacing: 100,
    animate: true,
    avoidOverlaps: true,
    nodeDimensionsIncludeLabels: true,
    //maxSimulationTime: 8000,
    //handleDisconnected: false,
    //alignment: alignmentFunc,
    /*
    gapInequalities: [
      // bricks below target
      { axis: 'y', right: cy.$id(4), left: cy.$id(2), gap: 50 },
      { axis: 'y', right: cy.$id(6), left: cy.$id(2), gap: 50 },
      { axis: 'y', right: cy.$id(8), left: cy.$id(2), gap: 50 },
      { axis: 'y', right: cy.$id(10), left: cy.$id(2), gap: 50 },
      { axis: 'y', right: cy.$id(12), left: cy.$id(2), gap: 50 },
      { axis: 'y', right: cy.$id(14), left: cy.$id(2), gap: 50 },

      // bricks same y
      { axis: 'y', right: cy.$id(4), left: cy.$id(6), gap: 0, equality: true },
      { axis: 'y', right: cy.$id(4), left: cy.$id(8), gap: 0, equality: true },
      { axis: 'y', right: cy.$id(4), left: cy.$id(10), gap: 0, equality: true },
      { axis: 'y', right: cy.$id(4), left: cy.$id(12), gap: 0, equality: true },
      { axis: 'y', right: cy.$id(4), left: cy.$id(14), gap: 0, equality: true },
    ],
    */
    /*
    rawConstraints: [
      {
        type: 'alignment', axis: 'y', offsets: [
          { 'node': cy.$id(4), 'offset': 0 },
          { 'node': cy.$id(6), 'offset': 0 },
          { 'node': cy.$id(8), 'offset': 0 },
        ]
      },
    ],
    */
    //refresh: 0.00000001
    refresh: 1 //10
  };
}

// Runs the cola.js layout engine; call this after updating the graph.
function colaRun() {
  /* Known to animate continuously from run to run
  if (!layout)
    layout = cy.layout(makeLayout());
  layout.stop();
  layout.run();
  */

  const d = makeLayout();
  //console.log(JSON.stringify(d)); //DEBUG
  layout = cy.layout(d);
  layout.run();

  //cy.elements("#10,#12,#14").layout(makeLayout(0)).run();
  //cy.elements("#4,#6,#8").layout(makeLayout(1)).run();

  //cy.elements().layout(layout).run();
}

// Button function: retrieves the FARG model from the server
function getModel() {
  //cy.center(1);
  $.get('getModel', updateGraphFromJSON);
}

// Button function: advances the FARG model one timestep
function stepButton() {
  $.get("step", updateGraphFromJSON);
}

// Button function: clears the graph and resets the FARG model
function resetButton() {
  cy.elements().remove();
  $.get("reset", updateGraphFromJSON);
}

function updateGraphFromJSON(data) {
  updateGraph(JSON.parse(data));
  colaRun();
}

var gg; // We save the last dict from the server here so we can look at
        // it in the Chrome debugger.

function updateGraph(g) {
  gg = g;

  $('#t').text(g.t);
  $('#nodecount').text(g.nodes.length);
  $('#edgecount').text(g.links.length);

  // Import nodes from server graph
  for (const sn of g.nodes) {  // sn = node as represented on the server
    var node = cy.$id(sn.id)   // node = cytoscape.js's representation of node
    if (node.empty()) {
      const d = {
        group: 'nodes',
        data: {
          id: sn.id,
          label: sn['display-name'],
          parent: sn.memberOf,
          class: sn.class
        },
        position: { x: (sn.id * 111) % 17 * 5, y: (sn.id * 119) % 17 * 5 }
      };
      //console.log(JSON.stringify(d)); //DEBUG
      node = cy.add(d);
      //cy.$id(sn.id).addClass(sn.class);
      node.addClass(sn.class);
      node.on('tap', function(e) { console.log(nodeInfo(e.target)); });
    }
  }

  // Import edges from server graph
  for (const se of g.links) {  // se = edge as represented on the server
    sid = makeEdgeId(
      se.source, se.source_port_label, se.target, se.target_port_label
    );
    edge = cy.$id(sid);
    if (edge.empty()) {
      const d = {
        group: 'edges',
        data: { id: sid,
                source: parseInt(se.source),
                source_port_label: se.source_port_label,
                target: parseInt(se.target),
                target_port_label: se.target_port_label,
                weight: Math.max(se.weight * 10, 0.5),
                class: se.class
              }
      };
      //console.log(JSON.stringify(d)); //DEBUG
      edge = cy.add(d);
      if (se.class)
        edge.addClass(se.class);
      edge.on('tap', function(event) { console.log(edgeInfo(event.target)); });
      colaRun();
    }
  }
}

function edgeInfo(edge) {
  return edge.id() + '\n' + JSON.stringify(edge.data()) + '\n\n' +
         nodeInfo(edge.source()) + '\n\n' +
         nodeInfo(edge.target());
}

function nodeInfo(node) {
  return node.id() + '\n' + JSON.stringify(node.data());
}

function makeEdgeId(source, source_port_label, target, target_port_label) {
  return (
    '' + source + '.' + source_port_label + '.' + target +
    '.' + target_port_label
  );
}

window.onload = function () {
  //document.getElementById('abb').addEventListener('click', abb);

  cy = cytoscape({
    container: document.getElementById('cy'),
    //layout: layout,
    randomize: false,
    style: [
      {
        selector: 'node',
        style: {
          shape: 'rectangle',
          //'background-color': 'red',
          'background-color': '#f0e0d0',
          label: 'data(label)',
          'text-valign': 'center',
          //width: 'data(width)'
          width: 'label',
          'background-color': '#E7D97F'   // manila
          //width: 40
        }
      },
      {
        selector: 'node.Brick',
        style: {
          'background-color': '#E17139'   // orange brickish
        }
      },
      {
        selector: 'node.Target',
        style: {
          'background-color': '#AEE983'  // light green
        }
      },
      {
        selector: 'node.Workspace',
        style: {
          'shape': 'round-rectangle',
          'background-color': '#F7E387'  // light manila
        }
      },
      {
        selector: 'node.Plus,node.Times',
        style: {
          'background-color': '#D6B1FE'  // lavender
        }
      },
      {
        selector: 'node.Avail',
        style: {
          'background-color': '#FFFF04'  // yellow
        }
      },
      { // agents
        selector: 'node.NumericalRelationScout,node.WantFullySourced,node.OperandsScout',
        style: {
          'background-color': lightUnsaturatedBlue
        }
      },
      {
        selector: 'node.OperandView',
        style: {
          'background-color': lightAquamarine
        }
      },
      {
        selector: 'node:parent',
        style: {
          'text-valign': 'top',
          'background-opacity': 0.333,
          'border-width': 0,
          'shape': 'round-rectangle'
        },
      },
      {
        selector: 'edge',
        style: {
          'curve-style': 'bezier',
          'target-arrow-shape': 'triangle-backcurve',
          'line-color': 'black',
          'target-arrow-color': 'black',
          'width': "data(weight)"
        }
      },
      {
        selector: 'edge.Support',
        style: {
          'display': 'none',
          'curve-style': 'bezier',
          'target-arrow-shape': 'triangle-backcurve',
          'line-color': 'green',
          'target-arrow-color': 'green',
          'width': "data(weight)"
        }
      },
      {
        selector: 'edge.Member',
        style: { 'display': 'none' }
      },
      {
        selector: 'edge.View',
        style: {
          'line-color': lightAquamarine,
          'target-arrow-color': lightAquamarine,
          'width': 2
        }
      },
      {
        selector: 'edge.Agent',
        style: {
          'line-color': lightUnsaturatedBlue,
          'target-arrow-color': lightUnsaturatedBlue,
          'width': 2
        }
      }
    ],
  });
  getModel();
}
